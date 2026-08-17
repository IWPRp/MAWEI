# Quality-control helpers for Metro Atlanta water-energy flows.
#
# Nothing here changes a flow value. These functions only measure, report and
# record. They are the regression oracle for the pipeline: run them before and
# after any change and compare.
#
# Entry points
#   check_node_balance()  per node x county x year x unit inflow/outflow residual
#   balance_report()      console summary + CSV, returns violations invisibly
#   check_manifest()      every artefact named in the manifest exists, case-exact
#   log_drop()            record how many rows a filter removed, and why
#   compare_runs()        diff two output trees CSV-by-CSV
#
# Hassan Niazi, Aug 2026, PNNL


# helpers ----

qc_dir_ensure <- function() {
  if (!dir.exists(QC_DIR)) dir.create(QC_DIR, recursive = TRUE, showWarnings = FALSE)
  QC_DIR
}

# ExtraNotes: group columns are discovered, not assumed. The metro frames have no
# `county` and the energy-water frame carries mixed `units`, so the balance has to
# adapt to whichever of those columns is actually present.
qc_group_cols <- function(df, by_county = TRUE) {
  cols <- character(0)
  if (by_county && "county" %in% names(df)) cols <- c(cols, "county")
  cols <- c(cols, "year")
  if ("units" %in% names(df)) cols <- c(cols, "units")
  cols
}


# node-level mass balance ----
#
# For every node that is genuinely intermediate (appears as BOTH a source and a
# target somewhere in the frame), inflow must equal outflow. Pure sources and
# pure sinks are terminal by construction and are reported but never flagged.
#
# Returns one row per node x group with:
#   inflow, outflow, residual (inflow - outflow), rel (residual / scale),
#   role ("intermediate" | "source" | "sink"), violation (logical)
check_node_balance <- function(df, label = "flows", tol = BALANCE_TOL,
                               by_county = TRUE, exempt = character(0)) {

  stopifnot(all(c("source", "target", "value") %in% names(df)))
  grp <- qc_group_cols(df, by_county)

  all_sources <- unique(df$source)
  all_targets <- unique(df$target)
  intermediates <- intersect(all_sources, all_targets)

  inflow <- df %>%
    group_by(across(all_of(grp)), node = target) %>%
    summarise(inflow = sum(value, na.rm = TRUE), .groups = "drop")

  outflow <- df %>%
    group_by(across(all_of(grp)), node = source) %>%
    summarise(outflow = sum(value, na.rm = TRUE), .groups = "drop")

  bal <- full_join(inflow, outflow, by = c(grp, "node")) %>%
    mutate(inflow  = replace_na(inflow, 0),
           outflow = replace_na(outflow, 0),
           role = case_when(
             node %in% intermediates ~ "intermediate",
             node %in% all_sources   ~ "source",
             TRUE                    ~ "sink"),
           residual = inflow - outflow,
           # ExtraNotes: scale by the larger leg, not by inflow. Scaling by inflow
           # makes a pure-source node look infinitely wrong (inflow == 0).
           scale = pmax(inflow, outflow),
           rel = if_else(scale > 0, residual / scale, 0),
           # a node with zero throughput on both legs is a phantom from
           # complete(); it is not a balance failure
           violation = role == "intermediate" &
                       scale > 0 &
                       abs(rel) > tol &
                       !(node %in% exempt)) %>%
    arrange(desc(abs(rel))) %>%
    mutate(label = label, .before = 1)

  bal
}


# Console summary plus a CSV audit trail. Returns the violating rows invisibly so
# a caller can gate on nrow() == 0.
balance_report <- function(df, label = "flows", tol = BALANCE_TOL,
                           by_county = TRUE, exempt = character(0),
                           write = TRUE) {

  bal <- check_node_balance(df, label, tol, by_county, exempt)
  viol <- bal %>% filter(violation)

  n_nodes <- n_distinct(bal$node)
  n_inter <- n_distinct(bal$node[bal$role == "intermediate"])

  cat(sprintf("\n== balance: %s ==\n", label))
  cat(sprintf("   %d nodes (%d intermediate) over %d rows\n",
              n_nodes, n_inter, nrow(bal)))

  # Exempted nodes are still reported, just not counted as failures, so a documented
  # limitation stays visible instead of disappearing from the audit.
  if (length(exempt) > 0) {
    ex <- bal %>% filter(node %in% exempt, role == "intermediate", scale > 0)
    if (nrow(ex) > 0) {
      ex_sum <- ex %>% group_by(node) %>%
        summarise(worst_rel = rel[which.max(abs(rel))],
                  abs_total = sum(abs(residual)), .groups = "drop") %>%
        arrange(desc(abs(worst_rel)))
      cat(sprintf("   EXEMPT %d node(s), reported but not gated:\n", nrow(ex_sum)))
      for (i in seq_len(nrow(ex_sum))) {
        cat(sprintf("     %-42s worst %+8.2f%%  sum|resid| %.4g\n",
                    ex_sum$node[i], ex_sum$worst_rel[i] * 100, ex_sum$abs_total[i]))
      }
    }
  }

  if (nrow(viol) == 0) {
    cat(sprintf("   PASS  all intermediate nodes balance within %.2f%%\n", tol * 100))
  } else {
    cat(sprintf("   FAIL  %d node-group(s) exceed %.2f%%, across %d distinct node(s)\n",
                nrow(viol), tol * 100, n_distinct(viol$node)))
    worst <- viol %>%
      group_by(node) %>%
      summarise(n = n(),
                worst_rel = rel[which.max(abs(rel))],
                worst_abs = residual[which.max(abs(rel))],
                .groups = "drop") %>%
      arrange(desc(abs(worst_rel))) %>%
      head(15)
    for (i in seq_len(nrow(worst))) {
      cat(sprintf("     %-42s n=%-4d worst %+8.2f%%  (%+.4g)\n",
                  worst$node[i], worst$n[i],
                  worst$worst_rel[i] * 100, worst$worst_abs[i]))
    }
  }

  if (write) {
    qc_dir_ensure()
    f <- file.path(QC_DIR, paste0("balance_", label, ".csv"))
    write_csv(bal, f)
    cat(sprintf("   -> %s\n", f))
  }

  invisible(viol)
}


# dropped-row ledger ----
#
# Wrap any filtering step so row loss is never silent:
#   df <- log_drop(df, df$value > 0, "energy gen: non-positive net generation")
# ExtraNotes: several real bugs in this pipeline were silent filter() drops
# (negative net generation, rejected < 0, NA counties). Route new filters through
# this so the loss shows up in outputs/qc/dropped_rows.csv.
.qc_drop_log <- new.env(parent = emptyenv())
.qc_drop_log$rows <- list()

log_drop <- function(df, keep, reason, show_values = TRUE) {
  keep <- keep & !is.na(keep)
  n_before <- nrow(df)
  dropped <- df[!keep, , drop = FALSE]
  n_dropped <- nrow(dropped)

  if (n_dropped > 0) {
    val_note <- ""
    if (show_values && "value" %in% names(dropped)) {
      val_note <- sprintf(" sum(value)=%.6g", sum(dropped$value, na.rm = TRUE))
    }
    message(sprintf("  [drop] %-58s %d/%d rows%s", reason, n_dropped, n_before, val_note))
    .qc_drop_log$rows[[length(.qc_drop_log$rows) + 1L]] <- tibble(
      reason = reason, n_dropped = n_dropped, n_before = n_before,
      value_dropped = if ("value" %in% names(dropped)) sum(dropped$value, na.rm = TRUE) else NA_real_
    )
  }
  df[keep, , drop = FALSE]
}

drop_report <- function(write = TRUE) {
  if (length(.qc_drop_log$rows) == 0) {
    cat("\n== dropped rows ==\n   none recorded\n")
    return(invisible(tibble()))
  }
  out <- bind_rows(.qc_drop_log$rows) %>%
    group_by(reason) %>%
    summarise(n_dropped = sum(n_dropped),
              value_dropped = sum(value_dropped, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(n_dropped))
  cat("\n== dropped rows ==\n")
  for (i in seq_len(nrow(out))) {
    cat(sprintf("   %-58s %6d rows  %+.6g\n",
                out$reason[i], out$n_dropped[i], out$value_dropped[i]))
  }
  if (write) {
    qc_dir_ensure()
    write_csv(out, file.path(QC_DIR, "dropped_rows.csv"))
  }
  invisible(out)
}

drop_log_reset <- function() {
  .qc_drop_log$rows <- list()
  invisible(NULL)
}


# artefact / manifest checks ----
#
# ExtraNotes: the case-exact test is the point. macOS resolves "dekalb" to
# "DeKalb" happily; GitHub Pages (Linux) does not. list.files() gives us the
# on-disk spelling so we can catch it here instead of in production.
check_manifest <- function(manifest_path = file.path(SAVE_DIR, "manifest.json"),
                           verbose = TRUE) {
  if (!file.exists(manifest_path)) {
    cat(sprintf("\n== manifest ==\n   MISSING  %s\n", manifest_path))
    return(invisible(tibble(path = character(), issue = character())))
  }
  mf <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)
  files <- mf$files
  root <- dirname(manifest_path)

  issues <- purrr::map_dfr(files$path, function(p) {
    full <- file.path(root, p)
    if (!file.exists(full)) return(tibble(path = p, issue = "missing"))
    # case-exactness: compare against the real directory listing
    real <- list.files(dirname(full), all.files = FALSE)
    if (!(basename(full) %in% real)) return(tibble(path = p, issue = "case mismatch"))
    tibble(path = character(), issue = character())
  })

  cat(sprintf("\n== manifest ==\n   %d entries\n", nrow(files)))
  if (nrow(issues) == 0) {
    cat("   PASS  every entry resolves case-exactly on disk\n")
  } else {
    cat(sprintf("   FAIL  %d problem(s)\n", nrow(issues)))
    if (verbose) for (i in seq_len(nrow(issues))) {
      cat(sprintf("     %-12s %s\n", issues$issue[i], issues$path[i]))
    }
  }
  invisible(issues)
}


# run-to-run comparison ----
#
# Diff every CSV shared by two output trees. Used to prove that a refactor moved
# nothing, and to quantify the deltas when it deliberately did.
compare_runs <- function(dir_a, dir_b, tol = 1e-9) {
  csv_a <- list.files(dir_a, pattern = "\\.csv$", recursive = TRUE)
  csv_b <- list.files(dir_b, pattern = "\\.csv$", recursive = TRUE)

  cat(sprintf("\n== compare %s -> %s ==\n", dir_a, dir_b))
  only_a <- setdiff(csv_a, csv_b)
  only_b <- setdiff(csv_b, csv_a)
  if (length(only_a)) cat(sprintf("   only in A (%d): %s\n", length(only_a),
                                  paste(head(only_a, 8), collapse = ", ")))
  if (length(only_b)) cat(sprintf("   only in B (%d): %s\n", length(only_b),
                                  paste(head(only_b, 8), collapse = ", ")))

  shared <- intersect(csv_a, csv_b)
  out <- purrr::map_dfr(shared, function(f) {
    a <- suppressMessages(read_csv(file.path(dir_a, f), show_col_types = FALSE))
    b <- suppressMessages(read_csv(file.path(dir_b, f), show_col_types = FALSE))
    key <- intersect(names(a), c("county", "year", "source", "target", "units"))
    if (!("value" %in% names(a) && "value" %in% names(b)) || length(key) == 0) {
      return(tibble(file = f, rows_a = nrow(a), rows_b = nrow(b),
                    n_changed = NA_integer_, n_only_a = NA_integer_,
                    n_only_b = NA_integer_, max_abs_delta = NA_real_,
                    sum_a = NA_real_, sum_b = NA_real_))
    }
    j <- full_join(a %>% select(all_of(key), value_a = value),
                   b %>% select(all_of(key), value_b = value), by = key)
    tibble(file = f, rows_a = nrow(a), rows_b = nrow(b),
           n_changed = sum(!is.na(j$value_a) & !is.na(j$value_b) &
                             abs(j$value_a - j$value_b) > tol),
           n_only_a = sum(is.na(j$value_b)),
           n_only_b = sum(is.na(j$value_a)),
           max_abs_delta = suppressWarnings(max(abs(j$value_a - j$value_b), na.rm = TRUE)),
           sum_a = sum(a$value, na.rm = TRUE), sum_b = sum(b$value, na.rm = TRUE))
  })

  if (nrow(out)) {
    for (i in seq_len(nrow(out))) {
      flag <- if (isTRUE(out$n_changed[i] == 0 && out$n_only_a[i] == 0 &&
                         out$n_only_b[i] == 0)) "same" else "DIFF"
      cat(sprintf("   %-4s %-44s rows %5d->%5d  chg %5s  +%-5s -%-5s  sum %.6g -> %.6g\n",
                  flag, out$file[i], out$rows_a[i], out$rows_b[i],
                  out$n_changed[i], out$n_only_b[i], out$n_only_a[i],
                  out$sum_a[i], out$sum_b[i]))
    }
    qc_dir_ensure()
    write_csv(out, file.path(QC_DIR, "compare_runs.csv"))
  }
  invisible(out)
}


# RUN QC
# originally was run_qc.R
#
# # Data-only pipeline runner: builds every flow table, writes the CSVs, runs the
# mass-balance audit, and skips all 64 HTML widget renders.
#
# Purpose: a fast feedback loop for data-correctness work. A full run of
# R/flows_energy_water.R spends almost all of its wall time in
# htmlwidgets::saveWidget(); this script gets the same numbers in a fraction of
# the time so balance fixes can be iterated on quickly.
#
#   Rscript R/run_qc.R                      # audit only, writes nothing
#   MAWEI_SAVE_DIR=outputs/files_check/ Rscript R/run_qc.R --write-csv


args <- commandArgs(trailingOnly = TRUE)
WRITE_CSV <- "--write-csv" %in% args
# --analysis runs the ANALYSIS blocks inside the flows scripts. Off by default so the balance
# check stays fast, but available here because those blocks depend on intermediate objects and
# this runner is the cheapest way to build them: it skips plotting and artefact writing.
RUN_ANALYSIS <- "--analysis" %in% args

# Suppress artefact writing inside the sourced scripts; this runner decides what
# to write. Set via the environment so the flags survive each script's own
# re-source of functions.R.
Sys.setenv(MAWEI_SAVE_FILES = "0", MAWEI_MAKE_PLOT = "0",
           MAWEI_ANALYSIS = if (RUN_ANALYSIS) "1" else "0")

source("functions.R")

t0 <- Sys.time()
# ExtraNotes: reset BEFORE sourcing. log_drop() records during the pipeline build, so
# resetting afterwards wipes exactly the entries we want to see.
drop_log_reset()
message("== building water flows ==")
suppressMessages(suppressWarnings(source(paste0(SCRIPTS_DIR, "flows_water.R"))))
message("== building energy flows ==")
suppressMessages(suppressWarnings(source(paste0(SCRIPTS_DIR, "flows_energy.R"))))
message(sprintf("== pipeline built in %.1f s ==", as.numeric(difftime(Sys.time(), t0, units = "secs"))))


# the frames that become published artefacts ----
#
# ExtraNotes: energy metro/county are in EJ internally; the published CSVs carry
# whatever the save block wrote, so audit in EJ and let unit stamping be a
# separate concern.
targets <- list(
  energy_metro  = list(df = "en_fuel_gen_use_loss_all_trade_metro", by_county = FALSE),
  energy_county = list(df = "en_fuel_gen_use_loss_all_trade",       by_county = TRUE),
  water_metro   = list(df = "df_water_metro_linear_wSW_discharge_type", by_county = FALSE),
  water_county  = list(df = "df_sankey_county_pws_balanced",        by_county = TRUE)
)

# Nodes exempted from the balance gate, with the reason each is expected to fail.
# ExtraNotes: an exemption is a documented limitation, not a silenced bug. Every entry
# here must be justified in analysis/METHODS_ENERGY.md or METHODS_WATER.md.
BALANCE_EXEMPT <- list(
  # Gross generation is only reported for the three large thermal plants, so the
  # conversion loss of the small distributed/backup units cannot be separated from
  # their fuel input. Combined magnitude ~0.0039 EJ against a ~11.9 EJ system (~0.03%).
  energy_metro  = c("Distributed-scale Generation", "On-Site Backup Generation"),
  energy_county = c("Distributed-scale Generation", "On-Site Backup Generation"),
  water_metro   = character(0),
  # Named treatment facilities are terminal in a COUNTY cut by construction. `ww_imports`
  # tags rows with the facility's host county while `ww_exports` tags them with the
  # contributing county, so in the exporting county's diagram the receiving plant has an
  # inflow and no exit - its discharge is attributed to the host county. At metro scope,
  # where the county key disappears, these nodes do balance. Accepted as a property of the
  # county view rather than a defect.
  water_county  = if (exists("ww_facility_sink_map")) unique(ww_facility_sink_map$facility_name) else character(0)
)

viol_all <- list()

# ExtraNotes: audit inside YEARS_TO_ENSURE only. The frames carry 2006-2065 rows
# (wastewater connections start 2006; PWS/wastewater plans project to 2065), and
# outside the study window the source data is sparse, so an unscoped balance check
# reports hundreds of failures that are really just absent years. The diagrams
# already filter to `years`, so this matches what is actually published.
for (nm in names(targets)) {
  spec <- targets[[nm]]
  if (!exists(spec$df)) {
    message(sprintf("  SKIP %s (%s not found)", nm, spec$df))
    next
  }
  df <- get(spec$df)
  yr_range <- range(df$year)
  n_out <- sum(!df$year %in% YEARS_TO_ENSURE)
  cat(sprintf("\n[%s] %d rows, years %d-%d (%d rows outside %d-%d)\n",
              nm, nrow(df), yr_range[1], yr_range[2], n_out,
              min(YEARS_TO_ENSURE), max(YEARS_TO_ENSURE)))
  df <- df %>% filter(year %in% YEARS_TO_ENSURE)
  viol_all[[nm]] <- balance_report(df, nm, by_county = spec$by_county,
                                   exempt = BALANCE_EXEMPT[[nm]] %||% character(0))
}

# targeted assertions on the specific defects being fixed ----
cat("\n== targeted checks ==\n")

chk <- function(label, ok, detail = "") {
  cat(sprintf("   %-4s %-52s %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label, detail))
  invisible(isTRUE(ok))
}

# 1. every county must carry an electricity import or export flow
if (exists("en_fuel_gen_use_loss_all_trade")) {
  traded <- en_fuel_gen_use_loss_all_trade %>%
    filter(source == "elec_import" | target == "elec_export") %>%
    distinct(county) %>% pull(county)
  chk("all 15 counties have elec import/export", length(traded) == length(counties),
      sprintf("%d/%d; missing: %s", length(traded), length(counties),
              paste(setdiff(counties, traded), collapse = ", ")))

  # 2. county names must be clean (no trailing/leading whitespace, known set)
  cty <- unique(en_fuel_gen_use_loss_all_trade$county)
  bad <- cty[!cty %in% counties]
  chk("energy county labels all canonical", length(bad) == 0,
      if (length(bad)) paste0("bad: ", paste0("'", bad, "'", collapse = ", ")) else "")

  # 3. no years outside the study period
  yrs <- sort(unique(en_fuel_gen_use_loss_all_trade$year))
  chk("energy years == YEARS_TO_ENSURE", identical(as.integer(yrs), as.integer(YEARS_TO_ENSURE)),
      sprintf("%d years, %d-%d", length(yrs), min(yrs), max(yrs)))
}

# 3b. published water frames must also be scoped to the study period
if (exists("df_sankey_county_pws_balanced")) {
  yrs <- sort(unique(df_sankey_county_pws_balanced$year))
  chk("water county years == YEARS_TO_ENSURE",
      identical(as.integer(yrs), as.integer(YEARS_TO_ENSURE)),
      sprintf("%d years, %d-%d", length(yrs), min(yrs), max(yrs)))
}
if (exists("df_water_metro_linear_wSW_discharge_type")) {
  yrs <- sort(unique(df_water_metro_linear_wSW_discharge_type$year))
  chk("water metro years == YEARS_TO_ENSURE",
      identical(as.integer(yrs), as.integer(YEARS_TO_ENSURE)),
      sprintf("%d years, %d-%d", length(yrs), min(yrs), max(yrs)))
}

# 4. the three thermal plants must close
if (exists("en_fuel_gen_use_loss_all_trade")) {
  plant_bal <- check_node_balance(
    en_fuel_gen_use_loss_all_trade %>% filter(year %in% YEARS_TO_ENSURE),
    "plants", by_county = TRUE) %>%
    filter(grepl("Bowen|Yates|McDonough", node))
  worst <- if (nrow(plant_bal)) max(abs(plant_bal$rel)) else NA_real_
  chk("Bowen/Yates/McDonough balance", !is.na(worst) && worst <= BALANCE_TOL,
      sprintf("worst %.2f%% over %d node-groups", worst * 100, nrow(plant_bal)))
  # show the per-plant picture, since this is the headline defect
  if (nrow(plant_bal)) {
    plant_bal %>%
      group_by(node) %>%
      summarise(n = n(), inflow = sum(inflow), outflow = sum(outflow),
                worst_rel = rel[which.max(abs(rel))], .groups = "drop") %>%
      arrange(node) %>%
      purrr::pwalk(function(node, n, inflow, outflow, worst_rel) {
        cat(sprintf("        %-24s in %.5f  out %.5f  worst %+7.2f%%\n",
                    node, inflow, outflow, worst_rel * 100))
      })
  }
}

drop_report()

# optional CSV emission, matching the real save blocks ----
if (WRITE_CSV) {
  message("\n== writing CSVs to ", SAVE_DIR, " ==")
  for (d in c("energy", "water", "energy-water")) {
    dir.create(file.path(SAVE_DIR, d), recursive = TRUE, showWarnings = FALSE)
  }
  write_csv(en_fuel_gen_use_loss_all_trade_metro, file.path(SAVE_DIR, "energy/01_metro_energy_flows.csv"))
  write_csv(en_fuel_gen_use_loss_all_trade,       file.path(SAVE_DIR, "energy/02_county_energy_flows.csv"))
  write_csv(df_water_metro_linear_wSW_discharge_type, file.path(SAVE_DIR, "water/01_metro_water_flows.csv"))
  write_csv(df_sankey_county_pws_balanced,        file.path(SAVE_DIR, "water/02_county_water_flows.csv"))
}

n_viol <- sum(purrr::map_int(viol_all, ~ if (is.null(.x)) 0L else nrow(.x)))
cat(sprintf("\n== summary: %d balance violation row(s) ==\n", n_viol))
