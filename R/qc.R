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
# Hassan Niazi, PNNL

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

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


# ---------------------------------------------------------------------------
# node-level mass balance
# ---------------------------------------------------------------------------
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


# ---------------------------------------------------------------------------
# dropped-row ledger
# ---------------------------------------------------------------------------
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


# ---------------------------------------------------------------------------
# artefact / manifest checks
# ---------------------------------------------------------------------------
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


# ---------------------------------------------------------------------------
# run-to-run comparison
# ---------------------------------------------------------------------------
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
