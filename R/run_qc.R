# Data-only pipeline runner: builds every flow table, writes the CSVs, runs the
# mass-balance audit, and skips all 64 HTML widget renders.
#
# Purpose: a fast feedback loop for data-correctness work. A full run of
# R/flows_energy_water.R spends almost all of its wall time in
# htmlwidgets::saveWidget(); this script gets the same numbers in a fraction of
# the time so balance fixes can be iterated on quickly.
#
#   Rscript R/run_qc.R                      # audit only, writes nothing
#   MAWEI_SAVE_DIR=outputs/files_check/ Rscript R/run_qc.R --write-csv
#
# ExtraNotes: this is the regression gate for Phases 1-2. Compare its CSVs against
# outputs/files_baseline/ with compare_runs() to prove a change moved only what it
# was meant to move.

args <- commandArgs(trailingOnly = TRUE)
WRITE_CSV <- "--write-csv" %in% args

# Suppress artefact writing inside the sourced scripts; this runner decides what
# to write. Set via the environment so the flags survive each script's own
# re-source of functions.R.
Sys.setenv(MAWEI_SAVE_FILES = "0", MAWEI_MAKE_PLOT = "0", MAWEI_ANALYSIS = "0")

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

# ---------------------------------------------------------------------------
# the frames that become published artefacts
# ---------------------------------------------------------------------------
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
# here must be justified in docs_analysis/METHODS_ENERGY.md or METHODS_WATER.md.
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

# ---------------------------------------------------------------------------
# targeted assertions on the specific defects being fixed
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# optional CSV emission, matching the real save blocks
# ---------------------------------------------------------------------------
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
