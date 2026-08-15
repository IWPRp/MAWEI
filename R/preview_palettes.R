# Render the candidate palettes on the metro energy, water and energy-water diagrams
# so the choice can be made by eye rather than from hex codes.
#
#   Rscript R/preview_palettes.R
#   open outputs/palette_preview/index.html

Sys.setenv(MAWEI_SAVE_FILES = "0", MAWEI_MAKE_PLOT = "0")
suppressMessages(suppressWarnings(source("R/flows_energy_water.R")))
source("R/palettes.R")

OUT <- "outputs/palette_preview"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

diagrams <- list(
  energy = list(df = en_fuel_gen_use_loss_all_trade_metro %>%
                  group_by(year, source, target, units) %>%
                  summarise(value = sum(value) * EJ_to_PJ, .groups = "drop"),
                units = "PJ", title = "Metro Atlanta energy"),
  water  = list(df = df_water_metro_linear_wSW_discharge_type, units = "MGD",
                title = "Metro Atlanta water"),
  ew     = list(df = energy_water, units = "auto", title = "Metro Atlanta energy-water")
)

made <- character(0)
for (pname in names(SANKEY_PALETTE_CANDIDATES)) {
  for (dname in names(diagrams)) {
    d <- diagrams[[dname]]
    f <- file.path(OUT, sprintf("%s_%s.html", dname, pname))
    message("  ", f)
    p <- plot_sankey_enhanced(
      d$df, title = paste0(d$title, "  -  palette: ", pname),
      animate = TRUE, show_values_in_labels = TRUE, label_units = d$units,
      alt_units = if (dname == "ew") ew_alt_units else NULL,
      color_scheme = SANKEY_PALETTE_CANDIDATES[[pname]])
    save_sankey(p, f)
    made <- c(made, basename(f))
  }
}

# A single page to flick between them.
cells <- paste(unlist(lapply(names(diagrams), function(dn)
  lapply(names(SANKEY_PALETTE_CANDIDATES), function(pn)
    sprintf('<a href="%s_%s.html">%s &mdash; %s</a>', dn, pn, dn, pn)))),
  collapse = "\n")
writeLines(c(
  "<!doctype html><meta charset='utf-8'><title>MAWEI palette candidates</title>",
  "<style>body{font:15px/1.7 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;",
  "margin:3rem auto;max-width:44rem;color:#222}h1{font-size:1.4rem;font-weight:600}",
  "a{display:block;padding:.45rem .7rem;margin:.2rem 0;border:1px solid #e3e3e3;",
  "border-radius:6px;text-decoration:none;color:#1a4d7a}a:hover{background:#f6f9fc}",
  "p{color:#555}</style>",
  "<h1>MAWEI palette candidates</h1>",
  "<p>Each link takes the colour of the node it leaves. Losses sit at the top of the",
  "right-hand column; the named plants sit above the aggregate generation categories.</p>",
  cells), file.path(OUT, "index.html"))

message("\nOpen: ", normalizePath(file.path(OUT, "index.html")))
