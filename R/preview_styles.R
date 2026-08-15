# Render style variants of the three metro diagrams for review.
#   Rscript R/preview_styles.R
#   open outputs/style_preview/index.html
#
# Node palette and link style are independent, so this writes the full grid. Both are
# ordinary arguments to plot_sankey_enhanced(), e.g.
#   plot_sankey_enhanced(df, color_scheme = "signature", link_style = "nexus")

Sys.setenv(MAWEI_SAVE_FILES = "0", MAWEI_MAKE_PLOT = "0")
suppressMessages(suppressWarnings(source("R/flows_energy_water.R")))

unlink("outputs/style_preview", recursive = TRUE)

# Energy and water are single-domain, so only the node palette is in question there;
# the coupling classes only exist in the combined diagram.
preview_sankey_styles(
  en_fuel_gen_use_loss_all_trade_metro %>%
    group_by(year, source, target, units) %>%
    summarise(value = sum(value) * EJ_to_PJ, .groups = "drop"),
  label = "energy", units = "PJ", styles = "node",
  title = "Metro Atlanta energy")

preview_sankey_styles(
  df_water_metro_linear_wSW_discharge_type,
  label = "water", units = "MGD", styles = "node",
  title = "Metro Atlanta water")

preview_sankey_styles(
  energy_water, label = "energy-water", units = "auto", alt_units = ew_alt_units,
  styles = c("node", "nexus", "domain", "class"),
  title = "Metro Atlanta energy-water")
