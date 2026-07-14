# Metro Atlanta energy-water flows
#
# Hassan Niazi, May 2026

source("functions.R")
source("flows_water.R")
source("flows_energy.R")

ew_alt_units <- list(
  nodes = c("Bowen Plant", "Jack McDonough Plant", "Yates Plant", "Grid Electricity"),
  from_unit = "PJ", factor = PJ_to_GWh, label = "GWh"
)

# energy water ----

energy_water <- rbind(en_fuel_gen_use_loss_all_trade_metro, en4water %>% select(-county)) %>%
  group_by(year, source, target, units) %>%
  summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>%
  mutate(units = "PJ") %>%
  rbind(df_water_metro_linear_wSW_discharge_type) # from water script

plot_sankey_enhanced(energy_water %>% pretty_labels(),
                     animate = T, show_values_in_labels = T,
                     label_units = "auto", alt_units = ew_alt_units,
                     link_color_by_domain = TRUE)


# simplified energy-water ----
# aggregate some details for a cleaner diagram
energy_water %>% select(source, target) %>% distinct() -> energy_water_nodes
# write_csv(energy_water_nodes, paste0(DATA_DIR, "common_energy_water_nodes.csv"))


# read common_energy_water_simplified_map.csv and aggreagate using source_agg and target_agg
energy_water_simplified_map <- read_csv(paste0(DATA_DIR, "common_energy_water_simplified_map.csv")) %>% clean_col_names()

energy_water_simplified <- simplify_sankey(energy_water, energy_water_simplified_map)

plot_sankey_enhanced(energy_water_simplified %>% pretty_labels(),
                     animate = T, show_values_in_labels = T,
                     label_units = "auto", alt_units = ew_alt_units,
                     link_color_by_domain = TRUE)

# plot_sankey_pro(energy_water_simplified)

# energy water by county ----
# county-level: uses df_sankey_county_pws_balanced (has individual WW facility names)
# for metro-aggregated view use 'energy_water' above (WW facilities already aggregated to "in-county treatment")
energy_water_county <- rbind(en_fuel_gen_use_loss_all_trade, en4water_ww_elec_use) %>%
  group_by(county, year, source, target, units) %>%
  summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>%
  mutate(units = "PJ") %>%
  rbind(df_sankey_county_pws_balanced)

plot_sankey_enhanced(energy_water_county %>% pretty_labels(),
                     reg = "Fulton", animate = T, show_values_in_labels = T,
                     label_units = "auto", alt_units = ew_alt_units,
                     link_color_by_domain = TRUE)

if (MAKE_PLOT) plot_sankey_pro(energy_water_county, reg = "Fulton")
if (MAKE_PLOT) plot_sankey_pro(energy_water_county, reg = "Cobb")

# energy water simplified by county ----
energy_water_simplified_county <- simplify_sankey(energy_water_county, energy_water_simplified_map)

plot_sankey_enhanced(energy_water_simplified_county %>% pretty_labels(),
                     reg = "Fulton", animate = T, show_values_in_labels = T,
                     label_units = "auto", alt_units = ew_alt_units,
                     link_color_by_domain = TRUE)

# plot_sankey_pro(energy_water_simplified_county)
# plot_sankey_pro(energy_water_simplified_county, reg = "Fulton")

if (SAVE_FILES) {
###############################################################################%
# SAVING METRO ----
###############################################################################%

message("Saving energy-water outputs...")

write_csv(energy_water,
          file.path(SAVE_DIR, "energy-water/01_metro_ew_flows.csv"))
write_csv(energy_water_simplified,
          file.path(SAVE_DIR, "energy-water/02_metro_ew_simplified_flows.csv"))

save_sankey(
  plot_sankey_enhanced(energy_water,
                       animate = TRUE, show_values_in_labels = TRUE,
                       label_units = "auto", alt_units = ew_alt_units,
                       link_color_by_domain = TRUE),
  file.path(SAVE_DIR, "energy-water/01_metro_ew.html"))

save_sankey(
  plot_sankey_enhanced(energy_water_simplified,
                       animate = TRUE, show_values_in_labels = TRUE,
                       label_units = "auto", alt_units = ew_alt_units,
                       link_color_by_domain = TRUE),
  file.path(SAVE_DIR, "energy-water/02_metro_ew_simplified.html"))

###############################################################################%
# SAVING COUNTY ----
###############################################################################%

write_csv(energy_water_county,
          file.path(SAVE_DIR, "energy-water/03_county_ew_flows.csv"))
write_csv(energy_water_simplified_county,
          file.path(SAVE_DIR, "energy-water/04_county_ew_simplified_flows.csv"))

save_county_sankeys(
  energy_water_county, "energy-water", "03", "ew",
  prep_fn = identity, label_units = "auto", alt_units = ew_alt_units,
  link_color_by_domain = TRUE)

save_county_sankeys(
  energy_water_simplified_county, "energy-water", "04", "ew_simplified",
  prep_fn = identity, label_units = "auto", alt_units = ew_alt_units,
  link_color_by_domain = TRUE)

}
