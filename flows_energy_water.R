# Metro Atlanta energy-water flows
#
# Hassan Niazi, May 2026

source("functions.R")
source("flows_water.R")
source("flows_energy.R")


# energy water ----

energy_water <- rbind(en_fuel_gen_use_loss_all_trade_metro, en4water %>% select(-county)) %>%
  group_by(year, source, target, units) %>%
  summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>%
  rbind(df_water_metro_linear_wSW_discharge_type) # from water script

plot_sankey_enhanced(energy_water %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "")


# simplified energy-water ----
# aggregate some details for a cleaner diagram
energy_water %>% select(source, target) %>% distinct() -> energy_water_nodes
# write_csv(energy_water_nodes, paste0(DATA_DIR, "common_energy_water_nodes.csv"))


# read common_energy_water_simplified_map.csv and aggreagate using source_agg and target_agg
energy_water_simplified_map <- read_csv(paste0(DATA_DIR, "common_energy_water_simplified_map.csv")) %>% clean_col_names()

# energy_water_simplified <- energy_water %>%
#   left_join(energy_water_simplified_map %>% select(source, source_agg) %>% distinct(), by = "source") %>%
#   left_join(energy_water_simplified_map %>% select(target, target_agg) %>% distinct(), by = "target") %>%
#   mutate(source_agg = coalesce(source_agg, source),
#          target_agg = coalesce(target_agg, target)) %>%
#   group_by(year, source = source_agg, target = target_agg, units) %>%
#   summarise(value = sum(value), .groups = "drop")

energy_water_simplified <- simplify_sankey(energy_water, energy_water_simplified_map)

plot_sankey_enhanced(energy_water_simplified %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "")


plot_sankey_pro(energy_water_simplified)

# energy water by county ----
# county-level: uses df_sankey_county_pws_balanced (has individual WW facility names)
# for metro-aggregated view use 'energy_water' above (WW facilities already aggregated to "in-county treatment")
energy_water_county <- rbind(en_fuel_gen_use_loss_all_trade, en4water_ww_elec_use) %>%
  group_by(county, year, source, target, units) %>%
  summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>%
  rbind(df_sankey_county_pws_balanced)

plot_sankey_enhanced(energy_water_county %>% pretty_labels(),
                     reg = "Fulton", animate = T, show_values_in_labels = T, label_units = "")

plot_sankey_pro(energy_water_county, reg = "Fulton")
plot_sankey_pro(energy_water_county, reg = "Cobb")

# energy water simplified by county ----
# energy_water_simplified_county <- energy_water_county %>%
#   left_join(energy_water_simplified_map %>% select(source, source_agg) %>% distinct(), by = "source") %>%
#   left_join(energy_water_simplified_map %>% select(target, target_agg) %>% distinct(), by = "target") %>%
#   mutate(source_agg = coalesce(source_agg, source),
#          target_agg = coalesce(target_agg, target)) %>%
#   group_by(county, year, source = source_agg, target = target_agg, units) %>%
#   summarise(value = sum(value), .groups = "drop")

energy_water_simplified_county <- simplify_sankey(energy_water_county, energy_water_simplified_map)

plot_sankey_enhanced(energy_water_simplified_county %>% pretty_labels(),
                     reg = "Fulton", animate = T, show_values_in_labels = T, label_units = "")

plot_sankey_pro(energy_water_simplified_county)
plot_sankey_pro(energy_water_simplified_county, reg = "Fulton")

