# Metro Atlanta energy flows
#
# Hassan Niazi, Sep 2025

source("functions.R")


###############################################################################%

# SOCO data ----
# read in fuels, generation, onsite gen, demand, water use
en_fuels <- read_csv(paste0(DATA_DIR, "energy_fuels.csv.gz")) %>% clean_names()
en_gen <- read_csv(paste0(DATA_DIR, "energy_gen.csv.gz")) %>% clean_names()
en_gen_onsite <- read_csv(paste0(DATA_DIR, "energy_gen_onsite.csv.gz")) %>% clean_names()
en_use <- read_csv(paste0(DATA_DIR, "energy_use.csv.gz")) %>% pivot_longer(cols = !c(month, year, units), names_to = "enduse", values_to = "value")

## aggregate data ----
# interpolating/coping values, aggregating years, and converting units
en_fuels_agg <- en_fuels %>% group_by(across(!c(year, value))) %>%
  mutate(value = na.approx(value, na.rm = FALSE, rule = 2)) %>% # fill NAs
  ungroup() %>% group_by(across(-value)) %>%
  summarise(values = sum(value), .groups = "drop") %>% # sum years
  mutate(value = values * MMBtu_to_EJ, units = "EJ") # MMBtu to EJ
# TODO: coal and gas MMBtu could have different conversion factors

en_gen_agg <- en_gen %>% group_by(across(!c(year, value))) %>%
  mutate(value = na.approx(value, na.rm = FALSE, rule = 2)) %>% # fill NAs
  ungroup() %>% group_by(across(-value)) %>%
  summarise(values = sum(value), .groups = "drop") %>% # sum years
  mutate(value = values * MWh_to_EJ, units = "EJ") # MWh to EJ

en_gen_onsite_EJ <- en_gen_onsite %>% group_by(across(!c(year, value))) %>%
  mutate(valuef = na.approx(value, na.rm = FALSE, rule = 2)) %>% ungroup() %>% # fill NAs
  fill(valuef, .direction = "updown") %>%
  mutate(value = valuef * kWh_to_EJ, units = "EJ") # KWh to EJ

en_use_agg <- en_use %>% group_by(across(!c(year, value))) %>%
  mutate(value = na.approx(value, na.rm = FALSE, rule = 2)) %>% # fill NAs
  ungroup() %>% select(-month) %>% group_by(across(-value)) %>%
  summarise(values = sum(value), .groups = "drop") %>% # sum years
  mutate(value = values * MWh_to_EJ, units = "EJ") # MWh to EJ

## efficiency losses = gross - net ----
en_losses <- en_gen_agg %>% filter(gentype == "gross") %>%
  left_join(en_gen_agg %>% filter(gentype == "net"), by = c("facility_name", "county", "water_source", "fuel_type", "capacity_mw", "units", "year")) %>%
  mutate(losses = value.x - value.y)

###############################################################################%
## all flows from data ----
{
  en_fuels_agg_s <- en_fuels_agg %>% mutate(target = facility_name, units = "EJ") %>% select(county, source = fuel_type, target, year, value, units)
  en_gen_agg_s <- en_gen_agg %>% filter(gentype == "gross") %>% mutate(target = "electricity", units = "EJ") %>% select(county, source=facility_name, target, year, value, units)
  en_gen_onsite_EJ_s <- en_gen_onsite_EJ %>% mutate(source = "onsiteBTM", units = "EJ") %>% select(county, source, target=class, year, value, units)
  en_efficiency_losses_s <- en_losses %>% mutate(source = facility_name, target = "elec_own_use", units = "EJ") %>% select(county, source, target, year, value = losses, units)
  en_use_agg_s <- en_use_agg %>% mutate(source = "electricity", target = enduse, units = "EJ") %>% select(source, target, year, value, units) %>%
    filter(year %in% YEARS_TO_ENSURE)
}


# df_sankey_en_soco <- bind_rows(
#   en_fuels_agg %>% mutate(target = facility_name) %>% select(source = fuel_type, target, year, value),
#   en_gen_agg %>% filter(gentype == "gross") %>% mutate(target = "electricity") %>% select(source =facility_name, target, year, value),
#   # en_gen_onsite_EJ %>% mutate(source = paste0("onsite_",class), target = "electricity") %>% select(source, target, year, value),
#   en_gen_onsite_EJ %>% mutate(source = "onsiteBTM") %>% select(source, target=class, year, value),
#   en_losses %>% mutate(source = facility_name, target = "losses") %>% select(source, target, year, value = losses),
#   en_use_agg %>% mutate(source = "electricity", target = enduse) %>% select(source, target, year, value)
#   ) %>% filter(year < 2025)

df_sankey_en_soco <- rbind(en_fuels_agg_s %>% select(-county),
                      en_gen_agg_s %>% select(-county),
                      en_gen_onsite_EJ_s %>% select(-county),
                      en_efficiency_losses_s %>% select(-county),
                      # use isn't by county (yet, downscaled later)
                      en_use_agg_s)

# SOCO data plot
# plot_sankey_enhanced(df_sankey_en_soco)


###############################################################################%

# rejected energy ----
# fuel input - gross generation
en_rejected <- df_sankey_en_soco %>%
  # fuel inputs to each plant
  filter(target %in% c("Bowen", "Jack McDonough", "Yates")) %>%
  group_by(target, year) %>%
  summarise(fuel_input = sum(value), .groups = "drop") %>%
  # electricity generation from each plant
  left_join(df_sankey_en_soco %>%
              filter(source %in% c("Bowen", "Jack McDonough", "Yates"), target %in% c("elec_own_use", "electricity")) %>%
              # select(source, year, gross_generation = value),
              group_by(source, year) %>% summarise(gross_generation = sum(value), .groups = "drop"),
            by = c("target" = "source", "year")) %>%
  # bring in counties of plants
  left_join(en_gen_agg %>% distinct(facility_name, county), by = c("target" = "facility_name")) %>%
  mutate(rejected = fuel_input - gross_generation, source = target, target = "efficiency_losses", units = "EJ") %>%
  filter(rejected > 0) %>% # avoid 1 case of -0.00067 in Yates 2024
  select(county, source, target, year, value = rejected, units)


# NOTE: this imports calc is probably defunct because we'll need to calculate
# imports based on county level after accounting for all energy use and
# generation AFTER bringing in EIA and E4W. Keep it here for the interim SOCO diagram

# electricity imports ----
# electricity generation deficit = consumption - generation
en_elec_imports <- df_sankey_en_soco %>%
  # left_join(en_gen_agg %>% distinct(facility_name, county), by = c("target" = "facility_name")) %>%
  group_by(year) %>%
  summarise(total_generation = sum(value[source %in% c("Bowen", "Jack McDonough", "Yates") & target == "electricity"]),
            total_consumption = sum(value[source == "electricity"]),
            deficit = total_consumption - total_generation, .groups = "drop") %>%
  # only keep years with deficits (positive values)
  filter(deficit > 0) %>%
  mutate(source = "elec_import", target = "electricity", units = "EJ") %>%
  select(source, target, year, value = deficit, units)


# plot all elec
df_sankey_en_soco_all <- rbind(df_sankey_en_soco, en_rejected %>% select(-county), en_elec_imports)

# plot_sankey(df_sankey_en_soco_all %>% mutate(value = value * EJ_to_PJ, units = "PJ"))
plot_sankey_enhanced(df_sankey_en_soco_all %>% mutate(value = value * EJ_to_PJ, units = "PJ"),
                     animate = F, yr = 2024, show_values_in_labels = T, label_units = "PJ")


###############################################################################%

# EIA data ----
# we are going to process on plant level

###############################################################################%

## EIA 860 ----
# - 2 EIA 860 files: schedule 3.1 for generators and schedule 3.3 for solar generators.
# - This gives us plant level info on generation capacity, fuel type, and
# location/county mapping for the plants. it also has fuel type info but not
# fuel consumption or generation data, so we need to merge with 923 to get that info.
# - we need 860 because 923 doesn't have county mapping
# TODO: bring schedule 2 to get plant level info - specifically lat longs

eia860_sch31_generator_operable_GA <- map_dfr(2020:2024, function(yr) {
  read_csv(paste0(DATA_DIR, "eia860_3_1_Generator_Y", yr, "_operable.csv.gz"),
           show_col_types = FALSE) %>% clean_col_names() %>%
    mutate(across(c(utility_id, carbon_capture_technology, cofire_fuels,
                    switch_between_oil_and_natural_gas), as.character)) %>%
    filter(state == "GA", county %in% counties) %>%
    mutate(year = yr) %>%
    select(state, county, year,
           utility_id, utility_name, plant_code, plant_name,
           generator_id, technology, prime_mover,
           nameplate_capacity_mw, nameplate_power_factor,
           status, operating_year, sector_name, sector,
           energy_source_1, energy_source_2, startup_source_1, carbon_capture_technology,
           multiple_fuels, cofire_fuels, switch_between_oil_and_natural_gas)
})

# TODO: brought in solar but but 923 doesn't have anything on solar production so nothing changed on fuel inputs side
eia860_sch33_solar_operable <- read_csv(paste0(DATA_DIR, "eia860_3_3_Solar_Y2024_operable.csv.gz")) %>% clean_col_names() %>%
  filter(state == "GA") %>%
  filter(county %in% counties) %>%  # only metro atlanta counties
  mutate(year = 2024) %>%
  select(state, county, year,
         utility_name, plant_code, plant_name, # doesn't have utility_id
         generator_id, technology, prime_mover,
         sector_name, sector,
         nameplate_capacity_mw, operating_year, status, virtual_net_metering_agreement, virtual_net_metering_dc_capacity_mw)
# names(eia860_sch33_solar_operable)

# NOTES
# sector
  # 1 = Electric Utility
  # 2 = Independent Power Producer, Non-Combined Heat and Power
  # 3 = Independent Power Producer, Combined Heat and Power
  # 4 = Commercial, Non-Combined Heat and Power
  # 5 = Commercial, Combined Heat and Power
  # 6 = Industrial, Non-Combined Heat and Power
  # 7 = Industrial, Combined Heat and Power
# TODO: expand energy_source_1 abbreviations

plants_GA <- eia860_sch31_generator_operable_GA %>%
  select(county, plant_id=plant_code, plant_name) %>% distinct() %>%
  rbind(eia860_sch33_solar_operable %>%
          select(county, plant_id=plant_code, plant_name) %>% distinct()) %>%
  distinct()


###############################################################################%

## EIA 923 ----
# - 1 EIA 923 file: schedule 2 for plant level generation and fuel consumption.
# - we need 923 for fuel consumption and generation data. it also has plant level
# info but not lat longs or county, so we can merge with 860 to get that info.
# - let's start with 923 data (plant operational details) and see if we need more info

# EIA-923 Monthly Generation and Fuel Consumption Time Series File
eia923_sch2pg1_genfuel_GA <- map_dfr(2020:2024, function(yr) {
  df <- read_csv(paste0(DATA_DIR, "eia923_Schedule_2_3_4_5_M_12_", yr, "_Final_pg1.csv.gz"),
                 show_col_types = FALSE) %>% clean_col_names()
  # aer_fuel_type_code renamed to mer_fuel_type_code in 2022
  if ("aer_fuel_type_code" %in% names(df)) df <- df %>% rename(mer_fuel_type_code = aer_fuel_type_code)
  df %>%
    filter(plant_state == "GA", plant_id %in% plants_GA$plant_id) %>%
    select(state=plant_state, year, plant_id, plant_name, operator_id, operator_name,
           nerc_region, balancing_authority_code,
           naics_code, eia_sector_number, sector_name,
           reported_prime_mover, reported_fuel_type_code, mer_fuel_type_code,
           total_fuel_consumption_mmbtu, elec_fuel_consumption_mmbtu, net_generation_megawatthours)
})

# all(eia923_sch2pg1_genfuel_GA %>% mutate(fueldiff = total_fuel_consumption_mmbtu - elec_fuel_consumption_mmbtu) %>% pull(fueldiff) == 0) # should be true
# names(eia923_sch2pg1_genfuel_GA)

# gets plants in the region as per 860 and do label cleaning and mapping
eia923_sch2pg1_genfuel_GA_C <- eia923_sch2pg1_genfuel_GA %>%
  left_join(plants_GA, by = c("plant_id", "plant_name")) %>%
  reported_prime_mover_rename() %>% reported_fuel_rename() %>%
  mer_fuel_map_rename() %>% mer_fuel_map_agg()

# Note: either primary mover could be a target or a plant_name could be target; let's
# do plant name for now, but that will mask the generation type; but I guess
# that will be apparent from the fuel type

## fuel inputs ----
eia923_fuel_input_C <- eia923_sch2pg1_genfuel_GA_C %>%
  mutate(elec_fuel_consumption_EJ = elec_fuel_consumption_mmbtu * MMBtu_to_EJ, units = "EJ") %>%
  select(county, year, source=reported_fuel, target=plant_name, value=elec_fuel_consumption_EJ, units)

eia923_fuel_input <- eia923_fuel_input_C %>%
  remap_fuel_broad() %>% select(-source, source = fuel_broad) %>%
  remap_plants_agg() %>% select(-target, target = plant_aggregated) %>%
  group_by(county, year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop")

# plot_sankey(eia923_fuel_input, yr=2024, animate = F)

## electricity generation ----
eia923_electricity_gen <- eia923_sch2pg1_genfuel_GA_C %>%
  mutate(net_generation_EJ = net_generation_megawatthours * MWh_to_EJ, units = "EJ", target="electricity") %>%
  select(county, year, source=plant_name, target, value=net_generation_EJ, units) %>%
  remap_plants_agg("source") %>% select(-source, source = plant_aggregated) %>%
  group_by(county, year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  # TODO: if on-site genn, make it go to commerical direclty, bypass grid elec
  mutate(target = ifelse(grepl("site", source, ignore.case = T), "commercial", target)) %>%
  filter(value > 0) # 3 plants have negative net generation

# plot_sankey(rbind(eia923_fuel_input, eia923_electricity_gen), yr=2024, animate = F)



###############################################################################%

# EIA SEDS use data ----

eiaseds_codes <- read_csv(paste0(DATA_DIR, "eia_seds_codes_2024.csv.gz")) %>% rename_with(tolower)
# The MSNs are five-character codes, most of which are structured as follows:
#   First and second characters - describes an energy source (for example, NG for natural gas, MG for motor gasoline)
# Third and fourth characters - describes an energy sector or an energy activity (for example, RC for residential consumption, PR for production)
# Fifth character - describes a type of data (for example, P for data in physical unit, B for data in billion Btu)


# if filtered seds file doesn't exist, create it from the full file
seds_filtered_file <- paste0(DATA_DIR, "eia_seds_GA_2020_2024.csv.gz")
if (!file.exists(seds_filtered_file)) {
  seds_full_file <- paste0(DATA_DIR, EIA_SEDS_FILE)
  if (!file.exists(seds_full_file)) stop("EIA SEDS file not found: ", seds_full_file)
  read_csv(seds_full_file) %>% rename_with(tolower) %>%
    filter(year >= 2020 & year <= 2024, statecode == "GA") %>%
    write_csv(seds_filtered_file)
}

eiaseds <- read_csv(seds_filtered_file) %>%
  left_join(eiaseds_codes, by = "msn") %>%
  filter(data > 0) %>%
  # filter msn where the last character is B (Btu data)
  filter(substr(msn,5,5) == "B")


# EIA SEDS self-generated sources and target consumption
eiasedsGA <- eiaseds %>%
  filter(msn %in% seds_codes_get) %>% seds_target_set() %>%
  mutate(value = data * BBtu_to_EJ, units = "EJ")


## downscale use ----
# disaggregate all consumption to counties using population
# TODO: can improve industrial downscaling using some other data
census_pop <- read_csv(paste0(DATA_DIR, "cc-est2024-agesex-all.csv.gz")) %>% clean_col_names() %>%
  # year 2-6 = July estimates 2020-2024. # year 6 is 2024; the data goes from 2020 to 2024
  filter(stname == "Georgia", year >= 2) %>%
  mutate(ctyname = str_replace(ctyname, " County", ""),
         year = year + 2018, # 2->2020, 3->2021, ..., 6->2024
         statecode = "GA") %>%
  select(state=statecode, county = ctyname, year, pop = popestimate) %>%
  group_by(state, year) %>%
  mutate(pop_share = pop / sum(pop)) %>% ungroup() %>%
  filter(county %in% counties) # only metro atlanta counties

# disaggregate EIA SEDS consumption data
eiaseds_use <- eiasedsGA %>%
  left_join(census_pop %>% select(state, county, year, pop_share), by = c("statecode" = "state", "year")) %>%
  mutate(value_county = value * pop_share) %>%
  select(county, year, source=source, target=target, value=value_county, units)

# disaggregate stakeholder consumption data.
# TODO: is this everything? or do we need data from other utilities?
en_use_agg_C <- en_use_agg_s %>% mutate(state = "GA", units = "EJ") %>%
  left_join(census_pop %>% select(state, county, year, pop_share), by = c("state", "year")) %>%
  mutate(value_county = value * pop_share) %>%
  select(county, year, source, target, value=value_county, units)


###############################################################################%

# energy for water ----
# linearize it by assigning the target ot EfW for the energy diagram
en4water_ww_elec_use_linear <- en4water_ww_elec_use %>%
  filter(grepl("electricity", source, ignore.case = T)) %>%
  mutate(target = "en4water", units = "EJ") %>%
  group_by(county, year, source, target, units) %>%
  select(county, year, source, target, value, units)


###############################################################################%

# all data ----
# my own calcs next

# linear efw (1/2)
en_fuel_gen_use_loss <- rbind(eia923_fuel_input, # fuel input
                              eia923_electricity_gen, en_gen_onsite_EJ_s, # generation
                              eiaseds_use, en_use_agg_C, # consumption
                              en4water_ww_elec_use_linear, # energy for water
                              en_efficiency_losses_s, en_rejected # fuel and generation energy difference.
) # transmission losses and elec transfers handled later

plot_sankey_enhanced(en_fuel_gen_use_loss %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     yr = 2024, animate = F, show_values_in_labels = T, label_units = "PJ")

# animated
plot_sankey_enhanced(en_fuel_gen_use_loss %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     show_values_in_labels = T, label_units = "PJ")

# plot_sankey_enhanced(en_fuel_gen_use_loss %>% group_by(county, year, source, target, units) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
#                      reg = "Cobb", yr = 2024, animate = F, show_values_in_labels = T, label_units = "PJ")


###############################################################################%

# transmission and distribution losses ----
# assume 6% losses for now: source EIA (5-7%) https://www.eia.gov/tools/faqs/faq.php?id=105&t=3
TD_LOSSES_PCT <- 0.06
en_transmission_losses <- en_fuel_gen_use_loss %>%
  filter(grepl("electricity", source, ignore.case = T)) %>%
  group_by(county, year, source, units) %>%
  summarise(total_consumption = sum(value), .groups = "drop") %>%
  mutate(tdloss = total_consumption * TD_LOSSES_PCT, target = "td_losses") %>%
  select(county, year, source, target, value = tdloss, units)


en_fuel_gen_use_loss_all <- rbind(en_fuel_gen_use_loss, en_transmission_losses)

plot_sankey_enhanced(en_fuel_gen_use_loss_all %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     yr = 2024, animate = T, show_values_in_labels = T, label_units = "PJ")


###############################################################################%
# loopy efw (2/2)
en_fuel_gen_use_loss_loop <- rbind(eia923_fuel_input, # fuel input
                              eia923_electricity_gen, en_gen_onsite_EJ_s, # generation
                              eiaseds_use, en_use_agg_C, # consumption
                              en4water_ww_elec_use, # energy for water
                              en_transmission_losses,
                              en_efficiency_losses_s, en_rejected # fuel and generation energy difference.
) # transmission losses and elec transfers handled later

plot_sankey_enhanced(en_fuel_gen_use_loss_loop %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     yr = 2024, animate = T, show_values_in_labels = T, label_units = "PJ")


plot_sankey_enhanced(en_fuel_gen_use_loss_loop %>%
                       group_by(county, year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     reg = "Cobb", yr = 2024, animate = T, show_values_in_labels = T, label_units = "PJ")


###############################################################################%

# electricity imports and exports ----

## metro level ----
# difference between metro level consumption and generation
# metro just need imports but why? because all counties consume more than it
# generate. and because what they generate is sent to grid, then pulled from the
# grid so we don't notice.

# assign outside metro imports to the difference

en_elec_trade_metro <- en_fuel_gen_use_loss_all %>%
  filter(grepl("electricity", target, ignore.case = T)) %>%
  group_by(year) %>%
  summarise(total_generation = sum(value), .groups = "drop") %>%
  left_join(en_fuel_gen_use_loss_all %>%
              filter(grepl("electricity", source, ignore.case = T)) %>%
              group_by(year) %>%
              summarise(total_consumption = sum(value), .groups = "drop"),
            by = c("year")) %>%
  mutate(deficit = total_consumption - total_generation,
         tradetype = ifelse(deficit > 0, "importing", "exporting"),
         # if consuming more, source import
         source = ifelse(deficit > 0, "out_metro_elec_import", "electricity"),
         # if generating more, target export
         target = ifelse(deficit > 0, "electricity", "out_metro_elec_export"),
         value = abs(deficit),
         units = "EJ") %>%
  select(year, source, target, value, units)

en_fuel_gen_use_loss_all_trade_metro <- en_fuel_gen_use_loss_all %>% select(-county) %>%
  rbind(en_elec_trade_metro)

plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade_metro %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     yr = 2024, animate = F, show_values_in_labels = T, label_units = "PJ")

# metro level without transportation target and Petroleum source
plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade_metro %>%
                       # filter(!(source == "Petroleum" & target == "transport")) %>%
                       filter(!(grepl("transport", target, ignore.case = T) & grepl("petroleum", source, ignore.case = T))) %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     yr = 2024, animate = F, show_values_in_labels = T, label_units = "PJ")


## county-level ----
# difference between county level consumption and generation
# target electricity = generation, source electricity = consumption
# thus deficit = consumption - generation
# if consumption > generation, we have imports (positive deficit)
# if generation > consumption, we have exports (negative deficit)

en_elec_trade_county <- en_fuel_gen_use_loss_all %>%
  filter(grepl("electricity", target, ignore.case = T)) %>%
  group_by(county, target, year) %>%
  summarise(total_generation = sum(value), .groups = "drop") %>%
  left_join(en_fuel_gen_use_loss_all %>%
              filter(grepl("electricity", source, ignore.case = T)) %>%
              group_by(county, source, year) %>%
              summarise(total_consumption = sum(value), .groups = "drop"),
            by = c("county", "year")) %>%
  mutate(deficit = total_consumption - total_generation,
         tradetype = ifelse(deficit > 0, "importing", "exporting"),
         # if consuming more, source import
         source = ifelse(deficit > 0, "elec_import", "electricity"),
         # if generating more, target export
         target = ifelse(deficit > 0, "electricity", "elec_export"),
         value = abs(deficit),
         units = "EJ") %>%
  select(county, year, source, target, value, units)


en_fuel_gen_use_loss_all_trade <- rbind(en_fuel_gen_use_loss_all, en_elec_trade_county)


plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     yr = 2024, animate = F, show_values_in_labels = T, label_units = "PJ")

plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade %>%
                       group_by(county, year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     reg = "Fulton", yr = 2024, animate = T, show_values_in_labels = T, label_units = "PJ")

# save this

# TODO: complete energy-water simplified diagram (?)
# TODO: create ew by county
# TODO: create ew simplified by county
# TODO: en-water deep dive calcs
# TODO: do the energy for water going to cross-county ww facilities bugfix
# TODO: improve colors; ability to pass on units column to have both MGD and PJ in labels
# TODO: see if we can do a static filter in htmls
# TODO: remove ww trade labeling but have insights of energy, water movement
# TODO: push to repository
# TODO: reporting
# TODO: paper draft



# energy water ----

energy_water <- rbind(en_fuel_gen_use_loss_all_trade_metro, en4water %>% select(-county)) %>%
  group_by(year, source, target, units) %>%
  summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>%
  rbind(df_water_metro_linear_wSW_discharge_type) # from water script

plot_sankey_enhanced(energy_water %>% pretty_labels(),
                     yr = 2024, animate = T, show_values_in_labels = T, label_units = "")


# simplified energy-water ----
# aggregate some details for a cleaner diagram
energy_water %>% select(source, target) %>% distinct() -> energy_water_nodes
# write_csv(energy_water_nodes, "energy_water_nodes.csv")


# read common_energy_water_simplified_map.csv and aggreagate using source_agg and target_agg
energy_water_simplified_map <- read_csv(paste0(DATA_DIR, "common_energy_water_simplified_map.csv.gz")) %>% clean_col_names()

energy_water_simplified <- energy_water %>%
  left_join(energy_water_simplified_map %>% select(source, source_agg) %>% distinct(), by = "source") %>%
  left_join(energy_water_simplified_map %>% select(target, target_agg) %>% distinct(), by = "target") %>%
  group_by(year, source = source_agg, target = target_agg, units) %>%
  summarise(value = sum(value), .groups = "drop")

plot_sankey_enhanced(energy_water_simplified %>% pretty_labels(),
                     yr = 2024, animate = T, show_values_in_labels = T, label_units = "")

# save energy-water simplified diagram as html
# write_csv(energy_water_simplified, paste0(SAVE_DIR, "energy_water_simplified_flows.csv"))
# htmlwidgets::saveWidget(
#   plot_sankey_enhanced(energy_water_simplified %>% pretty_labels(),
#                        yr = 2024, animate = F, show_values_in_labels = T, label_units = ""),
#   file = paste0(SAVE_DIR, "energy_water_simple.html"),
#   selfcontained = TRUE
# )

###############################################################################%
# archive
# plot_sankey_enhanced(rbind(eia923_fuel_input, eia923_electricity_gen, eiaseds_use, en_use_agg_C, en_gen_onsite_EJ_s,
#                            en_efficiency_losses_s,
#                            # en4water_ww_elec_use,
#                            # these two need to be revised after everything else is calculated
#                            en_rejected, en_elec_imports %>% mutate(county = "GA")) %>%
#                        group_by(year, source, target, units) %>%
#                        summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
#                      yr = 2024, animate = F, show_values_in_labels = T, label_units = "PJ")
#
# plot_sankey_enhanced(rbind(eia923_fuel_input, eia923_electricity_gen, eiaseds_use, en_use_agg_C, en_gen_onsite_EJ_s, en_efficiency_losses_s,
#                            # these two need to be revised after everything else is calculated
#                            en_rejected, en_elec_imports %>% mutate(county = "GA")) %>%
#                        mutate(value = value * EJ_to_PJ) %>% pretty_labels(), reg = "Cobb",
# yr = 2024, animate = F, show_values_in_labels = T, label_units = "PJ")
#
#
#
#
# plot_sankey(rbind(eia923_fuel_input %>% select(-county),
#                   eia923_electricity_gen %>% select(-county),
#                   eiaseds_use %>% select(-county),
#                   en_use_agg_C %>% select(-county),
#                   # en_use_agg %>% mutate(source = "electricity", target = enduse, units="EJ") %>% select(source, target, year, value, units),
#                   en_losses %>% mutate(source = facility_name, target = "losses", units="EJ") %>% select(source, target, year, value = losses, units),
#                   # en_elec_imports %>% mutate(units = "EJ"),
#                   en_rejected), yr = 2024, animate = F)
#
#
# plot_sankey_enhanced(rbind(eia923_fuel_input %>% select(-county), eia923_electricity_gen %>% select(-county),
#                      eiaseds_use %>% select(-county),
#                      en_use_agg_C %>% select(-county),
#                      # en_use_agg %>% mutate(source = "electricity", target = enduse, units="EJ") %>% select(source, target, year, value, units),
#                      en_losses %>% mutate(source = facility_name, target = "losses", units="EJ") %>% select(source, target, year, value = losses, units),
#                      # en_elec_imports %>% mutate(units = "EJ"),
#                      en_rejected %>% mutate(units = "EJ")) %>%
#                        group_by(year, source, target, units) %>%
#                        summarise(value = sum(value) * 1000, .groups = "drop") %>% pretty_labels()
#                      , show_values_in_labels = T, yr = 2024, animate = F, label_units = "PJ")



