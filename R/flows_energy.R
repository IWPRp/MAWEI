# Metro Atlanta energy flows
#
# Hassan Niazi, Sep 2025

source("functions.R")

###############################################################################%

# SOCO data ----
# read in fuels, generation, onsite gen, demand, water use
en_fuels <- read_csv(paste0(DATA_DIR, "energy_fuels.csv")) %>% clean_names()
en_gen <- read_csv(paste0(DATA_DIR, "energy_gen.csv")) %>% clean_names()
en_gen_onsite <- read_csv(paste0(DATA_DIR, "energy_gen_onsite.csv")) %>% clean_names()
en_use <- read_csv(paste0(DATA_DIR, "energy_use.csv")) %>% pivot_longer(cols = !c(month, year, units), names_to = "enduse", values_to = "value")

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

en_gen_onsite_EJ <- en_gen_onsite %>%
  # ExtraNotes: energy_gen_onsite.csv spells DeKalb as "Dekalb". The county string is
  # carried verbatim into the merged frame, and save_county_sankeys() filters with an
  # exact `county %in% reg` against the canonical list from common_county_fips.csv, so
  # every "Dekalb" row was silently excluded from the DeKalb county diagram while still
  # being counted in the metro total (which drops county). Canonicalise on ingest.
  mutate(county = str_trim(county),
         county = coalesce(counties[match(tolower(county), tolower(counties))], county)) %>%
  group_by(across(!c(year, value))) %>%
  mutate(valuef = na.approx(value, na.rm = FALSE, rule = 2)) %>% ungroup() %>% # fill NAs
  fill(valuef, .direction = "updown") %>%
  mutate(value = valuef * kWh_to_EJ, units = "EJ") # KWh to EJ
# ExtraNotes: fill(.direction = "updown") back-fills leading NAs with the first observed
# value, so a class with no data before 2023 is treated as flat at its 2023 level rather
# than absent. Source file spans 2018-2024; the study-period clamp happens downstream.

en_use_agg <- en_use %>% group_by(across(!c(year, value))) %>%
  mutate(value = na.approx(value, na.rm = FALSE, rule = 2)) %>% # fill NAs
  ungroup() %>% select(-month) %>% group_by(across(-value)) %>%
  summarise(values = sum(value), .groups = "drop") %>% # sum years
  mutate(value = values * MWh_to_EJ, units = "EJ") # MWh to EJ

## efficiency losses = gross - net ----
en_losses <- en_gen_agg %>% filter(gentype == "gross") %>%
  left_join(en_gen_agg %>% filter(gentype == "net"), by = c("facility_name", "county", "water_source", "fuel_type", "capacity_mw", "units", "year")) %>%
  mutate(losses = value.x - value.y)
# ExtraNotes: value.x = gross, value.y = net, so `losses` is the plant's own parasitic
# load (pumps, fans, mills, precipitators). Verified against EIA-923: SOCO `net` and
# 923 `net_generation_megawatthours` are identical, so gross is the ONLY quantity this
# spreadsheet adds. Own-use fractions: Bowen ~8.4-9.1%, McDonough ~1.33-1.39%,
# Yates ~6.0-11.7% of gross.
# CAUTION: Yates 2024 `net` is blank for all 12 months, so na.approx(rule = 2) copies
# the last 2023 value forward and the resulting own-use fraction (56.7%) is fabricated.
# Handled in the plant balance block below.

###############################################################################%
## all flows from data ----
{
  en_fuels_agg_s <- en_fuels_agg %>% mutate(target = facility_name, units = "EJ") %>% select(county, source = fuel_type, target, year, value, units)
  en_gen_agg_s <- en_gen_agg %>% filter(gentype == "gross") %>% mutate(target = "electricity", units = "EJ") %>% select(county, source=facility_name, target, year, value, units)
  en_gen_onsite_EJ_s <- en_gen_onsite_EJ %>% mutate(source = "onsiteBTM", units = "EJ") %>%
    select(county, source, target=class, year, value, units) %>%
    filter(year %in% YEARS_TO_ENSURE)
  # ExtraNotes: clamped to the study period. The source file starts in 2018, and unlike
  # en_use_agg_s this frame was previously unfiltered, injecting 2018-2019 rows into the
  # merged energy frame.
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
# ExtraNotes: interim SOCO-only diagram. It computes its own rejected term from the
# spreadsheet's own fuel and gross figures (fuel - gross), which is self-consistent
# within this frame. It deliberately does NOT reuse `en_rejected` from the main
# pipeline, because that one is built on EIA-923 fuel and is defined further down,
# after the 923 ingestion.
en_rejected_soco_only <- en_fuels_agg %>%
  filter(facility_name %in% SOCO_THERMAL_PLANTS) %>%
  group_by(source = facility_name, year) %>%
  summarise(fuel_input = sum(value), .groups = "drop") %>%
  left_join(en_gen_agg %>% filter(gentype == "gross", facility_name %in% SOCO_THERMAL_PLANTS) %>%
              group_by(source = facility_name, year) %>%
              summarise(gross = sum(value), .groups = "drop"),
            by = c("source", "year")) %>%
  mutate(value = fuel_input - gross, target = "efficiency_losses", units = "EJ") %>%
  select(source, target, year, value, units)

df_sankey_en_soco_all <- rbind(df_sankey_en_soco, en_rejected_soco_only, en_elec_imports)

# plot_sankey(df_sankey_en_soco_all %>% mutate(value = value * EJ_to_PJ, units = "PJ"))
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_en_soco_all %>% mutate(value = value * EJ_to_PJ, units = "PJ"),
                     animate = T, show_values_in_labels = T, label_units = "PJ")


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
  mer_fuel_map_rename() %>% mer_fuel_map_agg() %>%
  # broad fuel class is needed here (not just later) so the heat-rate convention can
  # be harmonised before any EJ conversion happens
  remap_fuel_broad("reported_fuel") %>%
  normalize_noncombustible_heat_rate()
# ExtraNotes: EIA reported hydro/solar fuel input at a fossil-fuel-equivalent heat
# rate (~8766 Btu/kWh) through 2021 and at 3412 Btu/kWh from 2022, so the raw series
# has a spurious 2.57x step at 2022 for non-combustibles while fossil fuels show no
# break. normalize_noncombustible_heat_rate() puts every year on the 3412 Btu/kWh
# (100% efficiency) convention. Deliberate method choice: it removes an artefact that
# otherwise dominates the apparent year-to-year change in hydro and solar (Allatoona
# in Bartow: 0.00158 -> 0.000527 EJ from 2021 to 2022 for similar real generation).
# The untouched series is kept as elec_fuel_consumption_mmbtu_reported.

# ExtraNotes: EIA-923 writes "." for missing and quotes thousands-separated numbers,
# so a single stray "." turns a whole column to character and the * MMBtu_to_EJ
# multiply then fails silently. Assert numeric before relying on it.
stopifnot(is.numeric(eia923_sch2pg1_genfuel_GA_C$elec_fuel_consumption_mmbtu),
          is.numeric(eia923_sch2pg1_genfuel_GA_C$net_generation_megawatthours))

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

# plot_sankey(eia923_fuel_input, yr=2024, animate = T)

## electricity generation ----
# ExtraNotes: NET generation, not gross. Gross exists only in the utility spreadsheet
# (energy_gen.csv) and only for the three large thermal plants; the gross-net
# difference is the plants' own parasitic load, routed to elec_own_use below.
eia923_electricity_gen <- eia923_sch2pg1_genfuel_GA_C %>%
  mutate(net_generation_EJ = net_generation_megawatthours * MWh_to_EJ, units = "EJ", target="electricity") %>%
  select(county, year, source=plant_name, target, value=net_generation_EJ, units) %>%
  remap_plants_agg("source") %>% select(-source, source = plant_aggregated) %>%
  group_by(county, year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  # Behind-the-meter output is consumed on site and never reaches the grid, so it is
  # re-pointed at the commercial sector and excluded from the electricity node (and
  # therefore from each county's import/export deficit).
  # ExtraNotes: matched on an explicit label set, NOT grepl("site"). The old regex
  # swallowed anything containing "site", and "On-Site Backup Generation" now also
  # holds Bartow Davidson (a 2.6 MW utility solar farm, online 12/2023) and the
  # Hewlett Packard Enterprise standby genset. Explicit matching keeps it auditable.
  mutate(target = ifelse(source %in% BEHIND_THE_METER_AGGREGATES, "commercial", target))

eia923_electricity_gen <- eia923_electricity_gen %>%
  log_drop(eia923_electricity_gen$value > 0, "energy gen: non-positive net generation")
# ExtraNotes: negative annual net generation occurs when a unit consumed more than it
# produced (standby/auxiliary load). Dropped rather than netted, which is why a plant
# can be missing from a given year entirely.

# plot_sankey(rbind(eia923_fuel_input, eia923_electricity_gen), yr=2024, animate = T)

###############################################################################%

# rejected energy ----
# Thermal-plant energy balance for the three plants where gross generation is known.
#
#   fuel input (EIA-923)
#     -> net generation   = 923 net                    -> electricity
#     -> own use          = SOCO gross - SOCO net      -> elec_own_use
#     -> rejected heat    = fuel input - SOCO gross    -> efficiency_losses
#
# net + (gross - net) + (fuel - gross) == fuel, so the node closes algebraically.
#
# ExtraNotes: this replaces a version that summed the plant's SOCO outflows to BOTH
# `electricity` and `elec_own_use` and called the total "gross_generation". That sum is
# 2*gross - net, i.e. gross was subtracted twice, giving
# rejected = fuel - 2*gross + net. It was self-consistent inside the SOCO-only
# diagram, but the merged frame supplies the generation leg from 923 *net*, so the
# node was left with a residual of exactly the own-use term (Bowen: 0.00285/0.0834 =
# 3.4%). Closing against gross alone removes the residual entirely.
#
# ExtraNotes: fuel input is taken from EIA-923, not from the SOCO spreadsheet. The two
# agree to 1e-14 for 2020-2023, but Yates 2024 is blank in SOCO and na.approx(rule = 2)
# fabricates a value ~2x too small; using 923 throughout avoids a negative rejected
# term (the old code's `filter(rejected > 0)` silently dropped that row, leaving Yates
# 2024 with outflow 0.01253 against inflow 0.01186 EJ - a hard violation, not rounding).
en_plant_fuel_923 <- eia923_fuel_input %>%
  filter(target %in% c("Bowen Plant", "Yates Plant", "Jack McDonough")) %>%
  group_by(county, plant_agg = target, year) %>%
  summarise(fuel_input = sum(value), .groups = "drop")

en_plant_gross_soco <- en_gen_agg %>%
  filter(gentype == "gross", facility_name %in% SOCO_THERMAL_PLANTS) %>%
  remap_plants_agg("facility_name") %>%
  group_by(plant_agg = plant_aggregated, year) %>%
  summarise(gross = sum(value), .groups = "drop")

en_plant_balance <- en_plant_fuel_923 %>%
  left_join(en_plant_gross_soco, by = c("plant_agg", "year")) %>%
  mutate(rejected = fuel_input - gross, units = "EJ")

# Yates 2024: SOCO gross is real but SOCO net is fabricated, and 923 fuel is ~2x the
# SOCO figure, so `gross` can exceed what the 923 fuel would support. Rather than drop
# the row, hold the plant's own-use fraction at its 2020-2023 mean and rebuild gross
# from the 923 net generation, which is measured.
# ExtraNotes: this is the single data gap in the study period. Documented as a
# limitation; affects Yates 2024 only.
en_plant_neg <- en_plant_balance %>% filter(rejected < 0)
if (nrow(en_plant_neg) > 0) {
  message("  plant balance: ", nrow(en_plant_neg),
          " plant-year(s) with fuel < gross, repairing: ",
          paste(unique(paste(en_plant_neg$plant_agg, en_plant_neg$year)), collapse = ", "))
}

en_plant_own_use_frac <- en_losses %>%
  filter(facility_name %in% SOCO_THERMAL_PLANTS, year %in% YEARS_TO_ENSURE) %>%
  remap_plants_agg("facility_name") %>%
  mutate(frac = losses / value.x) %>%
  # exclude the fabricated Yates 2024 point from its own reference mean
  anti_join(en_plant_neg %>% select(plant_agg, year), by = c("plant_aggregated" = "plant_agg", "year")) %>%
  group_by(plant_agg = plant_aggregated) %>%
  summarise(own_use_frac = mean(frac, na.rm = TRUE), .groups = "drop")

en_plant_net_923 <- eia923_electricity_gen %>%
  filter(source %in% c("Bowen Plant", "Yates Plant", "Jack McDonough")) %>%
  group_by(county, plant_agg = source, year) %>%
  summarise(net_gen = sum(value), .groups = "drop")

en_plant_balance_fix <- en_plant_balance %>%
  left_join(en_plant_own_use_frac, by = "plant_agg") %>%
  left_join(en_plant_net_923, by = c("county", "plant_agg", "year")) %>%
  mutate(
    # gross implied by measured net and the plant's typical own-use share
    gross_implied = net_gen / (1 - own_use_frac),
    gross_used = if_else(rejected < 0 | is.na(gross), gross_implied, gross),
    own_use = gross_used - net_gen,
    rejected = fuel_input - gross_used) %>%
  select(county, plant_agg, year, fuel_input, net_gen, gross_used, own_use, rejected, units)

stopifnot(all(en_plant_balance_fix$rejected > -1e-12, na.rm = TRUE),
          all(en_plant_balance_fix$own_use > -1e-12, na.rm = TRUE))

# own parasitic load -> elec_own_use
en_efficiency_losses_agg <- en_plant_balance_fix %>%
  mutate(source = plant_agg, target = "elec_own_use") %>%
  select(county, source, target, year, value = own_use, units)

# rejected heat -> efficiency_losses
en_rejected <- en_plant_balance_fix %>%
  mutate(source = plant_agg, target = "efficiency_losses") %>%
  select(county, source, target, year, value = rejected, units)




###############################################################################%

# EIA SEDS use data ----

eiaseds_codes <- read_csv(paste0(DATA_DIR, "eia_seds_codes_2024.csv")) %>% rename_with(tolower)
# The MSNs are five-character codes, most of which are structured as follows:
#   First and second characters - describes an energy source (for example, NG for natural gas, MG for motor gasoline)
# Third and fourth characters - describes an energy sector or an energy activity (for example, RC for residential consumption, PR for production)
# Fifth character - describes a type of data (for example, P for data in physical unit, B for data in billion Btu)


# if filtered seds file doesn't exist, create it from the full file
seds_filtered_file <- paste0(DATA_DIR, "eia_seds_GA_2020_2024.csv")
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
# linearize it by assigning the target to EfW for the energy diagram
en4water_ww_elec_use_linear <- en4water_ww_elec_use %>%
  filter(grepl("electricity", source, ignore.case = T)) %>%
  mutate(target = "en4water", units = "EJ") %>%
  group_by(county, year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  filter(year %in% YEARS_TO_ENSURE)
validate_flows(en4water_ww_elec_use_linear, "en4water_ww_elec_use_linear", strict_years = TRUE)
# ExtraNotes: this filter is the single fix for the 2006-2065 year leak on the energy
# side. The water pipeline carries 2006 (earliest wastewater connection record) through
# 2065 (management-plan projection horizon); before clamping, this frame contributed 860
# rows of which only 75 were in the study period, and those 785 stray rows propagated
# into electricity->td_losses and en4water->energy_services/rejected_energy, giving the
# published energy CSVs 60 years of mostly-empty rows.

###############################################################################%

# all data ----
# my own calcs next

# linear efw (1/2)
en_fuel_gen_use_loss <- rbind(eia923_fuel_input, # fuel input
                              eia923_electricity_gen, en_gen_onsite_EJ_s, # generation
                              eiaseds_use, en_use_agg_C, # consumption
                              en4water_ww_elec_use_linear, # energy for water
                              en_efficiency_losses_agg, en_rejected # plant own use + rejected heat
) # transmission losses and elec transfers handled later
# ExtraNotes: uses en_efficiency_losses_agg (plant-aggregate labels: "Bowen Plant",
# "Yates Plant", "Jack McDonough") rather than the raw-facility en_efficiency_losses_s
# ("Bowen", "Yates"). The raw names collided with the 923 aggregate names, splitting each
# plant into two nodes that only appeared merged because pretty_labels() renamed them at
# draw time - so Bowen showed +65.7% imbalance while its own-use and rejected heat sat on
# a separate, invisible "Bowen" node.

if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "PJ")

# plot_sankey_enhanced(en_fuel_gen_use_loss %>% group_by(county, year, source, target, units) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
#                      reg = "Cobb", animate = T, show_values_in_labels = T, label_units = "PJ")


###############################################################################%

# energy services ----
# all uses should two downstream flows: energy services and rejected energy
# industrial sector 49% efficiency
# ag, commercial, govt, residential, and PWS (or water services generally) sector have 65% efficiency
# assuming 65% for anything else

SECTOR_EFFICIENCY <- c(
  industrial = 0.49,
  agricultural = 0.65,
  commercial = 0.65,
  government = 0.65,
  residential = 0.65,
  transport = 0.65,
  en4water = 0.65
)

DEFAULT_EFFICIENCY <- 0.65

# energy services and rejected energy
# split each sector's total energy intake into useful energy (services) and waste (rejected)
# ExtraNotes: END_USE_SECTORS is now an explicit constant (functions.R) instead of
# setdiff(unique(target), unique(source)). The derived version would classify any plant
# aggregate with no generation in a year as an end-use sector and then invent 65% "energy
# services" at a generator node. It happens not to trigger today (the "Other" aggregate
# does emit generation), so this is hardening rather than a behaviour change - asserted
# below so a future divergence is loud rather than silent.
end_use_sectors <- intersect(END_USE_SECTORS, unique(en_fuel_gen_use_loss$target))
end_use_sectors_derived <- setdiff(unique(en_fuel_gen_use_loss$target),
                                   unique(en_fuel_gen_use_loss$source)) %>%
  setdiff(c("efficiency_losses", "elec_own_use"))
if (!setequal(end_use_sectors, end_use_sectors_derived)) {
  warning("end_use_sectors: explicit list and topology-derived list disagree.\n",
          "  explicit only: ", paste(setdiff(end_use_sectors, end_use_sectors_derived), collapse = ", "),
          "\n  derived only:  ", paste(setdiff(end_use_sectors_derived, end_use_sectors), collapse = ", "),
          call. = FALSE)
}

en_sector_totals <- en_fuel_gen_use_loss %>%
  filter(target %in% end_use_sectors) %>%
  group_by(county, year, source = target, units) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(efficiency = if_else(source %in% names(SECTOR_EFFICIENCY),
                              SECTOR_EFFICIENCY[source], DEFAULT_EFFICIENCY))

en_services_rejected <- rbind(
  en_sector_totals %>% mutate(target = "energy_services", value = value * efficiency),
  en_sector_totals %>% mutate(target = "rejected_energy", value = value * (1 - efficiency))
) %>% select(county, year, source, target, value, units)


if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss %>% rbind(en_services_rejected) %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "PJ")


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


en_fuel_gen_use_loss_all <- rbind(en_fuel_gen_use_loss, en_transmission_losses, en_services_rejected)

if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss_all %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "PJ")


###############################################################################%
# loopy efw (2/2)
en_fuel_gen_use_loss_loop <- rbind(eia923_fuel_input, # fuel input
                              eia923_electricity_gen, en_gen_onsite_EJ_s, # generation
                              eiaseds_use, en_use_agg_C, # consumption
                              en4water_ww_elec_use, # energy for water
                              en_transmission_losses,
                              en_efficiency_losses_agg, en_rejected, # plant own use + rejected heat
                              en_services_rejected
) # transmission losses and elec transfers handled later

if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss_loop %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "PJ")


if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss_loop %>%
                       group_by(county, year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     reg = "Cobb", animate = T, show_values_in_labels = T, label_units = "PJ")


###############################################################################%

# electricity imports and exports ----

## metro level ----
# difference between metro level consumption and generation
# metro just need imports but why? because all counties consume more than it
# generate. and because what they generate is sent to grid, then pulled from the
# grid so we don't notice.

# assign outside metro imports to the difference

# ExtraNotes: built on a complete year grid so a year with no generation at all still
# yields a trade row. `electricity` balances to 0.00% at metro in every year of the
# study period, confirming that routing T&D losses as an extra outflow from the
# electricity node (rather than deducting them from delivered demand) is correct: the
# SEDS/utility demand figures are METERED, so losses sit upstream of the meter and
# generation + imports must cover metered use + T&D losses.
en_elec_trade_metro <- expand_grid(year = YEARS_TO_ENSURE) %>%
  left_join(en_fuel_gen_use_loss_all %>%
              filter(grepl("electricity", target, ignore.case = T)) %>%
              group_by(year) %>%
              summarise(total_generation = sum(value), .groups = "drop"),
            by = "year") %>%
  left_join(en_fuel_gen_use_loss_all %>%
              filter(grepl("electricity", source, ignore.case = T)) %>%
              group_by(year) %>%
              summarise(total_consumption = sum(value), .groups = "drop"),
            by = "year") %>%
  mutate(across(c(total_generation, total_consumption), ~replace_na(., 0)),
         deficit = total_consumption - total_generation,
         tradetype = ifelse(deficit > 0, "importing", "exporting"),
         # if consuming more, source import
         source = ifelse(deficit > 0, "out_metro_elec_import", "electricity"),
         # if generating more, target export
         target = ifelse(deficit > 0, "electricity", "out_metro_elec_export"),
         value = abs(deficit),
         units = "EJ") %>%
  filter(value > 0) %>%
  select(year, source, target, value, units)

en_fuel_gen_use_loss_all_trade_metro <- en_fuel_gen_use_loss_all %>% select(-county) %>%
  rbind(en_elec_trade_metro) %>%
  filter(year %in% YEARS_TO_ENSURE) %>%
  group_by(year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop")
validate_flows(en_fuel_gen_use_loss_all_trade_metro,
               "en_fuel_gen_use_loss_all_trade_metro", strict_years = TRUE)
# ExtraNotes: re-aggregated after dropping `county` so the metro frame has one row per
# (year, source, target, units). Without this, dropping county leaves 15 duplicate rows
# per flow, which validate_flows() would reject as duplicates.

if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade_metro %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "PJ")

# metro level without transportation target and Petroleum source
if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade_metro %>%
                       # filter(!(source == "Petroleum" & target == "transport")) %>%
                       filter(!(grepl("transport", target, ignore.case = T) & grepl("petroleum", source, ignore.case = T))) %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "PJ")


## county-level ----
# difference between county level consumption and generation
# target electricity = generation, source electricity = consumption
# thus deficit = consumption - generation
# if consumption > generation, we have imports (positive deficit)
# if generation > consumption, we have exports (negative deficit)

en_elec_trade_county <- expand_grid(county = counties, year = YEARS_TO_ENSURE) %>%
  left_join(en_fuel_gen_use_loss_all %>%
              filter(grepl("electricity", target, ignore.case = T)) %>%
              group_by(county, year) %>%
              summarise(total_generation = sum(value), .groups = "drop"),
            by = c("county", "year")) %>%
  left_join(en_fuel_gen_use_loss_all %>%
              filter(grepl("electricity", source, ignore.case = T)) %>%
              group_by(county, year) %>%
              summarise(total_consumption = sum(value), .groups = "drop"),
            by = c("county", "year")) %>%
  mutate(across(c(total_generation, total_consumption), ~replace_na(., 0)),
         deficit = total_consumption - total_generation,
         tradetype = ifelse(deficit > 0, "importing", "exporting"),
         # if consuming more, source import
         source = ifelse(deficit > 0, "elec_import", "electricity"),
         # if generating more, target export
         target = ifelse(deficit > 0, "electricity", "elec_export"),
         value = abs(deficit),
         units = "EJ") %>%
  filter(value > 0) %>%
  select(county, year, source, target, value, units)
# ExtraNotes: THE IMPORT FIX. The previous version started from the generation side and
# left_joined consumption onto it, so a county with no row whose target is "electricity"
# was dropped by the join and never got a trade flow at all. Six counties have no
# EIA-860 generator in any year - Cherokee, Clayton, Fayette, Hall, Henry, Paulding - so
# they showed zero imports despite consuming electricity, which is physically impossible.
# Douglas showed imports only from 2022 because its sole generator (Turnipseed Solar)
# first appears in the 2022 EIA-860; Rockdale's only generator (Milstead hydro) appears
# in 2020 alone. Starting from expand_grid(county, year) makes generation default to 0,
# so every county-year gets an explicit import or export.
# What was previously displayed as "Electricity Imports" in e.g. Henry was a zero-valued
# placeholder created by complete() inside plot_sankey_enhanced() (which runs before the
# county filter), not a real flow - hence a label that appeared and vanished between
# animation frames.

# Every county must now trade; a county that neither imports nor exports would mean its
# generation exactly equals its consumption, which does not happen in practice.
traded_counties <- union(
  en_elec_trade_county$county[en_elec_trade_county$source == "elec_import"],
  en_elec_trade_county$county[en_elec_trade_county$target == "elec_export"])
if (!setequal(traded_counties, counties)) {
  warning("electricity trade: no import/export flow for ",
          paste(setdiff(counties, traded_counties), collapse = ", "), call. = FALSE)
}


en_fuel_gen_use_loss_all_trade <- rbind(en_fuel_gen_use_loss_all, en_elec_trade_county) %>%
  filter(year %in% YEARS_TO_ENSURE)
validate_flows(en_fuel_gen_use_loss_all_trade, "en_fuel_gen_use_loss_all_trade",
               strict_years = TRUE)
# ExtraNotes: clamped at publication. validate_flows(strict_years = TRUE) now also
# asserts no year outside YEARS_TO_ENSURE and that every county label is canonical, so
# the 2006-2065 leak and the "Dekalb" spelling cannot come back unnoticed.

# Report the trade balance, which is a headline result in its own right.
if (exists("en_elec_trade_county")) {
  message("  electricity trade by county (EJ, mean over ",
          min(YEARS_TO_ENSURE), "-", max(YEARS_TO_ENSURE), "):")
  en_elec_trade_county %>%
    mutate(dir = if_else(source == "elec_import", "import", "export")) %>%
    group_by(county, dir) %>% summarise(v = mean(value), .groups = "drop") %>%
    arrange(desc(v)) %>%
    purrr::pwalk(function(county, dir, v)
      message(sprintf("    %-10s %-7s %.5f", county, dir, v)))
}


if (MAKE_PLOT) plot_sankey_pro(en_fuel_gen_use_loss_all_trade)

if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     animate = T, show_values_in_labels = T, label_units = "PJ")


if (MAKE_PLOT) plot_sankey_enhanced(en_fuel_gen_use_loss_all_trade %>%
                       group_by(county, year, source, target, units) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% pretty_labels(),
                     reg = "Fulton", animate = T, show_values_in_labels = T, label_units = "PJ")


# TODO: improve colors; ability to pass on units column to have both MGD and PJ in labels
# TODO: remove ww trade labeling but have insights of energy, water movement
# TODO: push to repository; then to pnnl github
# TODO: reporting
# TODO: paper draft

if (SAVE_FILES) {
###############################################################################%
# SAVING METRO ----
###############################################################################%

message("Saving energy outputs...")

write_csv(en_fuel_gen_use_loss_all_trade_metro,
          file.path(SAVE_DIR, "energy/01_metro_energy_flows.csv"))

save_sankey(
  plot_sankey_enhanced(
    en_fuel_gen_use_loss_all_trade_metro %>%
      group_by(year, source, target, units) %>%
      summarise(value = sum(value) * EJ_to_PJ, .groups = "drop"),
    animate = TRUE, show_values_in_labels = TRUE, label_units = "PJ"),
  file.path(SAVE_DIR, "energy/01_metro_energy.html"))

###############################################################################%
# SAVING COUNTY ----
###############################################################################%

write_csv(en_fuel_gen_use_loss_all_trade,
          file.path(SAVE_DIR, "energy/02_county_energy_flows.csv"))

save_county_sankeys(en_fuel_gen_use_loss_all_trade, "energy", "02", "energy",
                    prep_fn = function(df) df %>%
                      group_by(county, year, source, target, units) %>%
                      summarise(value = sum(value) * EJ_to_PJ, .groups = "drop"),
                    label_units = "PJ")

}
