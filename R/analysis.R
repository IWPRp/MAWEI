# MAWEI analysis — metrics, counter-factuals and validation
#
# Reads the published flow tables (fast: no pipeline rerun) and writes every number the
# manuscript quotes to docs_analysis/analysis_outputs/ as CSV, so a figure or a sentence can
# always be traced to a file.
#
#   Rscript R/analysis.R
#
# Sections
#   A  traditional accounting: sector totals, shares, trends, per-capita
#   B  water performance: NRW counter-factual, I&I, circularity, septic
#   C  energy performance: fuel shift, efficiency, LLNL comparison, self-sufficiency
#   D  the coupling: intensities both ways, asymmetry
#   E  networks and space: transfer network, basin concentration, spatial coverage
#   F  scenarios: data-centre load growth
#   G  independent validation against EPA NPDES
#
# Hassan Niazi / MAWEI

source("functions.R")
suppressMessages(library(sf))

OUT <- "docs_analysis/analysis_outputs/"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
put <- function(x, name) {
  write_csv(x, paste0(OUT, name, ".csv"))
  message(sprintf("  %-42s %4d rows", paste0(name, ".csv"), nrow(x)))
  invisible(x)
}

message("== reading published flow tables ==")
wm <- read_csv(paste0(SAVE_DIR, "/water/01_metro_water_flows.csv"), show_col_types = FALSE)
wc <- read_csv(paste0(SAVE_DIR, "/water/02_county_water_flows.csv"), show_col_types = FALSE)
em <- read_csv(paste0(SAVE_DIR, "/energy/01_metro_energy_flows.csv"), show_col_types = FALSE)
ec <- read_csv(paste0(SAVE_DIR, "/energy/02_county_energy_flows.csv"), show_col_types = FALSE)

# The energy tables are written in EJ; everything reported here is PJ.
em <- em %>% mutate(value = value * EJ_to_PJ, units = "PJ")
ec <- ec %>% mutate(value = value * EJ_to_PJ, units = "PJ")

pop <- read_csv(paste0(DATA_DIR, "cc-est2024-agesex-all.csv.gz"), show_col_types = FALSE,
                progress = FALSE) %>% clean_col_names() %>%
  filter(stname == "Georgia", year >= 2) %>%
  mutate(county = str_replace(ctyname, " County", ""), year = year + 2018) %>%
  select(county, year, pop = popestimate) %>%
  filter(county %in% counties)
pop_metro <- pop %>% group_by(year) %>% summarise(pop = sum(pop), .groups = "drop")

# County geometry and the facility coordinates, needed by the spatial and data-centre sections.
sf_cty <- st_read(paste0(DATA_DIR, "geojson-counties-fips.json"), quiet = TRUE) %>%
  rename_with(tolower) %>% filter(id %in% fips) %>%
  mutate(county = name) %>% select(county, geometry)
xy <- read_csv(paste0(DATA_DIR, "spatial_facility_coords.csv"), show_col_types = FALSE)

# Latest study year, used wherever a cross-section is reported.
YR <- max(YEARS_TO_ENSURE)

# Helper: total inflow to / outflow from a node, per year
inflow  <- function(df, node) df %>% filter(target == node) %>%
  group_by(year) %>% summarise(v = sum(value), .groups = "drop")
outflow <- function(df, node) df %>% filter(source == node) %>%
  group_by(year) %>% summarise(v = sum(value), .groups = "drop")

# Plant labels differ between the water and energy tables ("Bowen Plant" vs "Bowen") and again
# in the spatial extract, so every side is reduced to a common key rather than joined on the raw
# label. Joining on the raw label silently produces an empty result.
plant_key <- function(x) case_when(grepl("Bowen", x) ~ "Bowen",
                                  grepl("McDonough", x) ~ "Jack McDonough",
                                  grepl("Yates", x) ~ "Yates", TRUE ~ NA_character_)

###############################################################################%
message("\n== A. traditional accounting ==")

# A1 water: the headline balance, every year. This is the paper's Table 1 water panel.
water_balance <- tibble(year = YEARS_TO_ENSURE) %>%
  left_join(outflow(wm, "surfaceWater") %>% rename(surface = v), by = "year") %>%
  left_join(outflow(wm, "groundwaterAllBasins") %>% rename(groundwater = v), by = "year") %>%
  left_join(outflow(wm, "subsurface") %>% rename(inflow_infiltration = v), by = "year") %>%
  left_join(outflow(wm, "ww_imports") %>% rename(transfers_in = v), by = "year") %>%
  left_join(outflow(wm, "publicWatSup") %>% rename(pws_throughput = v), by = "year") %>%
  left_join(inflow(wm, "wastewater") %>% rename(ww_collected = v), by = "year") %>%
  left_join(inflow(wm, "in-county treatment") %>% rename(treated = v), by = "year") %>%
  left_join(inflow(wm, "losses") %>% rename(losses = v), by = "year") %>%
  left_join(inflow(wm, "septic") %>% rename(septic = v), by = "year") %>%
  mutate(withdrawal = surface + groundwater,
         system_input = surface + groundwater + inflow_infiltration + transfers_in,
         gw_share_pct = 100 * groundwater / withdrawal,
         loss_share_pct = 100 * losses / system_input) %>%
  left_join(pop_metro, by = "year") %>%
  mutate(withdrawal_gpcd = withdrawal * 1e6 / pop)
put(water_balance, "A1_water_balance_metro")

# A2 energy: the same, for energy. Source nodes are those never appearing as a target.
en_sources <- setdiff(unique(em$source), unique(em$target))
energy_balance <- tibble(year = YEARS_TO_ENSURE) %>%
  left_join(em %>% filter(source %in% en_sources) %>% group_by(year) %>%
              summarise(primary_input = sum(value), .groups = "drop"), by = "year") %>%
  left_join(outflow(em, "out_metro_elec_import") %>% rename(elec_imports = v), by = "year") %>%
  left_join(inflow(em, "electricity") %>% rename(elec_supply = v), by = "year") %>%
  left_join(inflow(em, "energy_services") %>% rename(services = v), by = "year") %>%
  left_join(inflow(em, "rejected_energy") %>% rename(rejected_enduse = v), by = "year") %>%
  left_join(inflow(em, "efficiency_losses") %>% rename(plant_rejected = v), by = "year") %>%
  left_join(inflow(em, "td_losses") %>% rename(td_losses = v), by = "year") %>%
  left_join(inflow(em, "elec_own_use") %>% rename(plant_own_use = v), by = "year") %>%
  left_join(inflow(em, "en4water") %>% rename(water_services_energy = v), by = "year") %>%
  mutate(across(everything(), ~replace_na(., 0)),
         total_input = primary_input,
         all_rejected = rejected_enduse + plant_rejected + td_losses + plant_own_use,
         enduse_efficiency_pct = 100 * services / (services + rejected_enduse),
         system_efficiency_pct = 100 * services / total_input,
         import_share_pct = 100 * elec_imports / elec_supply) %>%
  left_join(pop_metro, by = "year") %>%
  mutate(energy_gj_per_capita = total_input * 1e6 / pop)
put(energy_balance, "A2_energy_balance_metro")

# A3 water by sector, with shares. Sector nodes receive from supply and emit to collection.
WSEC <- c("residential", "commercial", "industrial", "agricultural")
water_sector <- wm %>% filter(target %in% WSEC) %>%
  group_by(year, sector = target) %>% summarise(supplied = sum(value), .groups = "drop") %>%
  left_join(wm %>% filter(source %in% WSEC, target == "wastewater") %>%
              group_by(year, sector = source) %>%
              summarise(returned = sum(value), .groups = "drop"), by = c("year", "sector")) %>%
  left_join(wm %>% filter(source %in% WSEC, target == "losses") %>%
              group_by(year, sector = source) %>%
              summarise(consumed = sum(value), .groups = "drop"), by = c("year", "sector")) %>%
  mutate(across(c(returned, consumed), ~replace_na(., 0))) %>%
  group_by(year) %>% mutate(share_pct = 100 * supplied / sum(supplied)) %>% ungroup() %>%
  # Return ratio is the fraction of supplied water that comes back as sewage; its complement is
  # consumptive use plus any cross-dataset inconsistency. Irrigation-heavy sectors sit lowest.
  mutate(return_ratio = returned / supplied)
put(water_sector, "A3_water_by_sector")

# A4 energy by sector
ESEC <- intersect(END_USE_SECTORS, unique(em$target))
energy_sector <- em %>% filter(target %in% ESEC) %>%
  group_by(year, sector = target) %>% summarise(consumed = sum(value), .groups = "drop") %>%
  group_by(year) %>% mutate(share_pct = 100 * consumed / sum(consumed)) %>% ungroup()
put(energy_sector, "A4_energy_by_sector")

# A5 fuel mix and the 2020->2024 shift
fuel_mix <- em %>% filter(source %in% en_sources) %>%
  group_by(year, fuel = source) %>% summarise(pj = sum(value), .groups = "drop") %>%
  group_by(year) %>% mutate(share_pct = 100 * pj / sum(pj)) %>% ungroup()
put(fuel_mix, "A5_fuel_mix")

fuel_shift <- fuel_mix %>% filter(year %in% range(YEARS_TO_ENSURE)) %>%
  select(fuel, year, pj, share_pct) %>%
  pivot_wider(names_from = year, values_from = c(pj, share_pct)) %>%
  rename_with(~gsub("_20", "_", .)) %>%
  mutate(pj_change_pct = 100 * (pj_24 - pj_20) / pj_20,
         share_change_pp = share_pct_24 - share_pct_20) %>%
  arrange(desc(pj_24))
put(fuel_shift, "A5b_fuel_shift_2020_2024")

# A6 basin withdrawals and concentration.
# Herfindahl index on basin shares: 1 means a single-source system, 1/n a perfectly spread one.
basin <- wm %>% filter(source == "surfaceWater") %>%
  group_by(year, basin = target) %>% summarise(mgd = sum(value), .groups = "drop") %>%
  group_by(year) %>% mutate(share_pct = 100 * mgd / sum(mgd)) %>% ungroup()
put(basin, "A6_basin_withdrawals")
put(basin %>% group_by(year) %>%
      summarise(hhi = sum((share_pct / 100)^2),
                top_basin = basin[which.max(mgd)],
                top_share_pct = max(share_pct), .groups = "drop"),
    "A6b_basin_concentration")

# A7 discharge destinations
discharge <- wm %>% filter(source == "in-county treatment") %>%
  group_by(year, destination = target) %>% summarise(mgd = sum(value), .groups = "drop") %>%
  group_by(year) %>% mutate(share_pct = 100 * mgd / sum(mgd)) %>% ungroup()
put(discharge, "A7_discharge_destinations")

# A8 county profile: the cross-sectional table behind most county figures
county_profile <- wc %>% filter(source == "publicWatSup") %>%
  group_by(county, year) %>%
  summarise(pws_out = sum(value), nrw = sum(value[target == "losses"]), .groups = "drop") %>%
  mutate(nrw_pct = 100 * nrw / pws_out) %>%
  left_join(wc %>% filter(source == "subsurface") %>% group_by(county, year) %>%
              summarise(ii = sum(value), .groups = "drop"), by = c("county", "year")) %>%
  left_join(wc %>% filter(target == "wastewater") %>% group_by(county, year) %>%
              summarise(collected = sum(value), .groups = "drop"), by = c("county", "year")) %>%
  left_join(wc %>% filter(target == "septic") %>% group_by(county, year) %>%
              summarise(septic = sum(value), .groups = "drop"), by = c("county", "year")) %>%
  left_join(ec %>% filter(target %in% ESEC) %>% group_by(county, year) %>%
              summarise(energy_pj = sum(value), .groups = "drop"), by = c("county", "year")) %>%
  left_join(pop, by = c("county", "year")) %>%
  mutate(across(c(ii, collected, septic), ~replace_na(., 0)),
         ii_share_pct = 100 * ii / collected,
         septic_share_pct = 100 * septic / (septic + collected),
         pws_gpcd = pws_out * 1e6 / pop)
put(county_profile, "A8_county_profile")

# A9 growth rates. CAGR over the period, so water and energy are comparable to population.
cagr <- function(first, last, n) 100 * ((last / first)^(1 / n) - 1)
n_yr <- length(YEARS_TO_ENSURE) - 1
growth <- tibble(
  quantity = c("population", "water withdrawal", "public supply", "wastewater collected",
               "energy input", "electricity supply", "water services energy"),
  first = c(pop_metro$pop[1], water_balance$withdrawal[1], water_balance$pws_throughput[1],
            water_balance$ww_collected[1], energy_balance$total_input[1],
            energy_balance$elec_supply[1], energy_balance$water_services_energy[1]),
  last  = c(last(pop_metro$pop), last(water_balance$withdrawal), last(water_balance$pws_throughput),
            last(water_balance$ww_collected), last(energy_balance$total_input),
            last(energy_balance$elec_supply), last(energy_balance$water_services_energy))) %>%
  mutate(total_change_pct = 100 * (last - first) / first,
         cagr_pct = cagr(first, last, n_yr),
         # Decoupling: growth in a resource relative to growth in population. Below 1 means the
         # region is using less per person over time.
         vs_population = cagr_pct / cagr(pop_metro$pop[1], last(pop_metro$pop), n_yr))
put(growth, "A9_growth_and_decoupling")

###############################################################################%
message("\n== B. water performance ==")

# B1 NRW counter-factuals. NRW is supplied as a fixed per-county fraction, so this is a
# cross-sectional efficiency question, never a trend.
nrw24 <- county_profile %>% filter(year == max(YEARS_TO_ENSURE))
metro_nrw <- 100 * sum(nrw24$nrw) / sum(nrw24$pws_out)
best_nrw <- min(nrw24$nrw_pct)
nrw_cf <- nrw24 %>%
  mutate(gap_to_metro = pmax(nrw - pws_out * metro_nrw / 100, 0),
         gap_to_best  = pmax(nrw - pws_out * best_nrw / 100, 0)) %>%
  select(county, pws_out, nrw, nrw_pct, gap_to_metro, gap_to_best) %>%
  arrange(desc(gap_to_best))
put(nrw_cf, "B1_nrw_counterfactual")

# Receiving-type labels are title case in the published table ("Reuse", "River"), so these
# lookups match case-insensitively rather than assuming a convention.
dest_mgd <- function(pattern, yr = max(YEARS_TO_ENSURE)) {
  v <- discharge %>% filter(year == yr, grepl(pattern, destination, ignore.case = TRUE)) %>%
    pull(mgd) %>% sum()
  if (length(v) == 0) NA_real_ else v
}
thermo_total <- wm %>% filter(year == max(YEARS_TO_ENSURE), grepl("Basin", source),
                              grepl("Bowen|McDonough|Yates", target)) %>% pull(value) %>% sum()

put(tibble(scenario = c("all counties at metro average", "all counties at best observed county",
                        "reference: thermoelectric withdrawal", "reference: reuse volume"),
           basis_pct = c(metro_nrw, best_nrw, NA, NA),
           mgd = c(sum(nrw_cf$gap_to_metro), sum(nrw_cf$gap_to_best),
                   thermo_total, dest_mgd("^reuse$"))),
    "B1b_nrw_scenarios")

# B2 the energetic cost of I&I. Infiltration is collected, conveyed and treated at the same
# intensity as real sewage, so it carries a real and avoidable energy penalty.
ii_energy <- county_profile %>%
  mutate(ii_mg_yr = ii * DAYS_PER_YEAR,
         kwh_treat = ii_mg_yr * WW_TREATMENT_ENERGY_INT,
         kwh_convey = ii_mg_yr * DISTRIBUTION_ENERGY_INT,
         kwh_total = kwh_treat + kwh_convey,
         pj_total = kwh_total * kWh_to_EJ * EJ_to_PJ) %>%
  select(county, year, ii, ii_share_pct, kwh_total, pj_total)
put(ii_energy, "B2_ii_energy_penalty")

ii_metro <- ii_energy %>% group_by(year) %>%
  summarise(ii_mgd = sum(ii), pj = sum(pj_total), .groups = "drop") %>%
  left_join(energy_balance %>% select(year, water_services_energy, total_input), by = "year") %>%
  mutate(share_of_water_energy_pct = 100 * pj / water_services_energy,
         # households equivalent: US average is ~10,500 kWh/yr per home
         homes_equivalent = (pj / EJ_to_PJ / kWh_to_EJ) / 10500)
put(ii_metro, "B2b_ii_energy_metro")

# B3 circularity. Reuse as a share of treated effluent is the standard water-reuse metric;
# adding septic return gives a broader "returned to land or reused" figure.
circ <- discharge %>%
  mutate(d = str_to_lower(destination)) %>%
  filter(d %in% c("reuse", "land")) %>%
  group_by(year, d) %>% summarise(mgd = sum(mgd), .groups = "drop") %>%
  pivot_wider(names_from = d, values_from = mgd) %>%
  left_join(water_balance %>% select(year, treated, withdrawal, septic), by = "year") %>%
  mutate(across(any_of(c("reuse", "land")), ~replace_na(., 0)),
         reuse_pct_of_effluent = 100 * reuse / treated,
         reuse_pct_of_withdrawal = 100 * reuse / withdrawal,
         land_pct_of_effluent = 100 * land / treated,
         nonstream_return_pct = 100 * (reuse + land + septic) / (treated + septic))
put(circ, "B3_circularity")

# B4 septic vs sewered structure
septic_struct <- county_profile %>%
  select(county, year, septic, collected, septic_share_pct, pop) %>%
  mutate(septic_gpcd = septic * 1e6 / pop)
put(septic_struct, "B4_septic_structure")

###############################################################################%
message("\n== C. energy performance ==")

# C1 county electricity self-sufficiency: local generation against local consumption.
# Both sides must exclude the trade flows themselves. The county electricity node balances by
# construction, so counting elec_export as consumption makes every exporting county appear
# exactly 100% self-sufficient and destroys the metric.
elec_gen <- ec %>% filter(target == "electricity", source != "elec_import") %>%
  group_by(county, year) %>% summarise(gen = sum(value), .groups = "drop")
elec_use <- ec %>% filter(source == "electricity", target != "elec_export") %>%
  group_by(county, year) %>% summarise(use = sum(value), .groups = "drop")
selfsuff <- full_join(elec_gen, elec_use, by = c("county", "year")) %>%
  mutate(across(c(gen, use), ~replace_na(., 0)),
         self_sufficiency_pct = 100 * gen / use,
         surplus_pj = gen - use,
         net_position = case_when(gen > use * 1.001 ~ "exporter",
                                  gen < use * 0.999 ~ "importer",
                                  TRUE ~ "balanced"))
put(selfsuff, "C1_electricity_self_sufficiency")

# C2 useful energy against the LLNL benchmark, and corrected for a realistic vehicle fleet.
# The LLNL convention gives transport 0.65, but a light-duty fleet delivers 0.20-0.25 of fuel
# energy to motion. Since transport is about half of metro end use, the published efficiency is
# materially optimistic and the corrected figure belongs beside it.
eff_corrected <- energy_sector %>%
  mutate(eff_llnl = if_else(sector %in% names(SECTOR_EFFICIENCY),
                            SECTOR_EFFICIENCY[sector], DEFAULT_EFFICIENCY),
         eff_real = if_else(grepl("transport", sector), TRANSPORT_EFFICIENCY_REAL, eff_llnl),
         services_llnl = consumed * eff_llnl,
         services_real = consumed * eff_real) %>%
  group_by(year) %>%
  summarise(enduse = sum(consumed),
            services_llnl = sum(services_llnl),
            services_real = sum(services_real), .groups = "drop") %>%
  mutate(eff_llnl_pct = 100 * services_llnl / enduse,
         eff_real_pct = 100 * services_real / enduse,
         overstatement_pp = eff_llnl_pct - eff_real_pct)
put(eff_corrected, "C2_efficiency_llnl_vs_corrected")

# C3 generation by plant, with capacity factor where capacity is known
plant_gen <- em %>% filter(target == "electricity", source != "out_metro_elec_import") %>%
  group_by(year, plant = source) %>% summarise(pj = sum(value), .groups = "drop")
plant_cap <- xy %>% filter(kind == "power plant") %>%
  select(plant_name = name, capacity_mw = capacity, lat, lon)
put(plant_gen %>%
      mutate(k = plant_key(plant)) %>%
      left_join(plant_cap, by = c("k" = "plant_name")) %>%
      mutate(mwh = pj / EJ_to_PJ / MWh_to_EJ,
             capacity_factor = mwh / (capacity_mw * HOURS_PER_YEAR)) %>%
      select(-k),
    "C3_generation_by_plant")

###############################################################################%
message("\n== D. the coupling ==")

# D1 energy intensity of water, by county: kWh per million gallons delivered.
# This is the standard comparable unit for water-sector energy and lets a county be placed
# against published benchmarks rather than only against its neighbours.
en4w_county <- ec %>% filter(target == "en4water") %>%
  group_by(county, year) %>% summarise(pj = sum(value), .groups = "drop")
w_delivered <- wc %>% filter(source == "publicWatSup", target != "losses") %>%
  group_by(county, year) %>% summarise(mgd = sum(value), .groups = "drop")
intensity_w <- full_join(en4w_county, w_delivered, by = c("county", "year")) %>%
  filter(!is.na(pj), !is.na(mgd), mgd > 0) %>%
  mutate(kwh_per_mg = (pj / EJ_to_PJ / kWh_to_EJ) / (mgd * DAYS_PER_YEAR))
put(intensity_w, "D1_energy_intensity_of_water")

# D2 water intensity of electricity: gallons withdrawn per kWh generated, by plant.
# Cooling technology is legible in this number: once-through withdraws far more per kWh than a
# recirculating tower, which withdraws little and consumes most of what it takes.
thermo_w <- wm %>% filter(grepl("Basin", source), grepl("Bowen|McDonough|Yates", target)) %>%
  mutate(plant = plant_key(target)) %>%
  group_by(year, plant) %>% summarise(mgd = sum(value), .groups = "drop")
# The metro table carries TWO distinct loss sinks whose labels differ only by case: `losses`
# (lower case) is municipal non-revenue water plus sectoral consumptive use, while `Losses`
# (title case, from pretty_labels) is thermoelectric evaporative consumption. Matching case-
# insensitively here would silently merge two unrelated quantities.
thermo_c <- wm %>% filter(grepl("Bowen|McDonough|Yates", source),
                          str_to_lower(target) == "losses") %>%
  mutate(plant = plant_key(source)) %>%
  group_by(year, plant) %>% summarise(consumed_mgd = sum(value), .groups = "drop")
stopifnot(nrow(thermo_c) > 0)
intensity_e <- thermo_w %>%
  left_join(thermo_c, by = c("year", "plant")) %>%
  left_join(plant_gen %>% mutate(plant = plant_key(plant)) %>% filter(!is.na(plant)) %>%
              group_by(year, plant) %>% summarise(pj = sum(pj), .groups = "drop"),
            by = c("year", "plant")) %>%
  mutate(mwh = pj / EJ_to_PJ / MWh_to_EJ,
         gal_per_kwh_withdrawn = (mgd * 1e6 * DAYS_PER_YEAR) / (mwh * 1000),
         gal_per_kwh_consumed = (consumed_mgd * 1e6 * DAYS_PER_YEAR) / (mwh * 1000),
         consumption_ratio = consumed_mgd / mgd)
put(intensity_e, "D2_water_intensity_of_electricity")

# D3 coupling asymmetry. Each direction is expressed as a share of its own system, so the two
# are directly comparable and the binding direction is explicit.
asym <- energy_balance %>% select(year, water_services_energy, total_input, elec_supply) %>%
  left_join(thermo_w %>% group_by(year) %>% summarise(thermo_mgd = sum(mgd), .groups = "drop"),
            by = "year") %>%
  left_join(water_balance %>% select(year, withdrawal, system_input), by = "year") %>%
  mutate(e4w_share_of_energy_pct = 100 * water_services_energy / total_input,
         e4w_share_of_electricity_pct = 100 * water_services_energy / elec_supply,
         w4e_share_of_withdrawal_pct = 100 * thermo_mgd / withdrawal,
         asymmetry_ratio = w4e_share_of_withdrawal_pct / e4w_share_of_energy_pct)
put(asym, "D3_coupling_asymmetry")

###############################################################################%
message("\n== E. networks and space ==")

# E1 inter-county sewage transfer network. Degree and net position show which counties are
# structurally dependent on a neighbour's treatment capacity.
tr <- wc %>% filter(grepl("^inFrom", source)) %>%
  mutate(from_county = str_split_i(source, "_", 2)) %>%
  group_by(from_county, to_county = county, year) %>%
  summarise(mgd = sum(value), .groups = "drop")
put(tr, "E1_transfer_edges")

net <- bind_rows(
  tr %>% group_by(county = from_county, year) %>%
    summarise(exported = sum(mgd), out_degree = n_distinct(to_county), .groups = "drop"),
  tr %>% group_by(county = to_county, year) %>%
    summarise(imported = sum(mgd), in_degree = n_distinct(from_county), .groups = "drop")) %>%
  group_by(county, year) %>%
  summarise(across(c(exported, imported, out_degree, in_degree),
                   ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  left_join(county_profile %>% select(county, year, collected), by = c("county", "year")) %>%
  mutate(net_mgd = imported - exported,
         export_dependency_pct = 100 * exported / collected,
         role = case_when(exported > 0 & imported > 0 ~ "both",
                          exported > 0 ~ "net exporter",
                          imported > 0 ~ "net importer", TRUE ~ "self-contained"))
put(net, "E1b_transfer_network_nodes")

# E2 spatial coverage of the coordinate resolution. Reported because it is the binding constraint
# on any facility-level map, and because the contrast between the two candidate sources is itself
# a methodological result: the state permit inventory is usable, the federal outfall layer is not.
put(xy %>% group_by(kind, coord_decimals) %>% summarise(n = n(), .groups = "drop"),
    "E2_coordinate_precision")

###############################################################################%
message("\n== F. scenarios ==")

# F1 data-centre load growth. Metro Atlanta has become one of the fastest-growing data-centre
# markets in the United States, so the question is what incremental electricity demand does to
# the water footprint of supply. Two cooling routes bracket the answer: evaporative cooling at
# the data centre, or no on-site water but more generation upstream.
DC_SCENARIOS <- c(500, 1000, 2000, 3000)   # MW of new continuous load
DC_LOAD_FACTOR <- 0.9
DC_WUE_L_PER_KWH <- 1.8                    # on-site evaporative water use efficiency
gen_wi <- intensity_e %>% filter(year == max(YEARS_TO_ENSURE)) %>%
  summarise(w = weighted.mean(gal_per_kwh_withdrawn, mwh, na.rm = TRUE),
            c = weighted.mean(gal_per_kwh_consumed, mwh, na.rm = TRUE))
stopifnot(is.finite(gen_wi$w), is.finite(gen_wi$c))

dc <- tibble(new_load_mw = DC_SCENARIOS) %>%
  mutate(twh_yr = new_load_mw * DC_LOAD_FACTOR * HOURS_PER_YEAR / 1e6,
         pj_yr = twh_yr * 3.6,
         pct_of_metro_electricity = 100 * pj_yr / last(energy_balance$elec_supply),
         onsite_cooling_mgd = twh_yr * 1e9 * DC_WUE_L_PER_KWH / 3.785 / 1e6 / DAYS_PER_YEAR,
         upstream_withdrawal_mgd = twh_yr * 1e9 * gen_wi$w / 1e6 / DAYS_PER_YEAR,
         upstream_consumption_mgd = twh_yr * 1e9 * gen_wi$c / 1e6 / DAYS_PER_YEAR,
         total_water_mgd = onsite_cooling_mgd + upstream_consumption_mgd,
         pct_of_metro_withdrawal = 100 * total_water_mgd / last(water_balance$withdrawal),
         # the comparison that matters: could recovered leakage cover it?
         pct_of_nrw_recoverable = 100 * total_water_mgd / sum(nrw_cf$gap_to_best))
put(dc, "F1_datacentre_scenarios")

###############################################################################%
message("\n== G. validation against EPA NPDES ==")

norm_fac <- function(x) x %>% str_to_lower() %>% str_replace_all("[^a-z0-9 ]", " ") %>%
  str_remove_all("\\b(wpcp|wrf|wwtp|wtp|plant|water|pollution|control|reclamation|facility|the|of|city|county|authority|inc|llc|co|dept|department)\\b") %>%
  str_squish()

wwt <- read_csv(paste0(DATA_DIR, "water_wastewater_treatment.csv"), show_col_types = FALSE) %>%
  rename_all(tolower) %>% rename_with(~gsub(" |-", "_", .), everything())

# The EPA ECHO outfall layer is read directly rather than through a cached extract, so no
# intermediate file has to be kept. Its coordinates are unusable (mostly whole-degree
# placeholders) but its design flow and receiving water are an independent check on the study's
# manually compiled facility table.
ECHO <- paste0(DATA_DIR, "epa_npdes_outfalls_layer.csv.gz")
if (file.exists(ECHO)) {
  echo_raw <- read_csv(ECHO,
    col_select = c(FACILITY_NAME, STATE_CODE, FAC_COUNTY_NAME,
                   TOTAL_DESIGN_FLOW_NMBR, STATE_WATER_BODY_NAME),
    col_types = cols(.default = col_character()), progress = FALSE) %>%
    clean_col_names() %>%
    filter(state_code == "GA") %>%
    mutate(county = counties[match(str_to_lower(str_trim(fac_county_name)),
                                   str_to_lower(counties))],
           design_flow_mgd = as.numeric(total_design_flow_nmbr)) %>%
    filter(!is.na(county))

  epa_fac <- echo_raw %>% group_by(county, facility_name) %>%
    summarise(epa_design_mgd = suppressWarnings(max(design_flow_mgd, na.rm = TRUE)),
              epa_receiving = paste(unique(na.omit(state_water_body_name)), collapse = " | "),
              .groups = "drop") %>%
    mutate(epa_design_mgd = ifelse(is.finite(epa_design_mgd), epa_design_mgd, NA_real_),
           k = norm_fac(facility_name))

  val_cap <- wwt %>% select(county, facility_name, permitted_capacity) %>% distinct() %>%
    mutate(k = norm_fac(facility_name)) %>%
    inner_join(epa_fac %>% select(county, k, epa_name = facility_name, epa_design_mgd),
               by = c("county", "k")) %>%
    filter(!is.na(epa_design_mgd), epa_design_mgd > 0, !is.na(permitted_capacity)) %>%
    mutate(ratio = permitted_capacity / epa_design_mgd,
           agrees_10pct = abs(ratio - 1) < 0.10) %>%
    select(-k) %>% arrange(desc(abs(log(ratio))))
  put(val_cap, "G1_validation_permitted_capacity")

  put(tibble(metric = c("study facilities total",
                        "study facilities name-matched to EPA",
                        "of those, agreeing within 10% on capacity",
                        "median ratio study:EPA capacity",
                        "log-log correlation of capacities",
                        "study facilities with coordinates",
                        "share of historic throughput with coordinates (%)"),
             value = c(n_distinct(wwt$facility_name),
                       nrow(val_cap), sum(val_cap$agrees_10pct),
                       round(median(val_cap$ratio), 3),
                       round(cor(log(val_cap$permitted_capacity),
                                 log(val_cap$epa_design_mgd)), 3),
                       sum(xy$kind == "wastewater plant"),
                       round(100 * sum(wwt$average[wwt$facility_name %in%
                               xy$name[xy$kind == "wastewater plant"]], na.rm = TRUE) /
                             sum(wwt$average, na.rm = TRUE), 2))),
      "G1b_validation_summary")
} else {
  message("  EPA ECHO layer absent; skipping validation section")
}

###############################################################################%
message("\n== H. data centres ==")

# Metro Atlanta is the second-largest US data-centre market after northern Virginia, so the
# question is not whether load grows but what its water and cost consequences are. Two datasets
# carry it: an inventory of existing facilities, and a siting projection that gives, per
# projected facility, its IT power, cooling technology split, water demand, water consumption and
# capital cost. That removes the need for the coarse assumptions the earlier scenario used.
DC <- paste0(DATA_DIR, "datacenter_atlas/")

# H1 existing inventory. County names arrive with a " County" suffix, so they are stripped and
# matched case-insensitively rather than assumed to be canonical.
dc_existing <- read_csv(paste0(DC, "im3_open_source_data_center_atlas_v2026.02.09/",
                               "im3_open_source_data_center_atlas_v2026.02.09.csv"),
                        show_col_types = FALSE, progress = FALSE) %>%
  filter(state_abb == "GA") %>%
  mutate(county = counties[match(str_to_lower(str_remove(str_trim(county), " County$")),
                                 str_to_lower(counties))])
dc_metro <- dc_existing %>% filter(!is.na(county))
put(dc_metro %>% select(county, operator, name, sqft, type, lat, lon), "H1_datacentres_existing")

put(dc_metro %>% group_by(county) %>%
      summarise(facilities = n(), sqft = sum(sqft, na.rm = TRUE), .groups = "drop") %>%
      mutate(sqft_share_pct = 100 * sqft / sum(sqft),
             # Power is estimated from floor area because the inventory reports no capacity.
             # 150 W per square foot of IT load is a mid-range figure for a modern facility, so
             # this is an order-of-magnitude estimate and is labelled as such.
             est_it_mw = sqft * 150 / 1e6,
             est_twh_yr = est_it_mw * 0.9 * HOURS_PER_YEAR / 1e6) %>%
      arrange(desc(sqft)),
    "H1b_datacentres_existing_by_county")

# H2 projections. Each file is one (growth, market-gravity) combination; every projected facility
# is a polygon carrying its own engineering and cost attributes, so the metro figure comes from a
# spatial intersection with the county boundaries rather than from a state share.
dc_files <- list.files(paste0(DC, "im3_projected_data_centers_v1.1"),
                       pattern = "geojson$", recursive = TRUE, full.names = TRUE)
# The county boundaries carry no declared CRS while the projection files are WGS84, so the CRS is
# set explicitly on both sides. st_intersects refuses to compare geometries whose CRS differ, and
# assuming they match silently would be worse than the error.
metro_union <- sf_cty %>% st_set_crs(4326) %>% st_union()

dc_proj <- map_dfr(dc_files, function(f) {
  g <- suppressWarnings(st_read(f, quiet = TRUE)) %>% filter(region == "georgia")
  if (nrow(g) == 0) return(NULL)
  g <- st_make_valid(st_transform(st_set_crs(g, st_crs(g) %||% 4326), 4326))
  # A projected site counts as metro if its polygon intersects the fifteen-county area.
  in_metro <- lengths(st_intersects(g, metro_union)) > 0
  g %>% st_drop_geometry() %>% mutate(in_metro = in_metro)
})

put(dc_proj %>%
      group_by(growth_scenario, market_gravity_weight) %>%
      summarise(ga_facilities = n(),
                ga_it_mw = sum(data_center_it_power_mw),
                ga_water_demand_mgy = sum(cooling_water_demand_mgy),
                ga_water_consumption_mgy = sum(cooling_water_consumption_mgy),
                ga_cost_musd = sum(total_cost_million_usd),
                metro_facilities = sum(in_metro),
                metro_it_mw = sum(data_center_it_power_mw[in_metro]),
                metro_water_demand_mgy = sum(cooling_water_demand_mgy[in_metro]),
                metro_water_consumption_mgy = sum(cooling_water_consumption_mgy[in_metro]),
                metro_cost_musd = sum(total_cost_million_usd[in_metro]),
                water_cooled_frac = mean(water_cooling_frac),
                .groups = "drop") %>%
      mutate(metro_share_pct = 100 * metro_facilities / ga_facilities,
             # Convert the projection's annual volumes to the MGD the water diagram uses, and
             # express against the measured system so the scenario is anchored, not free-floating.
             metro_water_demand_mgd = metro_water_demand_mgy / DAYS_PER_YEAR,
             metro_consumption_mgd = metro_water_consumption_mgy / DAYS_PER_YEAR,
             pct_of_metro_withdrawal = 100 * metro_water_demand_mgd / last(water_balance$withdrawal),
             pct_of_thermo_withdrawal = 100 * metro_water_demand_mgd / thermo_total,
             twh_yr = metro_it_mw * 0.9 * HOURS_PER_YEAR / 1e6,
             pj_yr = twh_yr * 3.6,
             pct_of_metro_electricity = 100 * pj_yr / last(energy_balance$elec_supply)) %>%
      arrange(factor(growth_scenario, levels = c("low", "moderate", "high", "higher")),
              market_gravity_weight),
    "H2_datacentre_projections")

# H3 the cooling-technology trade-off, which is the substantive water-energy finding here.
# A water-cooled facility evaporates water but spends little energy on cooling; a mechanically
# cooled one uses no water but adds electrical load, which is met upstream by generation that
# itself withdraws water. The projection carries both fractions, so the trade-off can be
# quantified rather than asserted.
gen_wi2 <- intensity_e %>% filter(year == max(YEARS_TO_ENSURE)) %>%
  summarise(c = weighted.mean(gal_per_kwh_consumed, mwh, na.rm = TRUE)) %>% pull(c)

put(dc_proj %>% filter(in_metro) %>%
      group_by(growth_scenario, market_gravity_weight) %>%
      summarise(n = n(),
                onsite_consumption_mgd = sum(cooling_water_consumption_mgy) / DAYS_PER_YEAR,
                cooling_energy_mwh = sum(cooling_energy_demand_mwh),
                .groups = "drop") %>%
      mutate(cooling_energy_water_mgd = cooling_energy_mwh * 1000 * gen_wi2 / 1e6 / DAYS_PER_YEAR,
             total_water_mgd = onsite_consumption_mgd + cooling_energy_water_mgd,
             upstream_share_pct = 100 * cooling_energy_water_mgd / total_water_mgd),
    "H3_datacentre_cooling_tradeoff")

# H4 where projected sites fall, by county. This is what makes the scenario local rather than
# regional: siting concentrates in a few counties, and those counties' own water systems differ.
dc_sites <- map_dfr(dc_files, function(f) {
  g <- suppressWarnings(st_read(f, quiet = TRUE)) %>% filter(region == "georgia")
  if (nrow(g) == 0) return(NULL)
  g <- st_make_valid(st_transform(st_set_crs(g, st_crs(g) %||% 4326), 4326))
  ctr <- suppressWarnings(st_centroid(g))
  hit <- st_intersects(ctr, st_set_crs(sf_cty, 4326))
  g %>% st_drop_geometry() %>%
    mutate(county = map_chr(hit, ~ if (length(.x)) sf_cty$county[.x[1]] else NA_character_),
           lon = st_coordinates(ctr)[, 1], lat = st_coordinates(ctr)[, 2])
}) %>% filter(!is.na(county))
put(dc_sites, "H4_datacentre_projected_sites")

put(dc_sites %>% group_by(growth_scenario, market_gravity_weight, county) %>%
      summarise(sites = n(), it_mw = sum(data_center_it_power_mw),
                water_mgd = sum(cooling_water_demand_mgy) / DAYS_PER_YEAR,
                cost_musd = sum(total_cost_million_usd), .groups = "drop"),
    "H4b_datacentre_sites_by_county")

###############################################################################%
message("\n== I. cost ==")

# Costs are not in the flow data, so every figure here is an order-of-magnitude estimate built
# from published unit costs and labelled as such. They are included because a volume alone does
# not tell a utility whether an intervention is worth making, and because the data-centre
# projection supplies a capital cost that would otherwise sit without comparison.
#
# Unit costs, mid-range US municipal values, stated so they can be replaced:
COST_WATER_TREAT_USD_PER_MG   <- 1500   # produce and deliver potable water
COST_WW_TREAT_USD_PER_MG      <- 2200   # collect and treat wastewater
COST_ELECTRICITY_USD_PER_KWH  <- 0.11   # industrial/municipal tariff
COST_LEAK_REPAIR_USD_PER_MGD  <- 2.2e6  # capital to permanently recover 1 MGD of leakage

put(tibble(item = c("water treatment and delivery", "wastewater collection and treatment",
                    "electricity", "leakage recovery capital"),
           unit = c("USD per MG", "USD per MG", "USD per kWh", "USD per MGD recovered"),
           value = c(COST_WATER_TREAT_USD_PER_MG, COST_WW_TREAT_USD_PER_MG,
                     COST_ELECTRICITY_USD_PER_KWH, COST_LEAK_REPAIR_USD_PER_MGD)),
    "I0_cost_assumptions")

# I1 the annual operating cost of the losses the diagram quantifies. Non-revenue water is water
# produced and paid for but never billed; infiltration is water never supplied yet collected,
# pumped and treated. Both are pure waste in cost terms, which is what makes them the first
# place a utility should look.
cost_losses <- county_profile %>% filter(year == YR) %>%
  select(county, nrw, ii, collected) %>%
  left_join(ii_energy %>% filter(year == YR) %>% select(county, kwh_total), by = "county") %>%
  mutate(nrw_cost_usd_yr = nrw * DAYS_PER_YEAR * COST_WATER_TREAT_USD_PER_MG,
         ii_treat_cost_usd_yr = ii * DAYS_PER_YEAR * COST_WW_TREAT_USD_PER_MG,
         ii_energy_cost_usd_yr = kwh_total * COST_ELECTRICITY_USD_PER_KWH,
         total_loss_cost_usd_yr = nrw_cost_usd_yr + ii_treat_cost_usd_yr) %>%
  arrange(desc(total_loss_cost_usd_yr))
put(cost_losses, "I1_cost_of_losses")

# I2 payback on leakage recovery, the comparison that turns a volume into a decision.
put(nrw_cf %>%
      mutate(annual_saving_usd = gap_to_best * DAYS_PER_YEAR * COST_WATER_TREAT_USD_PER_MG,
             capital_usd = gap_to_best * COST_LEAK_REPAIR_USD_PER_MGD,
             payback_years = if_else(annual_saving_usd > 0,
                                     capital_usd / annual_saving_usd, NA_real_)) %>%
      select(county, nrw_pct, recoverable_mgd = gap_to_best,
             annual_saving_usd, capital_usd, payback_years) %>%
      arrange(desc(recoverable_mgd)),
    "I2_leakage_recovery_payback")

# I3 the data-centre capital cost against what the same money would buy in leakage recovery.
# Both are capital, both buy water, so the comparison is legitimate and it is the sharpest way to
# state the opportunity cost.
put(dc_proj %>% filter(in_metro) %>%
      group_by(growth_scenario) %>%
      summarise(sites = n() / n_distinct(market_gravity_weight),
                cost_musd = mean(tapply(total_cost_million_usd, market_gravity_weight, sum)),
                water_mgd = mean(tapply(cooling_water_demand_mgy, market_gravity_weight, sum)) /
                            DAYS_PER_YEAR, .groups = "drop") %>%
      mutate(leakage_mgd_for_same_capital = cost_musd * 1e6 / COST_LEAK_REPAIR_USD_PER_MGD,
             ratio = leakage_mgd_for_same_capital / water_mgd),
    "I3_datacentre_capital_vs_leakage")

###############################################################################%
message("\n== J. spatial facility network ==")

# The transfer records name a destination FACILITY, and every one of those facilities now has a
# coordinate, so the sewage network can be drawn as it physically exists rather than as
# county-to-county abstraction. Origins are given as places within a county, so an origin is
# placed at its county centroid while a destination sits at its plant.
fac_xy <- xy %>% filter(kind == "wastewater plant") %>%
  select(facility = name, fac_county = county, fac_lat = lat, fac_lon = lon,
         fac_capacity = capacity, fac_basin = permit_basin)
cty_xy <- sf_cty %>% st_set_crs(4326) %>% st_centroid() %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>% st_drop_geometry()

conn_raw <- read_csv(paste0(DATA_DIR, "water_wastewater_connections.csv"),
                     show_col_types = FALSE) %>% rename_all(tolower) %>% rename(value = flow)

net_edges <- conn_raw %>%
  filter(year %in% YEARS_TO_ENSURE, fromcounty != tocounty,
         !grepl("copied", tolower(notes))) %>%
  replace_na(list(value = 0)) %>%
  # collapse the duplicate reporting exactly as the pipeline does, so the network totals agree
  group_by(fromcounty, fromplace, tocounty, tofacility, year) %>%
  summarise(mgd = max(value), .groups = "drop") %>%
  group_by(from_county = fromcounty, to_county = tocounty, facility = tofacility, year) %>%
  summarise(mgd = sum(mgd), .groups = "drop") %>%
  left_join(cty_xy %>% select(county, from_lon = lon, from_lat = lat),
            by = c("from_county" = "county")) %>%
  left_join(fac_xy, by = "facility") %>%
  # Great-circle distance between origin county centre and receiving plant. Conveyance energy
  # scales with distance and lift, so a long haul is a different proposition from a short one and
  # the network cannot be read on volume alone.
  mutate(distance_km = 6371 * acos(pmin(1,
           sin(from_lat * pi/180) * sin(fac_lat * pi/180) +
           cos(from_lat * pi/180) * cos(fac_lat * pi/180) *
           cos((fac_lon - from_lon) * pi/180))),
         # Conveyance energy uses the same intensity as municipal distribution, doubled for an
         # inter-county haul, matching the convention in the water pipeline.
         conveyance_kwh_yr = mgd * DAYS_PER_YEAR * DISTRIBUTION_ENERGY_INT * 2,
         treatment_kwh_yr = mgd * DAYS_PER_YEAR * WW_TREATMENT_ENERGY_INT,
         energy_cost_usd_yr = (conveyance_kwh_yr + treatment_kwh_yr) * COST_ELECTRICITY_USD_PER_KWH,
         mgd_km = mgd * distance_km)
put(net_edges, "J1_spatial_transfer_edges")

put(net_edges %>% filter(year == YR) %>%
      summarise(routes = n(), volume_mgd = sum(mgd),
                mean_distance_km = weighted.mean(distance_km, mgd, na.rm = TRUE),
                max_distance_km = max(distance_km, na.rm = TRUE),
                transport_work_mgd_km = sum(mgd_km, na.rm = TRUE),
                conveyance_gwh_yr = sum(conveyance_kwh_yr) / 1e6,
                energy_cost_musd_yr = sum(energy_cost_usd_yr) / 1e6),
    "J1b_spatial_network_summary")

# J2 receiving-plant loading: how much of each plant's throughput arrives from another county.
# A plant treating mostly imported sewage is doing regional work, which is invisible in a
# county-level account.
put(net_edges %>% filter(year == YR) %>%
      group_by(facility, fac_county, fac_capacity, fac_basin) %>%
      summarise(imported_mgd = sum(mgd), origin_counties = n_distinct(from_county),
                .groups = "drop") %>%
      mutate(imported_share_of_capacity_pct = 100 * imported_mgd / fac_capacity) %>%
      arrange(desc(imported_mgd)),
    "J2_receiving_plant_loading")

# J3 basin transfers. A transfer that crosses a basin divide moves water permanently out of one
# watershed and into another, which is a different act from moving it within a basin and is the
# form of transfer that matters legally in the ACF system.
put(net_edges %>% filter(year == YR, !is.na(fac_basin)) %>%
      left_join(fac_xy %>% select(from_facility_county = fac_county, origin_basin = fac_basin) %>%
                  distinct(from_facility_county, .keep_all = TRUE),
                by = c("from_county" = "from_facility_county")) %>%
      mutate(crosses_basin = !is.na(origin_basin) & origin_basin != fac_basin) %>%
      group_by(from_county, to_county, facility, origin_basin, dest_basin = fac_basin,
               crosses_basin) %>%
      summarise(mgd = sum(mgd), .groups = "drop") %>%
      arrange(desc(mgd)),
    "J3_interbasin_transfers")

###############################################################################%
message("\n== K. basins ==")

# The basin is the unit that matters legally and hydrologically, and it is not the county. A
# withdrawal permit, a discharge permit and an interstate compact all operate on watersheds, so
# the same flows have to be re-expressed on that geography before they can speak to policy.
BASIN_FILE <- paste0(DATA_DIR, "spatial_basins.geojson")

if (file.exists(BASIN_FILE)) {
  basins_sf <- st_read(BASIN_FILE, quiet = TRUE)
  cty_basin <- read_csv(paste0(DATA_DIR, "spatial_county_basin_area.csv"), show_col_types = FALSE)

  # K1 the basin balance. Withdrawal comes straight from the metro table; discharge has to be
  # routed through the receiving plant's basin, because effluent leaves the system where the plant
  # sits, not where the sewage was generated.
  basin_w <- wm %>% filter(source == "surfaceWater") %>%
    mutate(basin = str_remove(target, " Basin")) %>%
    group_by(basin, year) %>% summarise(withdrawal_mgd = sum(value), .groups = "drop")

  basin_thermo <- wm %>% filter(grepl("Basin", source), grepl("Bowen|McDonough|Yates", target)) %>%
    mutate(basin = str_remove(source, " Basin")) %>%
    group_by(basin, year) %>% summarise(thermo_mgd = sum(value), .groups = "drop")

  # Effluent by the basin of the receiving plant. Facility-level discharge is allocated from the
  # county treatment total in proportion to each plant's share of its county's capacity, because
  # the published county frame does not carry per-plant discharge.
  fac_basin <- xy %>% filter(kind == "wastewater plant", !is.na(permit_basin)) %>%
    mutate(basin = case_when(grepl("Chattahoochee", permit_basin) ~ "Chattahoochee",
                             grepl("Coosa|Etowah|Oostanaula|Coosawattee", permit_basin) ~ "Coosa_Etowah",
                             grepl("Ocmulgee", permit_basin) ~ "Ocmulgee",
                             grepl("Oconee", permit_basin) ~ "Oconee",
                             grepl("Flint", permit_basin) ~ "Flint",
                             grepl("Tallapoosa", permit_basin) ~ "Tallapoosa",
                             TRUE ~ permit_basin)) %>%
    group_by(county, basin) %>% summarise(cap = sum(capacity, na.rm = TRUE), .groups = "drop") %>%
    group_by(county) %>% mutate(cap_share = cap / sum(cap)) %>% ungroup()

  basin_d <- county_profile %>% select(county, year, collected) %>%
    left_join(fac_basin, by = "county", relationship = "many-to-many") %>%
    filter(!is.na(basin)) %>%
    mutate(discharge_mgd = collected * cap_share) %>%
    group_by(basin, year) %>% summarise(discharge_mgd = sum(discharge_mgd), .groups = "drop")

  basin_bal <- basin_w %>%
    full_join(basin_thermo, by = c("basin", "year")) %>%
    full_join(basin_d, by = c("basin", "year")) %>%
    mutate(across(c(withdrawal_mgd, thermo_mgd, discharge_mgd), ~replace_na(., 0))) %>%
    left_join(basins_sf %>% st_drop_geometry() %>%
                select(basin, area_sqkm, metro_area_sqkm, metro_share_pct), by = "basin") %>%
    mutate(total_withdrawal_mgd = withdrawal_mgd + thermo_mgd,
           net_export_mgd = total_withdrawal_mgd - discharge_mgd,
           # Withdrawal per unit of the basin that lies inside the metro. This is the closest
           # available proxy for pressure on the resource: a large withdrawal from a basin that is
           # barely in the study area is a different proposition from the same volume taken from a
           # basin the metro sits on top of.
           withdrawal_per_sqkm = total_withdrawal_mgd / pmax(metro_area_sqkm, 1),
           # A basin returning less than it takes is a net exporter of water out of that watershed.
           return_ratio = if_else(total_withdrawal_mgd > 0,
                                  discharge_mgd / total_withdrawal_mgd, NA_real_))
  put(basin_bal, "K1_basin_balance")

  # K2 which counties draw on which basin, and how much. This is the join that lets a county
  # result be read as a basin result.
  put(cty_basin %>%
        left_join(county_profile %>% filter(year == YR) %>%
                    select(county, pws_out, collected, nrw, ii), by = "county") %>%
        mutate(across(c(pws_out, collected, nrw, ii), ~ . * share_pct / 100,
                      .names = "{.col}_attributed")),
      "K2_county_basin_attribution")

  # K3 basins ranked by the share of metro supply they carry, against their share of metro land.
  # A basin supplying far more than its footprint is doing disproportionate work.
  put(basin_bal %>% filter(year == YR) %>%
        mutate(supply_share_pct = 100 * total_withdrawal_mgd / sum(total_withdrawal_mgd),
               land_share_pct = 100 * metro_area_sqkm / sum(metro_area_sqkm),
               burden_ratio = supply_share_pct / pmax(land_share_pct, 0.01)) %>%
        select(basin, total_withdrawal_mgd, supply_share_pct, metro_area_sqkm, land_share_pct,
               burden_ratio, discharge_mgd, return_ratio) %>%
        arrange(desc(burden_ratio)),
      "K3_basin_burden")

  # K4 inter-basin transfer of sewage. A transfer that crosses a divide moves water permanently
  # out of one watershed into another, which is the form of transfer that matters legally in the
  # Apalachicola-Chattahoochee-Flint system.
  cty_main <- cty_basin %>% group_by(county) %>% slice_max(share_pct, n = 1) %>%
    ungroup() %>% select(county, origin_basin = basin)
  put(net_edges %>% filter(year == YR) %>%
        left_join(cty_main, by = c("from_county" = "county")) %>%
        mutate(dest_basin = case_when(
                 grepl("Chattahoochee", fac_basin) ~ "Chattahoochee",
                 grepl("Coosa|Etowah|Oostanaula|Coosawattee", fac_basin) ~ "Coosa_Etowah",
                 grepl("Ocmulgee", fac_basin) ~ "Ocmulgee",
                 grepl("Oconee", fac_basin) ~ "Oconee",
                 grepl("Flint", fac_basin) ~ "Flint",
                 grepl("Tallapoosa", fac_basin) ~ "Tallapoosa", TRUE ~ fac_basin),
               crosses_divide = !is.na(origin_basin) & !is.na(dest_basin) &
                                origin_basin != dest_basin) %>%
        group_by(from_county, origin_basin, facility, fac_county, dest_basin, crosses_divide) %>%
        summarise(mgd = sum(mgd), distance_km = mean(distance_km), .groups = "drop") %>%
        arrange(desc(crosses_divide), desc(mgd)),
      "K4_interbasin_sewage_transfer")
} else {
  message("  basin layer absent; skipping basin section")
}

###############################################################################%
message("\n== L. gross and net transfers ==")

# Transfers have to be reported BOTH ways. Gross is the water actually moved, which is what
# determines pumping energy and pipe capacity. Net is the balance after offsetting exchanges,
# which is what determines whether a county depends on a neighbour. Reporting only one hides
# something: gross alone overstates dependency, net alone understates infrastructure use.
#
# ExtraNotes: county pairs genuinely exchange in BOTH directions, because utility service areas
# interleave and do not follow county lines. Eight of fifteen active pairs are bidirectional, so
# the distinction is material rather than theoretical.
conn_dir <- conn_raw %>%
  filter(year %in% YEARS_TO_ENSURE, fromcounty != tocounty,
         !grepl("copied", tolower(notes))) %>%
  replace_na(list(value = 0)) %>%
  group_by(fromcounty, fromplace, tocounty, tofacility, year) %>%
  summarise(mgd = max(value), .groups = "drop") %>%
  group_by(from_county = fromcounty, to_county = tocounty, year) %>%
  summarise(mgd = sum(mgd), .groups = "drop")

transfer_pairs <- conn_dir %>%
  mutate(a = pmin(from_county, to_county), b = pmax(from_county, to_county)) %>%
  group_by(a, b, year) %>%
  summarise(directions = n(),
            a_to_b = sum(mgd[from_county == first(a)]),
            b_to_a = sum(mgd[from_county == first(b)]),
            gross_mgd = sum(mgd), .groups = "drop") %>%
  mutate(net_mgd = abs(a_to_b - b_to_a),
         offsetting_mgd = gross_mgd - net_mgd,
         bidirectional = directions > 1,
         # Which way the net flow runs, which is the direction a map arrow should point.
         net_from = if_else(a_to_b >= b_to_a, a, b),
         net_to   = if_else(a_to_b >= b_to_a, b, a))
put(transfer_pairs, "L1_transfer_gross_net_pairs")

put(transfer_pairs %>% group_by(year) %>%
      summarise(pairs = n(), bidirectional_pairs = sum(bidirectional),
                gross_mgd = sum(gross_mgd), net_mgd = sum(net_mgd),
                offsetting_mgd = sum(offsetting_mgd), .groups = "drop") %>%
      mutate(offsetting_share_pct = 100 * offsetting_mgd / gross_mgd),
    "L1b_transfer_gross_net_summary")

# Per county, both measures side by side. A county with large gross and small net is trading, not
# depending.
put(bind_rows(
      conn_dir %>% group_by(county = from_county, year) %>%
        summarise(gross_out = sum(mgd), .groups = "drop"),
      conn_dir %>% group_by(county = to_county, year) %>%
        summarise(gross_in = sum(mgd), .groups = "drop")) %>%
      group_by(county, year) %>%
      summarise(across(c(gross_out, gross_in), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
      mutate(gross_total = gross_out + gross_in,
             net_position = gross_in - gross_out,
             trade_intensity = if_else(abs(net_position) > 0,
                                       gross_total / abs(net_position), NA_real_)) %>%
      left_join(county_profile %>% select(county, year, collected), by = c("county", "year")),
    "L2_transfer_by_county_gross_net")

###############################################################################%
message("\n== M. settlement, demographics and economics ==")

# Census tracts give a settlement geography 92 times finer than the county. The ACS 5-year tract
# extract carries measured population, income, housing and commuting, so density is a measurement
# rather than the tract-area proxy an earlier version had to use.
#
# ExtraNotes: the vintage differs on purpose. The ACS extract is the 2018-2022 5-year product while
# the flows cover 2020-2024, because no annual tract-level population exists -- 1-year ACS is not
# published below 65,000 population. It is therefore used for STRUCTURE, how population and income
# are distributed WITHIN a county, and never for trend. County-year population still comes from the
# annual Census vintage-2024 estimates. Tract population sums to the 2020 county estimate with a
# median error of 0.19% and a maximum of 1.04%, which is the check that attribution is right.
ACS_FILE <- paste0(DATA_DIR, "acs_tract_metro.csv")

if (file.exists(ACS_FILE)) {
  acs <- read_csv(ACS_FILE, show_col_types = FALSE, progress = FALSE)

  # M1 county settlement and socio-economic profile.
  #
  # ExtraNotes: two densities are reported and they answer different questions. Area density is
  # population over land area -- how crowded the county is. Population-weighted density is the
  # density of the tract the average RESIDENT lives in, which is what determines infrastructure
  # cost per customer. Their ratio measures how unevenly people are distributed: 1.0 would be
  # perfectly uniform.
  #
  # ExtraNotes: per-tract statistics are named distinctly from the county totals. Writing
  # `land_km2 = sum(land_km2)` alongside `median(land_km2)` makes summarise() rebind the name
  # mid-expression, so every spread came out as exactly 1. Vector-consuming statistics must be
  # computed before any same-name aggregation.
  acs_cty <- acs %>%
    group_by(county) %>%
    summarise(tracts = n(),
              acs_pop = sum(pop),
              land_km2 = sum(land_km2),
              water_km2 = sum(water_km2),
              # settlement pattern
              pw_density = weighted.mean(pop_density, pop),
              median_tract_density = median(pop_density),
              d90 = quantile(pop_density, 0.9),
              d10 = quantile(pop_density, 0.1),
              # housing
              housing_units = sum(housing_units),
              vacancy_pct = 100 * sum(housing_vacant) / sum(housing_units),
              persons_per_hh = weighted.mean(persons_per_hh, housing_occupied, na.rm = TRUE),
              # economics
              median_hh_income = median(median_hh_income, na.rm = TRUE),
              income_p10 = quantile(median_hh_income, 0.1, na.rm = TRUE),
              income_p90 = quantile(median_hh_income, 0.9, na.rm = TRUE),
              poverty_pct = 100 * sum(poverty_below) / sum(poverty_universe),
              # commuting
              mean_commute_min = weighted.mean(mean_commute_min, commuters, na.rm = TRUE),
              drove_alone_pct = 100 * sum(commute_drove_alone) / sum(commuters),
              transit_pct = 100 * sum(commute_transit) / sum(commuters),
              wfh_pct = 100 * sum(commute_wfh) / sum(commuters),
              .groups = "drop") %>%
    mutate(area_density = acs_pop / land_km2,
           # How much denser the average resident's neighbourhood is than the county average.
           density_unevenness = pw_density / area_density,
           # Interdecile density ratio. Preferred over max/median because a single very large
           # rural tract can dominate a max-based measure.
           density_p90_p10 = d90 / pmax(d10, 1),
           income_p90_p10 = income_p90 / pmax(income_p10, 1),
           water_share_pct = 100 * water_km2 / (land_km2 + water_km2)) %>%
    select(-d90, -d10) %>%
    arrange(desc(pw_density))
  put(acs_cty, "M1_settlement_by_county")

  # M1b tract-level distribution, metro-wide. Reported as quantiles rather than tract rows because
  # a single-tract 5-year estimate carries a margin of error of 20-30% of the estimate; the
  # distribution is robust where an individual value is not.
  put(acs %>%
        summarise(tracts = n(), pop = sum(pop),
                  across(c(pop_density, median_hh_income, mean_commute_min, poverty_rate,
                           vacancy_rate, persons_per_hh),
                         list(p10 = ~ quantile(.x, .1, na.rm = TRUE),
                              p50 = ~ quantile(.x, .5, na.rm = TRUE),
                              p90 = ~ quantile(.x, .9, na.rm = TRUE)))) %>%
        pivot_longer(-c(tracts, pop), names_to = "metric", values_to = "value"),
      "M1b_tract_distribution_metro")

  # M3 does settlement pattern explain the county spread in water performance?
  #
  # ExtraNotes: this is the question the tract data exists to answer. The infiltration factor and
  # the non-revenue rate are supplied per county with no stated basis, and if they track density or
  # income they are behaving like real infrastructure-age proxies; if they track nothing they are
  # closer to administrative assumptions. Fifteen counties is a small n, so the correlation is
  # reported with its p-value and read as suggestive, never as evidence of mechanism.
  wperf <- county_profile %>% filter(year == YR) %>%
    select(county, nrw_pct, ii_share_pct, septic_share_pct, pws_gpcd) %>%
    left_join(intensity_w %>% filter(year == YR) %>% select(county, kwh_per_mg), by = "county") %>%
    left_join(acs_cty %>% select(county, pw_density, area_density, density_unevenness,
                                 median_hh_income, poverty_pct, mean_commute_min,
                                 drove_alone_pct, transit_pct, persons_per_hh, vacancy_pct),
              by = "county")

  cor_pairs <- expand_grid(
    y = c("nrw_pct", "ii_share_pct", "septic_share_pct", "pws_gpcd", "kwh_per_mg"),
    x = c("pw_density", "area_density", "density_unevenness", "median_hh_income", "poverty_pct",
          "mean_commute_min", "transit_pct", "persons_per_hh")) %>%
    rowwise() %>%
    mutate(n = sum(complete.cases(wperf[[y]], wperf[[x]])),
           r = suppressWarnings(cor(wperf[[y]], wperf[[x]],
                                    use = "pairwise.complete.obs", method = "spearman")),
           p = tryCatch(suppressWarnings(
                 cor.test(wperf[[y]], wperf[[x]], method = "spearman")$p.value),
                 error = function(e) NA_real_)) %>%
    ungroup() %>%
    arrange(desc(abs(r)))
  put(wperf, "M3_water_performance_vs_settlement")
  put(cor_pairs, "M3b_settlement_correlations")

  top <- cor_pairs %>% slice(1)
  message("  ANALYSIS: strongest settlement association: ", top$y, " vs ", top$x,
          " rho = ", round(top$r, 3), ", p = ", signif(top$p, 3), ", n = ", top$n)

  # M2 population allocated to BASIN by tract centroid. Now a measured sum rather than a tract
  # count, which is the substantive gain from the ACS extract: basin population no longer assumes
  # tracts hold equal population.
  if (exists("basins_sf")) {
    ACS_GEO <- paste0(DATA_DIR, "acs_tract_metro.geojson")
    tr_geo <- st_read(ACS_GEO, quiet = TRUE) %>% st_set_crs(4326)
    tr_ctr <- suppressWarnings(st_centroid(tr_geo))
    hit <- st_intersects(tr_ctr, st_set_crs(basins_sf, 4326))

    tr_basin <- tr_ctr %>% st_drop_geometry() %>%
      mutate(basin = map_chr(hit, ~ if (length(.x)) basins_sf$basin[.x[1]] else NA_character_)) %>%
      filter(!is.na(basin)) %>%
      group_by(basin) %>%
      summarise(tracts = n(), population = sum(pop),
                pw_density = weighted.mean(pop_density, pop),
                median_hh_income = median(median_hh_income, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pop_share_pct = 100 * population / sum(population)) %>%
      left_join(basin_bal %>% filter(year == YR) %>%
                  select(basin, total_withdrawal_mgd, discharge_mgd), by = "basin") %>%
      # gpcd is the diagnostic: a basin supplying more than its residents use is exporting water
      # to another basin's population through the distribution network.
      mutate(withdrawal_gpcd = 1e6 * total_withdrawal_mgd / pmax(population, 1),
             discharge_gpcd = 1e6 * discharge_mgd / pmax(population, 1)) %>%
      arrange(desc(population))
    put(tr_basin, "M2_population_by_basin")
  }
} else {
  message("  ACS tract extract absent; run Rscript R/prep_acs.R -- skipping settlement section")
}

message("\n== done: ", length(list.files(OUT)), " tables in ", OUT, " ==")
