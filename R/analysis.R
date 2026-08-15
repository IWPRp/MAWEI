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
plant_cap <- read_csv(paste0(DATA_DIR, "spatial_plants_metro.csv"), show_col_types = FALSE) %>%
  select(plant_name, capacity_mw, lat, lon)
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

# E2 spatial coverage of the EPA extract. Reported because it is the binding constraint on any
# facility-level map: most NPDES outfall coordinates are whole-degree placeholders.
outf <- read_csv(paste0(DATA_DIR, "spatial_ww_outfalls_metro.csv"), show_col_types = FALSE)
put(outf %>% group_by(coord_decimals) %>%
      summarise(records = n(), potw_records = sum(is_potw), .groups = "drop") %>%
      mutate(pct = 100 * records / sum(records)),
    "E2_npdes_coordinate_precision")

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

epa_fac <- outf %>% group_by(county, facility_name) %>%
  summarise(epa_design_mgd = suppressWarnings(max(design_flow_mgd, na.rm = TRUE)),
            epa_receiving = paste(unique(na.omit(receiving_water)), collapse = " | "),
            best_dp = max(coord_decimals), .groups = "drop") %>%
  mutate(epa_design_mgd = ifelse(is.finite(epa_design_mgd), epa_design_mgd, NA_real_),
         k = norm_fac(facility_name))

val_cap <- wwt %>% select(county, facility_name, permitted_capacity) %>% distinct() %>%
  mutate(k = norm_fac(facility_name)) %>%
  inner_join(epa_fac %>% select(county, k, epa_name = facility_name, epa_design_mgd, best_dp),
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
                      "study facilities with mappable coords (>=3dp)",
                      "EPA metro facilities with mappable coords"),
           value = c(n_distinct(wwt$facility_name),
                     nrow(val_cap), sum(val_cap$agrees_10pct),
                     round(median(val_cap$ratio), 3),
                     round(cor(log(val_cap$permitted_capacity), log(val_cap$epa_design_mgd)), 3),
                     sum(val_cap$best_dp >= 3),
                     sum(epa_fac$best_dp >= 3))),
    "G1b_validation_summary")

message("\n== done: ", length(list.files(OUT)), " tables in ", OUT, " ==")
