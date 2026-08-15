# Tract-level demographics and economics from the ACS geodatabase
#
#   Rscript R/prep_acs.R
#
# Writes TWO small files from a 1.0 GB Esri geodatabase, so the analysis never touches the
# original:
#   data/acs_tract_metro.csv       1386 metro tracts x ~30 attributes, no geometry
#   data/acs_tract_metro.geojson   the same tracts, simplified, for the settlement maps
#
# Source
#   data/ACS_2022_5YR_TRACT_13_GEORGIA.gdb   Census ACS 2018-2022 5-year estimates, Georgia,
#                                            census-tract summary level, Esri file geodatabase.
#                                            2796 tracts x 30 subject layers.
#
# ExtraNotes: this replaces the cartographic boundary file (`cb_2025_13_tract_500k`) as the
# settlement data source. That file carries geometry ONLY -- STATEFP, COUNTYFP, TRACTCE, GEOID,
# NAME, ALAND, AWATER and nothing else -- so tract AREA had to stand in for density on the
# reasoning that tracts are drawn to hold ~4000 people. This geodatabase carries the actual
# population, so density becomes a measurement rather than a proxy, and income, housing and
# commuting arrive with it.
#
# ExtraNotes: the join key between the geometry layer and the 30 attribute layers is `GEOIDFQ`
# (fully-qualified GEOID, "1400000US13121010101"), NOT `GEOID`. The attribute layers carry no
# other identifier -- no state, county or tract column -- so county attribution has to come from
# the geometry layer and be carried across. Joining on `GEOID` fails silently with zero matches.
#
# ExtraNotes: ACS fields are named `<table>_E<n>` for the estimate and `<table>_M<n>` for the
# 90% margin of error. Only estimates are extracted here, but the margins exist in the source and
# matter for any tract-level claim: a 5-year estimate for a single tract can carry a margin of
# 20-30% of the estimate. Tract values are therefore aggregated to county or basin before being
# reported, never quoted individually.
#
# ExtraNotes: vintage mismatch is deliberate and must be stated. The ACS extract is the 2018-2022
# 5-year product, while the flow study covers 2020-2024. There is no annual tract-level population
# series -- 1-year ACS estimates are not published below 65,000 population -- so a 5-year product
# centred on 2020 is the only tract-level option. It is used for STRUCTURE (how population is
# distributed inside a county) and never for TREND. County-year population continues to come from
# the Census vintage-2024 estimates, which are annual.
#
# Hassan Niazi / MAWEI

source("functions.R")

ACS_GDB   <- paste0(DATA_DIR, "ACS_2022_5YR_TRACT_13_GEORGIA.gdb")
GEOM_LYR  <- "ACS_2022_5YR_TRACT_13_GEORGIA"
OUT_CSV   <- paste0(DATA_DIR, "acs_tract_metro.csv")
OUT_GEO   <- paste0(DATA_DIR, "acs_tract_metro.geojson")

if (!dir.exists(ACS_GDB)) {
  stop("ACS geodatabase not found at ", ACS_GDB, "\n",
       "It is gitignored (1.0 GB). Download the ACS 5-year tract geodatabase for Georgia and\n",
       "place it there, or use the committed ", basename(OUT_CSV), " instead.")
}

message("== extracting ACS tract attributes ==")

# The fields wanted, by subject layer. Each is (layer, field, output name, kind).
# ExtraNotes: kept to a deliberately small set. The geodatabase holds ~2500 fields per layer and
# 30 layers; pulling everything would defeat the purpose of producing a committable extract. The
# selection is driven by what the water and energy analysis can actually use: population for
# density and per-capita normalisation, housing for the septic and connection story, income for
# affordability of the loss-recovery scenarios, and commuting for the transport-energy finding.
ACS_WANT <- tribble(
  ~layer,                        ~field,        ~out,                  ~kind,
  # population
  "X01_AGE_AND_SEX",             "B01003_E001", "pop",                 "count",
  "X01_AGE_AND_SEX",             "B01002_E001", "median_age",          "value",
  # housing -- bears directly on septic vs sewered, and on occupancy
  "X25_HOUSING_CHARACTERISTICS", "B25001_E001", "housing_units",       "count",
  "X25_HOUSING_CHARACTERISTICS", "B25002_E002", "housing_occupied",    "count",
  "X25_HOUSING_CHARACTERISTICS", "B25002_E003", "housing_vacant",      "count",
  "X25_HOUSING_CHARACTERISTICS", "B25010_E001", "persons_per_hh",      "value",
  # income and poverty -- who can pay for infrastructure renewal
  "X19_INCOME",                  "B19013_E001", "median_hh_income",    "value",
  "X19_INCOME",                  "B19301_E001", "income_per_capita",   "value",
  "X17_POVERTY",                 "B17001_E001", "poverty_universe",    "count",
  "X17_POVERTY",                 "B17001_E002", "poverty_below",       "count",
  # commuting -- the mechanism behind transport being half of end-use energy
  "X08_COMMUTING",               "B08301_E001", "commuters",           "count",
  "X08_COMMUTING",               "B08301_E003", "commute_drove_alone", "count",
  "X08_COMMUTING",               "B08301_E010", "commute_transit",     "count",
  "X08_COMMUTING",               "B08301_E021", "commute_wfh",        "count",
  "X08_COMMUTING",               "B08013_E001", "commute_agg_minutes", "count"
)

###############################################################################%
## tract geometry and county attribution ----

# ExtraNotes: read the geometry layer first because it is the ONLY layer carrying county identity.
# ALAND/AWATER are in square metres and are the authoritative land area -- preferred over a
# computed area because they exclude water consistently with how the Census defines the tract.
tracts <- st_read(ACS_GDB, layer = GEOM_LYR, quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(county_fips = paste0(STATEFP, COUNTYFP)) %>%
  select(geoidfq = GEOIDFQ, geoid = GEOID, tract = TRACTCE, county_fips,
         land_m2 = ALAND, water_m2 = AWATER)

fips_map <- read_csv(paste0(DATA_DIR, "common_county_fips.csv"),
                     show_col_types = FALSE, progress = FALSE) %>%
  clean_col_names() %>%
  transmute(county_fips = sprintf("%05d", as.integer(fip)), county) %>%
  filter(county %in% counties)

tracts_metro <- tracts %>% inner_join(fips_map, by = "county_fips")

message("  ", nrow(tracts_metro), " of ", nrow(tracts),
        " Georgia tracts fall in the fifteen study counties")
stopifnot(nrow(tracts_metro) > 1000, n_distinct(tracts_metro$county) == length(counties))

###############################################################################%
## attributes ----

# ExtraNotes: read each subject layer ONCE and pull only the wanted columns. Reading a layer per
# field would re-parse a multi-hundred-megabyte table for every variable.
attrs <- ACS_WANT %>%
  group_by(layer) %>%
  group_map(function(spec, key) {
    d <- st_read(ACS_GDB, layer = key$layer, quiet = TRUE)
    missing <- setdiff(spec$field, names(d))
    if (length(missing)) {
      warning("fields absent from ", key$layer, ": ", paste(missing, collapse = ", "))
      spec <- spec %>% filter(field %in% names(d))
    }
    d %>% select(geoidfq = GEOIDFQ, all_of(setNames(spec$field, spec$out)))
  }) %>%
  reduce(full_join, by = "geoidfq")

acs <- tracts_metro %>%
  st_drop_geometry() %>%
  left_join(attrs, by = "geoidfq")

stopifnot(nrow(acs) == nrow(tracts_metro), !anyNA(acs$pop))

###############################################################################%
## derived measures ----

# ExtraNotes: densities use LAND area, not total area. Metro Atlanta's reservoir counties (Hall on
# Lanier, Forsyth, Cherokee on Allatoona) have 8-9% of their area under water, so including it
# would understate their settlement density by about a tenth for a purely hydrographic reason.
acs_out <- acs %>%
  mutate(
    land_km2       = land_m2 / 1e6,
    water_km2      = water_m2 / 1e6,
    water_share    = water_m2 / pmax(land_m2 + water_m2, 1),
    pop_density    = pop / pmax(land_km2, 1e-9),
    hu_density     = housing_units / pmax(land_km2, 1e-9),
    vacancy_rate   = housing_vacant / pmax(housing_units, 1),
    poverty_rate   = poverty_below / pmax(poverty_universe, 1),
    # Mean commute is the aggregate travel time divided by the commuters it covers. B08013 counts
    # aggregate minutes for workers who commute, so the denominator excludes those working at home.
    mean_commute_min = commute_agg_minutes / pmax(commuters - commute_wfh, 1),
    drove_alone_share = commute_drove_alone / pmax(commuters, 1),
    transit_share     = commute_transit / pmax(commuters, 1),
    wfh_share         = commute_wfh / pmax(commuters, 1)) %>%
  select(county, geoid, geoidfq, tract, pop, median_age, land_km2, water_km2, water_share,
         pop_density, housing_units, housing_occupied, housing_vacant, hu_density, vacancy_rate,
         persons_per_hh, median_hh_income, income_per_capita, poverty_universe, poverty_below,
         poverty_rate, commuters, commute_drove_alone, commute_transit, commute_wfh,
         mean_commute_min, drove_alone_share, transit_share, wfh_share) %>%
  arrange(county, geoid)

write_csv(acs_out, OUT_CSV)
message("  wrote ", OUT_CSV, ": ", nrow(acs_out), " tracts x ", ncol(acs_out), " fields")

###############################################################################%
## simplified geometry for maps ----

# ExtraNotes: simplify in an equal-area projection (EPSG:5070), never in lat/long. st_simplify's
# dTolerance is in the units of the coordinate system, so a tolerance meant as metres is read as
# degrees and the geometry is either untouched or destroyed. Tract boundaries need a finer
# tolerance than the basin polygons (200 m against 500 m) because a tract can be a few hundred
# metres across in the urban core.
geo <- tracts_metro %>%
  left_join(acs_out %>% select(geoidfq, pop, pop_density, median_hh_income, mean_commute_min),
            by = "geoidfq") %>%
  select(county, geoid, pop, pop_density, median_hh_income, mean_commute_min) %>%
  st_transform(5070) %>%
  st_simplify(dTolerance = 200, preserveTopology = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON")

if (file.exists(OUT_GEO)) unlink(OUT_GEO)
st_write(geo, OUT_GEO, quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=4", "RFC7946=YES"))
message("  wrote ", OUT_GEO, ": ",
        round(file.size(OUT_GEO) / 1024), " kB from a ",
        round(sum(file.size(list.files(ACS_GDB, full.names = TRUE))) / 1e6), " MB geodatabase")

###############################################################################%
## county rollup, as a check ----

# ExtraNotes: the check that matters is whether tract population sums to the county population the
# rest of the study uses. The two come from different Census products -- a 5-year ACS estimate
# against a vintage-2024 annual estimate -- so they will NOT match exactly, and a mismatch of a few
# percent is expected rather than wrong. A mismatch of tens of percent would mean tracts have been
# attributed to the wrong county.
roll <- acs_out %>%
  group_by(county) %>%
  summarise(tracts = n(), acs_pop = sum(pop), .groups = "drop")

pop_ref <- read_csv(paste0(DATA_DIR, "cc-est2024-agesex-all.csv.gz"),
                    show_col_types = FALSE, progress = FALSE) %>%
  clean_col_names() %>%
  filter(stname == "Georgia", year == 2) %>%
  mutate(county = str_replace(ctyname, " County", "")) %>%
  filter(county %in% counties) %>%
  select(county, census_pop = popestimate)

chk <- roll %>% left_join(pop_ref, by = "county") %>%
  mutate(diff_pct = 100 * (acs_pop - census_pop) / census_pop)
message("  tract-to-county population agreement, ACS 2018-2022 vs Census 2020:")
message("    median |diff| ", round(median(abs(chk$diff_pct)), 2), "%, max ",
        round(max(abs(chk$diff_pct)), 2), "% (", chk$county[which.max(abs(chk$diff_pct))], ")")
if (max(abs(chk$diff_pct)) > 15)
  warning("a county differs by more than 15% -- check tract attribution")

message("== done ==")
