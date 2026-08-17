# Facility coordinates: resolve plants and wastewater plants to lat/lon
#
#   Rscript R/prep_data.R
#
# Writes ONE file, data/spatial_facility_coords.csv, holding every located facility in the
# study with a source and a precision for each coordinate. Sourced by R/analysis.R and
# R/plots.R.
#
# Sources
#   data/eia860_Plant_Y2024_GA.csv          EIA-860 schedule 2. Joins on plant_code, so the
#                                           plant coordinates are exact, not name-matched.
#   data/WRP Permit Inventory May 2025.xlsx GA EPD permit inventory. Every metro row carries a
#                                           coordinate and most are at 6 decimals.
#   data/common_ww_facility_wrp_map.csv     Hand-curated study facility -> WRP permit mapping.
#
# ExtraNotes: the study facility list and the permit inventory share no key -- the sink map's
# `permit` column is permitted capacity in MGD, not a permit number -- and the naming conventions
# differ structurally ("HENRY COUNTY WALNUT CREEK" against "Henry County Water Authority (Walnut
# Creek WRF)"). Automated name similarity reached only 57% of permitted capacity and, worse, made
# confident errors between sibling plants of one utility, pairing "COBB RL SUTTON WRF" with "South
# Cobb WRF". The mapping is therefore curated by hand from permit number, city, county, basin and
# permitted capacity together, and reviewed county by county. Coordinates are NOT stored in the
# mapping: it holds only the key pair, so a refreshed permit inventory updates positions with no
# edit to the mapping.
#
# ExtraNotes: the EPA ECHO outfall layer is deliberately NOT used for position. 89% of its metro
# coordinates are whole-degree placeholders (~111 km error at this latitude) and it has no usable
# point for any of the three Atlanta water reclamation centers. It remains useful for design flow
# and receiving water, which is how the validation section of analysis.R uses it.
#
# Hassan Niazi, Aug 2026

source("functions.R")
suppressMessages(library(readxl))

message("== resolving facility coordinates ==")

# Metro bounding box. A coordinate outside it is an error (sign flip, defaulted value, wrong
# state), never a distant facility.
BBOX <- list(lat = c(32.5, 35.2), lon = c(-85.8, -83.0))
in_bbox <- function(lat, lon) between(lat, BBOX$lat[1], BBOX$lat[2]) &
                              between(lon, BBOX$lon[1], BBOX$lon[2])

# Precision as the smallest number of decimals that reproduces the value.
# ExtraNotes: deriving this by formatting the number fails, because format() pads every element of
# a vector to a COMMON width, so a whole-degree placeholder sitting among 6-decimal values reports
# as having 6 decimals. That defect silently passed a 111 km error as high precision.
coord_precision <- function(x, max_dp = 8L) {
  out <- rep(NA_integer_, length(x))
  for (d in 0:max_dp) {
    hit <- is.na(out) & !is.na(x) & abs(x - round(x, d)) < 1e-10
    out[hit] <- d
  }
  out[is.na(out) & !is.na(x)] <- max_dp
  out
}

###############################################################################%
## generating plants ----

plant_xy <- read_csv(paste0(DATA_DIR, "eia860_Plant_Y2024_GA.csv"),
                     show_col_types = FALSE, progress = FALSE) %>%
  clean_col_names() %>%
  filter(state == "GA", county %in% counties) %>%
  mutate(across(c(latitude, longitude), as.numeric)) %>%
  select(county, plant_code, lat = latitude, lon = longitude,
         water_source = name_of_water_source)

# Capacity is aggregated WITHIN a year and then reduced across years, never summed over all five.
# ExtraNotes: the same generator appears in every annual file, so a naive sum multiplies nameplate
# capacity by the number of years -- Bowen came out at 17,493 MW against an actual ~3,500 MW.
plants <- map_dfr(2020:2024, function(yr) {
  read_csv(paste0(DATA_DIR, "eia860_3_1_Generator_Y", yr, "_operable.csv.gz"),
           show_col_types = FALSE, progress = FALSE) %>% clean_col_names() %>%
    filter(state == "GA", county %in% counties) %>%
    mutate(year = yr) %>%
    select(county, year, plant_code, plant_name, technology, nameplate_capacity_mw)
}) %>%
  group_by(county, plant_code, plant_name, year) %>%
  summarise(cap = sum(nameplate_capacity_mw, na.rm = TRUE),
            technology = paste(sort(unique(technology)), collapse = "; "), .groups = "drop") %>%
  group_by(county, plant_code, plant_name) %>%
  summarise(capacity_mw = max(cap),
            technology = technology[which.max(year)], .groups = "drop") %>%
  left_join(plant_xy, by = c("county", "plant_code")) %>%
  mutate(kind = "power plant", name = plant_name, capacity = capacity_mw,
         capacity_units = "MW", detail = technology,
         id = as.character(plant_code), coord_source = "EIA-860 schedule 2")

message("  power plants: ", sum(!is.na(plants$lat)), " of ", nrow(plants), " located")

###############################################################################%
## wastewater plants ----

wrp <- read_excel(paste0(DATA_DIR, "WRP Permit Inventory May 2025.xlsx"), sheet = 1) %>%
  rename_all(~tolower(gsub(" ", "_", .))) %>%
  # ExtraNotes: coordinates arrive as TEXT in this workbook, so they must be coerced before any
  # arithmetic; left as character they propagate as NA through every downstream join.
  mutate(across(c(latitude, longitude), ~suppressWarnings(as.numeric(.)))) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  # ExtraNotes: filtered to the bounding box, NOT to the fifteen study counties. Several plants
  # serve one county while being permitted in another -- Clayton County's Northeast WRF sits in
  # Henry, Fulton's Little River WRF in Cherokee, and Villa Rica's two plants are permitted in
  # Carroll while serving Douglas. A county filter here silently drops exactly those cases, which
  # are the inter-jurisdictional couplings this study is about.
  filter(in_bbox(latitude, longitude)) %>%
  select(wrp_permit = permit_no, permit_name, permit_type, permit_county = county,
         permit_city = city, permit_basin = river_basin, lat = latitude, lon = longitude)

fac_map <- read_csv(paste0(DATA_DIR, "common_ww_facility_wrp_map.csv"), show_col_types = FALSE)

wwt <- read_csv(paste0(DATA_DIR, "water_wastewater_treatment.csv"), show_col_types = FALSE) %>%
  rename_all(tolower) %>% rename_with(~gsub(" |-", "_", .), everything()) %>%
  distinct(county, facility_name, permitted_capacity, level_of_treatment)

# Every mapped permit must exist in the inventory, and every mapped facility in the study list.
# A typo in either column would otherwise silently drop a facility rather than fail.
bad_permit <- setdiff(fac_map$wrp_permit, wrp$wrp_permit)
bad_fac <- anti_join(fac_map, wwt, by = c("county", "facility_name"))
if (length(bad_permit)) stop("mapping references unknown permits: ", paste(bad_permit, collapse = ", "))
if (nrow(bad_fac)) stop("mapping references unknown facilities: ",
                        paste(bad_fac$facility_name, collapse = " | "))

ww <- wwt %>%
  left_join(fac_map, by = c("county", "facility_name")) %>%
  left_join(wrp, by = "wrp_permit") %>%
  # Several study rows legitimately share one permit: a reuse train beside its parent plant, a
  # pond stage, a duplicated report, or Atlanta's single permit covering three plants. Flagging
  # the shared points lets a distance or density analysis exclude them while county-level maps
  # still use them.
  group_by(wrp_permit) %>%
  mutate(site_shared = !is.na(wrp_permit) & n() > 1) %>%
  ungroup() %>%
  mutate(kind = "wastewater plant", name = facility_name, id = wrp_permit,
         capacity = permitted_capacity, capacity_units = "MGD",
         detail = level_of_treatment, coord_source = "GA EPD WRP permit inventory")

matched_cap <- 100 * sum(ww$capacity[!is.na(ww$lat)], na.rm = TRUE) /
                     sum(ww$capacity, na.rm = TRUE)
message("  wastewater plants: ", sum(!is.na(ww$lat)), " of ", nrow(ww),
        " located (", round(matched_cap, 1), "% of permitted capacity)")

unmapped <- ww %>% filter(is.na(lat)) %>% arrange(desc(capacity))
if (nrow(unmapped)) {
  dir.create(QC_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(unmapped %>% select(county, facility_name, permitted_capacity, level_of_treatment),
            file.path(QC_DIR, "spatial_unmapped_facilities.csv"))
  message("    unmapped, largest first: ",
          paste0(head(unmapped$facility_name, 3), " (", head(round(unmapped$capacity, 2), 3), ")",
                 collapse = ", "))
}

###############################################################################%
## one combined output ----

coords <- bind_rows(
  plants %>% select(kind, county, name, id, capacity, capacity_units, detail,
                    lat, lon, coord_source, water_source),
  ww %>% select(kind, county, name, id, capacity, capacity_units, detail,
                lat, lon, coord_source, permit_basin, permit_city, site_shared)) %>%
  filter(!is.na(lat)) %>%
  mutate(coord_decimals = pmin(coord_precision(lat), coord_precision(lon))) %>%
  arrange(kind, county, desc(capacity))

stopifnot(all(in_bbox(coords$lat, coords$lon)),
          all(coords$coord_decimals >= 3))

write_csv(coords, paste0(DATA_DIR, "spatial_facility_coords.csv"))
message("  -> data/spatial_facility_coords.csv (", nrow(coords), " located facilities, ",
        "min precision ", min(coords$coord_decimals), " decimals)")

###############################################################################%
## river basins ----
# The USGS Watershed Boundary Dataset for region 03 is 365 MB and covers the whole South
# Atlantic-Gulf. What this study needs is the few basins the metro draws from, at the aggregation
# the water pipeline already uses.
#
# ExtraNotes: HUC8 is the right level. HUC6 merges Coosa and Tallapoosa into one unit, erasing a
# distinction the withdrawal data makes, while HUC10 and HUC12 subdivide far below anything the
# flow records resolve. Ten HUC8 units touch the metro and dissolve into the six named basins the
# pipeline reports, plus Broad, which carries no withdrawals.
#
# ExtraNotes: polygons are kept at FULL basin extent, not clipped to the county boundary. The
# headwater position of metro Atlanta on the Chattahoochee is the most policy-relevant fact in the
# water results, and clipping to the metro is precisely what would hide it.

WBD_FILE <- paste0(DATA_DIR, "WBD_03_HU2_GPKG/WBD_03_HU2_GPKG.gpkg")
BASIN_OUT <- paste0(DATA_DIR, "spatial_basins.geojson")

if (file.exists(WBD_FILE)) {
  message("\n== building basin extract ==")

  # HUC8 unit -> the basin label used throughout the pipeline. Stated explicitly rather than
  # pattern-matched: "Coosawattee" and "Oostanaula" are Coosa-system tributaries whose names
  # contain neither "Coosa" nor "Etowah", so any regex would silently drop them.
  HUC8_TO_BASIN <- c(
    "03130001" = "Chattahoochee",   # Upper Chattahoochee
    "03130002" = "Chattahoochee",   # Middle Chattahoochee-Lake Harding
    "03150104" = "Coosa_Etowah",    # Etowah
    "03150102" = "Coosa_Etowah",    # Coosawattee
    "03150103" = "Coosa_Etowah",    # Oostanaula
    "03130005" = "Flint",           # Upper Flint
    "03070103" = "Ocmulgee",        # Upper Ocmulgee
    "03070101" = "Oconee",          # Upper Oconee
    "03150108" = "Tallapoosa",      # Upper Tallapoosa
    "03060104" = "Broad")           # touches the metro edge; carries no withdrawals

  h8 <- st_read(WBD_FILE, layer = "WBDHU8", quiet = TRUE) %>%
    filter(huc8 %in% names(HUC8_TO_BASIN)) %>%
    st_transform(4326) %>%
    mutate(basin = HUC8_TO_BASIN[huc8])

  cty_sf <- st_read(paste0(DATA_DIR, "geojson-counties-fips.json"), quiet = TRUE) %>%
    rename_with(tolower) %>% filter(id %in% fips) %>% st_set_crs(4326) %>%
    mutate(county = name) %>% select(county)
  metro <- st_union(cty_sf)

  basins <- h8 %>%
    group_by(basin) %>%
    summarise(huc8_units = n(), huc8_codes = paste(sort(huc8), collapse = ";"),
              area_sqkm = sum(areasqkm), .groups = "drop") %>%
    # ExtraNotes: simplification must happen in a PROJECTED CRS. st_simplify interprets
    # dTolerance in the units of the coordinate system, so on lat/long data it neither simplifies
    # predictably nor honours a metre tolerance; the file came out unchanged at 1.8 MB until this
    # was fixed. Albers equal-area for the conterminous US (EPSG:5070) is the standard choice here
    # and preserves area, which is what the overlap weights depend on.
    st_transform(5070) %>%
    st_simplify(dTolerance = 500, preserveTopology = TRUE) %>%   # 500 m
    st_make_valid() %>%
    # st_make_valid can return a GEOMETRYCOLLECTION when simplification leaves a degenerate
    # sliver, and most downstream operations refuse that class.
    st_collection_extract("POLYGON") %>%
    st_cast("MULTIPOLYGON", warn = FALSE) %>%
    st_transform(4326)

  # How much of each basin lies inside the study area. This is the weight any spatial allocation
  # needs, and it is what makes the basin layer joinable to the county results.
  #
  # ExtraNotes: computed as a separate join, not inside mutate(). st_intersection() returns a
  # geometry column, and assigning one inside mutate() on an sf object replaces the active
  # geometry and invalidates the layer. Note also that the WBD geometry column is named `shape`,
  # not `geometry`, so st_area() is called on the object rather than on a named column.
  metro_area <- suppressWarnings(st_intersection(basins %>% select(basin), metro)) %>%
    mutate(a = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
    st_drop_geometry() %>%
    group_by(basin) %>% summarise(metro_area_sqkm = sum(a), .groups = "drop")

  basins <- basins %>%
    left_join(metro_area, by = "basin") %>%
    mutate(metro_area_sqkm = replace_na(metro_area_sqkm, 0),
           metro_share_pct = 100 * metro_area_sqkm / area_sqkm)

  # ExtraNotes: coordinate PRECISION, not vertex count, is what makes a GeoJSON large. st_simplify
  # removes vertices but each survivor is still written with ~15 significant digits. Four decimal
  # places is about 11 m at this latitude, far finer than a simplified basin boundary can support,
  # and it cuts the file by roughly an order of magnitude.
  st_write(basins, BASIN_OUT, delete_dsn = TRUE, quiet = TRUE,
           layer_options = "COORDINATE_PRECISION=4")
  message("  basins: ", nrow(basins), " from ", nrow(h8), " HUC8 units -> ",
          basename(BASIN_OUT), " (", round(file.size(BASIN_OUT) / 1e3), " kB)")

  # Which basins each county sits in, by overlap area. A county spanning a divide draws from more
  # than one basin, and that split is the spatial fact behind an inter-basin transfer.
  cb <- suppressWarnings(st_intersection(cty_sf, basins %>% select(basin))) %>%
    mutate(area_sqkm = as.numeric(units::set_units(st_area(.), "km^2"))) %>%
    st_drop_geometry() %>%
    group_by(county) %>% mutate(share_pct = 100 * area_sqkm / sum(area_sqkm)) %>%
    ungroup() %>% arrange(county, desc(share_pct))

  write_csv(cb, paste0(DATA_DIR, "spatial_county_basin_area.csv"))
  message("  -> data/spatial_county_basin_area.csv (", nrow(cb), " county-basin overlaps)")
} else {
  message("  WBD geopackage absent; skipping basin extract")
}

message("== done ==")


# PREP ACS

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
# Hassan Niazi, Aug 2026

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
