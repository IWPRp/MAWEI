# Facility coordinates: resolve plants and wastewater plants to lat/lon
#
#   Rscript R/prep_spatial.R
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
# point for any of the three Atlanta water reclamation centres. It remains useful for design flow
# and receiving water, which is how the validation section of analysis.R uses it.
#
# Hassan Niazi / MAWEI

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
message("== done ==")
