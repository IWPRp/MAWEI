# Ancillary functions to support Metro Atlanta Water-Energy Flows analysis and plotting.
#
# Hassan Niazi, PNNL, July 2026


library(tidyverse)
library(plotly)
library(htmlwidgets)
library(sf)
library(RColorBrewer)
library(ggsci)
library(purrr)
library(zoo)

DATA_DIR <- "data/"
SAVE_DIR <- "outputs/files/"
SCRIPTS_DIR <- "R/"
SAVE_FILES <- F
MAKE_PLOT <- F

# --- Sankey color scheme switch ---
# "vivid"  : high-contrast true-representative colors
# "muted"  : softer same-family tones
# FALSE    : no named colors (Spectral/RColorBrewer fallback)
COLOR_SCHEME <- "vivid"


# --- Diagram saving mode ---
# "selfcontained" : each HTML embeds all JS/CSS (~1-2 MB each, fully portable)
# "shared_libs"   : HTMLs reference one shared lib folder (~50 KB each + one ~4 MB folder)
SAVE_MODE <- "selfcontained"

# create directory if it doesn't exist
if (!dir.exists(SAVE_DIR)) {
  dir.create(SAVE_DIR, recursive = TRUE)
}

for (domain in c("energy", "water", "energy-water")) {
  dir.create(file.path(SAVE_DIR, domain, "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(SAVE_DIR, domain, "diagrams"), recursive = TRUE, showWarnings = FALSE)
}

# county names
counties <- read_csv(paste0(DATA_DIR, "common_county_fips.csv"))$county
fips <- read_csv(paste0(DATA_DIR, "common_county_fips.csv"))$fip

YEARS_TO_ENSURE <- 2020:2024

# units
BBtu_to_EJ <- 1.055e-6  # billion British thermal units to exajoules
MMBtu_to_EJ <- 1.055e-9
kWh_to_EJ <- 3.6e-12    # 1 kWh = 3.6e6 J = 3.6e-12 EJ
MWh_to_EJ <- 3.6e-9
EJ_to_PJ <- 1e3         # exajoules to petajoules
EJ_to_TJ <- 1e6         # exajoules to terajoules
PJ_to_GWh <- 277.778    # 1 PJ = 277.778 GWh
MGD_to_GPM <- 694.4444444444445 # million gallons per day to gallons per minute
HP_to_KW <- 0.7457      # horsepower to kilowatts
PUMPING_EFFICIENCY <- 0.55 # typical range 0.5-0.7
WATER_HORSEPOWER <- 3960 # constant
HOURS_PER_YEAR <- 8760
HOURS_PER_DAY <- 24
DAYS_PER_YEAR <- 365


# ---------------------------------------------------------------------------
# Sankey node color palettes
# ---------------------------------------------------------------------------
# Two palettes available; selected by COLOR_SCHEME above.
# Per-call override: plot_sankey_enhanced(..., color_scheme = FALSE) → Spectral
# Unmapped nodes get SANKEY_DEFAULT_COLOR via resolve_node_color().

SANKEY_DEFAULT_COLOR <- "#C8C8C8"

# ---- Vivid: high-contrast, true-representative colors --------------------
SANKEY_COLORS_VIVID <- list(

  # --- Fossil fuels ---
  "Coal"                    = "#1A1A1A",
  "Natural Gas"             = "#E87D2F",
  "Petroleum"               = "#8B4513",

  # --- Renewables ---
  "Solar"                   = "#FFD700",
  "Biomass"                 = "#228B22",
  "Hydroelectric Water"     = "#1E90FF",
  "Geothermal"              = "#CD5C5C",
  "Energy Storage"          = "#9370DB",
  "Other"                   = "#A0A0A0",

  # --- On-site / distributed generation ---
  "Onsite / BehindTheMeter" = "#DAA520",
  "Onsite Solar/DER"        = "#FFC125",
  "Distributed-scale Generation" = "#B8860B",
  "On-Site Backup Generation" = "#A0522D",

  # --- Power plants ---
  "Bowen Plant"             = "#CC3333",
  "Jack McDonough Plant"    = "#D2691E",
  "Yates Plant"             = "#B8860B",

  # --- Generation and grid ---
  "Thermoelectric Generation" = "#FF8C00",
  "Utility-scale Generation"  = "#E07020",
  "Electricity Imports"       = "#FFB347",
  "Electricity Exports"       = "#F0A030",
  "Out-Metro Electricity Imports"  = "#FFC04D",
  "Out-Metro Electricity Exports"  = "#E8A830",

  # --- Energy losses and services ---
  "Efficiency Losses"            = "#808080",
  "Transmission & Dist. Losses"  = "#A9A9A9",
  "Plants Own Use"               = "#696969",
  "Energy Services"              = "#4CAF50",
  "Rejected Energy"              = "#B0B0B0",

  # --- End-use sectors ---
  "Residential Use"         = "#5B9BD5",
  "Commercial Use"          = "#BF8F00",
  "Industrial Use"          = "#707070",
  "Agricultural Use"        = "#6B8E23",
  "Government Use"          = "#7B68AE",
  "Transportation Use"      = "#C0392B",

  # --- Water basins ---
  "Chattahoochee Basin"     = "#0047AB",
  "Coosa_Etowah Basin"      = "#2E8BC0",
  "Flint Basin"             = "#1560BD",
  "Ocmulgee Basin"          = "#3A75C4",
  "Oconee Basin"            = "#4682B4",
  "Tallapoosa Basin"        = "#5CACEE",
  "Surface Water (all basins)" = "#1C86EE",
  "Groundwater"             = "#36648B",
  "Groundwater (all basins)" = "#36648B",

  # --- Water supply and distribution ---
  "Public Water Supply"     = "#4169E1",
  "Losses"                  = "#778899",
  "Infiltration and Inflow" = "#5F9EA0",

  # --- Wastewater ---
  "Wastewater Collection"   = "#6A0DAD",
  "Septic Systems"          = "#9370DB",
  "In-County Treatment"     = "#800080",
  "Wastewater Treated"      = "#BA55D3",
  "Wastewater Transfer Inflows (within Metro Atlanta)"  = "#7B2FBE",
  "Wastewater Transfer Outflows (within Metro Atlanta)" = "#9060C0",
  "Total Wastewater Treatment" = "#7B2FBE",

  # --- Discharge destinations ---
  "Discharge"               = "#008B8B",
  "discharge"               = "#008B8B",
  "Creek"                   = "#20B2AA",
  "River"                   = "#2F9E9E",
  "Lake"                    = "#1C86EE",
  "Reservoir"               = "#1874CD",
  "Wetland"                 = "#3CB371",
  "Reuse"                   = "#00CED1",
  "Land"                    = "#6B8E23",

  # --- Energy-for-water bridge ---
  "Water Services Energy"   = "#008080",
  "en4water"                = "#008080",
  "Groundwater Extraction"  = "#2E8B8B",
  "Surface Water Withdrawal" = "#207878",
  "Groundwater Treatment"   = "#388E8E",
  "Surface Water Treatment" = "#2E7D7D",
  "Groundwater Distribution" = "#3AA0A0",
  "Surface Water Distribution" = "#308888",
  "Wastewater Treatment"    = "#7B2FBE",
  "Wastewater Transport"    = "#6A0DAD",

  # --- Simplified E-W diagram aggregates ---
  "Basins"                  = "#1C86EE",
  "Renewables"              = "#228B22",
  "Small-scale generation"  = "#B8860B",
  "Energy Losses"           = "#808080",
  "Water Losses"            = "#778899",
  "Disposal"                = "#008B8B"
)

# ---- Muted: softer same-family tones (original palette) ------------------
SANKEY_COLORS_MUTED <- list(

  "Coal"                    = "#636363",
  "Natural Gas"             = "#E8994E",
  "Petroleum"               = "#A67B5B",

  "Solar"                   = "#F0C75E",
  "Biomass"                 = "#8DB580",
  "Hydroelectric Water"     = "#6BB5C9",
  "Geothermal"              = "#C47A5A",
  "Energy Storage"          = "#B0A878",
  "Other"                   = "#B0B0B0",

  "Onsite / BehindTheMeter" = "#D4B870",
  "Onsite Solar/DER"        = "#E8C95E",
  "Distributed-scale Generation" = "#C8B060",
  "On-Site Backup Generation" = "#B8A860",

  "Bowen Plant"             = "#D47B6A",
  "Jack McDonough Plant"    = "#C49A6C",
  "Yates Plant"             = "#BF8B67",

  "Thermoelectric Generation" = "#E8B960",
  "Utility-scale Generation"  = "#D4A858",
  "Electricity Imports"       = "#F0D070",
  "Electricity Exports"       = "#E8C868",
  "Out-Metro Electricity Imports"  = "#F5D76E",
  "Out-Metro Electricity Exports"  = "#E8D070",

  "Efficiency Losses"            = "#C0B8AC",
  "Transmission & Dist. Losses"  = "#B8B0A4",
  "Plants Own Use"               = "#CCC0B0",
  "Energy Services"              = "#A8B8A0",
  "Rejected Energy"              = "#C8B8A8",

  "Residential Use"         = "#7BA3B0",
  "Commercial Use"          = "#A89070",
  "Industrial Use"          = "#8D9880",
  "Agricultural Use"        = "#90A868",
  "Government Use"          = "#8898A8",
  "Transportation Use"      = "#A89888",

  "Chattahoochee Basin"     = "#3A80A8",
  "Coosa_Etowah Basin"      = "#5898B8",
  "Flint Basin"             = "#4888A0",
  "Ocmulgee Basin"          = "#6098A8",
  "Oconee Basin"            = "#5090B0",
  "Tallapoosa Basin"        = "#7AB8E0",
  "Surface Water (all basins)" = "#4A90B8",
  "Groundwater"             = "#5A7898",
  "Groundwater (all basins)" = "#5A7898",

  "Public Water Supply"     = "#4A88B0",
  "Losses"                  = "#88A8B8",
  "Infiltration and Inflow" = "#6898A8",

  "Wastewater Collection"   = "#8B6EA0",
  "Septic Systems"          = "#A088A8",
  "In-County Treatment"     = "#9878A0",
  "Wastewater Treated"      = "#A890B0",
  "Wastewater Transfer Inflows (within Metro Atlanta)"  = "#9080A0",
  "Wastewater Transfer Outflows (within Metro Atlanta)" = "#9888A8",
  "Total Wastewater Treatment" = "#9070A0",

  "Discharge"               = "#5098B0",
  "discharge"               = "#5098B0",
  "Creek"                   = "#60A8B8",
  "River"                   = "#5090A8",
  "Lake"                    = "#6898C0",
  "Reservoir"               = "#5888B0",
  "Wetland"                 = "#70B0A8",
  "Reuse"                   = "#80C8B8",
  "Land"                    = "#88A898",

  "Water Services Energy"   = "#5EAAB0",
  "en4water"                = "#5EAAB0",
  "Groundwater Extraction"  = "#5898A8",
  "Surface Water Withdrawal" = "#5090A0",
  "Groundwater Treatment"   = "#6098A0",
  "Surface Water Treatment" = "#5890A0",
  "Groundwater Distribution" = "#6890A0",
  "Surface Water Distribution" = "#6088A0",
  "Wastewater Treatment"    = "#8870A0",
  "Wastewater Transport"    = "#8068A0",

  "Basins"                  = "#4A90B8",
  "Renewables"              = "#8DB580",
  "Small-scale generation"  = "#C8B060",
  "Energy Losses"           = "#C0B8AC",
  "Water Losses"            = "#88A8B8",
  "Disposal"                = "#5098B0"
)

# ---- Active palette (selected by COLOR_SCHEME) ----------------------------
SANKEY_COLORS <- if (identical(COLOR_SCHEME, "vivid")) SANKEY_COLORS_VIVID else
                 if (identical(COLOR_SCHEME, "muted")) SANKEY_COLORS_MUTED else
                 list()

# Pattern-based fallback for nodes not in SANKEY_COLORS (facility names,
# water bodies, discharge points, inter-county transfers).
resolve_node_color <- function(node_name, palette) {
  if (node_name %in% names(palette)) return(palette[[node_name]])
  nm <- toupper(node_name)
  dc  <- if ("Discharge" %in% names(palette)) palette[["Discharge"]] else "#008B8B"
  ww  <- if ("Wastewater Collection" %in% names(palette)) palette[["Wastewater Collection"]] else "#6A0DAD"
  lk  <- if ("Lake" %in% names(palette)) palette[["Lake"]] else "#1C86EE"
  rv  <- if ("River" %in% names(palette)) palette[["River"]] else "#2F9E9E"
  re  <- if ("Reuse" %in% names(palette)) palette[["Reuse"]] else "#00CED1"
  la  <- if ("Land" %in% names(palette)) palette[["Land"]] else "#6B8E23"
  if (grepl("_ds$", node_name))                          return(dc)
  if (grepl("^inFrom_", node_name))                      return(ww)
  if (grepl("WRF|WPCP|WWTP|WRC|LAS|POND", nm))          return(ww)
  if (grepl("REUSE", nm))                                return(re)
  if (grepl("LAND APPLICATION", nm))                     return(la)
  if (grepl("LAKE|RESERVOIR", nm))                       return(lk)
  if (grepl("RIVER|CREEK|BRANCH|TRIBUTARY", nm))         return(rv)
  if (grepl("SPRING", nm))                               return(rv)
  if (grepl("SNAPPING SHOALS", nm))                      return(ww)
  if (grepl("^VARIOUS$|^SMALL PERMITS$", nm))            return("#A0A0A0")
  if (grepl("FORSYTH|GWINNETT|PAULDING|ROCKDALE|FULTON|DEKALB|COBB|HENRY|HALL|CHEROKEE|BARTOW|CLAYTON|COWETA|DOUGLAS|FAYETTE", nm))
                                                          return(ww)
  SANKEY_DEFAULT_COLOR
}


repeats <- function(df) {
  df %>% group_by(across(everything())) %>%
    filter(n() > 1) %>% ungroup()
}

# validate flow data frames for completeness and correctness
validate_flows <- function(df, label = "flows") {
  required <- c("source", "target", "year", "value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop(label, ": missing columns: ", paste(missing, collapse = ", "))

  for (col in required) {
    n_na <- sum(is.na(df[[col]]))
    if (n_na > 0) stop(label, ": ", n_na, " NA values in '", col, "'")
  }

  group_cols <- intersect(c("county", "year", "source", "target", "units"), names(df))
  dupes <- df %>% group_by(across(all_of(group_cols))) %>% filter(n() > 1) %>% ungroup()
  if (nrow(dupes) > 0) stop(label, ": ", nrow(dupes), " duplicate rows found")

  if (any(df$value < 0)) stop(label, ": ", sum(df$value < 0), " negative values")

  years_present <- sort(unique(df$year))
  years_missing <- setdiff(YEARS_TO_ENSURE, years_present)
  if (length(years_missing) > 0) stop(label, ": missing years: ", paste(years_missing, collapse = ", "))

  invisible(df)
}

simplify_sankey <- function(df, map, bin_ww_imports = TRUE) {
  if (bin_ww_imports) {
    df <- df %>%
      mutate(source = if_else(grepl("inFrom", source), "ww_imports", source),
             target = if_else(grepl("inFrom", target), "ww_imports", target))
  }
  df %>%
    left_join(map, by = c("source", "target")) %>%
    mutate(source = coalesce(source_agg, source),
           target = coalesce(target_agg, target)) %>%
    select(-source_agg, -target_agg) %>%
    group_by(across(c(-value))) %>%
    summarise(value = sum(value), .groups = "drop")
}

node_throughput_by_unit <- function(df) {
  bind_rows(
    df %>% group_by(node = source, units) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop"),
    df %>% group_by(node = target, units) %>%
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
  ) %>%
    group_by(node, units) %>%
    summarise(total = max(total, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(units), total > 0)
}

format_node_label <- function(nd, nd_totals, alt_units = NULL, prefix = "") {
  if (nrow(nd_totals) == 0) return(nd)
  parts <- nd_totals %>% arrange(units) %>%
    purrr::pmap_chr(function(node, units, total, ...) {
      lbl <- paste0(prefix, round(total, 1), " ", units)
      if (!is.null(alt_units) && units == alt_units$from_unit && nd %in% alt_units$nodes) {
        alt_val <- total * alt_units$factor
        lbl <- paste0(lbl, " (", round(alt_val, 0), " ", alt_units$label, ")")
      }
      lbl
    })
  paste0(nd, "\n", paste(parts, collapse = " | "))
}

save_sankey <- function(widget, filepath) {
  filepath <- normalizePath(filepath, mustWork = FALSE)
  widget <- plotly::partial_bundle(widget)
  if (SAVE_MODE == "shared_libs") {
    libdir <- normalizePath(file.path(SAVE_DIR, "shared_libs"), mustWork = FALSE)
    dir.create(libdir, recursive = TRUE, showWarnings = FALSE)
    htmlwidgets::saveWidget(widget, file = filepath,
                            selfcontained = FALSE, libdir = libdir)
  } else {
    htmlwidgets::saveWidget(widget, file = filepath, selfcontained = TRUE)
    files_dir <- sub("\\.html$", "_files", filepath)
    if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
  }
}

save_county_sankeys <- function(df, domain_dir, prefix, suffix, prep_fn, label_units,
                                alt_units = NULL, color_scheme = NULL,
                                link_color_by_domain = FALSE) {
  diag_dir <- file.path(SAVE_DIR, domain_dir, "diagrams")
  for (cty in sort(counties)) {
    message("  ", cty, " ", suffix)
    p <- plot_sankey_enhanced(prep_fn(df), reg = cty, animate = TRUE,
                              show_values_in_labels = TRUE, label_units = label_units,
                              alt_units = alt_units, color_scheme = color_scheme,
                              link_color_by_domain = link_color_by_domain)
    save_sankey(p, file.path(diag_dir, paste0(prefix, "_county_", cty, "_", suffix, ".html")))
  }
}

clean_col_names <- function(df) {
  names(df) <- tolower(names(df))
  # replace parentheses with underscores and clean up spaces
  names(df) <- gsub("\\s*[()]\\s*", "_", names(df)) %>% # replace parentheses with underscores
    gsub("\\s+|/|\\?|-", "_", .) %>% # replace spaces, slashes, and question marks with underscores
    gsub("_{2,}", "_", .) %>% # replace multiple underscores with a single underscore
    gsub("_$", "", .) # remove trailing underscore

  return(df)
  }

clean_names <- function(df) {

  df <- clean_col_names(df)

  # clean column names - remove "...X" suffixes
  names(df) <- gsub("\\.\\.\\..*", "", names(df))
  # pivot longer on year columns
  df_long <- df %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = "year",
                 values_to = "value") %>%
    mutate(year = as.integer(year))

  return(df_long)
}


# EIA NOTES ----
EIA_SEDS_FILE <- "eia_seds_complete_seds_2024_update.csv.gz"
if (!file.exists(paste0(DATA_DIR, EIA_SEDS_FILE))) {
  stop(paste("File", EIA_SEDS_FILE, "not found in data directory. Download from EIA SEDS."))
}


## 923 NOTES ----
# EIA Sector Number and Sector Name	EIA’s internal consolidated NAICS sectors.For internal purposes, EIA consolidates the NAICS categories into seven groups.  These are shown below in the Sector Codes and Names table:
  # 1	Electric Utility: Traditional regulated electric utilities
  # 2	NAICS-22 Non-Cogen: Independent power producers which are not cogenerators
  # 3	NAICS-22 Cogen: Independent power producers which are cogenerators, but whose primary business purpose is the sale of electricity to the public
  # 4	Commercial NAICS Non-Cogen: Commercial non-cogeneration facilities that produce electric power, are connected to the gird, and can sell power to the public
  # 5	Commercial NAICS Cogen: Commercial cogeneration facilities that produce electric power, are connected to the grid, and can sell power to the public
  # 6	Industrial NAICS Non-Cogen: Industrial non-cogeneration facilities that produce electric power, are connected to the gird, and can sell power to the public
  # 7	Industrial NAICS Cogen: Industrial cogeneration facilities that produce electric power, are connected to the gird, and can sell power to the public

# Reported Primer Mover	Type of prime mover:
  # BA	Energy Storage, Battery
  # BT	Turbines Used in a Binary Cycle. Including those used for geothermal applications
  # CA	Combined-Cycle -- Steam Part
  # CE	Energy Storage, Compressed Air
  # CP	Energy Storage, Concentrated Solar Power
  # CS	Combined-Cycle Single-Shaft Combustion Turbine and Steam Turbine share of single generator
  # CT	Combined-Cycle Combustion Turbine Part
  # ES	Energy Storage, Other (Specify on Schedule 9, Comments)
  # FC	Fuel Cell
  # FW	Energy Storage, Flywheel
  # GT	Combustion (Gas) Turbine. Including Jet Engine design
  # HA	Hydrokinetic, Axial Flow Turbine
  # HB	Hydrokinetic, Wave Buoy
  # HK	Hydrokinetic, Other
  # HY	Hydraulic Turbine. Including turbines associated with delivery of water by pipeline.
  # IC	Internal Combustion (diesel, piston, reciprocating) Engine
  # PS	Energy Storage, Reversible Hydraulic Turbine (Pumped Storage)
  # OT	Other
  # ST	Steam Turbine. Including Nuclear, Geothermal, and Solar Steam (does not include Combined Cycle).
  # PV	Photovoltaic
  # WT	Wind Turbine, Onshore
  # WS	Wind Turbine, Offshore

reported_prime_mover_rename <- function(df_sankey, col_name="reported_prime_mover") {
  df_sankey %>%
    mutate(
      prime_mover := case_when(
        !!sym(col_name) %in% c("BA") ~ "Energy Storage, Battery",
        !!sym(col_name) %in% c("BT") ~ "Turbines Used in a Binary Cycle",
        !!sym(col_name) %in% c("CA") ~ "Combined-Cycle -- Steam Part",
        !!sym(col_name) %in% c("CE") ~ "Energy Storage, Compressed Air",
        !!sym(col_name) %in% c("CP") ~ "Energy Storage, Concentrated Solar Power",
        !!sym(col_name) %in% c("CS") ~ "Combined-Cycle Single-Shaft Combustion Turbine and Steam Turbine",
        !!sym(col_name) %in% c("CT") ~ "Combined-Cycle Combustion Turbine Part",
        !!sym(col_name) %in% c("ES") ~ "Energy Storage, Other",
        !!sym(col_name) %in% c("FC") ~ "Fuel Cell",
        !!sym(col_name) %in% c("FW") ~ "Energy Storage, Flywheel",
        !!sym(col_name) %in% c("GT") ~ "Combustion (Gas) Turbine",
        !!sym(col_name) %in% c("HA") ~ "Hydrokinetic, Axial Flow Turbine",
        !!sym(col_name) %in% c("HB") ~ "Hydrokinetic, Wave Buoy",
        !!sym(col_name) %in% c("HK") ~ "Hydrokinetic, Other",
        !!sym(col_name) %in% c("HY") ~ "Hydraulic Turbine",
        !!sym(col_name) %in% c("IC") ~ "Internal Combustion Engine",
        !!sym(col_name) %in% c("PS") ~ "Energy Storage, Reversible Hydraulic Turbine (Pumped Storage)",
        !!sym(col_name) %in% c("OT") ~ "Other",
        !!sym(col_name) %in% c("ST") ~ "Steam Turbine",
        !!sym(col_name) %in% c("PV") ~ "Photovoltaic",
        !!sym(col_name) %in% c("WT") ~ "Wind Turbine, Onshore",
        !!sym(col_name) %in% c("WS") ~ "Wind Turbine, Offshore",
        TRUE ~ "Unknown"
      )
    )
}


# Reported Fuel Type Code	The fuel code reported to EIA.Two or three letter alphanumeric:
  # AB	Agricultural By-Products
  # ANT	Anthracite Coal
  # BFG	Blast Furnace Gas
  # BIT	Bituminous Coal
  # BLQ	Black Liquor
  # DFO	Distillate Fuel Oil. Including diesel, No. 1, No. 2, and No. 4 fuel oils.
  # GEO	Geothermal
  # H2	Hydrogen
  # JF	Jet Fuel
  # KER	Kerosene
  # LFG	Landfill Gas
  # LIG	Lignite Coal
  # MSB	Biogenic Municipal Solid Waste
  # MSN	Non-biogenic Municipal Solid Waste
  # MWH	Electricity used for energy storage
  # NG	Natural Gas
  # NUC	Nuclear. Including Uranium, Plutonium, and Thorium.
  # OBG	Other Biomass Gas. Including digester gas, methane, and other biomass gases.
  # OBL	Other Biomass Liquids
  # OBS	Other Biomass Solids
  # OG	Other Gas
  # OTH	Other Fuel
  # PC	Petroleum Coke
  # PG	Gaseous Propane
  # PUR	Purchased Steam
  # RC	Refined Coal
  # RFO	Residual Fuel Oil. Including No. 5 & 6 fuel oils and bunker C fuel oil.
  # SC	Coal-based Synfuel. Including briquettes, pellets, or extrusions, which are formed by binding materials or processes that recycle materials.
  # SGC	Coal-Derived Synthesis Gas
  # SGP	Synthesis Gas from Petroleum Coke
  # SLW	Sludge Waste
  # SUB	Subbituminous Coal
  # SUN	Solar
  # TDF	Tire-derived Fuels
  # WAT	Water at a Conventional Hydroelectric Turbine and water used in Wave Buoy Hydrokinetic Technology, current Hydrokinetic Technology, Tidal Hydrokinetic Technology, and Pumping Energy for Reversible (Pumped Storage) Hydroelectric Turbines.
  # WC	Waste/Other Coal. Including anthracite culm, bituminous gob, fine coal, lignite waste, waste coal.
  # WDL	Wood Waste Liquids, excluding Black Liquor. Including red liquor, sludge wood, spent sulfite liquor, and other wood-based liquids.
  # WDS	Wood/Wood Waste Solids. Including paper pellets, railroad ties, utility polies, wood chips, bark, and other wood waste solids.
  # WH	Waste Heat not directly attributed to a fuel source
  # WND	Wind
  # WO	Waste/Other Oil. Including crude oil, liquid butane, liquid propane, naphtha, oil waste, re-refined moto oil, sludge oil, tar oil, or other petroleum-based liquid wastes.

reported_fuel_rename <- function(df_sankey, col_name="reported_fuel_type_code") {
  df_sankey %>%
    mutate(
      reported_fuel := case_when(
        !!sym(col_name) %in% c("AB") ~ "Agricultural By-Products",
        !!sym(col_name) %in% c("ANT") ~ "Anthracite Coal",
        !!sym(col_name) %in% c("BFG") ~ "Blast Furnace Gas",
        !!sym(col_name) %in% c("BIT") ~ "Bituminous Coal",
        !!sym(col_name) %in% c("BLQ") ~ "Black Liquor",
        !!sym(col_name) %in% c("DFO") ~ "Distillate Fuel Oil",
        !!sym(col_name) %in% c("GEO") ~ "Geothermal",
        !!sym(col_name) %in% c("H2") ~ "Hydrogen",
        !!sym(col_name) %in% c("JF") ~ "Jet Fuel",
        !!sym(col_name) %in% c("KER") ~ "Kerosene",
        !!sym(col_name) %in% c("LFG") ~ "Landfill Gas",
        !!sym(col_name) %in% c("LIG") ~ "Lignite Coal",
        !!sym(col_name) %in% c("MSB") ~ "Biogenic Municipal Solid Waste",
        !!sym(col_name) %in% c("MSN") ~ "Non-biogenic Municipal Solid Waste",
        !!sym(col_name) %in% c("MWH") ~ "Electricity used for energy storage",
        !!sym(col_name) %in% c("NG") ~ "Natural Gas",
        !!sym(col_name) %in% c("NUC") ~ "Nuclear",
        !!sym(col_name) %in% c("OBG") ~ "Other Biomass Gas",
        !!sym(col_name) %in% c("OBL") ~ "Other Biomass Liquids",
        !!sym(col_name) %in% c("OBS") ~ "Other Biomass Solids",
        !!sym(col_name) %in% c("OG") ~ "Other Gas",
        !!sym(col_name) %in% c("OTH") ~ "Other Fuel",
        !!sym(col_name) %in% c("PC") ~ "Petroleum Coke",
        !!sym(col_name) %in% c("PG") ~ "Gaseous Propane",
        !!sym(col_name) %in% c("PUR") ~ "Purchased Steam",
        !!sym(col_name) %in% c("RC") ~ "Refined Coal",
        !!sym(col_name) %in% c("RFO") ~ "Residual Fuel Oil",
        !!sym(col_name) %in% c("SC") ~ "Coal-based Synfuel",
        !!sym(col_name) %in% c("SGC") ~ "Coal-Derived Synthesis Gas",
        !!sym(col_name) %in% c("SGP") ~ "Synthesis Gas from Petroleum Coke",
        !!sym(col_name) %in% c("SLW") ~ "Sludge Waste",
        !!sym(col_name) %in% c("SUB") ~ "Subbituminous Coal",
        !!sym(col_name) %in% c("SUN") ~ "Solar",
        !!sym(col_name) %in% c("TDF") ~ "Tire-derived Fuels",
        !!sym(col_name) %in% c("WAT") ~ "Water for Conventional Hydroelectric",
        !!sym(col_name) %in% c("WC") ~ "Waste/Other Coal",
        !!sym(col_name) %in% c("WDL") ~ "Wood Waste Liquids",
        !!sym(col_name) %in% c("WDS") ~ "Wood/Wood Waste Solids",
        !!sym(col_name) %in% c("WH") ~ "Waste Heat",
        !!sym(col_name) %in% c("WND") ~ "Wind",
        !!sym(col_name) %in% c("WO") ~ "Waste/Other Oil",
        TRUE ~ "Unknown"
      )
    )
}

# MER Fuel Type Code	A partial aggregation of the reported fuel type codes into larger categories used by EIA in, for example, the Monthly Energy Review (MER).Two or three letter alphanumeric.  See the Fuel Code table (Table 5), below:
  # SUN	Solar PV and thermal
  # COL	Coal
  # DFO	Distillate Petroleum
  # GEO	Geothermal
  # HPS	Hydroelectric Pumped Storage
  # HYC	Hydroelectric Conventional
  # MLG	Biogenic Municipal Solid Waste and Landfill Gas
  # NG	Natural Gas
  # NUC	Nuclear
  # OOG	Other Gases
  # ORW	Other Renewables
  # OTH	Other (including nonbiogenic MSW)
  # PC	Petroleum Coke
  # RFO	Residual Petroleum
  # WND	Wind
  # WOC	Waste Coal
  # WOO	Waste Oil
  # WWW	Wood and Wood Waste


# rename MER fuel type code, pass df and col_name with code
mer_fuel_map_rename <- function(df_sankey, col_name="mer_fuel_type_code") {
  df_sankey %>%
    mutate(
      mer_fuel_type := case_when(
        !!sym(col_name) %in% c("SUN") ~ "Solar",
        !!sym(col_name) %in% c("COL") ~ "Coal",
        !!sym(col_name) %in% c("DFO") ~ "Distillate Petroleum",
        !!sym(col_name) %in% c("GEO") ~ "Geothermal",
        !!sym(col_name) %in% c("HPS") ~ "Hydroelectric Pumped Storage",
        !!sym(col_name) %in% c("HYC") ~ "Hydroelectric Conventional",
        !!sym(col_name) %in% c("MLG") ~ "Biogenic Municipal Solid Waste and Landfill Gas",
        !!sym(col_name) %in% c("NG") ~ "Natural Gas",
        !!sym(col_name) %in% c("NUC") ~ "Nuclear",
        !!sym(col_name) %in% c("OOG") ~ "Other Gases",
        !!sym(col_name) %in% c("ORW") ~ "Other Renewables",
        !!sym(col_name) %in% c("OTH") ~ "Other (including nonbiogenic MSW)",
        !!sym(col_name) %in% c("PC") ~ "Petroleum Coke",
        !!sym(col_name) %in% c("RFO") ~ "Residual Petroleum",
        !!sym(col_name) %in% c("WND") ~ "Wind",
        !!sym(col_name) %in% c("WOC") ~ "Waste Coal",
        !!sym(col_name) %in% c("WOO") ~ "Waste Oil",
        !!sym(col_name) %in% c("WWW") ~ "Wood and Wood Waste",
        TRUE ~ "Unknown"
      )
    )
}

mer_fuel_map_agg <- function(df_sankey, col_name="mer_fuel_type_code") {
  df_sankey %>%
    mutate(
      mer_fuel_type_agg := case_when(
        !!sym(col_name) %in% c("SUN") ~ "Solar",
        !!sym(col_name) %in% c("COL", "WOC") ~ "Coal",
        !!sym(col_name) %in% c("DFO", "RFO", "WOO") ~ "Petroleum",
        !!sym(col_name) %in% c("GEO") ~ "Geothermal",
        !!sym(col_name) %in% c("HPS", "HYC") ~ "Hydroelectric",
        !!sym(col_name) %in% c("MLG") ~ "Biogenic MSW and Landfill Gas",
        !!sym(col_name) %in% c("NG") ~ "Natural Gas",
        !!sym(col_name) %in% c("NUC") ~ "Nuclear",
        !!sym(col_name) %in% c("OOG") ~ "Other Gases",
        !!sym(col_name) %in% c("ORW", "WWW") ~ "Other Renewables",
        !!sym(col_name) %in% c("OTH") ~ "Other (including nonbiogenic MSW)",
        TRUE ~ "Unknown"
      )
    )
}

# remap fuels to broader categories
# > unique(eia923_fuel_input_C$source)
# [1] "Bituminous Coal"
# [2] "Distillate Fuel Oil"
# [3] "Subbituminous Coal"
# [4] "Natural Gas"
# [5] "Water for Conventional Hydroelectric"
# [6] "Solar"
# [7] "Landfill Gas"
# [8] "Electricity used for energy storage"

remap_fuel_broad <- function(df_sankey, col_name="source") {
  df_sankey %>%
    mutate(
      fuel_broad := case_when(
        !!sym(col_name) %in% c("Bituminous Coal", "Subbituminous Coal", "Anthracite Coal", "Waste/Other Coal", "Lignite Coal") ~ "Coal",
        !!sym(col_name) %in% c("Distillate Fuel Oil",  "Residual Fuel Oil", "Tire-derived Fuels", "Waste/Other Oil", "Kerosene", "Jet Fuel") ~ "Petroleum",
        !!sym(col_name) %in% c("Natural Gas", "Other Gas", "Blast Furnace Gas") ~ "Natural Gas",
        !!sym(col_name) %in% c("Water at a Conventional Hydroelectric Turbine", "Water for Conventional Hydroelectric") ~ "Hydroelectric Water",
        !!sym(col_name) %in% c("Solar", "Photovoltaic") ~ "Solar",
        !!sym(col_name) %in% c("Landfill Gas", "Other Biomass Gas", "Biogenic Municipal Solid Waste", "Other Biomass Liquids", "Other Biomass Solids", "Wood/Wood Waste Solids", "Wood Waste Liquids", "Black Liquor") ~ "Biomass",
        !!sym(col_name) %in% c("Geothermal") ~ "Geothermal",
        !!sym(col_name) %in% c("Wind", "Wind Turbine, Onshore") ~ "Wind",
        !!sym(col_name) %in% c("Electricity used for energy storage") ~ "Energy Storage",
        TRUE ~ "Other"
      )
    )
}


# aggregate plants into categories
# > unique(eia923_fuel_input_C$target)
# [1] "Bowen"
# [2] "Jack McDonough"
# [3] "Morgan Falls"
# [4] "Yates"
# [5] "Buford"
# [6] "Allatoona"
# [7] "Inforum"
# [8] "CNN Center"
# [9] "Shepherd Center"
# [10] "191 Peachtree Tower"
# [11] "Emory Decatur Hospital"
# [12] "Sun Trust Plaza"
# [13] "Atlanta Gift Mart LP"
# [14] "Georgia Pacific Center"
# [15] "Bank of America Plaza"
# [16] "State Farm Support Center East"
# [17] "Emory Hillandale Hospital"
# [18] "Laredo Bus Facility Solar Canopies"
# [19] "MAS ASB Cogen Plant"
# [20] "Hickory Ridge Landfill Solar Project"
# [21] "Georgia LFG Richland Creek Plant"
# [22] "Atlanta Falcons Solar"
# [23] "Solar BESS Hybrid"
# [24] "Tech Square Microgrid"
# [25] "Turnipseed Solar, LLC"
# [26] "Bartow Davidson"
remap_plants <- function(df, col_name = "target") {
  df %>%
    mutate(
      plant_aggregated = case_when(
        !!sym(col_name) %in% c("Bowen") ~ "Bowen Plant", # large coal plant
        !!sym(col_name) %in% c("Yates") ~ "Yates Plant", # large natural gas / legacy coal plant
        !!sym(col_name) %in% c("Jack McDonough") ~ "Jack McDonough", # large combined-cycle gas plant
        !!sym(col_name) %in% c("Morgan Falls", "Buford", "Allatoona") ~ "Hydroelectric Plants", # conventional hydro generation
        !!sym(col_name) %in% c("Georgia LFG Richland Creek Plant") ~ "Landfill Gas / Biogas", # renewable methane-based generation
        !!sym(col_name) %in% c("MAS ASB Cogen Plant") ~ "Cogeneration / CHP", # on-site combined heat and power system
        !!sym(col_name) %in% c("Laredo Bus Facility Solar Canopies",
                               "Hickory Ridge Landfill Solar Project",
                               "Atlanta Falcons Solar",
                               "Turnipseed Solar, LLC",
                               "Solar BESS Hybrid"
                               ) ~ "Solar Projects (Utility & Distributed)", # solar and solar + storage sites
        !!sym(col_name) %in% c("Tech Square Microgrid") ~ "Microgrid / Advanced Distributed Energy", # smart grid / campus-level system
        !!sym(col_name) %in% c("Inforum",
                               "CNN Center",
                               "Shepherd Center",
                               "191 Peachtree Tower",
                               "Emory Decatur Hospital",
                               "Sun Trust Plaza",
                               "Atlanta Gift Mart LP",
                               "Georgia Pacific Center",
                               "Bank of America Plaza",
                               "State Farm Support Center East",
                               "Emory Hillandale Hospital"
                               ) ~ "Commercial Building Energy Sites", # large downtown or institutional facilities with on-site or efficiency energy systems
        !!sym(col_name) %in% c("Bartow Davidson") ~ "Other Generation / Industrial Site", # smaller or industrial-scale generation not fitting elsewhere
        TRUE ~ "Other"
      )
    )
}

# aggregate plants into broader categories
remap_plants_agg <- function(df, col_name = "target") {
  df %>%
    mutate(
      plant_aggregated = case_when(
        !!sym(col_name) %in% c("Bowen") ~ "Bowen Plant", # large coal plant
        !!sym(col_name) %in% c("Yates") ~ "Yates Plant", # large natural gas / legacy coal plant
        !!sym(col_name) %in% c("Jack McDonough") ~ "Jack McDonough", # large combined-cycle gas plant

        # conventional hydro generation grouped with small renewables
        !!sym(col_name) %in% c("Morgan Falls", "Buford", "Allatoona"
                               # ) ~ "Hydro & Renewable Plants",
                               ) ~ "Utility-scale Generation",
        # landfill gas, solar, CHP, and microgrid assets
        !!sym(col_name) %in% c("Georgia LFG Richland Creek Plant",
                               "MAS ASB Cogen Plant",
                               "Laredo Bus Facility Solar Canopies",
                               "Hickory Ridge Landfill Solar Project",
                               "Atlanta Falcons Solar",
                               "Turnipseed Solar, LLC",
                               "Solar BESS Hybrid",
                               "Tech Square Microgrid"
        # ) ~ "Renewables & Distributed Energy",
        ) ~ "Distributed-scale Generation",
        # building-based or industrial on-site generation
        !!sym(col_name) %in% c("Inforum",
                               "CNN Center",
                               "Shepherd Center",
                               "191 Peachtree Tower",
                               "Emory Decatur Hospital",
                               "Sun Trust Plaza",
                               "Atlanta Gift Mart LP",
                               "Georgia Pacific Center",
                               "Bank of America Plaza",
                               "State Farm Support Center East",
                               "Emory Hillandale Hospital",
                               "Bartow Davidson"
        # ) ~ "Commercial & Institutional Sites",
        ) ~ "On-Site Backup Generation",

        TRUE ~ "Other"
      )
    )
}





## SEDS ----
# from interflow
# CLCCB	COM_coal_demand_total_total_bbtu_from_EPD_coal_total_total_total_bbtu
# CLICB	IND_coal_demand_total_total_bbtu_from_EPD_coal_total_total_total_bbtu
# EMACB	TRA_biomass_demand_total_total_bbtu_from_EPD_biomass_total_total_total_bbtu
# GECCB	COM_geothermal_demand_total_total_bbtu_from_EPD_geothermal_total_total_total_bbtu
# GERCB	RES_geothermal_demand_total_total_bbtu_from_EPD_geothermal_total_total_total_bbtu
# NGACB	TRA_natgas_demand_total_total_bbtu_from_EPD_natgas_total_total_total_bbtu
# NGCCB	COM_natgas_demand_total_total_bbtu_from_EPD_natgas_total_total_total_bbtu
# NGICB	IND_natgas_demand_total_total_bbtu_from_EPD_natgas_total_total_total_bbtu
# NGRCB	RES_natgas_demand_total_total_bbtu_from_EPD_natgas_total_total_total_bbtu
# PAACB	TRA_petroleum_demand_total_total_bbtu_from_EPD_petroleum_total_total_total_bbtu
# PACCB	COM_petroleum_demand_total_total_bbtu_from_EPD_petroleum_total_total_total_bbtu
# PAICB	IND_petroleum_demand_total_total_bbtu_from_EPD_petroleum_total_total_total_bbtu
# PARCB	RES_petroleum_demand_total_total_bbtu_from_EPD_petroleum_total_total_total_bbtu
# SOCCB	COM_solar_demand_total_total_bbtu_from_EPD_solar_total_total_total_bbtu
# SORCB	RES_solar_demand_total_total_bbtu_from_EPD_solar_total_total_total_bbtu
# WDRCB	RES_biomass_demand_total_total_bbtu_from_EPD_biomass_total_total_total_bbtu
# WWCCB	COM_biomass_demand_total_total_bbtu_from_EPD_biomass_total_total_total_bbtu
# WWICB	IND_biomass_demand_total_total_bbtu_from_EPD_biomass_total_total_total_bbtu
# WYCCB	COM_wind_demand_total_total_bbtu_from_EPD_wind_total_total_total_bbtu
# WYICB	IND_wind_demand_total_total_bbtu_from_EPD_wind_total_total_total_bbtu

seds_codes_get <- c(
  "CLCCB", "CLICB", # coal
  "EMACB", # biomass for transport
  "GEICB", "GECCB", "GERCB", # geothermal
  "NGACB", "NGCCB", "NGICB", "NGRCB", # natural gas
  "PAACB", "PACCB", "PAICB", "PARCB", # petroleum
  "SOCCB", "SORCB", # solar
  "WDRCB", "WWCCB", "WWICB", # biomass
  "WYCCB", "WYICB" # wind
)

# use seds codes to set sources and targets
# sources: coal, natural gas, petroleum, biomass, solar, wind, geothermal
# targets: residential, commercial, industrial, transport
seds_target_set <- function(df_sankey, col_name="msn") {
  df_sankey %>%
    mutate(
      source = case_when(
        !!sym(col_name) %in% c("CLCCB", "CLICB") ~ "Coal",
        !!sym(col_name) %in% c("EMACB") ~ "Biomass",
        !!sym(col_name) %in% c("GEICB", "GECCB", "GERCB") ~ "Geothermal",
        !!sym(col_name) %in% c("NGACB", "NGCCB", "NGICB", "NGRCB") ~ "Natural Gas",
        !!sym(col_name) %in% c("PAACB", "PACCB", "PAICB", "PARCB") ~ "Petroleum",
        !!sym(col_name) %in% c("SOCCB", "SORCB") ~ "Solar",
        !!sym(col_name) %in% c("WDRCB", "WWCCB", "WWICB") ~ "Biomass",
        !!sym(col_name) %in% c("WYCCB", "WYICB") ~ "Wind",
        TRUE ~ "Other"
      ),
      target = case_when(
        grepl("RC", !!sym(col_name)) ~ "residential",
        grepl("CC", !!sym(col_name)) ~ "commercial",
        grepl("IC", !!sym(col_name)) ~ "industrial",
        grepl("AC", !!sym(col_name)) ~ "transport",
        TRUE ~ "Other"
      )
    )
}

# PLOTTING ----

mytheme <- theme_minimal() + theme(
  panel.background = element_blank(),
  # panel.grid.major = element_blank(),
  panel.grid.major = element_line(color = "gray95", linewidth = 0.2),
  panel.grid.minor = element_blank(),
  panel.border = element_rect(fill = NA, color = "black"),
  strip.text = element_text(face = "bold"),
  # plot.title = element_text(face = "bold"),
  # show x and y ticks
  axis.ticks = element_line(color = "black"),
  # legend.position = "bottom"
  legend.text = element_text(size = 9),     # labels inside the legend
  legend.title = element_text(size = 9, face = "bold")
)

# combined water and energy rename
pretty_labels <- function(df_sankey) {
  df_sankey %>%
    mutate(
      source = case_when(
        # water sources
        # source == "surfaceWater" ~ "Surface Water",
        source == "surfaceWater" ~ "Surface Water (all basins)",
        source == "publicWatSup" ~ "Public Water Supply",
        source == "groundwater" ~ "Groundwater",
        source == "groundwaterAllBasins" ~ "Groundwater (all basins)",
        # source == "subsurface" ~ "Shallow Subsurface Water",
        source == "subsurface" ~ "Infiltration and Inflow",
        source == "agricultural" ~ "Agricultural Use",
        source == "industrial" ~ "Industrial Use",
        source == "residential" ~ "Residential Use",
        source == "commercial" ~ "Commercial Use",
        source == "losses" ~ "Losses",
        source == "wastewater" ~ "Wastewater Collection",
        source == "ww_imports" ~ "Wastewater Transfer Inflows (within Metro Atlanta)",
        source == "ww_exports" ~ "Wastewater Transfer Outflows (within Metro Atlanta)",
        source == "septic" ~ "Septic Systems",
        source == "in-county treatment" ~ "In-County Treatment",

        # energy sources
        source == "Coal" ~ "Coal",
        source == "Gas" ~ "Natural Gas",
        source == "onsiteBTM" ~ "Onsite / BehindTheMeter",
        source == "Electricity Imports" ~ "Electricity Imports",
        source == "Bowen" ~ "Bowen Plant",
        source == "Jack McDonough" ~ "Jack McDonough Plant",
        source == "McDonough" ~ "Jack McDonough Plant",
        source == "Yates" ~ "Yates Plant",
        # source == "Electricity" ~ "Grid Electricity",
        # source == "electricity" ~ "Grid Electricity",
        source == "Electricity" ~ "Thermoelectric Generation",
        source == "electricity" ~ "Thermoelectric Generation",
        source == "elec_import" ~ "Electricity Imports",
        source == "elec_export" ~ "Electricity Exports",
        source == "out_metro_elec_import" ~ "Out-Metro Electricity Imports",
        source == "out_metro_elec_export" ~ "Out-Metro Electricity Exports",
        source == "government" ~ "Government Use",
        source == "transport" ~ "Transportation Use",

        # energy for water
        source == "extract_groundwater" ~ "Groundwater Extraction",
        source == "extract_surfaceWater" ~ "Surface Water Withdrawal",
        source == "treat_groundwater" ~ "Groundwater Treatment",
        source == "treat_surfaceWater" ~ "Surface Water Treatment",
        source == "distribute_groundwater" ~ "Groundwater Distribution",
        source == "distribute_surfaceWater" ~ "Surface Water Distribution",
        # TODO: hack to make the node match. In reality should be split into
        # in-county and out-of-county treatment. but leaving it out because with
        # ww exports the energy to move stuff will also need to be accounted.
        # One idea is to add back source into the grouping of this table en4ww_treat
        # and have two separate nodes for in-county and out-of-county treatment energy use.
        # then use freshwater distribution energy coeff to add to exports energy use.
        source == "treat_wastewater" ~ "Total Wastewater Treatment",
        source == "en_wwtreat" ~ "Wastewater Treatment",
        source == "en_wwdist" ~ "Wastewater Transport",
        source == "en4water" ~ "Water Services Energy",

        TRUE ~ source
      ),
      target = case_when(
        # water targets
        target == "publicWatSup" ~ "Public Water Supply",
        target == "groundwater" ~ "Groundwater",
        target == "groundwaterAllBasins" ~ "Groundwater (all basins)",
        target == "agricultural" ~ "Agricultural Use",
        target == "industrial" ~ "Industrial Use",
        target == "residential" ~ "Residential Use",
        target == "commercial" ~ "Commercial Use",
        target == "losses" ~ "Losses",
        target == "wastewater" ~ "Wastewater Collection",
        target == "ww_imports" ~ "Wastewater Transfer Inflows (within Metro Atlanta)",
        target == "ww_exports" ~ "Wastewater Transfer Outflows (within Metro Atlanta)",
        target == "septic" ~ "Septic Systems",
        target == "wastewater_treated" ~ "Wastewater Treated",
        target == "in-county treatment" ~ "In-County Treatment",
        target == "discharge" ~ "Discharge",

        # energy targets
        target == "Coal" ~ "Coal",
        target == "Gas" ~ "Natural Gas",
        target == "onsiteBTM" ~ "Onsite Solar/DER",
        target == "Electricity Imports" ~ "Electricity Imports",
        target == "Bowen" ~ "Bowen Plant",
        target == "Jack McDonough" ~ "Jack McDonough Plant",
        target == "McDonough" ~ "Jack McDonough Plant",
        target == "Yates" ~ "Yates Plant",
        # target == "Electricity" ~ "Grid Electricity",
        # target == "electricity" ~ "Grid Electricity",
        target == "Electricity" ~ "Thermoelectric Generation",
        target == "electricity" ~ "Thermoelectric Generation",
        target == "elec_import" ~ "Electricity Imports",
        target == "elec_export" ~ "Electricity Exports",
        target == "out_metro_elec_import" ~ "Out-Metro Electricity Imports",
        target == "out_metro_elec_export" ~ "Out-Metro Electricity Exports",
        target == "government" ~ "Government Use",
        target == "transport" ~ "Transportation Use",
        target == "elec_own_use" ~ "Plants Own Use",
        target == "efficiency_losses" ~ "Efficiency Losses",
        target == "td_losses" ~ "Transmission & Dist. Losses",


        # energy for water
        target == "en4water" ~ "Water Services Energy",
        target == "extract_groundwater" ~ "Groundwater Extraction",
        target == "extract_surfaceWater" ~ "Surface Water Withdrawal",
        target == "treat_groundwater" ~ "Groundwater Treatment",
        target == "treat_surfaceWater" ~ "Surface Water Treatment",
        target == "distribute_groundwater" ~ "Groundwater Distribution",
        target == "distribute_surfaceWater" ~ "Surface Water Distribution",
        target == "treat_wastewater" ~ "Total Wastewater Treatment",
        target == "en_wwtreat" ~ "Wastewater Treatment",
        target == "en_wwdist" ~ "Wastewater Transport",
        target =="energy_services" ~ "Energy Services",
        target =="rejected_energy" ~ "Rejected Energy",

        TRUE ~ target
      )
    )
}

plot_sankey <- function(df_sankey, title = "Metro Atlanta Flows", yr = max(YEARS_TO_ENSURE),
                        animate = TRUE, animateby = year, years = YEARS_TO_ENSURE,
                        reg = counties, agg = TRUE, pretty_label = TRUE
                        ) {

  # stop if the data has multiple flows
  if (any(duplicated(df_sankey))) {
    head(repeats(df_sankey))
    stop("Data has multiple flows / repeated rows. Please check the data.")
  }

  # stop if the data has negative values
  if (any(df_sankey$value < 0)) {
    head(df_sankey %>% filter(value < 0))
    stop("Data has negative values. Please check the data.")
  }

  # validate yr
  if (!animate && !yr %in% years) stop("yr = ", yr, " not in years: ", paste(years, collapse = ", "))

  # if pretty label
  if (pretty_label) {
    df_sankey <- pretty_labels(df_sankey)
  }

  # complete the data to set the full canvas
  if ("county" %in% colnames(df_sankey)) {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(county, year, nesting(source, target), fill = list(value = 0))
  } else {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(year, nesting(source, target), fill = list(value = 0))
  }


  # generate node label before filtering
  node_labels <- unique(c(as.character(df_sankey$source), as.character(df_sankey$target)))

  # filter to selected years
  df_sankey <- df_sankey %>% filter(year %in% years)

  # filter to counties regions
  if ("county" %in% colnames(df_sankey)) {
    df_sankey <- df_sankey %>% filter(county %in% reg)
  }

  # aggregate over counties
  if (agg == TRUE & "county" %in% colnames(df_sankey)) {
    df_sankey <- df_sankey %>%
      group_by(across(-county)) %>% # groups by year, source, target
      summarise(value = sum(value), .groups = "drop")
  }

  if (animate == FALSE) {
    df_sankey <- df_sankey %>% filter(year == yr)
  }

  # plot the sankey
  p <- plot_ly(
    data = df_sankey,
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = node_labels,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = match(df_sankey$source, node_labels) - 1,
      target = match(df_sankey$target, node_labels) - 1,
      value = df_sankey$value,
      year = df_sankey$year
    ),
    frame = if(animate) ~df_sankey$year else NULL
  ) %>%
    layout(
      title = paste0(title, " for ", paste(reg, collapse = ", "),
                     if(!animate) paste0(" in ", yr) else paste0(" (", min(years), "-", max(years), ")")),
      font = list(size = 11)
    )

  if (animate == TRUE) {
    p <- p %>% animation_opts(2000, redraw = TRUE) %>%
      animation_slider(currentvalue = list(prefix = "Year ", font = list(color="red")))
  }

  return(p)
}

# plot_sankey(df_sankey)


# testing extra sankey features ----
{ # color plattes ----
  # With custom node colors
  water_node_colors <- list(
    "publicWatSup" = "#1f77b4",
    "groundwater" = "#ff7f0e",
    "agricultural" = "#2E8B57",
    "industrial" = "#4682B4",
    "residential" = "#FFD700",
    "commercial" = "#FF6347",
    "losses" = "#A9A9A9",
    "wastewater" = "#8B4513",
    "ww_imports" = "#1A5ACD",
    "ww_exports" = "#FF69B4",
    "septic" = "#20B2AA",
    "in-county treatment" = "#D2691E"
  )


  water_palette_classic <- c(
    "publicWatSup" = "#1f77b4", # water utility (blue)
    "groundwater" = "#2AA198", # aquifer (teal)
    "agricultural" = "#2E8B57", # ag (green)
    "industrial" = "#6B7280", # industrial (steel gray)
    "residential" = "#F1C40F", # residential (gold)
    "commercial" = "#E67E22", # commercial (orange)
    "losses" = "#9E9E9E", # losses (gray)
    "wastewater" = "#8E44AD", # wastewater (purple)
    "ww_imports" = "#3B5BDB", # imports (indigo)
    "ww_exports" = "#E83E8C", # exports (pink)
    "septic" = "#20B2AA", # septic (aqua)
    "in-county treatment" = "#2EC4B6" # treatment (cyan)
  )

  water_palette_colorblind <- c(
    "publicWatSup" = "#4477AA", # blue
    "groundwater" = "#44AA99", # bluish green
    "agricultural" = "#228833", # green
    "industrial" = "#999999", # gray
    "residential" = "#CCBB44", # yellow
    "commercial" = "#EE7733", # orange
    "losses" = "#888888", # medium gray
    "wastewater" = "#AA3377", # purple
    "ww_imports" = "#66CCEE", # cyan
    "ww_exports" = "#EE6677", # red/coral
    "septic" = "#117733", # dark green
    "in-county treatment" = "#88CCEE" # light cyan
  )

  water_palette_light <- c(
    "publicWatSup" = "#A6CEE3", # light blue
    "groundwater" = "#B2E2E2", # pale aqua
    "agricultural" = "#B2DF8A", # light green
    "industrial" = "#CFCFCF", # light gray
    "residential" = "#FFE082", # pale yellow
    "commercial" = "#FDBE85", # light orange
    "losses" = "#D9D9D9", # pale gray
    "wastewater" = "#CAB2D6", # lavender
    "ww_imports" = "#B3CDE3", # powder blue
    "ww_exports" = "#FBB4AE", # soft pink
    "septic" = "#CCEBC5", # mint
    "in-county treatment" = "#B3E2CD" # mint-cyan
  )

  water_palette_darkbg <- c(
    "publicWatSup" = "#4FC3F7", # bright cyan
    "groundwater" = "#00E676", # vivid green
    "agricultural" = "#76FF03", # lime
    "industrial" = "#CFD8DC", # light steel
    "residential" = "#FFD600", # amber
    "commercial" = "#FF6D00", # orange
    "losses" = "#B0BEC5", # steel gray
    "wastewater" = "#D500F9", # violet
    "ww_imports" = "#536DFE", # indigo
    "ww_exports" = "#FF4081", # pink
    "septic" = "#1DE9B6", # teal
    "in-county treatment" = "#00E5FF" # light cyan
  )

  water_palette_muted <- c(
    "publicWatSup" = "#4C78A8", # muted blue
    "groundwater" = "#72B7B2", # muted teal
    "agricultural" = "#59A14F", # muted green
    "industrial" = "#8C8C8C", # gray
    "residential" = "#F2CF5B", # mustard
    "commercial" = "#F58518", # muted orange
    "losses" = "#A0A0A0", # gray
    "wastewater" = "#B279A2", # mauve
    "ww_imports" = "#4C7DA6", # steel blue
    "ww_exports" = "#E45756", # coral red
    "septic" = "#76B7B2", # teal mint
    "in-county treatment" = "#54A24B" # green-cyan
  )

  water_palettes <- list(
    classic = water_palette_classic,
    colorblind = water_palette_colorblind,
    light = water_palette_light,
    darkbg = water_palette_darkbg,
    muted = water_palette_muted,
    node_colors = water_node_colors
  )

  energy_palette_classic <- c(
    "Coal" = "#4D4D4D", # charcoal
    "Gas" = "#F28E2B", # gas flame
    "onsiteBTM" = "#2CA02C", # behind-the-meter (solar/DER) green
    "Electricity Imports" = "#6C5CE7", # imports indigo

    "Bowen" = "#1F77B4", # plant blues
    "McDonough" = "#17BECF",
    "Yates" = "#9467BD",

    "Electricity" = "#FFD700", # grid = gold
    "losses" = "#A9A9A9", # losses gray

    "residential" = "#4C78A8",
    "commercial" = "#72B7B2",
    "industrial" = "#E15759",
    "government" = "#8E6C8A",
    "transport" = "#FF9F1C"
  )


  energy_palette_colorblind <- c(
    "Coal" = "#595959",
    "Gas" = "#E69F00",
    "onsiteBTM" = "#009E73",
    "Electricity Imports" = "#56B4E9",

    "Bowen" = "#0072B2",
    "McDonough" = "#CC79A7",
    "Yates" = "#D55E00",

    "Electricity" = "#F0E442",
    "losses" = "#999999",

    "residential" = "#56B4E9",
    "commercial" = "#009E73",
    "industrial" = "#D55E00",
    "government" = "#CC79A7",
    "transport" = "#0072B2"
  )


  energy_palette_light <- c(
    "Coal" = "#C0C0C0",
    "Gas" = "#FDB863",
    "onsiteBTM" = "#B2DF8A",
    "Electricity Imports" = "#C7CEEA",

    "Bowen" = "#A6CEE3",
    "McDonough" = "#CCEBC5",
    "Yates" = "#CAB2D6",

    "Electricity" = "#FFE082",
    "losses" = "#E0E0E0",

    "residential" = "#B3E5FC",
    "commercial" = "#B3CDE3",
    "industrial" = "#FBB4AE",
    "government" = "#D1C4E9",
    "transport" = "#FFCCBC"
  )


  energy_palette_darkbg <- c(
    "Coal" = "#BDBDBD",
    "Gas" = "#FFB300",
    "onsiteBTM" = "#00E676",
    "Electricity Imports" = "#7C4DFF",

    "Bowen" = "#40C4FF",
    "McDonough" = "#18FFFF",
    "Yates" = "#E040FB",

    "Electricity" = "#FFD600",
    "losses" = "#90A4AE",

    "residential" = "#64B5F6",
    "commercial" = "#1DE9B6",
    "industrial" = "#FF5252",
    "government" = "#B388FF",
    "transport" = "#FF6E40"
  )


  energy_palette_muted <- c(
    "Coal" = "#6B6B6B",
    "Gas" = "#DAA25E",
    "onsiteBTM" = "#59A14F",
    "Electricity Imports" = "#7E8CE0",

    "Bowen" = "#4C78A8",
    "McDonough" = "#72B7B2",
    "Yates" = "#B279A2",

    "Electricity" = "#F2CF5B",
    "losses" = "#A0A0A0",

    "residential" = "#5E81AC",
    "commercial" = "#8FBCBB",
    "industrial" = "#E07A5F",
    "government" = "#A78AB5",
    "transport" = "#F29E4C"
  )


  energy_palettes <- list(
    classic = energy_palette_classic,
    colorblind = energy_palette_colorblind,
    light = energy_palette_light,
    darkbg = energy_palette_darkbg,
    muted = energy_palette_muted
  )

}

# sankey pro helpers ----

hex_to_rgba <- function(hex_color, alpha = 1) {
  if (is.na(hex_color) || is.null(hex_color)) return(paste0("rgba(128,128,128,", alpha, ")"))
  if (substr(hex_color, 1, 1) != "#") {
    hex_color <- tryCatch(rgb(t(col2rgb(hex_color)), maxColorValue = 255),
                          error = function(e) "#808080")
  }
  rgb_vals <- col2rgb(hex_color)
  paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
}

blend_colors <- function(color1, color2, alpha = 0.3) {
  if (is.na(color1)) color1 <- "#808080"
  if (is.na(color2)) color2 <- "#808080"
  rgb1 <- tryCatch(col2rgb(color1), error = function(e) col2rgb("#808080"))
  rgb2 <- tryCatch(col2rgb(color2), error = function(e) col2rgb("#808080"))
  blended <- (rgb1 + rgb2) / 2
  paste0("rgba(", blended[1], ",", blended[2], ",", blended[3], ",", alpha, ")")
}

auto_detect_units <- function(df) {
  if (!"units" %in% names(df)) return("")
  u <- unique(df$units)
  u <- u[!is.na(u)]
  if (length(u) == 1) u else ""
}

# preferred positions for known nodes (pretty_labels names)
PREFERRED_POSITIONS <- list(
  # water sinks / peripherals
  "Losses"                  = list(x = 0.95, y = 0.05),
  "losses"                  = list(x = 0.95, y = 0.05),
  "Septic"                  = list(x = 0.95, y = 0.90),
  "septic"                  = list(x = 0.95, y = 0.90),
  "Wastewater Exports"      = list(x = 0.95, y = 0.80),
  "ww_exports"              = list(x = 0.95, y = 0.80),
  "Wastewater Imports"      = list(x = 0.05, y = 0.80),
  "ww_imports"              = list(x = 0.05, y = 0.80),
  # water sources
  "Surface Water"           = list(x = 0.05, y = 0.15),
  "surfaceWater"            = list(x = 0.05, y = 0.15),
  "Groundwater"             = list(x = 0.05, y = 0.35),
  "groundwater"             = list(x = 0.05, y = 0.35),
  "Public Water Supply"     = list(x = 0.35, y = 0.25),
  "publicWatSup"            = list(x = 0.35, y = 0.25)
)

# known water node names (raw + pretty) for domain color detection
WATER_NODE_NAMES <- c(
  names(water_palettes$classic),
  "Surface Water", "Groundwater", "Public Water Supply",
  "Wastewater", "Wastewater Imports", "Wastewater Exports",
  "In-County Treatment", "Septic", "Losses",
  "Agricultural", "Residential", "Commercial", "Industrial",
  "surfaceWater", "groundwater", "publicWatSup",
  "wastewater", "ww_imports", "ww_exports",
  "in-county treatment", "septic", "losses",
  "agricultural", "residential", "commercial", "industrial"
)

ENERGY_NODE_NAMES <- c(
  names(energy_palettes$classic),
  "Grid Electricity", "Electricity Imports", "Onsite / BehindTheMeter",
  "electricity", "elec_import", "onsiteBTM",
  "Coal", "Gas", "Petroleum", "Natural Gas",
  "Bowen", "McDonough", "Yates",
  "Government", "Transport", "government", "transport",
  "en4water"
)

compute_node_positions <- function(all_nodes, df, user_positions = NULL, use_preferred = TRUE) {
  sources <- unique(df$source)
  targets <- unique(df$target)
  pure_sources <- setdiff(sources, targets)
  pure_sinks   <- setdiff(targets, sources)
  intermediates <- intersect(sources, targets)

  n <- length(all_nodes)
  node_x <- numeric(n)
  node_y <- numeric(n)

  for (i in seq_along(all_nodes)) {
    nd <- all_nodes[i]
    if (nd %in% pure_sources) {
      n_grp <- length(pure_sources)
      idx <- match(nd, pure_sources)
      node_x[i] <- 0.1
      node_y[i] <- if (n_grp == 1) 0.5 else (idx - 1) / (n_grp - 1) * 0.8 + 0.1
    } else if (nd %in% pure_sinks) {
      n_grp <- length(pure_sinks)
      idx <- match(nd, pure_sinks)
      node_x[i] <- 0.9
      node_y[i] <- if (n_grp == 1) 0.5 else (idx - 1) / (n_grp - 1) * 0.8 + 0.1
    } else {
      n_grp <- length(intermediates)
      idx <- match(nd, intermediates)
      node_x[i] <- 0.5
      node_y[i] <- if (n_grp == 1) 0.5 else (idx - 1) / (n_grp - 1) * 0.8 + 0.1
    }
  }

  # overlay preferred positions
  if (use_preferred) {
    for (i in seq_along(all_nodes)) {
      if (all_nodes[i] %in% names(PREFERRED_POSITIONS)) {
        pos <- PREFERRED_POSITIONS[[all_nodes[i]]]
        node_x[i] <- pos$x
        node_y[i] <- pos$y
      }
    }
  }

  # overlay user positions (user always wins)
  if (!is.null(user_positions)) {
    for (i in seq_along(all_nodes)) {
      if (all_nodes[i] %in% names(user_positions)) {
        pos <- user_positions[[all_nodes[i]]]
        node_x[i] <- pos$x
        node_y[i] <- pos$y
      }
    }
  }

  list(x = node_x, y = node_y)
}

resolve_node_colors <- function(all_nodes, node_colors = NULL, color_mode = "auto",
                                 palette_name = "Spectral") {
  n_nodes <- length(all_nodes)

  if (!is.null(node_colors)) {
    # user provided named vector or list
    if (!is.null(names(node_colors))) {
      cols <- sapply(all_nodes, function(nd) {
        if (nd %in% names(node_colors)) as.character(node_colors[[nd]]) else "lightgray"
      })
    } else {
      cols <- rep(node_colors, length.out = n_nodes)
    }
    names(cols) <- all_nodes
    return(cols)
  }

  if (color_mode == "water") {
    pal <- water_palettes$classic
    cols <- sapply(all_nodes, function(nd) {
      if (nd %in% names(pal)) pal[[nd]] else "lightgray"
    })

  } else if (color_mode == "energy") {
    pal <- energy_palettes$classic
    cols <- sapply(all_nodes, function(nd) {
      if (nd %in% names(pal)) pal[[nd]] else "lightgray"
    })

  } else if (color_mode == "domain") {
    # auto-detect: water nodes -> water palette, energy nodes -> energy palette
    w_pal <- water_palettes$classic
    e_pal <- energy_palettes$classic
    cols <- sapply(all_nodes, function(nd) {
      if (nd %in% names(w_pal)) return(w_pal[[nd]])
      if (nd %in% names(e_pal)) return(e_pal[[nd]])
      if (nd %in% WATER_NODE_NAMES) return("#4C78A8")   # default water blue
      if (nd %in% ENERGY_NODE_NAMES) return("#E15759")   # default energy red
      "lightgray"
    })

  } else {
    # "auto" — professional distinct colors
    if (n_nodes <= 3) {
      cols <- RColorBrewer::brewer.pal(3, "Set2")[1:n_nodes]
    } else if (n_nodes <= 11) {
      cols <- RColorBrewer::brewer.pal(n_nodes, palette_name)
    } else {
      c1 <- RColorBrewer::brewer.pal(11, palette_name)
      c2 <- RColorBrewer::brewer.pal(min(9, n_nodes - 11), "Set1")
      cols <- c(c1, c2, rainbow(max(0, n_nodes - 20)))[1:n_nodes]
    }
    names(cols) <- all_nodes
  }

  names(cols) <- all_nodes
  cols
}

compute_node_labels <- function(all_nodes, df, units_str, show_values, value_stats, yr, animate) {
  if (!show_values) return(all_nodes)

  # helper: compute node throughput (max of inflows vs outflows)
  node_throughput <- function(df_slice) {
    bind_rows(
      df_slice %>% group_by(node = source) %>% summarise(total = sum(value, na.rm = TRUE), .groups = "drop"),
      df_slice %>% group_by(node = target) %>% summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    ) %>%
      group_by(node) %>%
      summarise(total = max(total, na.rm = TRUE), .groups = "drop")
  }

  if (!animate) {
    # static: exact values for yr
    totals <- node_throughput(df %>% filter(year == yr))
    sapply(all_nodes, function(nd) {
      val <- totals$total[totals$node == nd]
      if (length(val) > 0 && !is.na(val) && val > 0) {
        paste0(nd, "\n", round(val, 1), " ", units_str)
      } else nd
    })
  } else {
    # animated: compute stats across years
    yearly_totals <- df %>%
      group_by(year) %>%
      group_split() %>%
      map_dfr(function(yr_df) {
        node_throughput(yr_df) %>% mutate(year = yr_df$year[1])
      })

    stats <- yearly_totals %>%
      group_by(node) %>%
      summarise(
        mn = min(total, na.rm = TRUE),
        avg = mean(total, na.rm = TRUE),
        mx = max(total, na.rm = TRUE),
        .groups = "drop"
      )

    sapply(all_nodes, function(nd) {
      row <- stats %>% filter(node == nd)
      if (nrow(row) == 0 || row$mx == 0) return(nd)

      if (value_stats == "none") {
        nd
      } else if (value_stats == "avg") {
        paste0(nd, "\n(avg: ", round(row$avg, 1), " ", units_str, ")")
      } else if (value_stats == "range") {
        paste0(nd, "\n", round(row$mn, 1), " – ", round(row$mx, 1), " ", units_str)
      } else {
        # "all" — default
        paste0(nd, "\nmin: ", round(row$mn, 1), " | avg: ", round(row$avg, 1),
               " | max: ", round(row$mx, 1), " ", units_str)
      }
    })
  }
}

# plot_sankey_pro ----
#
# Unified Sankey plotting function for energy-water flow diagrams.
# Merges capabilities of plot_sankey, plot_sankey_enhanced, plot_sankey_advanced.
#
# Minimal call: plot_sankey_pro(df) — auto-detects everything
# Full control:  colors, positions, gradients, theme, hover, animation stats
#
plot_sankey_pro <- function(
    df,                                          # data.frame with source, target, year, value (and optionally county, units)
    title           = "Metro Atlanta Flows",     # plot title string
    yr              = max(YEARS_TO_ENSURE),       # year to show when animate = FALSE
    years           = YEARS_TO_ENSURE,            # years to include; controls animation slider range
    animate         = TRUE,                       # TRUE = animation slider across years; FALSE = static single year
    reg             = NULL,                       # county filter: NULL (all), character vector e.g. c("Cobb", "Fulton")
    agg             = TRUE,                       # TRUE = aggregate over counties; FALSE = keep county-level detail
    pretty_label    = TRUE,                       # TRUE = apply pretty_labels() to rename nodes for display
    # --- colors ---
    node_colors     = NULL,                       # NULL (auto) or named vector e.g. c("losses" = "gray", "Coal" = "#4D4D4D")
    color_mode      = "auto",                     # "auto" (Spectral palette), "water", "energy", "domain" (auto water+energy)
    palette_name    = "Spectral",                 # RColorBrewer palette when color_mode = "auto". Options: "Spectral", "Set2", "Set3", "Paired", etc.
    link_colors     = NULL,                       # NULL (auto from nodes), named vector, or unnamed color vector
    link_opacity    = 0.3,                        # link transparency 0-1. Lower = more transparent
    use_gradients   = FALSE,                      # TRUE = blend source+target colors for links; FALSE = source color only
    # --- labels ---
    show_values     = TRUE,                       # TRUE = show throughput values in node labels
    label_units     = NULL,                       # NULL (auto-detect from df$units), or string e.g. "MGD", "PJ", "TJ"
    value_stats     = "all",                      # animated label stats: "all" (min|avg|max), "avg", "range", "none"
    # --- positions ---
    node_positions  = NULL,                       # NULL (auto-layout) or named list: list("losses" = list(x=0.95, y=0.05))
    use_preferred   = FALSE,                      # TRUE = apply PREFERRED_POSITIONS for known nodes (requires arrangement = "snap" or "fixed")
    arrangement     = "snap",                     # "snap" (default, draggable with grid), "fixed" (locked), "freeform", "perpendicular"
    # --- layout ---
    theme           = "light",                    # "light" (white bg, black text) or "dark" (dark bg, white text)
    node_width      = 20,                         # node bar thickness in pixels
    node_pad        = 15,                         # vertical spacing between nodes in pixels
    show_toolbar    = TRUE,                       # TRUE = show plotly mode bar (zoom, pan, save)
    link_arrows     = 0                           # arrow length in px at link endpoints. 0 = no arrows
) {


  # --- validation ---
  if (!animate && !yr %in% years) {
    stop("yr = ", yr, " not in years: ", paste(years, collapse = ", "))
  }

  if (any(df$value < 0)) {
    stop("Data has negative values. Please check the data.")
  }

  # --- pretty labels (applied ONCE) ---
  if (pretty_label) {
    df <- pretty_labels(df)
  }

  # --- complete data canvas ---
  if ("county" %in% names(df)) {
    df <- as.data.frame(df) %>%
      complete(county, year, nesting(source, target), fill = list(value = 0))
  } else {
    df <- as.data.frame(df) %>%
      complete(year, nesting(source, target), fill = list(value = 0))
  }

  # --- filter to years ---
  df <- df %>% filter(year %in% years)

  # --- filter to counties/regions ---
  if ("county" %in% names(df) && !is.null(reg)) {
    df <- df %>% filter(county %in% reg)
  }

  # --- aggregate over counties ---
  if (agg && "county" %in% names(df)) {
    df <- df %>%
      group_by(across(-county)) %>%
      summarise(value = sum(value), .groups = "drop")
  }

  # --- auto-detect units ---
  units_str <- if (is.null(label_units)) auto_detect_units(df) else label_units

  # --- filter to yr if static ---
  if (!animate) {
    df <- df %>% filter(year == yr)
  }

  # --- nodes ---
  all_nodes <- unique(c(as.character(df$source), as.character(df$target)))

  # --- positions (only set x/y when user explicitly controls them) ---
  has_positions <- !is.null(node_positions) || use_preferred
  if (has_positions) {
    positions <- compute_node_positions(all_nodes, df, node_positions, use_preferred)
  }

  # --- node colors ---
  node_cols <- resolve_node_colors(all_nodes, node_colors, color_mode, palette_name)

  # --- link colors ---
  if (is.null(link_colors)) {
    if (use_gradients) {
      link_cols <- mapply(function(s, t) {
        blend_colors(node_cols[s], node_cols[t], link_opacity)
      }, as.character(df$source), as.character(df$target), USE.NAMES = FALSE)
    } else {
      link_cols <- sapply(as.character(df$source), function(s) {
        hex_to_rgba(node_cols[s], link_opacity)
      }, USE.NAMES = FALSE)
    }
  } else if (!is.null(names(link_colors))) {
    # named link colors: match by "source -> target", source, or target
    link_cols <- df %>%
      mutate(
        flow_key = paste(source, "->", target),
        color = case_when(
          flow_key %in% names(link_colors) ~ link_colors[flow_key],
          source %in% names(link_colors) ~ link_colors[source],
          target %in% names(link_colors) ~ link_colors[target],
          TRUE ~ "rgba(128,128,128,0.5)"
        )
      ) %>%
      pull(color)
  } else {
    link_cols <- rep(link_colors, length.out = nrow(df))
  }

  # --- node labels ---
  node_labels <- compute_node_labels(all_nodes, df, units_str, show_values, value_stats, yr, animate)

  # --- hover templates ---
  node_hover <- paste0("<b>%{label}</b><br>Total: %{value:,.1f} ", units_str, "<extra></extra>")
  link_hover <- paste0("%{source.label} \u2192 %{target.label}<br>",
                       "Flow: %{value:,.2f} ", units_str, "<extra></extra>")

  # --- theme colors ---
  bg_color    <- if (theme == "dark") "#1F1F1F" else "white"
  plot_bg     <- if (theme == "dark") "#2F2F2F" else "white"
  text_color  <- if (theme == "dark") "white" else "black"
  border_color <- if (theme == "dark") "white" else "black"
  slider_color <- if (theme == "dark") "white" else "red"

  # --- build plot ---
  node_config <- list(
    label = node_labels,
    color = unname(node_cols[all_nodes]),
    pad = node_pad,
    thickness = node_width,
    line = list(color = border_color, width = 0.5),
    hovertemplate = node_hover
  )
  if (has_positions) {
    node_config$x <- positions$x
    node_config$y <- positions$y
  }

  p <- plot_ly(
    type = "sankey",
    arrangement = arrangement,
    node = node_config,
    link = list(
      source = match(df$source, all_nodes) - 1,
      target = match(df$target, all_nodes) - 1,
      value = df$value,
      color = link_cols,
      arrowlen = link_arrows,
      hovertemplate = link_hover
    ),
    frame = if (animate) ~df$year else NULL
  )

  # --- title ---
  title_text <- paste0(
    title,
    if (!is.null(reg)) paste0(" for ", paste(reg, collapse = ", ")),
    if (!animate) paste0(" in ", yr)
  )

  p <- p %>% layout(
    title = list(
      text = title_text,
      font = list(size = 16, color = text_color),
      x = 0.5, xanchor = "center"
    ),
    font = list(size = 12, color = text_color),
    plot_bgcolor = plot_bg,
    paper_bgcolor = bg_color,
    margin = list(l = 10, r = 10, t = 80, b = 40)
  )

  # --- animation ---
  if (animate) {
    p <- p %>%
      animation_opts(2000, redraw = TRUE) %>%
      animation_slider(
        currentvalue = list(
          prefix = "Year ",
          font = list(color = slider_color, size = 14)
        )
      )
  }

  # --- toolbar ---
  p <- p %>% config(
    displayModeBar = show_toolbar,
    displaylogo = FALSE
  )

  return(p)
}

# Enhanced main function (kept as working backup)
# Preprocessing function used by plot_sankey_enhanced
prepare_sankey_enhanced <- function(df_sankey, node_colors = NULL, link_colors = NULL,
                                show_values_in_labels = FALSE, value_year = NULL,
                                pretty_label = TRUE,
                                units = "", alt_units = NULL, animate = FALSE,
                                color_scheme = NULL, link_color_by_domain = FALSE) {

  # if pretty label
  if (pretty_label) {
    df_sankey <- pretty_labels(df_sankey)
  }

  # get unique nodes in consistent order
  all_nodes <- unique(c(df_sankey$source, df_sankey$target))

  per_node <- (units == "auto" && "units" %in% names(df_sankey))

  # create node labels with values if requested
  if (show_values_in_labels && !animate) {
    df_for_labels <- if (!is.null(value_year)) {
      df_sankey %>% filter(year == value_year)
    } else {
      grp <- if (per_node) c("source", "target", "units") else c("source", "target")
      df_sankey %>%
        group_by(across(all_of(grp))) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    }

    if (per_node) {
      totals <- node_throughput_by_unit(df_for_labels)
      node_labels <- all_nodes %>%
        map_chr(~ format_node_label(.x, totals %>% filter(node == .x), alt_units))
    } else {
      node_totals <- bind_rows(
        df_for_labels %>% group_by(node = source) %>% summarise(total = sum(value, na.rm = TRUE), .groups = "drop"),
        df_for_labels %>% group_by(node = target) %>% summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
      ) %>%
        group_by(node) %>%
        summarise(total = max(total, na.rm = TRUE), .groups = "drop")

      node_labels <- all_nodes %>%
        map_chr(function(node) {
          total_val <- node_totals$total[node_totals$node == node]
          if (length(total_val) > 0 && !is.na(total_val[1]) && total_val[1] > 0) {
            paste0(node, "\n", round(total_val[1], 1), " ", units)
          } else {
            node
          }
        })
    }

  } else if (show_values_in_labels && animate) {
    grp <- if (per_node) c("source", "target", "units") else c("source", "target")
    avg_values <- df_sankey %>%
      group_by(across(all_of(grp))) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    if (per_node) {
      totals <- node_throughput_by_unit(avg_values)
      node_labels <- all_nodes %>%
        map_chr(function(nd) {
          lbl <- format_node_label(nd, totals %>% filter(node == nd), alt_units, prefix = "avg: ")
          if (grepl("\n", lbl)) {
            parts <- strsplit(lbl, "\n", fixed = TRUE)[[1]]
            paste0(parts[1], "\n(", parts[2], ")")
          } else lbl
        })
    } else {
      node_totals <- bind_rows(
        avg_values %>% group_by(node = source) %>% summarise(total = sum(value, na.rm = TRUE)),
        avg_values %>% group_by(node = target) %>% summarise(total = sum(value, na.rm = TRUE))
      ) %>%
        group_by(node) %>%
        summarise(total = max(total, na.rm = TRUE), .groups = "drop")

      node_labels <- all_nodes %>%
        map_chr(function(node) {
          total_val <- node_totals$total[node_totals$node == node]
          if (length(total_val) > 0 && !is.na(total_val[1]) && total_val[1] > 0) {
            paste0(node, "\n(avg: ", round(total_val[1], 1), " ", units, ")")
          } else {
            node
          }
        })
    }

  } else {
    node_labels <- all_nodes
  }

  # handle node colors
  # Priority: explicit node_colors > color_scheme param > COLOR_SCHEME global > Spectral
  use_named <- if (!is.null(color_scheme)) !identical(color_scheme, FALSE)
               else !identical(COLOR_SCHEME, FALSE)

  if (is.null(node_colors) && use_named) {
    palette <- if (is.list(color_scheme)) color_scheme else SANKEY_COLORS
    node_colors <- all_nodes %>%
      map_chr(~ resolve_node_color(.x, palette))
  } else if (is.null(node_colors)) {
    n_nodes <- length(all_nodes)
    if (n_nodes <= 3) {
      node_colors <- brewer.pal(3, "Set2")[1:n_nodes]
    } else if (n_nodes <= 11) {
      node_colors <- brewer.pal(n_nodes, "Spectral")
    } else {
      colors1 <- brewer.pal(11, "Spectral")
      colors2 <- brewer.pal(min(9, n_nodes - 11), "Set1")
      node_colors <- c(colors1, colors2, rainbow(max(0, n_nodes - 20)))[1:n_nodes]
    }
  } else if (is.list(node_colors) || (is.character(node_colors) && !is.null(names(node_colors)))) {
    node_colors <- all_nodes %>%
      map_chr(~ if(.x %in% names(node_colors)) node_colors[[.x]] else "lightgray")
  } else {
    node_colors <- rep(node_colors, length.out = length(all_nodes))
  }

  # handle link colors
  if (link_color_by_domain && "units" %in% names(df_sankey)) {
    energy_units <- c("EJ", "PJ", "TJ", "GWh", "kWh", "MWh", "BBtu", "MMBtu")
    link_colors <- ifelse(df_sankey$units %in% energy_units,
                          "#E8863A60",   # warm orange, 38% opacity
                          "#4A90D960")   # clear blue, 38% opacity
  } else if (is.null(link_colors)) {
    source_indices <- match(df_sankey$source, all_nodes)
    link_colors <- node_colors[source_indices]
    link_colors <- ifelse(startsWith(link_colors, "#"),
                          paste0(link_colors, "80"),
                          paste0(link_colors, "80"))
  } else if (is.list(link_colors) || (is.character(link_colors) && !is.null(names(link_colors)))) {
    link_colors <- df_sankey %>%
      mutate(
        flow_key = paste(source, "->", target),
        color = case_when(
          flow_key %in% names(link_colors) ~ link_colors[[flow_key]],
          source %in% names(link_colors) ~ link_colors[[source]],
          target %in% names(link_colors) ~ link_colors[[target]],
          TRUE ~ "rgba(128,128,128,0.5)"
        )
      ) %>%
      pull(color)
  } else {
    link_colors <- rep(link_colors, length.out = nrow(df_sankey))
  }

  return(list(
    df_sankey = df_sankey,
    node_labels = node_labels,
    node_names = all_nodes,
    node_colors = node_colors,
    link_colors = link_colors
  ))
}

plot_sankey_enhanced <- function(df_sankey, title = "Metro Atlanta Flows", yr = max(YEARS_TO_ENSURE),
                                 animate = TRUE, animateby = "year", years = YEARS_TO_ENSURE,
                                 reg = NULL, agg = TRUE, pretty_label = TRUE,
                                 node_colors = NULL, link_colors = NULL,
                                 show_values_in_labels = T, label_units = "",
                                 label_year = NULL, alt_units = NULL,
                                 color_scheme = NULL, link_color_by_domain = FALSE) {

  if (!animate && !yr %in% years) stop("yr = ", yr, " not in years: ", paste(years, collapse = ", "))

  # Existing validation code
  if (any(duplicated(df_sankey))) {
    stop("Data has multiple flows / repeated rows. Please check the data.")
  }

  if (any(df_sankey$value < 0)) {
    stop("Data has negative values. Please check the data.")
  }

  # if pretty label
  if (pretty_label) {
    df_sankey <- pretty_labels(df_sankey)
  }

  # Complete the data to set the full canvas
  nest_cols <- if ("units" %in% colnames(df_sankey)) c("source", "target", "units") else c("source", "target")
  if ("county" %in% colnames(df_sankey)) {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(county, year, nesting(!!!syms(nest_cols)), fill = list(value = 0))
  } else {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(year, nesting(!!!syms(nest_cols)), fill = list(value = 0))
  }

  # Filter to selected years
  df_sankey <- df_sankey %>% filter(year %in% years)

  # Filter to counties/regions if specified
  if ("county" %in% colnames(df_sankey) && !is.null(reg)) {
    df_sankey <- df_sankey %>% filter(county %in% reg)
  }

  # Aggregate over counties
  if (agg == TRUE & "county" %in% colnames(df_sankey)) {
    df_sankey <- df_sankey %>%
      group_by(across(-county)) %>%
      summarise(value = sum(value), .groups = "drop")
  }

  # Filter to specific year if not animating
  if (animate == FALSE) {
    df_sankey <- df_sankey %>% filter(year == yr)
  }

  # Prepare enhanced data with colors and labels
  sankey_data <- prepare_sankey_enhanced(
    df_sankey = df_sankey,
    node_colors = node_colors,
    link_colors = link_colors,
    show_values_in_labels = show_values_in_labels,
    value_year = if(animate) label_year else yr,
    units = label_units,
    alt_units = alt_units,
    pretty_label = pretty_label,
    animate = animate,
    color_scheme = color_scheme,
    link_color_by_domain = link_color_by_domain
  )

  # Create the plot
  p <- plot_ly(
    data = sankey_data$df_sankey,
    type = "sankey",
    arrangement = "snap",
    node = list(
      label = sankey_data$node_labels,
      color = sankey_data$node_colors,
      line = list(color = "black", width = 0.5),
      pad = 15,
      thickness = 20
    ),
    link = list(
      source = match(sankey_data$df_sankey$source, sankey_data$node_names) - 1,
      target = match(sankey_data$df_sankey$target, sankey_data$node_names) - 1,
      value = sankey_data$df_sankey$value,
      color = sankey_data$link_colors
    ),
    frame = if(animate) ~sankey_data$df_sankey$year else NULL
  ) %>%
    layout(
      title = list(
        text = paste0(title, if(!is.null(reg)) paste0(" for ", paste(reg, collapse = ", ")),
                      if(!animate) paste0(" in ", yr) else ""),
        font = list(size = 16)
      ),
      font = list(size = 12)
    )

  # Add animation controls
  if (animate == TRUE) {
    p <- p %>%
      animation_opts(2000, redraw = TRUE) %>%
      animation_slider(
        currentvalue = list(prefix = "Year ", font = list(color = "red", size = 14))
      )
  }

  return(p)
}


