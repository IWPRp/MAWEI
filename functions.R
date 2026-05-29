# Ancillary functions to support Metro Atlanta

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

# create directory if it doesn't exist
if (!dir.exists(SAVE_DIR)) {
  dir.create(SAVE_DIR, recursive = TRUE)
}

# county names
counties <- read_csv(paste0(DATA_DIR, "common_county_fips.csv"))$county
fips <- read_csv(paste0(DATA_DIR, "common_county_fips.csv"))$fip

YEARS_TO_ENSURE <- 2020:2024

# units
BBtu_to_EJ <- 1.055e-6 # billion British thermal units to exajoules
MMBtu_to_EJ <- 1.055e-9
kWh_to_EJ <- 3.6e-12 # 1 kWh = 3.6e6 J = 3.6e-12 EJ
MWh_to_EJ <- 3.6e-9
EJ_to_PJ <- 1e3 # exajoules to petajoules
EJ_to_TJ <- 1e6 # exajoules to terajoules
MGD_to_GPM <- 694.4444444444445 # million gallons per day to gallons per minute
HP_to_KW <- 0.7457 # horsepower to kilowatts
PUMPING_EFFICIENCY <- 0.55 # typical range 0.5-0.7
WATER_HORSEPOWER <- 3960 # constant
HOURS_PER_YEAR <- 8760
HOURS_PER_DAY <- 24
DAYS_PER_YEAR <- 365


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

# plotting ----

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
        source == "surfaceWater" ~ "Surface Water",
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
        source == "ww_imports" ~ "Wastewater Imports",
        source == "ww_exports" ~ "Wastewater Exports",
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
        source == "Electricity" ~ "Grid Electricity",
        source == "electricity" ~ "Grid Electricity",
        source == "elec_import" ~ "Electricity Imports",
        source == "elec_export" ~ "Electricity Exports",
        source == "out_metro_elec_import" ~ "Out-Metro Electricity Imports",
        source == "out_metro_elec_export" ~ "Out-Metro Electricity Exports",
        source == "government" ~ "Government Use",
        source == "transport" ~ "Transportation Use",

        # energy for water
        source == "extract_groundwater" ~ "Groundwater Extraction",
        source == "extract_surfaceWater" ~ "Surface Water Extraction",
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
        target == "ww_imports" ~ "Wastewater Imports",
        target == "ww_exports" ~ "Wastewater Exports",
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
        target == "Electricity" ~ "Grid Electricity",
        target == "electricity" ~ "Grid Electricity",
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
        target == "extract_surfaceWater" ~ "Surface Water Extraction",
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

# Preprocessing function to handle colors and labels
prepare_sankey_enhanced <- function(df_sankey, node_colors = NULL, link_colors = NULL,
                                show_values_in_labels = FALSE, value_year = NULL,
                                pretty_label = TRUE,
                                units = "", animate = FALSE) {

  # if pretty label
  if (pretty_label) {
    df_sankey <- pretty_labels(df_sankey)
  }

  # get unique nodes in consistent order
  all_nodes <- unique(c(df_sankey$source, df_sankey$target))

  # create node labels with values if requested
  if (show_values_in_labels && !animate) {
    # for static plots, calculate values for specific year
    df_for_labels <- if (!is.null(value_year)) {
      df_sankey %>% filter(year == value_year)
    } else {
      # use average across years if no specific year provided
      df_sankey %>%
        group_by(source, target) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    }

    # calculate total throughput for each node
    node_totals <- bind_rows(
      # outflows (node as source)
      df_for_labels %>%
        group_by(node = source) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop"),
      # inflows (node as target)
      df_for_labels %>%
        group_by(node = target) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop")
    ) %>%
      group_by(node) %>%
      summarise(total = max(total, na.rm = TRUE), .groups = "drop") # take the max of in or out

    # create formatted labels
    node_labels <- all_nodes %>%
      map_chr(function(node) {
        total_val <- node_totals$total[node_totals$node == node]
        if (length(total_val) > 0 && !is.na(total_val[1]) && total_val[1] > 0) {
          paste0(node, "\n", round(total_val[1], 1), " ", units)
        } else {
          node
        }
      })
  } else if (show_values_in_labels && animate) {
    # for animated plots, show average values with note
    avg_values <- df_sankey %>%
      group_by(source, target) %>%
      summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

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
  } else {
    node_labels <- all_nodes
  }

  # handle node colors
  if (is.null(node_colors)) {
    # create default color palette
    n_nodes <- length(all_nodes)
    if (n_nodes <= 3) {
      node_colors <- brewer.pal(3, "Set2")[1:n_nodes]
    } else if (n_nodes <= 11) {
      node_colors <- brewer.pal(n_nodes, "Spectral")
    } else {
      # For more than 11 nodes, combine palettes
      colors1 <- brewer.pal(11, "Spectral")
      colors2 <- brewer.pal(min(9, n_nodes - 11), "Set1")
      node_colors <- c(colors1, colors2, rainbow(max(0, n_nodes - 20)))[1:n_nodes]
    }
  } else if (is.list(node_colors) || (is.character(node_colors) && !is.null(names(node_colors)))) {
    # named colors mapping
    node_colors <- all_nodes %>%
      map_chr(~ if(.x %in% names(node_colors)) node_colors[[.x]] else "lightgray")
    # map_chr(~ if_else(.x %in% names(node_colors), node_colors[[.x]], "lightgray"))
  } else {
    # vector of colors
    node_colors <- rep(node_colors, length.out = length(all_nodes))
  }

  # handle link colors
  if (is.null(link_colors)) {
    # Default: use source node color with transparency
    source_indices <- match(df_sankey$source, all_nodes)
    link_colors <- node_colors[source_indices]
    # Add transparency (convert to rgba if needed)
    link_colors <- ifelse(startsWith(link_colors, "#"),
                          paste0(link_colors, "80"),
                          paste0(link_colors, "80"))
  } else if (is.list(link_colors) || (is.character(link_colors) && !is.null(names(link_colors)))) {
    # Named link colors
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
    # Vector of colors
    link_colors <- rep(link_colors, length.out = nrow(df_sankey))
  }

  return(list(
    df_sankey = df_sankey,
    node_labels = node_labels,
    node_names = all_nodes,  # Keep original names for matching
    node_colors = node_colors,
    link_colors = link_colors
  ))
}

# Enhanced main function
plot_sankey_enhanced <- function(df_sankey, title = "Metro Atlanta Flows", yr = max(YEARS_TO_ENSURE),
                                 animate = TRUE, animateby = "year", years = YEARS_TO_ENSURE,
                                 reg = NULL, agg = TRUE, pretty_label = TRUE,
                                 node_colors = NULL, link_colors = NULL,
                                 show_values_in_labels = T, label_units = "",
                                 label_year = NULL) {

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
  if ("county" %in% colnames(df_sankey)) {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(county, year, nesting(source, target), fill = list(value = 0))
  } else {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(year, nesting(source, target), fill = list(value = 0))
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
    pretty_label = pretty_label,
    animate = animate
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

# Example usage:
# Basic usage with default colors
plot_sankey_enhanced(df_sankey_wwtrade)


plot_sankey_enhanced(df_sankey_wwtrade,
                     node_colors = water_palettes$node_colors)

# With custom labels showing values
plot_sankey_enhanced(df_sankey_wwtrade,
                     animate = F,
                     # yr = 2025,
                     show_values_in_labels = TRUE,
                     label_units = "MGD",
                     node_colors = water_palettes$classic)



# With custom link colors
link_colors <- list(
  "agricultural" = "rgba(46,139,87,0.6)",  # Source-based coloring
  "agricultural -> losses" = "rgba(255,99,71,0.8)"  # Specific flow coloring
)

plot_sankey_enhanced(df_sankey_wwtrade,
                     # node_colors = node_colors,
                     link_colors = link_colors,
                     show_values_in_labels = TRUE,
                     label_units = "MGD")


# try 2 ----
plot_sankey_advanced <- function(df_sankey,
                                 title = "Metro Atlanta Flows",
                                 yr = max(YEARS_TO_ENSURE),
                                 animate = TRUE,
                                 years = YEARS_TO_ENSURE,
                                 reg = NULL,
                                 agg = TRUE,
                                 node_colors = NULL,
                                 link_colors = NULL,
                                 link_opacity = 0.2,
                                 use_gradients = TRUE,
                                 node_positions = NULL,
                                 show_values_in_labels = TRUE,
                                 color_palette = "Set3",
                                 theme = "light",
                                 node_width = 20,
                                 node_pad = 15) {

  # load required libraries
  require(plotly)
  require(dplyr)
  require(tidyr)
  require(RColorBrewer)

  # helper function to check for duplicates
  repeats <- function(df) {
    df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
  }

  # data validation
  if (!animate && !yr %in% years) stop("yr = ", yr, " not in years: ", paste(years, collapse = ", "))

  df_check <- df_sankey %>% select(source, target, year)
  if (any(duplicated(df_check))) {
    stop("data has multiple flows / repeated rows. Please check the data.")
  }

  if (any(df_sankey$value < 0)) {
    stop("data has negative values. Please check the data.")
  }

  # complete the data to set the full canvas
  if ("county" %in% colnames(df_sankey)) {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(county, year, nesting(source, target), fill = list(value = 0))
  } else {
    df_sankey <- as.data.frame(df_sankey) %>%
      complete(year, nesting(source, target), fill = list(value = 0))
  }

  # filter data based on parameters
  df_sankey <- df_sankey %>% filter(year %in% years)

  if ("county" %in% colnames(df_sankey) && !is.null(reg)) {
    df_sankey <- df_sankey %>% filter(county %in% reg)
  }

  # aggregate over counties if needed
  if (agg == TRUE && "county" %in% colnames(df_sankey)) {
    df_sankey <- df_sankey %>%
      group_by(across(-county)) %>%
      summarise(value = sum(value), .groups = "drop")
  }

  # filter to specific year if not animating
  if (animate == FALSE) {
    df_sankey <- df_sankey %>% filter(year == yr)
  }

  # generate unique node labels
  all_nodes <- unique(c(as.character(df_sankey$source), as.character(df_sankey$target)))
  n_nodes <- length(all_nodes)

  # calculate node values for labels
  if (show_values_in_labels && animate == FALSE) {
    # for static plots, use the specific year
    node_values <- df_sankey %>%
      filter(year == yr) %>%
      group_by(source) %>%
      summarise(out_flow = sum(value), .groups = "drop") %>%
      rename(node = source, flow = out_flow) %>%
      bind_rows(
        df_sankey %>%
          filter(year == yr) %>%
          group_by(target) %>%
          summarise(in_flow = sum(value), .groups = "drop") %>%
          rename(node = target, flow = in_flow)
      ) %>%
      group_by(node) %>%
      summarise(total_flow = max(flow), .groups = "drop")

    units <- if("units" %in% colnames(df_sankey)) df_sankey$units[1] else ""

    enhanced_labels <- sapply(all_nodes, function(node) {
      node_val <- node_values$total_flow[node_values$node == node]
      if(length(node_val) > 0 && !is.na(node_val) && node_val > 0) {
        paste0(node, " (", round(node_val, 1), " ", units, ")")
      } else {
        node
      }
    })
    node_display_labels <- as.character(enhanced_labels)
  } else {
    # for animated plots, use base labels to avoid confusion
    node_display_labels <- all_nodes
  }

  # set up node colors
  if (is.null(node_colors)) {
    if (n_nodes <= 12 && color_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      available_colors <- RColorBrewer::brewer.pal.info[color_palette, "maxcolors"]
      n_colors <- max(3, min(n_nodes, available_colors))
      node_colors <- RColorBrewer::brewer.pal(n_colors, color_palette)
      if (n_nodes > n_colors) {
        # extend with rainbow if needed
        additional_colors <- rainbow(n_nodes - n_colors, alpha = 0.8)
        node_colors <- c(node_colors, additional_colors)
      }
    } else {
      # for more nodes or invalid palette, use rainbow
      node_colors <- rainbow(n_nodes, alpha = 0.8)
    }
    names(node_colors) <- all_nodes
  } else {
    # ensure all nodes have colors
    missing_nodes <- setdiff(all_nodes, names(node_colors))
    if (length(missing_nodes) > 0) {
      additional_colors <- rainbow(length(missing_nodes), alpha = 0.8)
      names(additional_colors) <- missing_nodes
      node_colors <- c(node_colors, additional_colors)
    }
  }

  # set up node positions
  if (is.null(node_positions)) {
    # create smart default positions
    sources <- unique(df_sankey$source)
    targets <- unique(df_sankey$target)

    pure_sources <- setdiff(sources, targets)
    pure_sinks <- setdiff(targets, sources)
    intermediates <- intersect(sources, targets)

    node_x <- numeric(n_nodes)
    node_y <- numeric(n_nodes)

    for (i in seq_along(all_nodes)) {
      node <- all_nodes[i]
      if (node %in% pure_sources) {
        node_x[i] <- 0.1
        node_y[i] <- ifelse(length(pure_sources) == 1, 0.5,
                            (match(node, pure_sources) - 1) / (length(pure_sources) - 1) * 0.8 + 0.1)
      } else if (node %in% pure_sinks) {
        node_x[i] <- 0.9
        node_y[i] <- ifelse(length(pure_sinks) == 1, 0.5,
                            (match(node, pure_sinks) - 1) / (length(pure_sinks) - 1) * 0.8 + 0.1)
      } else {
        node_x[i] <- 0.5
        node_y[i] <- ifelse(length(intermediates) == 1, 0.5,
                            (match(node, intermediates) - 1) / (length(intermediates) - 1) * 0.8 + 0.1)
      }
    }
  } else {
    node_x <- node_positions$x[match(all_nodes, names(node_positions$x))]
    node_y <- node_positions$y[match(all_nodes, names(node_positions$y))]

    # fill missing positions
    missing_pos <- is.na(node_x) | is.na(node_y)
    if (any(missing_pos)) {
      node_x[missing_pos] <- seq(0.1, 0.9, length.out = sum(missing_pos))
      node_y[missing_pos] <- 0.5
    }
  }

  # helper function to convert hex to rgba
  hex_to_rgba <- function(hex_color, alpha = 1) {
    if (substr(hex_color, 1, 1) != "#") {
      # handle named colors
      hex_color <- rgb(t(col2rgb(hex_color)), maxColorValue = 255)
    }
    rgb_vals <- col2rgb(hex_color)
    paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
  }

  # helper function to blend colors
  blend_colors <- function(color1, color2, alpha = link_opacity) {
    rgb1 <- col2rgb(color1)
    rgb2 <- col2rgb(color2)
    blended_rgb <- (rgb1 + rgb2) / 2
    paste0("rgba(", blended_rgb[1], ",", blended_rgb[2], ",", blended_rgb[3], ",", alpha, ")")
  }

  # set up link colors
  if (is.null(link_colors)) {
    if (use_gradients) {
      # create gradient colors from source to target
      link_colors_vec <- mapply(function(src, tgt) {
        src_color <- node_colors[src]
        tgt_color <- node_colors[tgt]
        blend_colors(src_color, tgt_color, link_opacity)
      }, df_sankey$source, df_sankey$target)
    } else {
      # use source node color with opacity
      link_colors_vec <- sapply(df_sankey$source, function(src) {
        hex_to_rgba(node_colors[src], link_opacity)
      })
    }
  } else {
    link_colors_vec <- link_colors
  }

  # prepare data for plotly
  plot_data <- df_sankey %>%
    mutate(
      source_id = match(source, all_nodes) - 1,
      target_id = match(target, all_nodes) - 1,
      link_color = link_colors_vec
    )

  # create the plot
  p <- plot_ly(
    data = plot_data,
    type = "sankey",
    arrangement = "snap",
    node = list(
      pad = node_pad,
      thickness = node_width,
      label = node_display_labels,
      color = node_colors[all_nodes],
      x = node_x,
      y = node_y,
      line = list(
        color = if(theme == "dark") "white" else "black",
        width = 0.5
      )
    ),
    link = list(
      source = ~source_id,
      target = ~target_id,
      value = ~value,
      color = ~link_color
    )
  )

  # add animation if requested - correct way for sankey
  if (animate == TRUE) {
    p <- p %>%
      add_trace(frame = ~year) %>%
      animation_opts(
        2000,
        redraw = TRUE
      ) %>%
      animation_slider(
        currentvalue = list(
          prefix = "Year ",
          font = list(color = if(theme == "dark") "white" else "red", size = 14)
        )
      )
  }

  # set up layout
  title_text <- paste0(
    title,
    if(!is.null(reg)) paste0(" for ", paste(reg, collapse = ", ")),
    if(!animate) paste0(" in ", yr)
  )

  layout_config <- list(
    title = list(
      text = title_text,
      font = list(
        size = 18,
        color = if(theme == "dark") "white" else "black"
      ),
      x = 0.5,
      xanchor = "center"
    ),
    font = list(
      size = 12,
      color = if(theme == "dark") "white" else "black"
    ),
    plot_bgcolor = if(theme == "dark") "#2F2F2F" else "white",
    paper_bgcolor = if(theme == "dark") "#1F1F1F" else "white",
    margin = list(l = 10, r = 10, t = 80, b = 40)
  )

  p <- p %>% layout(layout_config)

  # add configuration options
  p <- p %>% config(
    displayModeBar = TRUE,
    displaylogo = FALSE
  )

  return(p)
}

# test with your data
plot_sankey_advanced(
  df_sankey_wwtrade,
  title = "Water Trade Flows",
  animate = F,
  node_colors = c("agricultural" = "#2E8B57", "losses" = "#CD5C5C"),
  use_gradients = T,
  node_positions = node_positions,
  link_opacity = 0.2,
  show_values_in_labels = T  # set to FALSE for animated plots
)


all_nodes <- unique(c(df_sankey_wwtrade$source, df_sankey_wwtrade$target))
# create circular arrangement
n_nodes <- length(all_nodes)
angles <- seq(0, 2*pi, length.out = n_nodes + 1)[1:n_nodes]
radius <- 0.3
center_x <- 0.5
center_y <- 0.5

# helper function to print current default positions for reference
get_default_positions <- function(df_sankey) {
  all_nodes <- unique(c(df_sankey$source, df_sankey$target))

  # this mimics the default positioning logic
  sources <- unique(df_sankey$source)
  targets <- unique(df_sankey$target)
  pure_sources <- setdiff(sources, targets)
  pure_sinks <- setdiff(targets, sources)
  intermediates <- intersect(sources, targets)

  positions <- data.frame(
    node = all_nodes,
    x = numeric(length(all_nodes)),
    y = numeric(length(all_nodes)),
    type = character(length(all_nodes))
  )

  for (i in seq_along(all_nodes)) {
    node <- all_nodes[i]
    if (node %in% pure_sources) {
      positions$x[i] <- 0.1
      positions$y[i] <- ifelse(length(pure_sources) == 1, 0.5,
                               (match(node, pure_sources) - 1) / (length(pure_sources) - 1) * 0.8 + 0.1)
      positions$type[i] <- "source"
    } else if (node %in% pure_sinks) {
      positions$x[i] <- 0.9
      positions$y[i] <- ifelse(length(pure_sinks) == 1, 0.5,
                               (match(node, pure_sinks) - 1) / (length(pure_sinks) - 1) * 0.8 + 0.1)
      positions$type[i] <- "sink"
    } else {
      positions$x[i] <- 0.5
      positions$y[i] <- ifelse(length(intermediates) == 1, 0.5,
                               (match(node, intermediates) - 1) / (length(intermediates) - 1) * 0.8 + 0.1)
      positions$type[i] <- "intermediate"
    }
  }

  return(positions)
}

# see default positions
default_pos <- get_default_positions(df_sankey_wwtrade)
print(default_pos)

# convert to position format
node_positions <- list(
  x = setNames(default_pos$x, default_pos$node),
  y = setNames(default_pos$y, default_pos$node)
)

plot_sankey_advanced(
  df_sankey_wwtrade,
  title = "Water Trade Flows",
  node_positions = node_positions,
  animate = F
)


# hard coding positions ----
node_positions <- list(
  x = c(
    # column 1 - water sources (left)
    "groundwater" = 0.05,
    "publicWatSup" = 0.05,

    # column 2 - water users (center-left)
    "residential" = 0.35,
    "commercial" = 0.35,
    "industrial" = 0.35,
    "agricultural" = 0.35,

    # column 2.5 - imports (before wastewater collection)
    "ww_imports" = 0.05,

    # column 3 - wastewater collection (center)
    "wastewater" = 0.65,

    # column 4 - final destinations (right)
    "losses" = 0.95,
    "septic" = 0.95,
    "in-county treatment" = 0.95,
    "ww_exports" = 0.95
  ),
  y = c(
    # column 1 vertical positions
    "groundwater" = 0.8,        # bottom (green)
    "publicWatSup" = 0.2,       # top (purple)

    # column 2 vertical positions (top to bottom)
    "residential" = 0.35,       # top (brown/red)
    "commercial" = 0.45,        # middle-top (orange)
    "industrial" = 0.55,        # middle-bottom (red)
    "agricultural" = 0.75,      # bottom (blue)

    # column 2.5
    "ww_imports" = 0.95,        # center level
    "wastewater" = 0.65,        # center (pink/magenta)
    "ww_exports" = 0.95,         # very bottom

    # column 4 vertical positions
    "losses" = 0.05,            # top (gray)
    "septic" = 0.25,            # middle-top (yellow)
    "in-county treatment" = 0.55# bottom (cyan/teal)

  )
)

# use with your function
plot_sankey_advanced(
  df_sankey_wwtrade,
  title = "Water Trade Flows",
  animate = F,
  node_positions = node_positions,
  # node_colors = c(
  #   "groundwater" = "#2E8B57",           # green
  #   "publicWatSup" = "#9370DB",          # purple
  #   "residential" = "#A0522D",           # brown
  #   "commercial" = "#FF8C00",            # orange
  #   "industrial" = "#DC143C",            # red
  #   "agricultural" = "#4682B4",          # blue
  #   "wastewater" = "#FF69B4",            # pink/magenta
  #   "ww_imports" = "#DDA0DD",            # light purple
  #   "losses" = "#808080",                # gray
  #   "septic" = "#FFD700",                # yellow/gold
  #   "in-county treatment" = "#20B2AA",   # teal/cyan
  #   "ww_exports" = "#FF6347"             # tomato
  # ),
  use_gradients = TRUE,
  link_opacity = 0.4,
  show_values_in_labels = T
)

