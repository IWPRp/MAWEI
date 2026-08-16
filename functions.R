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
library(jsonlite)

DATA_DIR <- "data/"
# SAVE_DIR is overridable from the shell so a run can be diverted to a scratch tree
# without editing this file, e.g.
#   MAWEI_SAVE_DIR=outputs/files_test/ Rscript R/flows_energy_water.R
# ExtraNotes: this is what makes regression testing possible - a full set of artefacts can be
# regenerated somewhere else and diffed against the published tree.
SAVE_DIR <- Sys.getenv("MAWEI_SAVE_DIR", unset = "outputs/files/")
if (!grepl("/$", SAVE_DIR)) SAVE_DIR <- paste0(SAVE_DIR, "/")
SCRIPTS_DIR <- "R/"

# Run-mode flags. Each is overridable from the shell, e.g.
#   MAWEI_SAVE_FILES=0 MAWEI_MAKE_PLOT=0 Rscript R/run_qc.R
# ExtraNotes: read from the environment rather than set here because each flows_*.R script
# re-sources this file, which would otherwise clobber a flag set by a calling script.
flag_env <- function(name, default) {
  v <- Sys.getenv(name, unset = NA)
  if (is.na(v) || !nzchar(v)) return(default)
  !(tolower(v) %in% c("0", "f", "false", "no"))
}
# SAVE_FILES <- flag_env("MAWEI_SAVE_FILES", TRUE)
# MAKE_PLOT  <- flag_env("MAWEI_MAKE_PLOT",  TRUE)
# ANALYSIS   <- flag_env("MAWEI_ANALYSIS",   TRUE)
SAVE_FILES <- FALSE
MAKE_PLOT  <- FALSE
ANALYSIS   <- FALSE

# --- QC ---
# QC_DIR holds machine-readable audit trails (mass-balance residuals, dropped-row
# ledgers, manifest checks). Written by R/qc.R helpers, never by the plot code.
QC_DIR <- Sys.getenv("MAWEI_QC_DIR", unset = "outputs/qc/")
if (!grepl("/$", QC_DIR)) QC_DIR <- paste0(QC_DIR, "/")
# Relative tolerance for node-level mass balance (0.005 = 0.5%).
BALANCE_TOL <- 0.005

# QC helpers (mass balance, dropped-row ledger, manifest + run comparison).
# Sourced here so every flows_*.R script gets them for free via functions.R.
source(paste0(SCRIPTS_DIR, "qc.R"))

# --- Sankey color scheme switch ---
# "vivid"  : high-contrast true-representative colors
# "muted"  : softer same-family tones
# FALSE    : no named colors (Spectral/RColorBrewer fallback)
COLOR_SCHEME <- "vivid"


# --- Diagram saving mode ---
# "selfcontained" : each HTML embeds all JS/CSS (~1-2 MB each, fully portable)
# "shared_libs"   : HTMLs reference one shared lib folder (~50 KB each + one ~4 MB folder)
SAVE_MODE <- "selfcontained"

# pandoc discovery ----
# htmlwidgets::saveWidget(selfcontained = TRUE) requires pandoc. RStudio bundles a copy and
# puts it on the path for its own sessions only, so probe the usual locations and export
# RSTUDIO_PANDOC, which is what rmarkdown::find_pandoc() reads.
# ExtraNotes: without this the pipeline is interactive-only, which rules out both scripted
# regeneration and any CI build of the web dashboard.
ensure_pandoc <- function(verbose = TRUE) {
  if (rmarkdown::pandoc_available()) return(invisible(TRUE))

  arch <- R.version$arch
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC", unset = NA),
    unname(Sys.which("pandoc")),
    sprintf("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/%s", arch),
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
    "/Applications/RStudio.app/Contents/Resources/app/bin/pandoc",
    "/Applications/quarto/bin/tools",
    "/opt/homebrew/bin", "/usr/local/bin", "/usr/bin"
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  for (cand in candidates) {
    dir <- if (grepl("pandoc$", cand) && !dir.exists(cand)) dirname(cand) else cand
    if (!dir.exists(dir)) next
    if (!file.exists(file.path(dir, "pandoc"))) next
    Sys.setenv(RSTUDIO_PANDOC = dir)
    if (rmarkdown::pandoc_available()) {
      if (verbose) message("  pandoc ", as.character(rmarkdown::pandoc_version()), " at ", dir)
      return(invisible(TRUE))
    }
  }

  warning("pandoc not found: falling back to SAVE_MODE = 'shared_libs'.\n",
          "  Install with `brew install pandoc` for self-contained diagrams.",
          call. = FALSE)
  invisible(FALSE)
}

HAS_PANDOC <- ensure_pandoc()
# ExtraNotes: degrade rather than abort. shared_libs still produces working HTML, just with
# an external library folder instead of one embedded blob.
if (!HAS_PANDOC && identical(SAVE_MODE, "selfcontained")) SAVE_MODE <- "shared_libs"

# create directory if it doesn't exist
if (!dir.exists(SAVE_DIR)) {
  dir.create(SAVE_DIR, recursive = TRUE)
}

# All artefacts for a domain live directly in its folder: CSV tables, HTML diagrams and
# the slim JSON the web dashboard reads. A flat layout keeps a diagram next to the data it
# was built from, and means one path pattern covers every artefact.
for (domain in c("energy", "water", "energy-water")) {
  dir.create(file.path(SAVE_DIR, domain), recursive = TRUE, showWarnings = FALSE)
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

# Water-sector energy intensities ----
# Defined here rather than inside the water pipeline so the analysis scripts use the same values
# by construction. Duplicating them at the point of use let the two drift silently.
# Source: PNNL interflow (https://pnnl.github.io/interflow/), national averages.
# ExtraNotes: surface water costs about twice groundwater to treat (coagulation, flocculation,
# sedimentation, filtration versus aquifer filtration plus disinfection), while groundwater costs
# far more to lift. Distribution dominates both, and wastewater treatment dominates everything --
# which is what makes infiltration into sewers an energy problem as much as a hydraulic one.
FRESH_SW_TREAT_ENERGY_INT <- 405   # kWh per million gallons
FRESH_GW_TREAT_ENERGY_INT <- 205   # kWh/MG
DISTRIBUTION_ENERGY_INT   <- 1040  # kWh/MG
WW_TREATMENT_ENERGY_INT   <- 2080  # kWh/MG, secondary treatment
PUMPING_HEAD_GW <- 125  # ft; GA domestic wells 50-150, public supply 150-750
PUMPING_HEAD_SW <- 25   # ft; typical surface intake

# End-use efficiency and delivery losses ----
# Useful-energy fractions follow the Lawrence Livermore (LLNL) energy-flow convention so the
# diagrams are comparable with the national and state Sankeys readers already know.
# ExtraNotes: industry is lower because much of its energy is process heat lost up the stack; the
# 0.65 applied elsewhere is a building-sector figure. Two consequences must be stated wherever
# the useful-energy split is reported: transportation is given 0.65 although a light-duty fleet
# delivers nearer 0.20-0.25 to motion, which makes the metro figure optimistic given transport is
# about half of end use; and because these are fixed coefficients, the services/rejected split
# carries no information beyond sectoral mix and cannot show efficiency improving over time.
SECTOR_EFFICIENCY <- c(
  industrial   = 0.49,
  agricultural = 0.65,
  commercial   = 0.65,
  government   = 0.65,
  residential  = 0.65,
  transport    = 0.65,
  en4water     = 0.65
)
DEFAULT_EFFICIENCY <- 0.65

# A realistic light-duty fleet efficiency, used only to report how much the LLNL transport
# coefficient overstates useful energy. Not used in the published diagrams.
TRANSPORT_EFFICIENCY_REAL <- 0.225

# Transmission and distribution losses: midpoint of EIA's 5-7% national range.
# ExtraNotes: applied to electricity leaving the grid node and treated as an additional OUTFLOW
# rather than a deduction from delivered demand, because the SEDS and utility demand figures are
# metered downstream of the losses. Generation plus imports must therefore cover metered use plus
# losses; the metro electricity node closes exactly under this convention.
TD_LOSSES_PCT <- 0.06

# Where the ANALYSIS blocks inside the flows scripts write their tables. Kept alongside the
# tables from R/analysis.R so the manuscript draws every number from one directory; the P-prefix
# marks a result that can only be computed inside the pipeline, from intermediate objects that
# no published flow table retains.
ANALYSIS_DIR <- Sys.getenv("MAWEI_ANALYSIS_DIR", "docs_analysis/analysis_outputs")
if (ANALYSIS) dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# EIA non-combustible heat-rate convention ----
# 1 kWh == 3412 Btu by definition, so 3.412 MMBtu/MWh is a 100%-efficient
# conversion. EIA reported non-combustible renewables at a *fossil-fuel-equivalent*
# heat rate (~8766 Btu/kWh) through 2021 and switched to 3412 Btu/kWh from 2022.
# Measured in this dataset (elec_fuel_consumption_mmbtu / net_generation_MWh):
#   Hydro  8766, 8843, 3412, 3412, 3412   (2020..2024)
#   Solar  8766, 8842, 3411, 3411, 3412
#   Coal   9918, 9832, 9948, 9825, 9940   <- no break, so this is convention only
# Untreated, non-combustible fuel input is 2.57x inflated in 2020-21, which dominates the
# apparent year-on-year change in hydro and solar and would manufacture ~61% of spurious
# "rejected heat" at a hydro dam under any fuel-minus-generation loss rule.
MMBTU_PER_MWH <- 3.412            # 1 MWh at 100% efficiency
NONCOMBUSTIBLE_FUELS <- c("Hydroelectric", "Solar", "Wind", "Geothermal")

# Plant aggregates whose output is consumed on site and never reaches the grid.
# ExtraNotes: an explicit set rather than a name pattern, so that adding an aggregate to the
# behind-the-meter category is always a deliberate act and cannot happen by coincidence of
# naming.
BEHIND_THE_METER_AGGREGATES <- c("On-Site Backup Generation")

# The demand sectors that receive delivered energy and split it into useful services vs
# rejected energy.
# ExtraNotes: stated outright rather than inferred from graph topology. Inferring it would
# classify any plant aggregate that happened to generate nothing in a given year as an end-use
# sector, and then invent useful "energy services" at a generator node.
END_USE_SECTORS <- c("residential", "commercial", "industrial", "government",
                     "transport", "agricultural", "en4water")

# The three large thermal plants for which the utility spreadsheet supplies gross
# generation, letting own-use (gross - net) and rejected heat (fuel - gross) be
# separated. Every other plant aggregate has only net generation available.
SOCO_THERMAL_PLANTS <- c("Bowen", "Jack McDonough", "Yates")

# Force fuel input == net generation for non-combustible fuels, in every year.
# ExtraNotes: METHOD CHOICE that affects published numbers. Harmonises the whole study period
# onto EIA's current convention so renewable fuel input is comparable across years and carries
# no conversion loss. The reported series is preserved as <fuel_col>_reported for audit.
normalize_noncombustible_heat_rate <- function(df,
                                               fuel_broad_col = "fuel_broad",
                                               fuel_col = "elec_fuel_consumption_mmbtu",
                                               gen_col = "net_generation_megawatthours",
                                               verbose = TRUE) {
  stopifnot(all(c(fuel_broad_col, fuel_col, gen_col) %in% names(df)))
  is_nc <- df[[fuel_broad_col]] %in% NONCOMBUSTIBLE_FUELS
  reported <- df[[fuel_col]]
  implied <- ifelse(is_nc, df[[gen_col]] * MMBTU_PER_MWH, reported)

  if (verbose && any(is_nc)) {
    chg <- sum(reported[is_nc], na.rm = TRUE)
    new <- sum(implied[is_nc], na.rm = TRUE)
    message(sprintf(
      "  heat-rate normalisation: %d non-combustible rows, fuel input %.4g -> %.4g MMBtu (%+.1f%%)",
      sum(is_nc), chg, new, (new - chg) / chg * 100))
  }

  df[[paste0(fuel_col, "_reported")]] <- reported
  df[[fuel_col]] <- implied
  df
}

# Sankey palettes ----
# Selected per call:  plot_sankey_enhanced(df, color_scheme = "signature")
# Or globally:        COLOR_SCHEME <- "signature"
# A named palette, a bare list of node = colour pairs, or FALSE for a Brewer ramp.
#
# Design intent common to all of them:
#   - a fuel or water source keeps its conventional physical association, so the reader
#     needs the legend once rather than continuously
#   - the grid node holds a reserved accent, being the hinge of the energy diagram
#   - terminal losses are desaturated, so waste recedes and useful output advances
#   - energy runs warm and water runs cool, which is what lets the combined
#     energy-water diagram be read at a glance
#
# Unmapped nodes fall through to resolve_node_color(), which matches facility and
# water-body naming patterns; those are too numerous to enumerate.

SANKEY_DEFAULT_COLOR <- "#C8C8C8"

`%||%` <- function(a, b) if (is.null(a)) b else a

## vivid: true-to-source and high contrast ----
SANKEY_COLORS_VIVID <- list(
  # fossil fuels
  "Coal" = "#1A1A1A", "Natural Gas" = "#E87D2F", "Petroleum" = "#8B4513",
  # renewables
  "Solar" = "#FFD700", "Biomass" = "#228B22", "Hydroelectric" = "#1E90FF",
  "Geothermal" = "#CD5C5C", "Energy Storage" = "#9370DB", "Wind" = "#63B8CF",
  "Renewables" = "#2E9E52", "Other" = "#A0A0A0",
  # on-site and distributed generation
  "Onsite / BehindTheMeter" = "#DAA520", "Onsite Solar/DER" = "#FFC125",
  "Distributed Gen." = "#B8860B", "On-Site Gen." = "#A0522D",
  "Small-scale generation" = "#B8860B",
  # power plants
  "Bowen Plant" = "#CC3333", "Jack McDonough Plant" = "#D2691E", "Yates Plant" = "#B8860B",
  # generation and grid
  "Grid Electricity" = "#FF8C00", "Utility-scale Gen." = "#E07020",
  "Electricity Imports" = "#FFB347", "Electricity Exports" = "#F0A030",
  "Electricity Imports (out-metro)" = "#FFC04D", "Exports (out-metro)" = "#E8A830",
  # energy losses and services
  "Efficiency Losses" = "#808080", "T&D Losses" = "#A9A9A9",
  "Plants Own Use" = "#696969", "Energy Losses" = "#8F8F8F",
  "Energy Services" = "#4CAF50", "Rejected Energy" = "#B0B0B0",
  # demand sectors
  "Residential Use" = "#5B9BD5", "Commercial Use" = "#BF8F00", "Industrial Use" = "#707070",
  "Agricultural Use" = "#6B8E23", "Government Use" = "#7B68AE",
  "Transportation Use" = "#C0392B",
  # basins and water sources
  "Chattahoochee Basin" = "#0047AB", "Coosa_Etowah Basin" = "#2E8BC0",
  "Flint Basin" = "#1560BD", "Ocmulgee Basin" = "#3A75C4", "Oconee Basin" = "#4682B4",
  "Tallapoosa Basin" = "#5CACEE", "Basins" = "#2E8BC0",
  "Surface Water" = "#1C86EE", "Groundwater" = "#36648B",
  "Groundwater" = "#36648B", "Public Water Supply" = "#4169E1",
  "Infiltration and Inflow" = "#5F9EA0",
  # wastewater
  "Wastewater Collection" = "#6A0DAD", "Septic Systems" = "#9370DB",
  "In-County Treatment" = "#800080", "Wastewater Treated" = "#BA55D3",
  "Transfers In (within Metro)" = "#7B2FBE",
  "Transfers Out (within Metro)" = "#9060C0",
  "Total Wastewater Treatment" = "#7B2FBE", "Wastewater Treatment" = "#7B2FBE",
  # water sinks
  "Losses" = "#778899", "Water Losses" = "#778899",
  "Discharge" = "#008B8B", "discharge" = "#008B8B", "Disposal" = "#008B8B",
  "Creek" = "#20B2AA", "River" = "#2F9E9E", "Lake" = "#1C86EE",
  "Reservoir" = "#1874CD", "Wetland" = "#3CB371", "Reuse" = "#00CED1",
  "Land" = "#6B8E23",
  # energy for water
  "Water Services Energy" = "#008080", "en4water" = "#008080",
  "Groundwater Extraction" = "#2E8B8B", "Surface Water Withdrawal" = "#207878",
  "Groundwater Treatment" = "#388E8E", "Surface Water Treatment" = "#2E7D7D",
  "Groundwater Distribution" = "#3AA0A0", "Surface Water Distribution" = "#308888"
)

## signature: vivid's logic on a wider lightness range ----
# Deeper plant tones so the mid-chain reads as structure rather than as more fuel, and a
# broader water range so the water diagram is not uniformly blue and purple: basins stay
# blue, the collection system moves to olive, treatment to green, and the sinks spread
# across teal, green and stone by receiving-body type.
SANKEY_COLORS_SIGNATURE <- modifyList(SANKEY_COLORS_VIVID, list(
  # fuels keep vivid's associations at slightly higher saturation
  "Coal" = "#2B2B2E", "Natural Gas" = "#E08A2E", "Petroleum" = "#8C5A2B",
  "Solar" = "#F2C230", "Biomass" = "#4E8B4A", "Hydroelectric" = "#2E6F94",
  "Geothermal" = "#B4553F", "Energy Storage" = "#6A5ACD",
  # plants: deep slate-violet, so generators are distinct from the fuels feeding them
  "Bowen Plant" = "#4A4351", "Yates Plant" = "#6B6076", "Jack McDonough Plant" = "#5A5165",
  "Utility-scale Gen." = "#857992", "Distributed Gen." = "#9C8FA8",
  "On-Site Gen." = "#B3A8BC", "Small-scale generation" = "#9C8FA8",
  "Grid Electricity" = "#D2691E",
  # water, deliberately diversified away from an all-blue ramp
  "Surface Water" = "#2E7CB0", "Groundwater" = "#1F5878",
  "Groundwater" = "#1F5878", "Public Water Supply" = "#1C6491",
  "Chattahoochee Basin" = "#1F6FA8", "Coosa_Etowah Basin" = "#3585B8",
  "Flint Basin" = "#4A9AC6", "Ocmulgee Basin" = "#5FAED2",
  "Oconee Basin" = "#77BFDD", "Tallapoosa Basin" = "#8FCEE7", "Basins" = "#3585B8",
  "Infiltration and Inflow" = "#7FA98C",
  "Wastewater Collection" = "#8A8A4E", "In-County Treatment" = "#41764A",
  "Wastewater Treated" = "#4F8A58",
  "Transfers In (within Metro)" = "#A39A5C",
  "Transfers Out (within Metro)" = "#B5AC6B",
  "Septic Systems" = "#A88C5F",
  "River" = "#26757E", "Creek" = "#39909A", "Lake" = "#3C7FA5",
  "Reservoir" = "#2B6B90", "Wetland" = "#4E8F5C", "Reuse" = "#3AA39A",
  "Land" = "#8A8B44", "Discharge" = "#1F6B72", "Disposal" = "#1F6B72",
  "Losses" = "#9AA3AB", "Water Losses" = "#9AA3AB"
))

SANKEY_PALETTES <- list(
  vivid     = SANKEY_COLORS_VIVID,
  signature = SANKEY_COLORS_SIGNATURE
)

# Resolve a palette argument to a named list.
sankey_palette <- function(color_scheme = NULL) {
  if (is.list(color_scheme)) return(color_scheme)
  nm <- if (!is.null(color_scheme)) color_scheme else COLOR_SCHEME
  if (identical(nm, FALSE)) return(list())
  if (is.character(nm) && length(nm) == 1 && nm %in% names(SANKEY_PALETTES)) {
    return(SANKEY_PALETTES[[nm]])
  }
  warning("unknown palette '", paste(nm, collapse = ", "), "'; using vivid. Available: ",
          paste(names(SANKEY_PALETTES), collapse = ", "), call. = FALSE)
  SANKEY_PALETTES$vivid
}

# Pattern-based fallback for nodes not named in the palette (facility names, water
# bodies, discharge points, inter-county transfers).
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
  if (grepl("WRF|WPCP|WWTP|WRC|LAS|POND", nm))           return(ww)
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


# Sankey link colouring ----
# Chosen with plot_sankey_enhanced(..., link_style = ):
#
#   "node"   every link takes the hue of the node it leaves, so a fuel or a water source
#            can be followed by eye through the whole diagram
#   "domain" one colour for energy, one for water. Loses individual identity but makes
#            the two systems unmistakable
#   "nexus"  both at once: the link keeps its source-node hue, tinted toward its class
#            colour. The default for combined energy-water diagrams
#
# The class of a flow is not simply its unit. A combined diagram contains four kinds:
# energy moving through the energy system, water moving through the water system, and the
# two coupling flows that are the actual subject of a nexus study - electricity consumed
# to move and treat water, and water withdrawn to cool generation. Giving the couplings
# their own colours makes the nexus visible rather than something to be inferred.

ENERGY_UNITS <- c("EJ", "PJ", "TJ", "GWh", "kWh", "MWh", "BBtu", "MMBtu")

SANKEY_CLASS_COLORS <- c(
  energy           = "#E07A2F",  # warm: energy moving through the energy system
  water            = "#2E7CB0",  # cool: water moving through the water system
  energy_for_water = "#7E57C2",  # violet: electricity spent moving and treating water
  water_for_energy = "#26A69A"   # teal: water withdrawn to cool generation
)

# Water-service nodes that consume energy, and generation nodes that consume water.
WATER_SERVICE_NODES <- c("Public Water Supply", "publicWatSup", "Water Services Energy",
                         "en4water", "In-County Treatment", "in-county treatment",
                         "Transfers Out (within Metro)", "ww_exports",
                         "Wastewater Treatment", "Total Wastewater Treatment")
GENERATION_NODES <- c("Bowen Plant", "Yates Plant", "Jack McDonough Plant", "Bowen", "Yates",
                      "Jack McDonough", "Grid Electricity", "electricity",
                      "Utility-scale Generation", "Distributed-scale Generation",
                      "On-Site Backup Generation", "Small-scale generation")

# Classify each flow into one of the four classes above.
sankey_link_class <- function(df) {
  is_energy <- if ("units" %in% names(df)) df$units %in% ENERGY_UNITS else rep(TRUE, nrow(df))
  case_when(
    # electricity delivered to a water service: the energy cost of the water system
    is_energy & df$target %in% WATER_SERVICE_NODES ~ "energy_for_water",
    # water delivered to generation: the water cost of the energy system
    !is_energy & df$target %in% GENERATION_NODES   ~ "water_for_energy",
    is_energy                                      ~ "energy",
    TRUE                                           ~ "water"
  )
}

# Build the per-link colour vector.
sankey_link_colors <- function(df, all_nodes, node_colors,
                               link_style = "node", link_alpha = 0.45,
                               class_weight = 0.55) {
  style <- match.arg(link_style, c("node", "domain", "nexus"))
  src_col <- node_colors[match(df$source, all_nodes)]

  if (style == "node") return(hex_to_rgba(src_col, link_alpha))

  cls <- sankey_link_class(df)
  cls_col <- unname(SANKEY_CLASS_COLORS[cls])

  if (style == "domain") {
    two <- ifelse(cls %in% c("energy", "energy_for_water"),
                  SANKEY_CLASS_COLORS[["energy"]], SANKEY_CLASS_COLORS[["water"]])
    return(hex_to_rgba(two, link_alpha))
  }
  # nexus: source-node hue pulled toward the class colour
  purrr::map2_chr(src_col, cls_col,
                  ~ blend_colors(.x, .y, weight = class_weight, alpha = link_alpha))
}


# Render a grid of style variants of one diagram, for choosing between them by eye.
#   preview_sankey_styles(energy_water, units = "auto", alt_units = ew_alt_units)
# Writes to outputs/style_preview/ and returns the index path invisibly.
preview_sankey_styles <- function(df, label = "diagram", units = "",
                                  alt_units = NULL,
                                  palettes = names(SANKEY_PALETTES),
                                  styles = c("node", "nexus", "domain"),
                                  out_dir = "outputs/style_preview",
                                  title = "Metro Atlanta flows") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  made <- list()
  for (pal in palettes) for (st in styles) {
    f <- file.path(out_dir, sprintf("%s__%s__%s.html", label, pal, st))
    message("  ", f)
    save_sankey(
      plot_sankey_enhanced(df, title = sprintf("%s  |  %s nodes  |  %s links", title, pal, st),
                           animate = TRUE, show_values_in_labels = TRUE,
                           label_units = units, alt_units = alt_units,
                           color_scheme = pal, link_style = st),
      f)
    made[[length(made) + 1L]] <- c(label = label, palette = pal, style = st,
                                   file = basename(f))
  }
  idx <- file.path(out_dir, "index.html")
  rows <- purrr::map_chr(made, ~ sprintf(
    '<a href="%s">%s &mdash; %s nodes, %s links</a>', .x[["file"]], .x[["label"]],
    .x[["palette"]], .x[["style"]]))
  existing <- if (file.exists(idx)) {
    grep("^<a href", readLines(idx, warn = FALSE), value = TRUE)
  } else character(0)
  writeLines(c(
    "<!doctype html><meta charset='utf-8'><title>MAWEI style preview</title>",
    "<style>body{font:15px/1.7 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;",
    "margin:3rem auto;max-width:46rem;color:#222}h1{font-size:1.4rem;font-weight:600}",
    "a{display:block;padding:.45rem .7rem;margin:.2rem 0;border:1px solid #e3e3e3;",
    "border-radius:6px;text-decoration:none;color:#1a4d7a}a:hover{background:#f6f9fc}",
    "p{color:#555}</style>", "<h1>MAWEI style preview</h1>",
    "<p>Node palette sets the colour of each node and, under the node link style, of the",
    "flows leaving it. Link style sets how flows are coloured: <b>node</b> inherits the",
    "source hue, <b>domain</b> splits energy from water, <b>nexus</b> keeps the source hue",
    "but tints it by flow class, and <b>class</b> uses flat class colours. The nexus and",
    "class styles distinguish four classes, separating the two coupling flows -",
    "energy spent on water, and water spent on energy - from flows internal to either",
    "system.</p>",
    unique(c(existing, rows))), idx)
  message("\nOpen: ", normalizePath(idx))
  invisible(idx)
}


repeats <- function(df) {
  df %>% group_by(across(everything())) %>%
    filter(n() > 1) %>% ungroup()
}

# validate flow data frames for completeness and correctness
# ExtraNotes: `strict_years` asserts that no year OUTSIDE YEARS_TO_ENSURE is present, not just
# that the wanted years exist. The source tables span 2006-2065 (wastewater connection records
# through management-plan projections), and because the diagrams filter on year, out-of-period
# rows are invisible in the artefacts while still bloating the published CSVs.
validate_flows <- function(df, label = "flows", strict_years = FALSE) {
  required <- c("source", "target", "year", "value")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop(label, ": missing columns: ", paste(missing, collapse = ", "))

  for (col in required) {
    n_na <- sum(is.na(df[[col]]))
    if (n_na > 0) stop(label, ": ", n_na, " NA values in '", col, "'")
  }

  # county is not in `required` because the metro frames legitimately lack it, but
  # when present an NA county silently corrupts every county-level cut.
  if ("county" %in% names(df)) {
    n_na <- sum(is.na(df$county))
    if (n_na > 0) stop(label, ": ", n_na, " NA values in 'county'")
    bad <- setdiff(unique(df$county), counties)
    if (length(bad) > 0) {
      stop(label, ": non-canonical county label(s): ",
           paste0("'", bad, "'", collapse = ", "),
           "\n  expected one of: ", paste(counties, collapse = ", "))
    }
  }

  group_cols <- intersect(c("county", "year", "source", "target", "units"), names(df))
  dupes <- df %>% group_by(across(all_of(group_cols))) %>% filter(n() > 1) %>% ungroup()
  if (nrow(dupes) > 0) stop(label, ": ", nrow(dupes), " duplicate rows found")

  if (any(df$value < 0)) stop(label, ": ", sum(df$value < 0), " negative values")

  years_present <- sort(unique(df$year))
  years_missing <- setdiff(YEARS_TO_ENSURE, years_present)
  if (length(years_missing) > 0) stop(label, ": missing years: ", paste(years_missing, collapse = ", "))

  if (strict_years) {
    extra <- setdiff(years_present, YEARS_TO_ENSURE)
    if (length(extra) > 0) {
      stop(label, ": ", length(extra), " year(s) outside YEARS_TO_ENSURE: ",
           paste(range(extra), collapse = "-"),
           "\n  -> clamp with filter(year %in% YEARS_TO_ENSURE) before publishing")
    }
  }

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

# Round a flow value for display, keeping small values informative.
# ExtraNotes: a fixed number of decimals renders every minor node as "0", which is
# indistinguishable from a node that genuinely carries nothing. Scaling the precision to
# the magnitude keeps small contributors readable without adding noise to large ones.
sankey_fmt <- function(v) {
  if (is.na(v)) return("")
  a <- abs(v)
  if (a >= 100) formatC(v, format = "f", digits = 0, big.mark = ",")
  else if (a >= 10)  formatC(v, format = "f", digits = 1)
  else if (a >= 1)   formatC(v, format = "f", digits = 2)
  else if (a > 0)    formatC(signif(v, 2), format = "g")
  else "0"
}

format_node_label <- function(nd, nd_totals, alt_units = NULL, prefix = "") {
  if (nrow(nd_totals) == 0) return(nd)
  parts <- nd_totals %>% arrange(units) %>%
    purrr::pmap_chr(function(node, units, total, ...) {
      lbl <- paste0(prefix, sankey_fmt(total), " ", units)
      if (!is.null(alt_units) && units == alt_units$from_unit && nd %in% alt_units$nodes) {
        alt_val <- total * alt_units$factor
        lbl <- paste0(lbl, " (", sankey_fmt(alt_val), " ", alt_units$label, ")")
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

save_metro_sankey <- function(df, domain_dir, stem, label_units,
                              alt_units = NULL, color_scheme = NULL,
                              link_style = "node", ...) {
  p <- plot_sankey_enhanced(df, animate = TRUE, show_values_in_labels = TRUE,
                            label_units = label_units, alt_units = alt_units,
                            color_scheme = color_scheme, link_style = link_style, ...)
  base <- file.path(SAVE_DIR, domain_dir, stem)
  save_sankey(p, paste0(base, ".html"))
  save_sankey_json(df, paste0(base, ".json"), units = label_units,
                   color_scheme = color_scheme, link_style = link_style)
  invisible(base)
}

save_county_sankeys <- function(df, domain_dir, prefix, suffix, prep_fn, label_units,
                                alt_units = NULL, color_scheme = NULL,
                                link_style = "node", ...) {
  for (cty in sort(counties)) {
    message("  ", cty, " ", suffix)
    prepped <- prep_fn(df)
    p <- plot_sankey_enhanced(prepped, reg = cty, animate = TRUE,
                              show_values_in_labels = TRUE, label_units = label_units,
                              alt_units = alt_units, color_scheme = color_scheme,
                              link_style = link_style, ...)
    stem <- file.path(SAVE_DIR, domain_dir, paste0(prefix, "_county_", cty, "_", suffix))
    save_sankey(p, paste0(stem, ".html"))
    save_sankey_json(prepped, paste0(stem, ".json"), reg = cty, units = label_units,
                     color_scheme = color_scheme, link_style = link_style)
  }
}


# Sankey JSON export ----
# The web dashboard renders its own Sankeys rather than embedding ours, so it needs the
# geometry and the values, not a rendered widget. A self-contained plotly HTML is 1-4 MB
# because it carries the whole library; this is 20-80 KB and is the same numbers.
#
# Node coordinates and colours are computed by the same functions the R diagrams use, so a
# diagram drawn in the browser is positioned and coloured identically to the one saved here.
# That is the point of exporting geometry rather than letting the browser solve its own
# layout: the two views cannot drift apart.
save_sankey_json <- function(df, filepath, reg = NULL, units = "",
                             years = YEARS_TO_ENSURE, color_scheme = NULL,
                             link_style = "node", pretty_label = TRUE) {

  d <- if (pretty_label) pretty_labels(df) else df
  if ("county" %in% names(d) && !is.null(reg)) d <- d %>% filter(county %in% reg)
  d <- d %>% filter(year %in% years)
  grp <- intersect(c("year", "source", "target", "units"), names(d))
  d <- d %>% group_by(across(all_of(grp))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    filter(value > 0)
  if (nrow(d) == 0) return(invisible(NULL))

  all_nodes <- unique(c(d$source, d$target))
  pal <- sankey_palette(color_scheme)
  node_colors <- purrr::map_chr(all_nodes, ~ resolve_node_color(.x, pal))
  pos <- sankey_node_positions(all_nodes, d)
  link_colors <- sankey_link_colors(d, all_nodes, node_colors, link_style = link_style)

  out <- list(
    meta = list(
      domain = if (is.null(reg)) "metro" else "county",
      scope = reg %||% "metro",
      units = if (nzchar(units)) units else unique(d$units),
      years = sort(unique(d$year)),
      link_style = link_style,
      generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ),
    nodes = purrr::imap(all_nodes, function(nd, i) list(
      id = i - 1L, label = nd, x = pos$x[i], y = pos$y[i],
      layer = pos$layer[i], color = node_colors[i]
    )),
    links = purrr::pmap(list(d$source, d$target, d$year, d$value,
                             d$units %||% rep("", nrow(d)), link_colors),
                        function(s, t, y, v, u, cl) list(
                          s = match(s, all_nodes) - 1L, t = match(t, all_nodes) - 1L,
                          year = y, value = round(v, 6), units = u, color = cl
                        ))
  )
  dir.create(dirname(filepath), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(out, filepath, auto_unbox = TRUE, digits = 8, null = "null")
  invisible(filepath)
}


# Artefact manifest ----
# One index of everything that was written, so a consumer never has to reconstruct paths
# from a naming convention. The dashboards previously rebuilt the file list from hardcoded
# county and domain vectors, which silently produced dead links whenever an artefact was
# renamed, missing or added.
write_manifest <- function(root = SAVE_DIR, path = file.path(SAVE_DIR, "manifest.json")) {
  files <- list.files(root, pattern = "\\.(html|csv|json)$", recursive = TRUE)
  files <- files[basename(files) != "manifest.json"]

  info <- tibble(path = files) %>%
    mutate(
      full = file.path(root, path),
      domain = dirname(path),
      base = basename(path),
      ext = tools::file_ext(base),
      kind = case_when(ext == "csv" ~ "table", ext == "json" ~ "data", TRUE ~ "diagram"),
      # county name is the token between "_county_" and the trailing suffix
      county = str_match(base, "_county_([A-Za-z]+)_")[, 2],
      scope = if_else(is.na(county), "metro", "county"),
      # ordering key: metro artefacts first, then counties alphabetically
      order_key = if_else(is.na(county), paste0("0_", base), paste0("1_", county, "_", base)),
      bytes = file.size(full),
      label = base %>% str_remove("\\.(html|csv|json)$") %>%
        str_remove("^[0-9]+_") %>% str_replace_all("_", " ")
    ) %>%
    filter(domain %in% c("energy", "water", "energy-water")) %>%
    arrange(domain, order_key) %>%
    select(path, domain, scope, county, kind, label, bytes)

  out <- list(
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    years = YEARS_TO_ENSURE,
    counties = sort(counties),
    domains = c("energy", "water", "energy-water"),
    files = info
  )
  jsonlite::write_json(out, path, auto_unbox = TRUE, pretty = TRUE)

  # A `const` copy as well: the local dashboard runs from file://, where fetch() of a
  # sibling JSON is blocked as a cross-origin request, but a <script> tag is not.
  writeLines(paste0("const MAWEI_MANIFEST = ",
                    jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE), ";"),
             file.path(dirname(path), "manifest.js"))

  message("  manifest: ", nrow(info), " artefacts -> ", path)
  invisible(info)
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
# ExtraNotes: filename case matters. macOS is case-insensitive and will resolve a wrong case
# silently, but Linux and CI will not.
# The national SEDS release is refreshed periodically and its filename carries a
# vintage, so it is discovered rather than hardcoded: the newest matching file wins.
# ExtraNotes: gz is preferred over plain csv when both are present, since only the
# compressed copy is tracked.
EIA_SEDS_FILE <- {
  cands <- list.files(DATA_DIR, pattern = "^eia_seds_Complete_seds_.*\\.csv(\\.gz)?$")
  if (length(cands) == 0) {
    stop("No eia_seds_Complete_seds_*.csv[.gz] found in ", DATA_DIR,
         ". Download the complete state dataset from EIA SEDS.")
  }
  gz <- grep("\\.gz$", cands, value = TRUE)
  pick <- sort(if (length(gz) > 0) gz else cands, decreasing = TRUE)[1]
  message("  EIA SEDS source: ", pick)
  pick
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
        !!sym(col_name) %in% c("Water at a Conventional Hydroelectric Turbine", "Water for Conventional Hydroelectric") ~ "Hydroelectric",
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
  out <- df %>%
    mutate(
      plant_aggregated = case_when(
        !!sym(col_name) %in% c("Bowen") ~ "Bowen Plant", # large coal plant
        !!sym(col_name) %in% c("Yates") ~ "Yates Plant", # large natural gas / legacy coal plant
        !!sym(col_name) %in% c("Jack McDonough") ~ "Jack McDonough", # large combined-cycle gas plant

        # conventional hydro generation grouped with small renewables
        # ExtraNotes: Milstead (Rockdale) is conventional hydro and is Rockdale's only
        # generator, so grouping it here keeps the county's generation visible.
        !!sym(col_name) %in% c("Morgan Falls", "Buford", "Allatoona", "Milstead"
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
        # ExtraNotes: Hewlett Packard Enterprise is a reciprocating-engine genset on natural
        # gas and distillate, i.e. data-centre standby power, so it belongs with the other
        # building-scale units.
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
                               "Bartow Davidson",
                               "Hewlett Packard Enterprise"
        # ) ~ "Commercial & Institutional Sites",
        ) ~ "On-Site Backup Generation",

        TRUE ~ "Other"
      )
    )

  # ExtraNotes: warn loudly on the catch-all. A plant landing in "Other" is either newly
  # appearing in EIA-860 or renamed, and either way it should be classified deliberately - an
  # unclassified plant distorts both the fuel mix and the generation total it is lumped into.
  unmapped <- out %>% filter(plant_aggregated == "Other") %>%
    pull(!!sym(col_name)) %>% unique()
  if (length(unmapped) > 0) {
    warning("remap_plants_agg(): ", length(unmapped),
            " plant(s) fell through to 'Other': ",
            paste(unmapped, collapse = ", "),
            "\n  -> add them to an explicit group in functions.R::remap_plants_agg()",
            call. = FALSE)
  }
  out
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
        source == "surfaceWater" ~ "Surface Water",
        source == "publicWatSup" ~ "Public Water Supply",
        source == "groundwater" ~ "Groundwater",
        source == "groundwaterAllBasins" ~ "Groundwater",
        # source == "subsurface" ~ "Shallow Subsurface Water",
        source == "subsurface" ~ "Infiltration and Inflow",
        source == "agricultural" ~ "Agricultural Use",
        source == "industrial" ~ "Industrial Use",
        source == "residential" ~ "Residential Use",
        source == "commercial" ~ "Commercial Use",
        source == "losses" ~ "Losses",
        source == "wastewater" ~ "Wastewater Collection",
        source == "ww_imports" ~ "Transfers In (within Metro)",
        source == "ww_exports" ~ "Transfers Out (within Metro)",
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
        source == "Electricity" ~ "Grid Electricity",
        source == "electricity" ~ "Grid Electricity",
        source == "Utility-scale Generation" ~ "Utility-scale Gen.",
        source == "Distributed-scale Generation" ~ "Distributed Gen.",
        source == "On-Site Backup Generation" ~ "On-Site Gen.",
        source == "elec_import" ~ "Electricity Imports",
        source == "elec_export" ~ "Electricity Exports",
        source == "out_metro_elec_import" ~ "Electricity Imports (out-metro)",
        source == "out_metro_elec_export" ~ "Exports (out-metro)",
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
        target == "groundwaterAllBasins" ~ "Groundwater",
        target == "agricultural" ~ "Agricultural Use",
        target == "industrial" ~ "Industrial Use",
        target == "residential" ~ "Residential Use",
        target == "commercial" ~ "Commercial Use",
        target == "losses" ~ "Losses",
        target == "wastewater" ~ "Wastewater Collection",
        target == "ww_imports" ~ "Transfers In (within Metro)",
        target == "ww_exports" ~ "Transfers Out (within Metro)",
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
        target == "Electricity" ~ "Grid Electricity",
        target == "electricity" ~ "Grid Electricity",
        target == "Utility-scale Generation" ~ "Utility-scale Gen.",
        target == "Distributed-scale Generation" ~ "Distributed Gen.",
        target == "On-Site Backup Generation" ~ "On-Site Gen.",
        target == "elec_import" ~ "Electricity Imports",
        target == "elec_export" ~ "Electricity Exports",
        target == "out_metro_elec_import" ~ "Electricity Imports (out-metro)",
        target == "out_metro_elec_export" ~ "Exports (out-metro)",
        target == "government" ~ "Government Use",
        target == "transport" ~ "Transportation Use",
        target == "elec_own_use" ~ "Plants Own Use",
        target == "efficiency_losses" ~ "Efficiency Losses",
        target == "td_losses" ~ "T&D Losses",


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



# sankey pro helpers ----

# Sankey node layout ----
# Plotly will solve a Sankey layout by itself, but it re-solves on every animation
# frame, so a node can move -- or swap places with a neighbour -- as the year
# changes. Supplying explicit node coordinates makes the geometry a property of the
# diagram rather than of the frame, which is what keeps the picture stable while the
# slider runs.
#
# The layout is declared as ordered layers. `x` fixes the column, and position within
# the layer list fixes the vertical order; `y` is then derived by stacking each layer
# in that order. Nodes not named here are appended to the layer implied by their
# topology and ranked by mean throughput, so a new node degrades gracefully instead
# of breaking the diagram.
#
# Conventions that the ordering encodes (plotly y = 0 is the TOP of the canvas):
#   - every loss and waste sink sits at the top of the right-hand column, so that
#     "what was wasted" reads as one block in both domains
#   - the named thermal plants sit above the aggregate generation categories
#   - fuels are ordered largest first, so major fuels sit above minor ones
#   - water sources read top-left down: surface water, groundwater, infiltration,
#     then inter-county inflows at the bottom
#   - septic sits directly under losses, being the other non-sewered terminus

# Column positions. Plotly draws a node's label to its right, except in the last
# column where it draws to the left, so the gap between the final two columns has to
# carry two labels and is deliberately the widest.
SANKEY_LAYER_X <- c(source = 0.02, basin = 0.15, plant = 0.24, supply = 0.32,
                    grid = 0.42, demand_w = 0.48, demand = 0.6, collect = 0.7, treat = 0.82,
                    sink = 0.98)

# Ordered node lists per layer. Order == vertical order, top first.
SANKEY_LAYOUT <- list(
  energy = list(
    source = c("Coal", "Natural Gas", "Petroleum", "Biomass", "Hydroelectric",
               "Solar", "Wind", "Geothermal", "Energy Storage", "Renewables", "Other",
               "Onsite / BehindTheMeter", "Onsite Solar/DER",
               "Electricity Imports", "Electricity Imports (out-metro)"),
    plant  = c("Bowen Plant", "Yates Plant", "Jack McDonough Plant",
               "Utility-scale Gen.", "Distributed Gen.",
               "On-Site Gen.", "Small-scale generation"),
    grid   = c("Grid Electricity"),
    demand = c("Residential Use", "Commercial Use", "Industrial Use",
               "Government Use", "Transportation Use", "Agricultural Use",
               "Water Services Energy"),
    sink   = c("Plants Own Use", "Efficiency Losses", "T&D Losses",
               "Energy Losses", "Rejected Energy", "Energy Services",
               "Electricity Exports", "Exports (out-metro)")
  ),
  water = list(
    source  = c("Surface Water", "Groundwater", "Groundwater",
                "Infiltration and Inflow",
                "Transfers In (within Metro)"),
    basin   = c("Chattahoochee Basin", "Coosa_Etowah Basin", "Ocmulgee Basin",
                "Flint Basin", "Oconee Basin", "Tallapoosa Basin", "Basins"),
    supply  = c("Public Water Supply"),
    demand_w  = c("Residential Use", "Commercial Use", "Industrial Use", "Agricultural Use",
                "Bowen Plant", "Yates Plant", "Jack McDonough Plant"),
    collect = c("Wastewater Collection"),
    treat   = c("In-County Treatment"),
    sink    = c("Losses", "Water Losses", "Septic Systems",
                "Transfers Out (within Metro)",
                "Discharge", "Disposal", "River", "Creek", "Lake", "Reservoir",
                "Wetland", "Land", "Reuse")
  )
)

# Which layer a node belongs to. Domains are searched in the order given, so a node
# named in more than one domain takes the layer of whichever domain dominates the
# diagram being drawn.
# ExtraNotes: the thermal plants appear in both domains - they consume fuel and produce
# electricity, and they also withdraw cooling water and discharge it. Which column they
# belong in therefore depends on the diagram: mid-chain among the generators in an energy
# diagram, alongside the other water users in a water diagram.
sankey_layer_of <- function(node, layouts) {
  for (lay in layouts) {
    for (ly in names(lay)) if (node %in% lay[[ly]]) return(ly)
  }
  NA_character_
}

# Domains present in a node set, most strongly represented first.
sankey_detect_domains <- function(all_nodes) {
  hits <- purrr::map_int(SANKEY_LAYOUT, function(lay) {
    sum(all_nodes %in% unlist(lay, use.names = FALSE))
  })
  hits <- hits[hits > 0]
  names(sort(hits, decreasing = TRUE))
}

# Nodes whose vertical position is pinned rather than derived from the stack.
# ExtraNotes: the derived stack spaces a layer by throughput, which is right for most nodes
# but wrong for the handful that carry the argument of the diagram: the reader should find
# the system's entry point, its distribution hub and its losses in the same place in every
# figure. y runs 0 at the TOP to 1 at the bottom. Edit these freely; anything not named here
# is stacked automatically, and a pinned node is simply lifted out of that stack.
SANKEY_NODE_Y <- c(
  # water: sources and hub top-aligned, collection deliberately below the demand sectors
  "Surface Water"                          = 0.06,
  "Chattahoochee Basin"                     = 0.06,
  "Coosa_Etowah Basin"                     = 0.26,
  "Ocmulgee Basin"                          = 0.46,
  "Flint Basin"                             = 0.66,
  "Oconee Basin"                            = 0.86,
  "Chattahoochee River"                            = 0.06,
  "Groundwater"                            = 0.70,
  "Residential Use"                            = 0.30,
  "Infiltration and Inflow"                             = 0.85,
  "Transfers In (within Metro)"  = 0.98,
  "Public Water Supply"                                 = 0.10,
  "Wastewater Collection"                               = 0.65,
  "In-County Treatment"                                 = 0.8,
  # right-hand column: losses at the top, septic directly beneath
  "Losses"                                              = 0.04,
  "Water Losses"                                        = 0.04,
  "Septic Systems"                                      = 0.16,
  "Discharge"                                           = 0.26,
  "Transfers Out (within Metro)"                                       = 0.34,
  # energy: grid hub high, loss stack above useful output
  "Electricity Imports (out-metro)"            = 0.06, # for metro
  "Electricity Imports"                        = 0.06, # for counties
  "Grid Electricity"                           = 0.30,
  "Plants Own Use"                                      = 0.3,
  "Efficiency Losses"                                   = 0.42,
  "T&D Losses"                                          = 0.52,
  "Energy Losses"                                       = 0.12,
  "Rejected Energy"                                     = 0.14
)

# Compute node x/y from the layout spec.
# ExtraNotes: y is derived by stacking each layer in declared order, weighted by mean
# throughput across ALL years and counties in the frame. Weighting by the mean rather than
# by the current frame is deliberate: it is what makes the geometry identical in every
# animation frame. Nodes absent from the spec are placed by topology (pure source /
# pure sink / intermediate) and ranked by throughput.
sankey_node_positions <- function(all_nodes, df, pad = 0.04, node_y = NULL) {

  domains <- sankey_detect_domains(all_nodes)
  if (length(domains) == 0) return(NULL)

  # layer lists in domain-priority order, for resolving a node named in both domains
  layouts <- SANKEY_LAYOUT[domains]
  # merged list, used only to look up the declared order within a layer
  layout <- list()
  for (d in domains) {
    for (ly in names(SANKEY_LAYOUT[[d]])) {
      layout[[ly]] <- unique(c(layout[[ly]], SANKEY_LAYOUT[[d]][[ly]]))
    }
  }

  throughput <- bind_rows(
    df %>% group_by(node = source) %>% summarise(v = sum(value, na.rm = TRUE), .groups = "drop"),
    df %>% group_by(node = target) %>% summarise(v = sum(value, na.rm = TRUE), .groups = "drop")
  ) %>% group_by(node) %>% summarise(v = max(v, na.rm = TRUE), .groups = "drop")

  srcs <- unique(df$source); tgts <- unique(df$target)

  assigned <- tibble(node = all_nodes) %>%
    mutate(layer = purrr::map_chr(node, sankey_layer_of, layouts = layouts),
           layer = case_when(
             !is.na(layer) ~ layer,
             # unnamed nodes: infer from topology. Facility and water-body names in the
             # county diagrams land here.
             node %in% srcs & !node %in% tgts ~ "source",
             node %in% tgts & !node %in% srcs ~ "sink",
             TRUE ~ "treat"),
           order_in_layer = purrr::map2_int(node, layer, function(nd, ly) {
             idx <- if (is.null(layout[[ly]])) NA_integer_ else match(nd, layout[[ly]])
             if (is.na(idx)) NA_integer_ else as.integer(idx)
           })) %>%
    left_join(throughput, by = "node") %>%
    mutate(v = replace_na(v, 0))

  # declared nodes keep their declared order; the rest follow, largest first
  assigned <- assigned %>%
    group_by(layer) %>%
    arrange(is.na(order_in_layer), order_in_layer, desc(v), .by_group = TRUE) %>%
    mutate(slot = row_number()) %>%
    ungroup()

  # Resolve nodes named in more than one domain.
  # ExtraNotes: the thermal plants are the case in point. They belong to both domains - they
  # burn fuel to make electricity, and they withdraw cooling water and discharge it. Which
  # column they occupy depends on what the diagram is about: in a water diagram they are
  # terminal users fed by the basins, so they sit with the other demand sectors; wherever the
  # grid node is present they are mid-chain converters and must sit upstream of it, or their
  # electricity output would run backwards. Presence of the grid node is therefore the test.
  multi <- purrr::keep(all_nodes, function(nd) {
    sum(purrr::map_lgl(SANKEY_LAYOUT, ~ nd %in% unlist(.x, use.names = FALSE))) > 1
  })
  if (length(multi) > 0 && length(domains) > 1) {
    has_grid <- any(all_nodes %in% SANKEY_LAYOUT$energy$grid)
    preferred_domain <- if (has_grid) "energy" else domains[1]
    lmap <- setNames(assigned$layer, assigned$node)
    for (nd in multi) {
      ly <- sankey_layer_of(nd, SANKEY_LAYOUT[preferred_domain])
      if (!is.na(ly)) lmap[[nd]] <- ly
    }
    assigned <- assigned %>%
      mutate(layer = unname(lmap[match(node, names(lmap))])) %>%
      group_by(layer) %>%
      arrange(is.na(order_in_layer), order_in_layer, desc(v), .by_group = TRUE) %>%
      mutate(slot = row_number()) %>%
      ungroup()
  }

  # stack within each layer, weighted by throughput so big nodes get proportionate room
  # ExtraNotes: the weight floor matters more than it looks. Metro flows span four orders of
  # magnitude - petroleum against a 2.6 MW solar farm - so a purely proportional stack gives
  # the smallest nodes no vertical separation and their labels collide into an illegible pile.
  # The floor buys each node enough room to be read at the cost of slightly compressing the
  # largest ones, which are legible regardless.
  min_w <- min(0.10, 0.9 / max(1, max(table(assigned$layer))))
  assigned <- assigned %>%
    group_by(layer) %>%
    mutate(w = if (sum(v) > 0) v / sum(v) else rep(1 / n(), n()),
           w = pmax(w, min_w), w = w / sum(w),
           cum = cumsum(w) - w / 2,
           y = pad + cum * (1 - 2 * pad)) %>%
    ungroup() %>%
    mutate(x = unname(SANKEY_LAYER_X[layer]),
           x = if_else(is.na(x), 0.5, x))

  # Apply the pinned positions last, so an explicit y always wins over the derived stack.
  pins <- c(SANKEY_NODE_Y, node_y)[!duplicated(names(c(SANKEY_NODE_Y, node_y)), fromLast = TRUE)]
  if (length(pins) > 0) {
    hit <- match(assigned$node, names(pins))
    assigned$y <- if_else(is.na(hit), assigned$y, unname(pins[hit]))
  }

  list(x = assigned$x[match(all_nodes, assigned$node)],
       y = assigned$y[match(all_nodes, assigned$node)],
       layer = assigned$layer[match(all_nodes, assigned$node)])
}


hex_to_rgba <- function(hex_color, alpha = 1) {
  if (length(hex_color) == 0) return(character(0))
  if (length(hex_color) > 1) return(vapply(hex_color, hex_to_rgba, character(1),
                                           alpha = alpha, USE.NAMES = FALSE))
  if (is.na(hex_color) || is.null(hex_color)) return(paste0("rgba(128,128,128,", alpha, ")"))
  # already an rgba() string, e.g. a hidden placeholder node - leave it alone
  if (grepl("^rgba?\\(", hex_color)) return(hex_color)
  if (substr(hex_color, 1, 1) != "#") {
    hex_color <- tryCatch(rgb(t(col2rgb(hex_color)), maxColorValue = 255),
                          error = function(e) "#808080")
  }
  rgb_vals <- col2rgb(hex_color)
  paste0("rgba(", rgb_vals[1], ",", rgb_vals[2], ",", rgb_vals[3], ",", alpha, ")")
}

blend_colors <- function(color1, color2, weight = 0.5, alpha = 1) {
  if (is.na(color1) || grepl("^rgba?\\(", color1)) color1 <- "#808080"
  if (is.na(color2)) color2 <- "#808080"
  rgb1 <- tryCatch(col2rgb(color1), error = function(e) col2rgb("#808080"))
  rgb2 <- tryCatch(col2rgb(color2), error = function(e) col2rgb("#808080"))
  b <- round(rgb1 * (1 - weight) + rgb2 * weight)
  paste0("rgba(", b[1], ",", b[2], ",", b[3], ",", alpha, ")")
}

auto_detect_units <- function(df) {
  if (!"units" %in% names(df)) return("")
  u <- unique(df$units)
  u <- u[!is.na(u)]
  if (length(u) == 1) u else ""
}

# preferred positions for known nodes (pretty_labels names)
# Preprocessing function used by plot_sankey_enhanced
prepare_sankey_enhanced <- function(df_sankey, node_colors = NULL, link_colors = NULL,
                                show_values_in_labels = FALSE, value_year = NULL,
                                pretty_label = TRUE,
                                units = "", alt_units = NULL, animate = FALSE,
                                color_scheme = NULL,
                                link_style = "node", link_alpha = 0.45,
                                class_weight = 0.55, use_layout = TRUE,
                                node_y = NULL) {

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
            paste0(node, "\n", sankey_fmt(total_val[1]), " ", units)
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
            paste0(node, "\n(avg: ", sankey_fmt(total_val[1]), " ", units, ")")
          } else {
            node
          }
        })
    }

  } else {
    node_labels <- all_nodes
  }

  # handle node colors
  # Priority: explicit node_colors > color_scheme > COLOR_SCHEME global > Brewer ramp
  use_named <- if (!is.null(color_scheme)) !identical(color_scheme, FALSE)
               else !identical(COLOR_SCHEME, FALSE)

  if (is.null(node_colors) && use_named) {
    palette <- sankey_palette(color_scheme)
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
  if (is.null(link_colors)) {
    link_colors <- sankey_link_colors(df_sankey, all_nodes, node_colors,
                                      link_style = link_style, link_alpha = link_alpha,
                                      class_weight = class_weight)
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

  # Nodes with no throughput anywhere in this frame are placeholders created by the
  # canvas-completion step; they keep their array slot so link indices stay valid, but
  # they are made invisible and unlabelled.
  # ExtraNotes: the array slot must be kept. Dropping the node would renumber every
  # link index and, in an animation, change the numbering between frames - which is the
  # other way a Sankey can appear to shuffle itself. Hiding rather than removing gives a
  # county diagram that shows only the flows that county actually has, without
  # destabilising the frames.
  throughput <- bind_rows(
    df_sankey %>% group_by(node = source) %>% summarise(v = sum(value, na.rm = TRUE), .groups = "drop"),
    df_sankey %>% group_by(node = target) %>% summarise(v = sum(value, na.rm = TRUE), .groups = "drop")
  ) %>% group_by(node) %>% summarise(v = max(v, na.rm = TRUE), .groups = "drop")
  is_phantom <- !(all_nodes %in% throughput$node[throughput$v > 0])
  if (any(is_phantom)) {
    node_labels[is_phantom] <- ""
    node_colors[is_phantom] <- "rgba(0,0,0,0)"
  }

  positions <- if (use_layout) sankey_node_positions(all_nodes, df_sankey, node_y = node_y) else NULL

  return(list(
    df_sankey = df_sankey,
    node_labels = node_labels,
    node_names = all_nodes,
    node_colors = node_colors,
    link_colors = link_colors,
    positions = positions,
    is_phantom = is_phantom
  ))
}

plot_sankey_enhanced <- function(df_sankey, title = "Metro Atlanta Flows", yr = max(YEARS_TO_ENSURE),
                                 animate = TRUE, years = YEARS_TO_ENSURE,
                                 reg = NULL, agg = TRUE, pretty_label = TRUE,
                                 node_colors = NULL, link_colors = NULL,
                                 show_values_in_labels = T, label_units = "",
                                 label_year = NULL, alt_units = NULL,
                                 # --- colour ---
                                 color_scheme = NULL, link_style = "node",
                                 link_alpha = 0.45, class_weight = 0.55,
                                 # --- geometry ---
                                 use_layout = TRUE, node_y = NULL,
                                 arrangement = "snap", drop_empty = TRUE,
                                 node_pad = 12, node_thickness = 16,
                                 node_line_color = NULL, node_line_width = 0) {
  # ExtraNotes: every argument past `df_sankey` has a default that produces a finished
  # diagram, so the one-line call plot_sankey_enhanced(df, reg = "Fulton") stays valid.
  # ExtraNotes: no node stroke by default. A stroke is drawn inside the node's own height,
  # so on a node carrying a thousandth of the system total - which is normal here, given the
  # range of magnitudes - it covers the fill entirely and the node reads as blank.

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

  # Drop flows that are zero in every year being drawn.
  # ExtraNotes: the canvas-completion step above deliberately gives every county the full
  # metro node set so that the node array, and therefore the link indices, are identical in
  # every animation frame. For a single county most of those flows never occur, so the
  # diagram would carry several hundred empty nodes. Judging emptiness on the sum ACROSS
  # the drawn years preserves the frame-to-frame stability that completion provides, while
  # removing what is genuinely absent: a flow present in only one year is kept, so its node
  # still exists in the frames where it is zero.
  if (drop_empty) {
    keep <- df_sankey %>%
      group_by(source, target) %>%
      mutate(.total = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      pull(.total) > 0
    df_sankey <- df_sankey[keep, , drop = FALSE]
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
    link_style = link_style,
    link_alpha = link_alpha,
    class_weight = class_weight,
    use_layout = use_layout,
    node_y = node_y
  )

  # Node geometry. Explicit coordinates plus arrangement = "snap" give plotly a fixed
  # column and a fixed vertical order, while still letting it resolve any overlap when a
  # node grows in a particular year. Without coordinates plotly solves the layout afresh
  # for every frame, which is what makes nodes drift or swap places during playback.
  node_spec <- list(
    label = sankey_data$node_labels,
    color = sankey_data$node_colors,
    pad = node_pad,
    thickness = node_thickness
  )
  if (node_line_width > 0) {
    node_spec$line <- list(color = node_line_color %||% "#FFFFFF", width = node_line_width)
  }
  pos <- sankey_data$positions
  if (!is.null(pos) && !any(is.na(pos$x))) {
    node_spec$x <- pos$x
    node_spec$y <- pos$y
  }

  # Create the plot
  p <- plot_ly(
    data = sankey_data$df_sankey,
    type = "sankey",
    arrangement = arrangement,
    node = node_spec,
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
      font = list(size = 12),
      margin = list(l = 10, r = 10, t = 50, b = 10)
    )

  # Add animation controls
  if (animate == TRUE) {
    p <- p %>%
      animation_opts(2000, redraw = TRUE) %>%
      animation_slider(
        currentvalue = list(prefix = "Year ", font = list(color = "#444444", size = 14))
      )
  }

  return(p)
}


# ANALYSIS FIGURES ----

## fig_helpers.R originally ----
# Contains the theme, palette, versioned output and spatial primitives for the MAWEI figures.

# MAWEI figure helpers — theme, palette, versioned output, spatial primitives
#
# Sourced by R/figures.R. Kept separate so the figure script stays readable and so the spatial
# Sankey machinery can be reused.
#
# Hassan Niazi, Aug 2026

suppressMessages({library(patchwork); library(sf); library(scales); library(ggrepel)})

# Versioned output. Each run writes to a NEW docs_analysis/figures_v<n>/ so an earlier round can
# never be overwritten and versions do not have to be renamed by hand. Set MAWEI_FIG_DIR to
# override, or MAWEI_FIG_REUSE=1 to write into the highest existing version.
fig_dir <- function(root = "docs_analysis") {
  override <- Sys.getenv("MAWEI_FIG_DIR", "")
  if (nzchar(override)) {
    dir.create(override, recursive = TRUE, showWarnings = FALSE)
    return(paste0(sub("/$", "", override), "/"))
  }
  existing <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  vers <- as.integer(str_match(existing, "^figures_v([0-9]+)$")[, 2])
  vers <- vers[!is.na(vers)]
  n <- if (length(vers) == 0) 0 else max(vers)
  if (!flag_env("MAWEI_FIG_REUSE", FALSE)) n <- n + 1
  d <- file.path(root, paste0("figures_v", n))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  paste0(d, "/")
}

theme_mawei <- function(base = 9) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.22, colour = "grey91"),
          # A faint frame around the plotting area separates data from axis labels and gives a
          # multi-panel figure a visible grid. Deliberately the same weight and tone as the axis
          # line so it reads as structure rather than as a box drawn on the data. Maps use
          # theme_map(), which has no border: a frame around a coastline reads as a graticule.
          panel.border = element_rect(colour = "grey86", fill = NA, linewidth = 0.35),
          axis.title = element_text(size = base - 0.5, colour = "grey25"),
          axis.text = element_text(size = base - 1.5, colour = "grey35"),
          plot.title = element_text(size = base + 0.5, face = "bold", colour = "grey10"),
          plot.subtitle = element_text(size = base - 1, colour = "grey35", lineheight = 1.05),
          plot.caption = element_text(size = base - 2.5, colour = "grey50", hjust = 0),
          legend.title = element_text(size = base - 1.5),
          legend.text = element_text(size = base - 1.5),
          legend.key.height = unit(0.32, "cm"),
          legend.key.width = unit(0.42, "cm"),
          strip.text = element_text(size = base - 0.5, face = "bold", colour = "grey15"),
          plot.tag = element_text(size = base + 2, face = "bold", colour = "grey20"))
}
theme_map <- function(base = 9) {
  theme_void(base_size = base) +
    theme(plot.title = element_text(size = base + 0.5, face = "bold", colour = "grey10"),
          plot.subtitle = element_text(size = base - 1, colour = "grey35", lineheight = 1.05),
          legend.title = element_text(size = base - 1.5),
          legend.text = element_text(size = base - 1.5),
          legend.key.height = unit(0.30, "cm"),
          legend.key.width = unit(0.38, "cm"),
          legend.position = "bottom",
          plot.margin = margin(4, 4, 4, 4),
          plot.tag = element_text(size = base + 2, face = "bold", colour = "grey20"))
}

C_WATER <- "#2E7CB0"; C_ENERGY <- "#E07A2F"
C_E4W   <- "#7E57C2"; C_W4E    <- "#26A69A"
C_LOSS  <- "#B0413E"; C_GOOD   <- "#2E8B57"; C_GREY <- "grey62"
C_FLOW  <- "#2E7CB0"; C_STILL  <- "#5AA9C7"; C_LAND <- "#8D6E63"

# Basin colours, fixed so a basin keeps its identity across every figure it appears in.
BASIN_COLS <- c(Chattahoochee = "#1F6FA8", Coosa_Etowah = "#E8A33D",
                Ocmulgee = "#4E9B6E", Flint = "#B4577A",
                Tallapoosa = "#7E6BA8", Oconee = "#8C8C8C", Broad = "#C6C6C6")

###############################################################################%
## spatial Sankey primitives ----
#
# A spatial Sankey draws each flow as a RIBBON whose width is proportional to volume, laid over a
# map at the true positions of its endpoints. geom_curve cannot do this: it draws a stroked line
# whose thickness is a fixed aesthetic in millimetres, so a "wide" flow is a thick line rather
# than a band with area. Ribbons therefore have to be built as polygons.
#
# ExtraNotes: the centreline is a quadratic Bezier, offset perpendicular to its own direction by
# half the ribbon width at each step, so the band keeps constant width along a curve instead of
# pinching on the inside of the bend. Width is TAPERED from source to target, which reads as
# direction without needing an arrowhead that would be lost under a wide band.

# Zoom to the fifteen counties with a small margin. `cty` is passed in because the helper file is
# sourced before the layers exist.
# ExtraNotes: the basin polygons are deliberately kept at FULL extent in the data, because the
# metro's headwater position is a finding. But a map framed on the basins spends most of its area
# on watershed far outside the study region. Clipping the VIEW rather than the data keeps both:
# basin edges run off the frame, which is itself the correct visual signal.
coord_metro <- function(cty, pad = 0.10) {
  bb <- sf::st_bbox(cty)
  coord_sf(xlim = c(bb[["xmin"]] - pad, bb[["xmax"]] + pad),
           ylim = c(bb[["ymin"]] - pad, bb[["ymax"]] + pad), expand = FALSE)
}

# Label positions for a stacked area or bar.
# ExtraNotes: position_stack() places the FIRST factor level at the TOP of the stack, so a label
# position computed from a descending sort lands on the wrong band. Deriving the position from the
# factor levels in their own order is the only way to keep label and band together; guessing the
# order is what put the basin labels on the wrong areas.
stack_label_y <- function(d, group, value) {
  d %>%
    mutate(.g = {{ group }}, .v = {{ value }}) %>%
    arrange(as.integer(factor(.g, levels = levels(factor(.g))))) %>%
    mutate(.top = sum(.v) - cumsum(.v) + .v,
           ypos = .top - .v / 2)
}

# Quadratic Bezier from (x0,y0) to (x1,y1), bowed sideways by `curv` of the chord length.
bezier_path <- function(x0, y0, x1, y1, curv = 0.18, n = 60) {
  mx <- (x0 + x1) / 2; my <- (y0 + y1) / 2
  dx <- x1 - x0; dy <- y1 - y0
  # control point offset perpendicular to the chord
  cx <- mx - curv * dy; cy <- my + curv * dx
  t <- seq(0, 1, length.out = n)
  tibble(x = (1 - t)^2 * x0 + 2 * (1 - t) * t * cx + t^2 * x1,
         y = (1 - t)^2 * y0 + 2 * (1 - t) * t * cy + t^2 * y1,
         t = t)
}

# One ribbon polygon. `w0` and `w1` are half-widths in degrees at source and target.
ribbon_poly <- function(x0, y0, x1, y1, w0, w1, curv = 0.18, n = 60, id = 1L) {
  p <- bezier_path(x0, y0, x1, y1, curv, n)
  # local direction, forward difference with the last step repeated
  dx <- c(diff(p$x), tail(diff(p$x), 1)); dy <- c(diff(p$y), tail(diff(p$y), 1))
  len <- sqrt(dx^2 + dy^2); len[len == 0] <- 1e-12
  # unit normal
  nx <- -dy / len; ny <- dx / len
  w <- w0 + (w1 - w0) * p$t
  bind_rows(
    tibble(x = p$x + nx * w, y = p$y + ny * w, ord = seq_len(n)),
    tibble(x = rev(p$x - nx * w), y = rev(p$y - ny * w), ord = n + seq_len(n))
  ) %>% mutate(rib = id)
}

# Build ribbons for a whole edge table. Widths are scaled so the largest flow reaches
# `max_w` degrees, which keeps the map legible whatever the units of `value`.
sankey_ribbons <- function(d, x0, y0, x1, y1, value, max_w = 0.055, taper = 0.45,
                           curv = 0.18) {
  d <- d %>% mutate(.v = {{ value }}) %>% filter(.v > 0) %>% arrange(.v)
  if (nrow(d) == 0) return(NULL)
  # Width scales with the SQUARE ROOT of volume. Linear width makes the largest flow swamp the
  # map, and area is what the eye integrates anyway.
  hw <- max_w * sqrt(d$.v / max(d$.v)) / 2
  purrr::pmap_dfr(
    list(d %>% pull({{ x0 }}), d %>% pull({{ y0 }}),
         d %>% pull({{ x1 }}), d %>% pull({{ y1 }}), hw, seq_len(nrow(d))),
    function(a, b, c, e, w, i)
      ribbon_poly(a, b, c, e, w0 = w, w1 = w * taper, curv = curv, id = i)) %>%
    left_join(d %>% mutate(rib = seq_len(nrow(d))), by = "rib")
}
