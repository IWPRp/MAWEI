# Metro Atlanta water flows analysis
# this processes raw water supply and wastewater data and prepares it for plotting sankeys
#
# Hassan Niazi, June 2025

source("functions.R")

###############################################################################%

# public water supply ----
# load pubic water supply data
df_pws <- read_csv(paste0(DATA_DIR, "water_publicwatersupply.csv")) %>%
  rename_all(tolower) %>%
  rename_with(~ gsub(" |-", "_", .), everything())

names(df_pws)

# STEPS:
# the first step is simple aggregations
# the second steps is redistributing some aggregated categories into our bins (e.g., irrigation needs to split up between residential and commercial)

## residential ----
# sum single family and multi family to residential if residential is missing.
# doing this because residential is only available when single or multifamily is not, so assuming some counties break it out some report the aggregated usages

df_pws_res <- df_pws %>%
  select(county, year, single_family, multifamily, residential) %>%
  mutate(residential_calc = if_else(is.na(residential), single_family + multifamily, residential)) %>%
  select(county, year, residential=residential_calc)
# ExtraNotes: counties report households EITHER split by dwelling type OR as one combined
# residential figure, never both, so the two conventions are mutually exclusive and coalescing
# them cannot double count. Summing unconditionally would double the counties that report both
# a total and its parts; taking only `residential` would drop the counties that report parts.


## commercial ----
# sum commercial, institutional, municipal and new_commercial into commercial_calc. Ignore missing values (NAs)
# adding muni here because Danny J said municipal is really government buildings

df_pws_com <- df_pws %>%
  select(county, year, commercial, institutional, municipal, new_commercial) %>%
  mutate(commercial_calc = rowSums(select(., commercial, institutional, municipal, new_commercial), na.rm = TRUE)) %>%
  select(county, year, commercial=commercial_calc)
# ExtraNotes: `municipal` is folded into commercial on utility advice that the category is
# government buildings, i.e. a commercial-type building load, not municipal system water. This
# is why the water diagram carries no separate government sector while the energy diagram does:
# the two source datasets draw the institutional boundary in different places.

## industrial ----
# sum industrial and anheuser_busch in industrial_calc. Ignore missing values (NAs)

df_pws_ind <- df_pws %>%
  select(county, year, industrial, anheuser_busch) %>%
  mutate(industrial_calc = rowSums(select(., industrial, anheuser_busch), na.rm = TRUE)) %>%
  select(county, year, industrial=industrial_calc)
# ExtraNotes: one brewery is reported as its own line item because it is large enough to be
# tracked individually by the utility. It is a single industrial customer, so it is folded into
# industrial rather than given a node, which would disclose one firm's consumption.

## agricultural ----
# ag stays ag, just replace NAs with 0
# but this flow is from pws, we will later introduce the self-supply source for ag

df_pws_ag <- df_pws %>%
  select(county, year, agricultural) %>%
  mutate(agricultural = if_else(is.na(agricultural), 0, agricultural))


names(df_pws)

# step 2: redistribute some aggregated categories into our categories
# the sequence is important, because the proportionally distribution would change if the order is changed

# specific categories to be handled
# irrigation gets split into residential and commercial. Update df_pws_res and df_pws_com
# other goes to all of above proportionally
# self_supplied gets distributed into residential, commercial, industrial and agricultural proportionally, the source is self_supplied not pws
# two ways to handle nrw: either directly link to the the pws source, or make it flow through all the categories (where they become the source)

## irrigation ----
# this irrigation is landscaping so splitting between residential and commercial
# combine df_pws_res and df_pws_com, create their share for county, year. join irrigation, and apply the share to residential and commercial, and sum previous res and comm and irr water in res and comm, call it res_wirr and com_wirr
df_pws_res_comm_wirrigation <- df_pws %>%
  select(county, year, irrigation) %>%
  # filter(!is.na(irrigation)) %>%
  mutate(irrigation = replace_na(irrigation, 0)) %>%
  left_join(df_pws_res, by = c("county", "year")) %>%
  left_join(df_pws_com, by = c("county", "year")) %>%
  mutate(residential_share = residential / (residential + commercial),
         commercial_share = commercial / (residential + commercial),
         residential_wirr = residential + (irrigation * residential_share),
         commercial_wirr = commercial + (irrigation * commercial_share)) %>%
  select(county, year, residential=residential_wirr, commercial=commercial_wirr)
# ExtraNotes: metered irrigation here is landscape watering, not agriculture, so it is split
# between households and businesses in proportion to their existing metered demand rather than
# sent to the agricultural sector. Irrigation is also the most consumptive urban end use: it
# largely evaporates instead of returning to the sewer, which is part of why the residential
# loss term is as large as it is.


## other ----
# gets split up between df_pws_ind df_pws_ag and df_pws_res_comm_wirrigation according to their shares
# this is the final sectoral breakdown of pws

df_pws_cira_other <- df_pws %>% # cira = comm ind res ag
  select(county, year, other) %>%
  mutate(other = replace_na(other, 0)) %>%
  left_join(df_pws_res_comm_wirrigation, by = c("county", "year")) %>%
  left_join(df_pws_ind, by = c("county", "year")) %>%
  left_join(df_pws_ag, by = c("county", "year")) %>%
  mutate(total = residential + commercial + industrial + agricultural,
         # calculate shares for each category
         residential_share = residential / total,
         commercial_share = commercial / total,
         industrial_share = industrial / total,
         agricultural_share = agricultural / total,
         # apply the shares to other
         residential_other = residential + (other * residential_share),
         commercial_other = commercial + (other * commercial_share),
         industrial_other = industrial + (other * industrial_share),
         agricultural_other = agricultural + (other * agricultural_share)) %>%
  select(county, year, residential=residential_other, commercial=commercial_other,
         industrial=industrial_other, agricultural=agricultural_other)
# ExtraNotes: the order of the two redistributions is a method decision, not an implementation
# detail. Irrigation is assigned FIRST and then `other` is spread over the resulting totals, so
# the irrigation volume influences the shares that `other` is split by. Reversing the order
# changes every sectoral total. Irrigation goes first because its destination is known on
# physical grounds, whereas `other` is a residual with no known destination.

# ANALYSIS: how much of each sector's reported demand is actually reallocated rather than
# metered to it. Only computable here, because downstream the reallocated volume is
# indistinguishable from directly metered demand. A sector whose total is largely reallocation
# carries correspondingly more method uncertainty, which is what this quantifies.
if (ANALYSIS) {
  pws_reallocation <- df_pws %>%
    select(county, year, irrigation, other, self_supplied, nrw) %>%
    mutate(across(everything(), ~replace_na(., 0))) %>%
    left_join(df_pws_cira_other %>%
                mutate(final_total = residential + commercial + industrial + agricultural) %>%
                select(county, year, final_total), by = c("county", "year")) %>%
    filter(year %in% YEARS_TO_ENSURE) %>%
    group_by(year) %>%
    summarise(across(c(irrigation, other, self_supplied, nrw, final_total), sum),
              .groups = "drop") %>%
    mutate(irrigation_pct = 100 * irrigation / final_total,
           other_pct = 100 * other / final_total,
           reallocated_pct = 100 * (irrigation + other) / final_total)
  write_csv(pws_reallocation, file.path(ANALYSIS_DIR, "P1_pws_reallocation_share.csv"))
  message("  ANALYSIS: reallocated share of sectoral demand, ", max(YEARS_TO_ENSURE), ": ",
          round(last(pws_reallocation$reallocated_pct), 1), "%")
}


###############################################################################%

# self_supplied ----
# self_supplied gets distributed into residential, commercial, industrial and agricultural proportionally, but the source is self_supplied not pws
# df_pws_self_supplied <- df_pws %>%
#   select(county, year, self_supplied) %>%
#   mutate(self_supplied = replace_na(self_supplied, 0)) %>%
#   left_join(df_pws_cira_other, by = c("county", "year")) %>%
#   mutate(total = residential + commercial + industrial + agricultural,
#          # calculate shares for each category
#          residential_share = residential / total,
#          commercial_share = commercial / total,
#          industrial_share = industrial / total,
#          agricultural_share = agricultural / total,
#          # apply the shares to self_supplied
#          residential_self_sup = (self_supplied * residential_share),
#          commercial_self_sup = (self_supplied * commercial_share),
#          industrial_self_sup = (self_supplied * industrial_share),
#          agricultural_self_sup = (self_supplied * agricultural_share)) %>%
#   select(county, year,
#          residential=residential_self_sup, commercial=commercial_self_sup,
#          industrial=industrial_self_sup, agricultural=agricultural_self_sup)

## pws self-supply ----
# self_supplied gets assigned to residential because mostly homes have wells
df_pws_self_supplied <- df_pws %>%
  select(county, year, self_supplied) %>%
  mutate(value = replace_na(self_supplied, 0), target = "residential") %>%
  select(county, year, target, value)
# ExtraNotes: self-supply is assigned wholly to households on private wells, and its source is
# set to groundwater downstream. The regional planning figure for self-supply (~45.9 MGD) is an
# order of magnitude above the management plan's reported groundwater supply (~3.6 MGD); the
# gap is agricultural and industrial self-supply, which are brought in separately below, so the
# two figures must not be compared directly.

## ag self supply ----
# the data in only for 2020 so let's write zeros for missing counties and extend for all years. the data should be complete

# df_ag_self_supplied <- read_csv(paste0(DATA_DIR, "water_selfsupply_ag.csv")) %>% rename_all(tolower) %>%
#   # filter(source=="total") %>%
#   select(county, year, value = total) %>%
#   complete(county = counties, year = unique(df_pws$year)) %>%  # add missing counties and years
#   group_by(county) %>%
#   mutate(value = value[year == 2020]) %>% # copy 2020 values forward to all years
#   ungroup() %>% replace_na(list(value = 0)) %>%
#   mutate(target = "agricultural")  %>%
#   select(county, year, target, value)

df_ag_self_supplied <- read_csv(paste0(DATA_DIR, "water_selfsupply_ag.csv")) %>% rename_all(tolower) %>%
  filter(source %in% c("surface", "ground")) %>%
  mutate(source = case_when(source == "surface" ~ "surfaceWater",
                            source == "ground"  ~ "groundwater")) %>%
  select(county, year, source, value = total) %>%
  complete(county = counties, year = unique(df_pws$year), source = c("surfaceWater", "groundwater")) %>%
  group_by(county, source) %>%
  mutate(value = value[year == 2020]) %>%
  ungroup() %>% replace_na(list(value = 0)) %>%
  mutate(target = "agricultural") %>%
  select(county, year, source, target, value)
# ExtraNotes: agricultural self-supply is observed for 2020 only and is held CONSTANT across the
# study period, so any interannual change in agricultural water is an artefact of the public
# supply data alone. Acceptable because agriculture is ~0.04% of metro supply; it would not be
# acceptable in a rural application of the same method. Counties absent from the file are true
# zeros rather than missing, since the source is a complete regional inventory.


# nrw / losses ----
# non-revenue water (NRW) is the difference between total water supplied and total water billed
# source is publicWatSup, target is NRW

df_pws_nrw <- df_pws %>%
  select(county, year, nrw) %>%
  mutate(value = replace_na(nrw, 0),
         source = "publicWatSup", target = "losses") %>%
  select(county, year, source, target, value)
# ExtraNotes: non-revenue water is wired straight from the supply node to the loss sink rather
# than routed through the end-use sectors. NRW is water that never reaches a customer -- mains
# leakage, unbilled use, meter error -- so passing it through a sector would inflate that
# sector's apparent demand. Consequence for interpretation: the `losses` sink is NOT all NRW.
# It also receives each sector's consumptive use (supply minus wastewater returned), so the NRW
# share must be read from this flow alone, not from the sink total.

###############################################################################%

# wastewater gen ----

# wastewater by sector
# separate septic and wastewater in residential
# I/I factor: apply it on wastewater and get the water from freshwater
# discharges

# read wastewater
df_wastewater <- read_csv(paste0(DATA_DIR, "water_wastewater.csv")) %>%
  rename_all(tolower) %>%
  rename_with(~ gsub(" |-|\\.+|/", "_", .), everything()) %>% # replace spaces \ - dots "\\.+" with underscores
  rename_with(~ gsub("_+", "_", .), everything()) %>%  # replace duplicate underscores
  rename_with(~ gsub("\\(|\\)", "", .), everything())  # replace () with nothing

names(df_wastewater)

## residential wastewater ----
# Counties report household wastewater either split into single/multi family or as a single
# combined `residential` figure, never both. Add self-supplied, then remove the septic-served
# volume so that only sewered flow enters the collection system: septic is routed separately
# to its own terminal node, and counting it in both places would double the household total.
# All quantities are MG/yr here; the conversion to MGD happens once, downstream.
df_wastewater_res <- df_wastewater %>%
  select(county, year, single_family, multi_family, residential, self_supplied,
         vol_septic_generated_mg) %>%
  mutate(residential_calc = if_else(is.na(residential),
                                    rowSums(across(c(single_family, multi_family)), na.rm = TRUE),
                                    residential),
         residentialww = residential_calc + replace_na(self_supplied, 0),
         residentialnoseptic = residentialww - replace_na(vol_septic_generated_mg, 0)) %>%
  select(county, year, residential = residentialnoseptic)
stopifnot(all(df_wastewater_res$residential >= 0, na.rm = TRUE))

## commercial wastewater ----
# sum commercial, institutional, municipal and new_commercial into commercial
df_wastewater_com <- df_wastewater %>%
  select(county, year, commercial, institutional, new_commercial) %>%
  mutate(commercial = rowSums(select(., commercial, institutional, new_commercial), na.rm = TRUE)) %>%
  select(county, year, commercial)

## industrial wastewater ----
# sum industrial and anheuser_busch in industrial
df_wastewater_ind <- df_wastewater %>%
  select(county, year, industrial) %>%
  mutate(industrial = rowSums(select(., industrial), na.rm = TRUE)) %>%
  select(county, year, industrial)

## agricultural wastewater ----
# agricultural stays agricultural, just replace NAs with 0
df_wastewater_ag <- df_wastewater %>%
  select(county, year, agricultural) %>%
  mutate(agricultural = if_else(is.na(agricultural), 0, agricultural)) %>%
  select(county, year, agricultural)

## irrigation ----
# split irrigation to residential and commercial so waste water from irrigation
# is basically return flow from lawns

df_wastewater_res_comm_wirrigation <- df_wastewater %>%
  select(county, year, irrigation) %>%
  mutate(irrigation = replace_na(irrigation, 0)) %>%
  left_join(df_wastewater_res, by = c("county", "year")) %>%
  left_join(df_wastewater_com, by = c("county", "year")) %>%
  mutate(residential_share = residential / (residential + commercial),
         commercial_share = commercial / (residential + commercial),
         residential_wirr = residential + (irrigation * residential_share),
         commercial_wirr = commercial + (irrigation * commercial_share)) %>%
  select(county, year, residential=residential_wirr, commercial=commercial_wirr)

## other  ----
# distribute "other" to all res com ind and ag if there is a non-zero value
df_wastewater_cira_other <- df_wastewater %>%
  select(county, year, other) %>%
  mutate(other = replace_na(other, 0)) %>%
  left_join(df_wastewater_res_comm_wirrigation, by = c("county", "year")) %>%
  left_join(df_wastewater_ind, by = c("county", "year")) %>%
  left_join(df_wastewater_ag, by = c("county", "year")) %>%
  mutate(total = residential + commercial + industrial + agricultural,
         # calculate shares for each category
         residential_share = residential / total,
         commercial_share = commercial / total,
         industrial_share = industrial / total,
         agricultural_share = agricultural / total,
         # apply the shares to other
         residential_other = residential + (other * residential_share),
         commercial_other = commercial + (other * commercial_share),
         industrial_other = industrial + (other * industrial_share),
         agricultural_other = agricultural + (other * agricultural_share)) %>%
  select(county, year, residential=residential_other, commercial=commercial_other,
         industrial=industrial_other, agricultural=agricultural_other)


# septic ----
# for now (why for now?), let's use vol_septic_generated_(mg) as value, source wastewater, target septic
df_wastewater_septic <- df_wastewater %>%
  select(county, year, vol_septic_generated_mg) %>%
  mutate(vol_septic_generated_mg = replace_na(vol_septic_generated_mg, 0),
         vol_septic_generated_mgd = vol_septic_generated_mg/365) %>%
  rename(value = vol_septic_generated_mgd) %>%
  mutate(source = "residential", target = "septic") %>%
  select(county, year, source, target, value)


###############################################################################%

# reconcile water supply and wastewater generation data ----
df_pws_ <- df_pws_cira_other %>%
  pivot_longer(cols = c(residential, commercial, industrial, agricultural),
               names_to = "target", values_to = "value") %>%
  mutate(source = "publicWatSup")

# check for a zero pws flow
if (any(df_pws_$value <= 0)) {
  cat(paste0("\nPublic water supply zero in ",
                 unique(paste(df_pws_$county[df_pws_$value <= 0])), " for: ",
                 paste(unique(df_pws_$target[df_pws_$value <= 0])
                 , collapse = ", ")))
  cat("\n => This is actually OK as not all counties have all end-uses especially for ag and industry")
}

# df_ss_ <- df_pws_self_supplied %>%
#   pivot_longer(cols = c(residential, commercial, industrial, agricultural),
#                names_to = "target", values_to = "value") %>%
#   mutate(source = "selfWatSup")

# df_ss_ <- rbind(df_pws_self_supplied, df_ag_self_supplied) %>% mutate(source = "groundwater")
df_ss_ <- rbind(df_pws_self_supplied %>% mutate(source = "groundwater"), df_ag_self_supplied)


## water supply ----
df_watersupp <- rbind(df_pws_, df_ss_, df_pws_nrw)

if (MAKE_PLOT) plot_sankey(df_watersupp)
# plot_sankey(df_watersupp, reg = "Hall") # most ag
# plot_sankey(df_watersupp, reg = "Douglas") # most industrial
# plot_sankey(df_watersupp, reg = "Fulton") # atlanta

# add wastewater data
df_wastewat <- df_wastewater_cira_other %>%
  pivot_longer(cols = c(residential, commercial, industrial, agricultural),
               names_to = "source", values_to = "value") %>%
  mutate(value = value / 365, # from MG to MGD
         target = "wastewater")

df_water_sup_waste <- rbind(df_watersupp, df_wastewat)

# plot_sankey(df_water_sup_waste) # losses not fully visible
if (MAKE_PLOT) plot_sankey(df_water_sup_waste, reg = "Hall") # losses not fully visible

###############################################################################%

# losses ----
# difference of total water supplied and total wastewater produced
total_water_supply <- df_water_sup_waste %>%
  filter(source %in% c("publicWatSup", "groundwater", "surfaceWater")) %>%
  filter(target != "losses") %>% # losses are a terminal node, not requiring another losses calculation
  group_by(county, year, target) %>%
  summarise(total_supply = sum(value), .groups = "drop")

total_wastewater <- df_wastewat %>%
  filter(target == "wastewater") %>%
  # add septic to wastewater because septic still contributes to total wastewater, just not to treated wastewater
  rbind(df_wastewater_septic) %>%
  group_by(county, year, source) %>%
  summarise(wastewater_generated = sum(value), .groups = "drop") %>%
  select(county, year, target=source, wastewater_generated)

# # one big issue, also noted by Katherine, is that the flows could be categorized
# differently across counties (e.g., municipally supplied ag. I think in the
# case of Hall County, this might fall under “Industrial,” but for others (such
# as Bartow), it is called out on its own. )

# # wastewater in some counties for certain categories is more than the water supply for those categories. e.g., industrial Hall 2020
# # but the total wastewater is still less than the total water supply
# # NOTE: this was true before excluding losses from total water supply
# paste0("Total water supply in Hall county in 2020: ",
#        sum(total_water_supply$total_supply[total_water_supply$county == "Hall" & total_water_supply$year == 2020]))
# paste0("Total wastewater in Hall county in 2020: ",
#       sum(total_wastewater$wastewater_generated[total_wastewater$county == "Hall" & total_wastewater$year == 2020]))

df_water_losses <- total_water_supply %>%
  left_join(total_wastewater, by = c("county", "year", "target")) %>%
  mutate(losses = total_supply - wastewater_generated,
         source = target, target = "losses") %>%
  select(county, year, source, target, value = losses)
# ExtraNotes: each sector's loss is a RESIDUAL, supply minus wastewater returned, not a measured
# quantity. Physically it is consumptive use (evaporation, irrigation, product incorporation,
# human consumption), but it also absorbs every inconsistency between the supply and wastewater
# datasets, which are separate utility submissions with different sectoral conventions. This is
# the single largest source of uncertainty in the water diagram and the reason the loss share
# should be reported as a range rather than a point estimate.

neg_losses <- df_water_losses %>% filter(value < 0) # should be none

# temp fix: add back negative losses into water supply for each category and remove negative losses or recalculate them
df_water_sup_waste_fix <- df_water_sup_waste %>%
  # remove groundwater and assign the fix to PWS + surface water only to avoid double counting e.g. Bartow 2024
  # (and because GW is very small to absorb statistical differences)
  filter(source != "groundwater") %>% filter(source != "surfaceWater") %>%
  left_join(neg_losses %>% rename(negloss=value), by = c("county", "year", "target" = "source")) %>%
  mutate(supply_adj = if_else(!is.na(negloss), value + abs(negloss), value)) %>%
  select(county, year, source, target, value = supply_adj) %>%
  # bring back groundwater
  rbind(df_water_sup_waste %>% filter(source == "groundwater"))

total_water_supply_fix <- df_water_sup_waste_fix %>%
  filter(source %in% c("publicWatSup", "groundwater", "surfaceWater")) %>%
  filter(target != "losses") %>% # losses are a terminal node, not requiring another losses calculation
  group_by(county, year, target) %>%
  summarise(total_supply = sum(value), .groups = "drop")

df_water_losses_fix_raw <- total_water_supply_fix %>%
  left_join(total_wastewater, by = c("county", "year", "target")) %>%
  mutate(losses = total_supply - wastewater_generated,
         source = target, target = "losses") %>%
  select(county, year, source, target, value = losses)

df_water_losses_fix_raw %>% filter(value < 0) # should be none
df_water_losses_fix <- df_water_losses_fix_raw %>% mutate(value = if_else(value < 0, 0, value)) # fixing just 4 edge cases in Bartow
# ExtraNotes: a negative residual means a sector returned more wastewater than it was supplied,
# which cannot happen physically and instead signals cross-sector misclassification between the
# two datasets (a use billed as industrial in one and commercial in the other). The correction
# raises that sector's supply to at least its return flow, and is applied to public supply and
# surface water only: groundwater is small and measured, so letting it absorb a statistical
# residual would corrupt the one inflow term that is directly reported.

# I/I ----
# increase total wastewater by i_i_factor, target wastewater
# infiltration and inflow due to negative pressure in the pipes (cavitation?), water comes from groundwater
df_water_i_i <- df_wastewat %>%
  filter(target == "wastewater") %>%
  left_join(df_wastewater %>% select(county, year, i_i_factor), by = c("county", "year")) %>%
  mutate(value_ii = value * i_i_factor,
         source_ii="subsurface") %>%
  select(county, year, source=source_ii, target, value=value_ii) %>%
  unique() # to avoid downstream issues; DeKalb has some duplicated due to no industrial and ag wastewater
# ExtraNotes: I&I is entered as its own SOURCE node (`subsurface`), not as an addition to sector
# wastewater, because it is groundwater and stormwater leaking into sewers rather than water any
# sector was supplied. It therefore enlarges the treatment burden without appearing anywhere in
# withdrawal, and is the reason treatment throughput exceeds metered use. The county I&I factor
# is a fixed multiplier applied to every year, so I&I cannot show a wet-year signal here; it
# tracks sewer condition, not weather.

# check if any I/I values are negative or zero (should not be)
if (any(df_water_i_i$value <= 0)) {
  cat(paste0("\nI/I values are negative or zero in ",
              unique(paste(df_water_i_i$county[df_water_i_i$value <= 0])),
             " for flows: " , paste(unique(df_water_i_i$source[df_water_i_i$value <= 0]),
              " in year(s): " , paste(unique(df_water_i_i$year[df_water_i_i$value <= 0]),
                    collapse = ", "))))

  cat("\n => Zero I/I is OK, as some counties have no wastewater from agriculture and industry")
}

# ANALYSIS: I&I as an infiltration RATE per unit of real sewage, alongside the resulting volume.
# The factor is an input assumption and the volume is a result, so reporting them together is the
# only way a reader can tell how much of the I&I finding is measurement and how much is
# assumption. The joined factor is captured before the select() below drops it.
if (ANALYSIS) {
  ii_factor <- df_wastewat %>%
    filter(target == "wastewater") %>%
    left_join(df_wastewater %>% select(county, year, i_i_factor), by = c("county", "year")) %>%
    filter(year %in% YEARS_TO_ENSURE) %>%
    group_by(county, year) %>%
    summarise(i_i_factor = first(i_i_factor),
              sector_ww_mgd = sum(value), .groups = "drop") %>%
    mutate(ii_mgd = sector_ww_mgd * i_i_factor,
           ii_share_of_collected_pct = 100 * ii_mgd / (sector_ww_mgd + ii_mgd))
  write_csv(ii_factor, file.path(ANALYSIS_DIR, "P0_ii_factor_and_volume.csv"))
  message("  ANALYSIS: I&I factor range ",
          paste(range(ii_factor$i_i_factor, na.rm = TRUE), collapse = "-"),
          ", mean ", round(mean(ii_factor$i_i_factor, na.rm = TRUE), 3),
          "; metro I&I share ",
          round(100 * sum(ii_factor$ii_mgd) / sum(ii_factor$sector_ww_mgd + ii_factor$ii_mgd), 1),
          "%")
}


###############################################################################%

# ww treatment ----
# we will use facility level FRACTIONS of treatment based on historical data
# (averages or 75%) and apply to wastewater VOLUMES calculated above (after I/I calculation)

# read wastewater treatment data (esp facility, treatment fraction, level of treatment, permit cap)
df_wastewater_treatment <- read_csv(paste0(DATA_DIR, "water_wastewater_treatment.csv")) %>%
  rename_all(tolower) %>%
  rename_with(~ gsub(" |-|\\.+|/", "_", .), everything()) %>% # replace spaces \ - dots "\\.+" with underscores
  rename_with(~ gsub("_+", "_", .), everything()) %>%  # replace duplicate underscores
  rename_with(~ gsub("\\(|\\)", "", .), everything())  # replace () with nothing


# ww interconnections ----
# filter out in-county flows: fromcounty == tocounty
# check duplicates: data is duplicated between counties (check with and without values; also check the copied flag in notes)
# probably remove the duplicated (on without value level; is values are different keep the max value)
# complete the data for each year; assign zeros to missing values

# read water_wastewater_connections.csv
df_ww_conn <- read_csv(paste0(DATA_DIR, "water_wastewater_connections.csv")) %>%
  rename_all(tolower) %>% rename(value = flow)

# diagnostic
# unique list of all
print(paste0("Source counties: ", length(unique(df_ww_conn$fromcounty)), " (", paste(unique(df_ww_conn$fromcounty), collapse = ", "), ")"))
print(paste0("Sink counties: ", length(unique(df_ww_conn$tocounty)), " (", paste(unique(df_ww_conn$tocounty), collapse = ", "), ")"))
cat(paste0("Source places: ", length(unique(df_ww_conn$fromplace)), " (\n ", paste(unique(df_ww_conn$fromplace), collapse = "\n "), ")"))
cat(paste0("Sink facilities: ", length(unique(df_ww_conn$tofacility)), " (\n ", paste(unique(df_ww_conn$tofacility), collapse = "\n "), ")"))

# interconnections
county2county <- df_ww_conn %>% select(county, year, source = fromcounty, target = tocounty, value) %>% unique() %>% replace_na(list(value= 0))
if (MAKE_PLOT) plot_sankey(county2county)
county2facility <- df_ww_conn %>% select(county, year, source = fromcounty, target = tofacility, value) %>% unique() %>% replace_na(list(value= 0))
if (MAKE_PLOT) plot_sankey(county2facility)
place2facility <- df_ww_conn %>% select(county, year, source = fromplace, target = tofacility, value) %>% unique() %>% replace_na(list(value= 0))
if (MAKE_PLOT) plot_sankey(place2facility)

# main interconnections logic
# if fromcounty = tocounty, mutate flow_type = "in-county" else "out-county"
# take only out-county flows
# take wastewater treated final table and subtract out-county flows
# Total treatment = In-county generation + Imports - Exports (for each facility)
df_ww_conn_type <- df_ww_conn %>%
  mutate(flow_type = if_else(fromcounty == tocounty, "in-county", "out-county")) %>%
  filter(flow_type == "out-county") %>% # keep only out-county flows
  filter(!grepl("copied", tolower(notes))) %>% # filter if notes contains "copied" string
  select(-notes) %>% replace_na(list(value= 0))

table(df_ww_conn_type$flow_type) # check counts

# Collapse duplicate records of the same physical connection, keeping the larger value where
# two records disagree.
# ExtraNotes: each transfer is reported twice, once on the exporting county's sheet and once on
# the importing county's, and the two entries frequently carry slightly different estimates. A
# single named origin cannot send two different volumes to the same facility in the same year,
# so a repeated (fromcounty, fromplace, tocounty, tofacility, year) key is by definition one
# connection reported twice. Deduplicating on the full key preserves genuinely distinct origins
# feeding a shared facility, which is common and must not be collapsed.
n_before <- nrow(df_ww_conn_type)
df_ww_conn_type <- df_ww_conn_type %>%
  group_by(fromcounty, fromplace, tocounty, tofacility, year) %>%
  summarise(value = max(value, na.rm = TRUE), .groups = "drop")
if (nrow(df_ww_conn_type) < n_before) {
  message("  ww connections: collapsed ", n_before, " records to ", nrow(df_ww_conn_type),
          " distinct connections (duplicate reporting between county sheets)")
}

# Remaining same-route records from different named origins. Kept and summed, but reported:
# these are either genuinely separate connections or the same flow described two ways, and only
# local knowledge can distinguish them.
ww_conn_ambiguous <- df_ww_conn_type %>%
  filter(year %in% YEARS_TO_ENSURE) %>%
  group_by(fromcounty, tocounty, tofacility, year) %>%
  filter(n() > 1) %>%
  arrange(fromcounty, tocounty, tofacility, year, desc(value)) %>%
  ungroup()
if (nrow(ww_conn_ambiguous) > 0) {
  dir.create(QC_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(ww_conn_ambiguous, file.path(QC_DIR, "ww_connections_ambiguous.csv"))
  message("  ww connections: ", n_distinct(paste(ww_conn_ambiguous$fromcounty,
          ww_conn_ambiguous$tocounty, ww_conn_ambiguous$tofacility)),
          " route(s) have multiple differing records -> ",
          file.path(QC_DIR, "ww_connections_ambiguous.csv"))
}

# ANALYSIS: how far apart the two counties' estimates of the SAME transfer are. This is a
# measure of how well neighbouring utilities agree about a shared pipe, and it can only be
# computed before deduplication collapses the pairs. A large systematic disagreement would
# undermine the transfer network results; a small one supports them.
if (ANALYSIS) {
  dup_pairs <- df_ww_conn %>%
    mutate(flow_type = if_else(fromcounty == tocounty, "in-county", "out-county")) %>%
    filter(flow_type == "out-county", !grepl("copied", tolower(notes))) %>%
    replace_na(list(value = 0)) %>%
    # Clamped to the study period. The connection table runs to 2065, and the projected years
    # carry a growth ramp on one leg only, which makes the two counties' figures diverge by
    # construction and would report a spurious 99% disagreement.
    filter(year %in% YEARS_TO_ENSURE) %>%
    group_by(fromcounty, fromplace, tocounty, tofacility, year) %>%
    filter(n() > 1) %>%
    summarise(lo = min(value), hi = max(value), n = n(), .groups = "drop") %>%
    mutate(abs_gap = hi - lo,
           rel_gap_pct = if_else(hi > 0, 100 * (hi - lo) / hi, 0))
  if (nrow(dup_pairs) > 0) {
    write_csv(dup_pairs, file.path(ANALYSIS_DIR, "P2_transfer_reporting_agreement.csv"))
    message("  ANALYSIS: ", nrow(dup_pairs), " doubly reported transfers; ",
            sum(dup_pairs$rel_gap_pct < 1), " agree within 1%, median disagreement ",
            round(median(dup_pairs$rel_gap_pct), 1), "%")
  }
}


# trade flows
df_ww_conn_trade <- df_ww_conn_type %>%
  group_by(fromcounty, fromplace, tocounty, tofacility, year) %>%
  summarise(trade = sum(value, na.rm = TRUE), .groups = "drop")

# sum of wastewater generated and I/I added
df_ww_tobetreated_gen <- rbind(df_water_i_i, df_wastewat) %>%
  filter(target == "wastewater") %>% # just in case
  group_by(county, year, target) %>%
  summarise(value = sum(value), .groups = "drop")

# Net inter-county transfer per county-year: what arrives from other counties minus what
# leaves for other counties.
# ExtraNotes: within the metro these two legs are identical in total, since every export from
# one county is an import to another. They differ only per county, which is what makes the
# county-level treatment allocation meaningful.
ww_net_trade <- full_join(
  df_ww_conn_trade %>% group_by(county = tocounty, year) %>%
    summarise(imports = sum(trade, na.rm = TRUE), .groups = "drop"),
  df_ww_conn_trade %>% group_by(county = fromcounty, year) %>%
    summarise(exports = sum(trade, na.rm = TRUE), .groups = "drop"),
  by = c("county", "year")) %>%
  mutate(across(c(imports, exports), ~replace_na(., 0)))

df_ww_tobetreated <- df_ww_tobetreated_gen %>%
  left_join(ww_net_trade, by = c("county", "year")) %>%
  mutate(across(c(imports, exports), ~replace_na(., 0)),
         generated = value,
         value = generated - exports)
# ExtraNotes: exports are subtracted but imports are NOT added here. Imported sewage reaches
# the receiving plant through its own explicit transfer flows, so adding it again at this point
# would count it twice. A facility's true throughput is therefore
#     (generated - exports)   via the county's own collection node
#   + imports                 via the inter-county transfer nodes
# which is what the discharge side sums.
# ExtraNotes: a county whose exports exceed its own generation would imply treating a negative
# volume. Clamp at zero and record it rather than let it propagate.
ww_treat_negative <- df_ww_tobetreated %>% filter(value < 0)
if (nrow(ww_treat_negative) > 0) {
  message("  ww treatment: ", nrow(ww_treat_negative),
          " county-year(s) export more than they generate; clamped to 0: ",
          paste(unique(ww_treat_negative$county), collapse = ", "))
  dir.create(QC_DIR, recursive = TRUE, showWarnings = FALSE)
  write_csv(ww_treat_negative, file.path(QC_DIR, "ww_treatment_negative.csv"))
}
df_ww_tobetreated <- df_ww_tobetreated %>%
  mutate(value = pmax(value, 0)) %>%
  select(county, year, target, value)

# check for more wastewater to be treated that permit capacity
total_permit_cap <- df_wastewater_treatment %>%
  group_by(county) %>%
  summarise(total_permit_capacity = sum(permitted_capacity), .groups = "drop") %>%
  left_join(df_ww_tobetreated %>% select(county, year, value), by = "county") %>%
  mutate(cap_left = total_permit_capacity - value)


# check if there is enough permit capacity for wastewater treatment
if (any(total_permit_cap$cap_left < 0)) {
  cat(paste0("Not enough permit capacity for wastewater treatment in \n",
               unique(paste(total_permit_cap$county[total_permit_cap$cap_left < 0])),
             "\n for plants: ", paste(unique(df_wastewater_treatment$facility_name[df_wastewater_treatment$county %in% total_permit_cap$county[total_permit_cap$cap_left < 0]]), collapse = ", ")
             # " in year(s): ", paste(total_permit_cap$year[total_permit_cap$cap_left < 0], collapse = ", ")
             ))

  # top treatment plants and counties with highest exceeded capacity
  cat(paste0("\nTop treatment plants and counties with highest exceeded capacity:\n",
             paste(head(arrange(total_permit_cap %>%
                                 left_join(df_wastewater_treatment %>% select(county, facility_name), by = "county") %>%
                                 select(county, facility_name, total_permit_capacity, value, cap_left),
                               cap_left), 20),
                   collapse = "\n")))

}


# calculate treatment fractions by facility using average
# fraction represents the share of each facility in the county's total treatment
# fraction = facility's average historic treatment / sum of all facilities' average in the county
# can change to 75%ile or other percentiles if needed
df_wastewater_treatment_fracs <- df_wastewater_treatment %>%
  select(county, facility_name, average) %>% # can change to percentiles here
  group_by(county) %>%
  mutate(total_treatment_hist = sum(average),
         treatment_fraction = average / total_treatment_hist) %>%
  replace_na(list(treatment_fraction = 0)) %>% # replace NAs with 0
  ungroup()
# ExtraNotes: a county's collected sewage is split between its plants in proportion to their
# historic average throughput, not their permitted capacity. Average throughput reflects the
# service areas actually connected to each plant, whereas permitted capacity reflects headroom
# for future growth and would over-assign flow to recently expanded plants. Capacity is used
# instead for the DISPOSAL split further down, where it is the only available basis.
# Consequence: facility-level volumes are modelled allocations, not reported measurements, so
# they are appropriate for system structure but not for regulatory comparison per plant.

# ANALYSIS: treatment capacity utilisation and concentration. Headroom against permit is a
# planning question the diagram itself cannot answer, and the Herfindahl index over facility
# shares says whether a county depends on one plant or spreads risk across several. Both need
# the facility-level allocation that exists only at this point in the pipeline.
if (ANALYSIS) {
  treat_util <- df_wastewater_treatment_fracs %>%
    left_join(df_wastewater_treatment %>%
                select(county, facility_name, permitted_capacity, level_of_treatment),
              by = c("county", "facility_name")) %>%
    left_join(df_ww_tobetreated %>% filter(year %in% YEARS_TO_ENSURE) %>%
                select(county, year, county_flow = value), by = "county",
              relationship = "many-to-many") %>%
    mutate(assigned = county_flow * treatment_fraction,
           utilisation_pct = 100 * assigned / permitted_capacity) %>%
    filter(!is.na(year))
  write_csv(treat_util, file.path(ANALYSIS_DIR, "P3_treatment_utilisation.csv"))

  treat_conc <- df_wastewater_treatment_fracs %>%
    group_by(county) %>%
    summarise(facilities = n(),
              hhi = sum(treatment_fraction^2),
              largest_share_pct = 100 * max(treatment_fraction), .groups = "drop") %>%
    arrange(desc(hhi))
  write_csv(treat_conc, file.path(ANALYSIS_DIR, "P4_treatment_concentration.csv"))
  message("  ANALYSIS: treatment concentration, most concentrated county: ",
          treat_conc$county[1], " (HHI ", round(treat_conc$hhi[1], 2), ", ",
          treat_conc$facilities[1], " plants)")
}

# apply treatment fractions by facility to wastewater volumes
df_wastewater_treated <- df_ww_tobetreated %>%
  left_join(df_wastewater_treatment_fracs, by = c("county")) %>%
  mutate(treated = value * treatment_fraction) %>%
  left_join(df_wastewater_treatment %>% select(county, facility_name, permitted_capacity, level_of_treatment),
            by = c("county", "facility_name")) %>%
  mutate(treatment_cap_left = if_else(level_of_treatment == "REUSE", 0, permitted_capacity - treated))

# warning if a county's treated exceeds it's permit capacity
if (any(df_wastewater_treated$treatment_cap_left > 0)) {
  cat(paste0("\n\nTreated wastewater exceeds permitted capacity in ",
                 unique(paste(df_wastewater_treated$county[df_wastewater_treated$treated > df_wastewater_treated$permitted_capacity])), " in year(s): ",
                 paste(unique(df_wastewater_treated$year[df_wastewater_treated$treated > df_wastewater_treated$permitted_capacity]), collapse = ", "), " for facilities: "
                 , paste(unique(df_wastewater_treated$facility_name[df_wastewater_treated$treated > df_wastewater_treated$permitted_capacity]),collapse = ", \n")
                 ))
}

df_wastewater_treated_ <- df_wastewater_treated %>%
  select(county, year, source=target, target=facility_name, value=treated)

# collapse all treatment plants in a county to one node for the all counties plot
df_wastewater_treated_agg <- df_wastewater_treated %>%
  group_by(county, year, source=target) %>%
  summarise(value = sum(treated), .groups = "drop") %>%
  mutate(target = county) # target is county

df_wastewater_treated_one_node <- df_wastewater_treated_agg %>% mutate(target = "wastewater_treated")

# plotting ----

# df_sankey <- rbind(df_water_sup_waste, df_water_losses, df_wastewater_septic, df_water_i_i, df_wastewater_treated_)
df_sankey <- rbind(df_water_sup_waste_fix, df_water_losses_fix, df_wastewater_septic, df_water_i_i, df_wastewater_treated_) %>%
  mutate(units = "MGD")
if (MAKE_PLOT) plot_sankey(df_sankey)

# single county (all)
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey, reg = "Bartow", show_values_in_labels = TRUE, animate = T)
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey, reg = "Bartow")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey, reg = "Bartow", year = 2024, show_values_in_labels = TRUE, animate = F)
# plot_sankey(df_sankey, reg = "Cherokee")
# plot_sankey(df_sankey, reg = "Clayton")
# plot_sankey(df_sankey, reg = "Cobb")
# plot_sankey(df_sankey, reg = "Coweta")
# plot_sankey(df_sankey, reg = "DeKalb")
# plot_sankey(df_sankey, reg = "Douglas")
# plot_sankey(df_sankey, reg = "Fayette")
# plot_sankey(df_sankey, reg = "Forsyth")
# plot_sankey(df_sankey, reg = "Fulton")
# plot_sankey(df_sankey, reg = "Gwinnett")
# plot_sankey(df_sankey, reg = "Hall")
# plot_sankey(df_sankey, reg = "Henry")
# plot_sankey(df_sankey, reg = "Paulding")
if (MAKE_PLOT) plot_sankey(df_sankey, reg = "Rockdale")

# save the county sankeys
if (F) {
  # create directory if it doesn't exist
  if (!dir.exists("water_counties")) {
    dir.create("water_counties")
  }

  for (county in counties) {
    p <- plot_sankey(df_sankey, reg = county) # create the plot

    # save as HTML
    filename <- paste0("water_counties/", county, "_sankey.html")
    saveWidget(p, filename, selfcontained = TRUE)

    cat("Saved:", filename, "\n")
  }
}

# all counties as end nodes
df_sankey_allc <- rbind(df_water_sup_waste_fix, df_water_losses_fix, df_wastewater_septic, df_water_i_i, df_wastewater_treated_agg)
if (MAKE_PLOT) plot_sankey(df_sankey_allc)


# just categories, no detail
df_sankey_agg <- rbind(df_water_sup_waste_fix, df_water_losses_fix, df_wastewater_septic, df_water_i_i, df_wastewater_treated_one_node) %>%
  select(-county) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD") %>% pretty_labels()


if (MAKE_PLOT) plot_sankey(df_sankey_agg)
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_agg, show_values_in_labels = TRUE, animate = T, label_units = "MGD")



###############################################################################%
# imports
ww_imports <- df_ww_conn_trade %>%
  mutate(fromcounty_fromplace = paste("inFrom", fromcounty, fromplace, sep = "_")) %>%
  select(county = tocounty, source = fromcounty_fromplace, target = tofacility, year, import = trade)

# plot_sankey(rbind(df_sankey, ww_imports %>% rename(value = import)), reg = "Cobb")

# exports
# ExtraNotes: aggregated rather than deduplicated. Dropping `fromplace` makes two genuinely
# distinct origins within the same county that happen to send equal volumes to the same
# facility look like one row, and discarding one would silently lose real flow. Duplicate
# reporting between county sheets is already resolved upstream, so summing here is safe and
# keeps the export total consistent with the net-trade figure used for the treatment split.
ww_exports <- df_ww_conn_trade %>%
  mutate(source = "wastewater") %>%
  group_by(county = fromcounty, source, target = tofacility, year) %>%
  summarise(export = sum(trade), .groups = "drop")

ww_exports_track <- df_ww_conn_trade %>%
  mutate(source = "wastewater") %>%
  mutate(tocounty_tofacility = paste("outTo" , tocounty, tofacility, sep = "_")) %>%
  group_by(county = fromcounty, source, target = tocounty_tofacility, year) %>%
  summarise(export = sum(trade), .groups = "drop")

# plot_sankey(rbind(df_sankey, ww_exports %>% rename(value = export)), reg = "Cobb")
# plot_sankey(rbind(df_sankey, ww_exports_track %>% rename(value = export)), reg = "Cobb")

# TODO: why is wastewater treated + export is more than wastewater generated.
# what if I add imports to wastewater generated?
# the aggregated plots looks balanced

# combine imports and exports
# but don't use with except for plotting counties because these are double
# counted i.e., exports of on county are imports to another county
ww_trade <- rbind(ww_imports %>% rename(value = import) %>% mutate(trade_type = "ww_imports"),
                  ww_exports %>% rename(value = export) %>% mutate(trade_type = "ww_exports"))

ww_trade_track <- rbind(ww_imports %>% rename(value = import) %>% mutate(trade_type = "ww_imports"),
                        ww_exports_track %>% rename(value = export) %>% mutate(trade_type = "ww_exports"))


ww_trade_comb <- ww_trade %>% select(-trade_type) %>%
  mutate(units = "MGD") # for doing calculations on facilities (preserves names)
ww_trade_comb_track <- ww_trade_track %>% select(-trade_type) %>%
  mutate(units = "MGD") # for plotting counties

# plot all counties
df_sankey_wwtrade_c_f <- rbind(df_sankey, ww_trade_comb_track)

if (MAKE_PLOT) plot_sankey(df_sankey_wwtrade_c_f)
if (MAKE_PLOT) plot_sankey(df_sankey_wwtrade_c_f, reg = "Cobb")
# plot_sankey(df_sankey_wwtrade_c_f, reg = "DeKalb")
# plot_sankey(df_sankey_wwtrade_c_f, reg = "Fulton")
# plot_sankey(df_sankey_wwtrade_c_f, reg = "Douglas")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_wwtrade_c_f, reg = "Douglas", show_values_in_labels = TRUE, animate = T, label_units = "MGD")

# save the county sankeys
if (F) {
  for (county in counties) {
    p <- plot_sankey(df_sankey_wwtrade_c_f, reg = county) # create the plot

    # save as HTML
    filename <- paste0("water_counties/", county, "_sankey_ww.html")
    saveWidget(p, filename, selfcontained = TRUE)

    cat("Saved:", filename, "\n")
  }
}


# counties aggregated ----
ww_trade_agg <- ww_trade %>%
  group_by(trade_type, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = if_else(trade_type == "ww_imports", "ww_imports", "wastewater"),
         target = if_else(trade_type == "ww_imports", "in-county treatment", "ww_exports")) %>%
  select(source, target, year, value)
# ExtraNotes: imported sewage arrives by pipe at a named treatment plant, so it targets the
# treatment node rather than the receiving county's collection system - matching the
# county-level view, where transfers are wired straight to the facility. Routing it through the
# collection node instead would make that node's inflow include water its own sectors never
# generated, while the discharge side would still be measured at the plant.

df_sankey_wwtrade <- df_sankey_agg <- rbind(df_water_sup_waste_fix, df_water_losses_fix, df_wastewater_septic, df_water_i_i,
                                            df_wastewater_treated_one_node %>% mutate(target = "in-county treatment")) %>%
  select(-county) %>% rbind(ww_trade_agg) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD")

if (MAKE_PLOT) plot_sankey(df_sankey_wwtrade %>% pretty_labels())

if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_wwtrade %>% pretty_labels(),
                     show_values_in_labels = TRUE, animate = T, label_units = "MGD")


###############################################################################%

# thermoelectric ----

thermoplants_water_use <- read_csv(paste0(DATA_DIR, "water_thermoplants.csv")) %>% clean_names() %>%
  # assign basins based on river names
  mutate(basin = case_when(grepl("Chattahoochee", water_source, ignore.case = TRUE) ~ "Chattahoochee Basin",
                           grepl("Etowah", water_source, ignore.case = TRUE) ~ "Coosa_Etowah Basin",
                           TRUE ~ "Other"))


# thermoelectric water use
energy_water_use_w <- thermoplants_water_use %>% filter(usetype == "withdrawal") %>%
  select(county, year, source = basin, target = facility_name, value, units)

# losses = consumption
energy_water_use_c <- thermoplants_water_use %>% filter(usetype == "consumption") %>%
  mutate(target = "losses") %>%
  select(county, year, source = facility_name, target, value, units)

# discharge = withdrawal - consumption
energy_water_use_d <- thermoplants_water_use %>%
  filter(usetype == "consumption") %>% mutate(target = "discharge") %>%
  select(county, year, source = facility_name, target, value, units) %>%
  left_join(energy_water_use_w %>% select(county, year, target, withdrawal = value),
            by = c("county", "year", "source" = "target")) %>%
  mutate(value = withdrawal - value) %>%
  select(county, year, source, target, value, units)


thermoelec_water_use <- rbind(energy_water_use_w, energy_water_use_c, energy_water_use_d) %>% pretty_labels()
# ExtraNotes: withdrawal is split into consumption (evaporated in the cooling tower, routed to
# `losses`) and discharge (returned to the river, warmer). Discharge is derived as withdrawal
# minus consumption, so each plant closes exactly by construction and the plant node cannot be
# used as an independent check on the reported figures. The withdrawal-to-consumption ratio is
# what distinguishes cooling technology: once-through plants withdraw far more than they
# consume, recirculating towers withdraw little and consume most of it.

if (MAKE_PLOT) plot_sankey_enhanced(rbind(df_sankey_wwtrade, thermoelec_water_use %>% select(!c(county))))


if (F) {
  # plot year value for each facility
  ggplot(data = thermoplants_water_use %>% select(county, facility_name, year, usetype, value, units) %>%
           rbind(energy_water_use_d %>% rename(usetype = target, facility_name = source)) %>%
           mutate(usetype = factor(str_to_title(usetype), levels = c("Withdrawal","Consumption","Discharge")))) +
    geom_line(aes(x = year, y = value, color = facility_name), linewidth = 1) +
    geom_point(aes(x = year, y = value, fill = facility_name, shape = facility_name), size = 2.5, color = "transparent", alpha = 0.6) +
    geom_line(aes(x = year, y = value, color = facility_name), linewidth = 1, linetype = "dashed") +
    geom_point(aes(x = year, y = value, fill = facility_name, shape = facility_name), size = 2.5, color = "transparent", alpha = 0.6) +
    facet_grid(. ~ usetype) +
    scale_x_continuous(breaks = seq(min(thermoplants_water_use$year), max(thermoplants_water_use$year), by = 3)) +
    scale_color_manual(values = c("Bowen" = "gray40", "Jack McDonough" = "dodgerblue2", "Yates" = "dodgerblue2")) +
    scale_fill_manual(values = c("Bowen" = "red3", "Jack McDonough" = "gold", "Yates" = "green3")) +
    scale_shape_manual(values = c("Bowen" = 21, "Jack McDonough" = 22, "Yates" = 23)) +
    labs(x = "Year", y = "Thermoelectric Water Use (MGD)",
         color = "Facility", shape = "Facility", fill = "Facility") +
    mytheme +
    theme(legend.position = c(0.9, 0.85),
          legend.box.background = element_rect(colour = "gray60", size = 0.1),
          legend.spacing = unit(0.001, "cm"),
          legend.key.height = unit(0.4, "cm"),
          strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 0))
}


###############################################################################%

# management plan data ----
# to cover missing flows. like the water source (surface water, groundwater)
# etc, or self-supplied water for various uses


## surface water sources ----
# only 2019 data so need to use 2019 shares to determine each year's flows
p_water_mgmtplan_surface <- read_csv(paste0(DATA_DIR, "water_mgmtplan_surface.csv")) %>% clean_col_names()


mgmtplan_surface <- p_water_mgmtplan_surface %>%
  # all SW to PWS; GW and self-supply will follow
  mutate(year = 2019, target = "publicWatSup", units = "MGD") %>%
  select(county, year, basin, source = water_supply_source, target, value = actual_annual_average_withdrawals_2019_mgd, units) %>%
  mutate(value = replace_na(value, 0)) %>%
  # aggregate up like 5 repetitions; which are actually different flows owned by different entities, but we're not tracking that
  group_by(county, year, basin, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop") %>% select(!year) %>%
  expand_grid(year = 2019:2025) %>% # expand to all years
  select(county, year, basin, source, target, value, units)
# ExtraNotes: the basin attribution of withdrawals is observed for 2019 ONLY and is copied to
# every year, so the basin composition of supply is fixed by construction. Interannual change in
# a basin's withdrawal reflects only the change in county demand, not any shift between sources.
# The volumes here are overwritten later by the public-supply balancing step, which rescales
# these surface inflows to match measured outflow; what survives from this table is the SHARE
# between basins, which is its real contribution.
# ExtraNotes: multiple intakes on the same basin are summed. They are separately permitted and
# separately owned, and that ownership detail is deliberately not tracked, so the diagram cannot
# speak to who holds which withdrawal right.


# NOTE: experimenting with making basins as intermediate nodes
# decision: do it only for the aggregated diagram, not for county level diagrams -> done later before energy-for-water
# create a surface water node for all the basins
# mgmtplan_surfaceWsrc_pws <- mgmtplan_surface %>%
#   group_by(county, year, basin, units) %>%
#   summarise(value = sum(value), .groups = "drop") %>%
#   mutate(source = "surfaceWater", target = basin) %>%
#   filter(value > 0) %>% # drop zero to avoid duplication
#   select(county, year, basin, source, target, value, units)

{ # create a basin county mapping
  mapping_basin_county <- mgmtplan_surface %>% select(county, basin, value) %>%
    # give minimal share to each county-basin share
    mutate(value = if_else(value == 0, 2, value)) %>%
    group_by(county, basin) %>% summarise(value = sum(value), .groups = "drop")

  # Read as: county X gets Y% of water from Z basin
  mapping_basin_county_byC <- mapping_basin_county %>%
    # making a basin-county coverage ratio based on use. Ideally should be based on area
    group_by(county) %>% mutate(county_basin_share = value / sum(value)) %>% ungroup() %>%
    arrange(county)

  # Read as: basin X gives Y% of water to county Z
  mapping_basin_county_byB <- mapping_basin_county %>%
    # making a basin-county coverage ratio based on use. Ideally should be based on area
    group_by(basin) %>% mutate(county_basin_share = value / sum(value)) %>% ungroup() %>%
    arrange(basin)
  }
# ExtraNotes: the county-basin overlap is weighted by withdrawal volume, not by land area, so a
# county's dominant basin is where it draws most water rather than where most of its territory
# lies. That is the correct weighting for allocating withdrawals but the wrong one for anything
# hydrological. Zero-volume county-basin pairs are given a nominal weight so a county that
# overlaps a basin without currently withdrawing from it still receives a small share, which
# keeps the mapping usable for the permitted self-supply split that has no county detail.


# no data on conveyance losses so all water leaving a water body is assumed to be used
# TODO: use each 2019 ratios to split up each year's sources. first compare if
# the supply data = use data above. if not we may want to just split up use
# using the basin ratios from here

if (MAKE_PLOT) plot_sankey_enhanced(rbind(df_sankey_wwtrade, mgmtplan_surface %>% select(!c(basin, county))))
# plot_sankey_enhanced(rbind(df_sankey_wwtrade, mgmtplan_surfaceWsrc_pws %>% select(!c(basin, county)), mgmtplan_surface %>% select(!c(basin, county))))

## groundwater sources ----
# only 2019 data so need to use 2019 shares to determine each year's flows
p_water_mgmtplan_ground <- read_csv(paste0(DATA_DIR, "water_mgmtplan_ground.csv")) %>% clean_col_names()

# mgmtplan_ground <- p_water_mgmtplan_ground %>%
#   # all GW to self-supply; SW and PWS will follow
#   # TODO: need to use 2019 shares to determine each year's flows
#   mutate(year = 2020, target = "groundwater", units = "MGD",
#          source = paste0(basin, "_GW")) %>%
#   select(county, year, basin, source, target, value = actual_monthly_average_withdrawals_2019_mgd, units) %>%
#   mutate(value = replace_na(value, 0)) %>%
#   # removing like 4 repetitions; which are actually different flows owned by different entities, but we're not tracking that
#   group_by(county, year, basin, source, target, units) %>%
#   summarise(value = sum(value), .groups = "drop") %>% pretty_labels()

# revising to make GW the source and PWS the target, which is what the report says.
# will lose basins here
mgmtplan_ground <- p_water_mgmtplan_ground %>%
  # TODO: need to use 2019 shares to determine each year's flows
  mutate(year = 2019, source = "groundwater", target = "publicWatSup", units = "MGD") %>%
  select(county, year, basin, source, target, value = actual_monthly_average_withdrawals_2019_mgd, units) %>%
  mutate(value = replace_na(value, 0)) %>%
  # removing like 4 repetitions; which are actually different flows owned by different entities, but we're not tracking that
  group_by(county, year, basin, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop") %>% select(!year) %>%
  expand_grid(year = 2019:2025) %>% # expand to all years
  select(county, year, basin, source, target, value, units)

if (MAKE_PLOT) plot_sankey(mgmtplan_ground, animate = T)

# Notes: major difference between Atlanta region commission data for
# 'self-supply' use 45.9 , which is assumed to be from groundwater, and this
# data from the report for groundwater supply 3.6.
# Actually, this is just GW; I will bring in self-supply after this and compare
# again. Also, the 45.9 could include SW self-supply for Ag.



## self supply sources ----
# mostly industrial and golf irrigation -? going to assign all use to industrial
# only permitted data, not actual withdrawals, so will just assume 0.85 of permit of permitted is used
# need to breakout by counties -> based on industrial use by county
p_water_mgmtplan_self <- read_csv(paste0(DATA_DIR, "water_mgmtplan_self.csv")) %>% clean_col_names()

PERMIT_USE_FACTOR <- 0.85
# ExtraNotes: industrial and golf-course self-supply is reported as PERMITTED withdrawal, not
# actual, so it is scaled to 85% of permit. Permit holders routinely withdraw below their limit,
# and using the permit unscaled would overstate industrial water. The factor is an assumption,
# not a measurement: it is the largest single judgement call in the water inputs and industrial
# self-supply should be treated as an order-of-magnitude estimate.

mgmtplan_self <- p_water_mgmtplan_self %>%
  # all self-supply to industrial
  mutate(target = "industrial", units = "MGD",
         value = monthly_average_day_permitted_withdrawal_mgd * PERMIT_USE_FACTOR) %>%
  select(basin, basin, target, value, units) %>%
  mutate(value = replace_na(value, 0))


# split industrial self supply by county based on the surface water withdrawals data by basin and counties (I added the counties)
mgmtplan_self_c <- mgmtplan_self %>%
  left_join(mapping_basin_county_byB, by = "basin") %>%
  mutate(value = value.x * county_basin_share)

# sw gw split
SW_GW_IND <- 0.65 # 65% surface water
# ExtraNotes: the permitted self-supply file does not distinguish surface from groundwater, so a
# fixed 65/35 split is imposed. Self-supplied industry sits on the same rivers as public supply
# but also runs wells where an aquifer is productive, so neither extreme is defensible; the split
# matters only for which source node the flow attaches to, not for the total.

mgmtplan_self_c_s <- mgmtplan_self_c %>%
  mutate(value = value * SW_GW_IND, source = basin) %>%
  bind_rows(mgmtplan_self_c %>%
              # mutate(value = value * (1-SW_GW_IND), source = paste0(basin, "_GW"))) %>%
              # revising to make GW the source and industrial the target, to simplify. We will lose basins here
              mutate(value = value * (1-SW_GW_IND), source = "groundwater")) %>%
  select(county, source, target, value, units) %>%
  mutate(basin = source)

# expand to all years
mgmtplan_self_c_s_y <- map_df(2019:2025, # add a year column; copy the data from 2019 to 2025
       ~ mgmtplan_self_c_s %>% mutate(year = .x))


###############################################################################%

## wastewater sinks mgmt plan ----
water_mgmtplan_wastewater <- read_csv(paste0(DATA_DIR, "water_mgmtplan_wastewater.csv")) %>% clean_col_names()

ww_allfacilities <- read_csv(paste0(DATA_DIR, "common_ww_allfacilities_mapping.csv")) %>% clean_col_names()

# prepare a mapping of wastewater treatment facilities to receiving water bodies
water_mgmtplan_wastewater_map <- water_mgmtplan_wastewater %>%
  right_join(ww_allfacilities %>% select(county, facility_name, matched_target),
            by = c("county", "wastewater_treatment_facilities" = "facility_name")) %>%
  rename(source = matched_target, target = receiving_water_body, permit = permitted_treatment_capacity_2021_mmf_mgd) %>%
  group_by(source) %>%
  mutate(# for each group of identical 'source'
    target = ifelse(is.na(target), target[!is.na(target)][1], target),
    permit = ifelse(is.na(permit), permit[!is.na(permit)][1], permit),
    basin = ifelse(is.na(basin), basin[!is.na(basin)][1], basin),
    receiving_type = ifelse(is.na(receiving_type), receiving_type[!is.na(receiving_type)][1], receiving_type)) %>%
  ungroup() %>%
  select(county, basin, facility_name = source, target, receiving_type, permit) %>%
  left_join(df_wastewater_treatment %>% select(facility_name, permitted_capacity)) %>%
  arrange(facility_name)

# wrote and filled a bunch of data from online
# if (F) {write_csv(water_mgmtplan_wastewater_map, "common_ww_facility_sink_map_initial.csv")}

# read the filled data
ww_facility_sink_map <- read_csv(paste0(DATA_DIR, "common_ww_facility_sink_map.csv"))

# calculate disposal shares to a sink from each facility
# ExtraNotes: grouped by (county, facility) so the shares split a facility's effluent across
# ITS OWN receiving water bodies. Grouping by facility alone would spread the share across the
# contributing counties a facility serves, which is a different dimension entirely and cannot
# be used as a disposal split. Permitted capacity is the apportioning basis.
ww_facility_sink_map_s <- ww_facility_sink_map %>%
  # ExtraNotes: deduplicate BEFORE computing shares, otherwise collapsing rows afterwards
  # leaves the shares no longer summing to one.
  distinct() %>%
  mutate(permit = replace_na(permit, 0),
         permitted_capacity = replace_na(permitted_capacity, 0),
         permit_adj = pmax(permitted_capacity, permit)) %>%
  group_by(county, facility_name) %>%
  mutate(total_permit = sum(permit_adj),
         # a facility whose permit figures are all zero still has to send its effluent
         # somewhere: fall back to an even split across its receiving bodies
         disposal_share = if_else(total_permit > 0, permit_adj / total_permit, 1 / n())) %>%
  ungroup() %>%
  select(-total_permit)
stopifnot(all(abs(
  ww_facility_sink_map_s %>% group_by(county, facility_name) %>%
    summarise(s = sum(disposal_share), .groups = "drop") %>% pull(s) - 1) < 1e-9))

# calculate wastewater sink flows
ww_sink <- df_sankey %>% filter(source == "wastewater") %>%
  rbind(ww_trade_comb %>% filter(grepl("inFrom", source))) %>% # bring in ww trade inflows
  select(county, year, source = target, value, units) %>%
  # to get sinks based on facility (and shares)
  left_join(ww_facility_sink_map_s, by = c("county", "source" = "facility_name")) %>%
  # apportion across the facility's receiving water bodies
  mutate(value = value * disposal_share) %>%
  select(county, year, basin, source, target, value, units) %>%
  group_by(county, basin, year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop")
# ExtraNotes: a facility mapped to several receiving water bodies must have its effluent
# apportioned between them; emitting the full volume to each would multiply the discharge.

if (MAKE_PLOT) plot_sankey(ww_sink)

# make it all linear by assigning downstream _ds to all sinks
ww_sink_downstream <- ww_sink %>%
  mutate(target = if_else(grepl("River|Stream|Creek|Lake|Reservoir|Basin|Branch", target),
                          paste0(target, "_ds"), target))

if (MAKE_PLOT) ww_sink_downstream %>% filter(!grepl("_ds", target)) %>% plot_sankey(animate = T)

if (MAKE_PLOT) plot_sankey(ww_sink_downstream)


## all management plan data ----
mgmtplan_all <- rbind(mgmtplan_surface, mgmtplan_ground, mgmtplan_self_c_s_y, ww_sink)

mgmtplan_all_downstream <- rbind(mgmtplan_surface, mgmtplan_ground, mgmtplan_self_c_s_y, ww_sink_downstream)

if (MAKE_PLOT) plot_sankey(mgmtplan_all)
if (MAKE_PLOT) plot_sankey(mgmtplan_all_downstream)

# plot_sankey(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)))

# plot_sankey_enhanced(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)) %>% pretty_labels())
if (MAKE_PLOT) plot_sankey_enhanced(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)) %>% pretty_labels(), reg = "Cobb")
if (MAKE_PLOT) plot_sankey_enhanced(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)) %>% pretty_labels(), reg = "Gwinnett")


###############################################################################%

df_sankey_ww_mgmt_C <- rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all_downstream %>% select(-basin))

# industrial discharge ----
# assign the difference between supply and use of industrial to discharge
industrial_discharge <- df_sankey_ww_mgmt_C %>%
  # supply total
  filter(grepl("industrial|Industrial", target)) %>%
  group_by(county, year) %>%
  summarise(industrial_supply = sum(value), .groups = "drop") %>%
  # use total
  left_join(df_sankey_ww_mgmt_C %>%
              filter(grepl("industrial|Industrial", source)) %>%
              group_by(county, year) %>%
              summarise(industrial_use = sum(value), .groups = "drop"), by = c("county", "year")) %>%
  replace_na(list(industrial_use = 0)) %>%
  mutate(value = industrial_supply - industrial_use,
         source = "industrial", target = "discharge", units = "MGD") %>%
  select(county, year, source, target, value, units)
# ExtraNotes: industry receives water from public supply, its own surface intakes and its own
# wells, but reports wastewater only where it discharges to a sewer. The unreturned remainder is
# sent to `discharge` as permitted direct discharge to a water body. Without this term the
# industrial node stays open by the whole self-supply volume, since self-supplied industry has
# no reason to appear in a sewer utility's records at all.



###############################################################################%

# COUNTY level ----
df_sankey_ww_mgmt_C_ind <- rbind(df_sankey_ww_mgmt_C, industrial_discharge)

# plot_sankey(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all_downstream %>% select(-basin)))
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind)
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Cobb")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Cobb", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Gwinnett", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Fulton", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "DeKalb", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Douglas", show_values_in_labels = TRUE, animate = T, label_units = "MGD")


# balance PWS ----

# Public water supply is closed by scaling its SURFACE inflows to match total outflows.
# Groundwater withdrawals are reported directly by the management plans and are treated as
# measured, so they are held fixed and surface water absorbs the residual: surface is scaled
# to (total outflow - groundwater), which makes surface + groundwater equal outflow exactly.
# Scaling surface to the full outflow instead would leave groundwater as an unmatched extra
# inflow and the node open by exactly the groundwater volume.

pws_out <- df_sankey_ww_mgmt_C_ind %>%
  filter(grepl("publicWatSup", source)) %>%
  group_by(county, year) %>%
  summarise(pws_out = sum(value), .groups = "drop")

pws_in_ground <- df_sankey_ww_mgmt_C_ind %>%
  filter(source == "groundwater", grepl("publicWatSup", target)) %>%
  group_by(county, year) %>%
  summarise(pws_in_ground = sum(value), .groups = "drop")

mgmtplan_surface_pws_scaled <- df_sankey_ww_mgmt_C_ind %>%
  filter(year >= 2020) %>%
  filter(source != "groundwater") %>% # groundwater is held fixed, not scaled
  filter(grepl("publicWatSup", target)) %>%
  group_by(county, year, target) %>%
  mutate(source_share = value / sum(value)) %>% ungroup() %>%
  left_join(pws_out, by = c("county", "year")) %>%
  left_join(pws_in_ground, by = c("county", "year")) %>%
  mutate(pws_in_ground = replace_na(pws_in_ground, 0),
         # a county drawing more groundwater than it supplies would imply negative surface
         # withdrawal; clamp and let the groundwater figure stand
         pws_surface_target = pmax(pws_out - pws_in_ground, 0),
         pws_in_scaled = source_share * pws_surface_target) %>%
  select(county, year, source, target, value = pws_in_scaled, units)

# ANALYSIS: the size of the supply-demand reconciliation, per county. The scaling factor is a
# direct measure of how far the withdrawal records and the demand records disagree before they
# are forced to close, so it is the best available indicator of input data quality per county.
# It exists only at this step: after the rebuild the node balances and the discrepancy is gone.
if (ANALYSIS) {
  pws_scaling <- df_sankey_ww_mgmt_C_ind %>%
    filter(year %in% YEARS_TO_ENSURE, grepl("publicWatSup", target), source != "groundwater") %>%
    group_by(county, year) %>%
    summarise(surface_reported = sum(value), .groups = "drop") %>%
    left_join(pws_out, by = c("county", "year")) %>%
    left_join(pws_in_ground, by = c("county", "year")) %>%
    mutate(pws_in_ground = replace_na(pws_in_ground, 0),
           surface_needed = pmax(pws_out - pws_in_ground, 0),
           # A county that reports essentially no surface withdrawal while supplying real volumes
           # makes the ratio meaningless, so the absolute gap is the primary measure and the
           # percentage is reported only where there is a meaningful base. Paulding is the case:
           # it reports ~0 surface inflow to public supply yet supplies about 15 MGD, which is a
           # reporting gap rather than a scaling adjustment.
           abs_gap_mgd = surface_needed - surface_reported,
           scale_factor = if_else(surface_reported > 0.5, surface_needed / surface_reported,
                                  NA_real_),
           adjustment_pct = if_else(surface_reported > 0.5,
                                    100 * abs_gap_mgd / surface_reported, NA_real_),
           flag = if_else(surface_reported <= 0.5, "no surface withdrawal reported", ""))
  write_csv(pws_scaling, file.path(ANALYSIS_DIR, "P5_pws_supply_demand_reconciliation.csv"))
  message("  ANALYSIS: supply-demand reconciliation, median |adjustment| ",
          round(median(abs(pws_scaling$adjustment_pct), na.rm = TRUE), 1),
          "%, ", sum(pws_scaling$flag != ""), " county-year(s) report no surface withdrawal")
}

# check total surface water supply across all counties before scaling
df_sankey_ww_mgmt_C_ind %>%
  filter(year == 2024) %>%
  filter(grepl("surfaceWater", source) | grepl("publicWatSup", target)) %>%
  summarise(total_sw_supply = sum(value))

# check total PWS before and after scaling for 2024
df_sankey_ww_mgmt_C_ind %>%
  filter(year == 2024) %>%
  filter(grepl("publicWatSup", source) | grepl("publicWatSup", target)) %>%
  group_by(year, flow_type = if_else(grepl("publicWatSup", source), "outflow", "inflow")) %>%
  summarise(total_pws = sum(value), .groups = "drop") %>%
  pivot_wider(names_from = flow_type, values_from = total_pws) %>%
  replace_na(list(inflow = 0, outflow = 0)) %>%
  mutate(diff = inflow - outflow)


# NOTE: the code above fixes this. Keeping this here in case need to check imbalances again
# check imbalances in PWS flows by county, year
#  calculate total pwsInflows, total pwsOutflows, and their difference
#  if difference > 0, add a source node "unaccountedPWSsource" to pws
#  if difference < 0, add a target node "unaccountedPWSsink"
#  for negatives, assign the target to "Discharge". this could be done now
#  for positives, redistribute to sources proportionally. this is more complex
# this has a flaw of assuming all unaccounted water goes to discharge, which may not be true.

# pws_diff <- df_sankey_water_county %>%
#   filter(grepl("publicWatSup", target)) %>%
#   group_by(county, year) %>%
#   summarise(total_inflow = sum(value), .groups = "drop") %>%
#   left_join(df_sankey_water_county %>%
#               filter(grepl("publicWatSup", source)) %>%
#               group_by(county, year) %>%
#               summarise(total_outflow = sum(value), .groups = "drop"),
#             by = c("county", "year")) %>%
#   # extrapolate NAs using approx
#   group_by(county) %>%
#   mutate(total_outflow = zoo::na.approx(total_outflow, year, rule = 2)) %>%
#   ungroup() %>%
#   mutate(diff = total_inflow - total_outflow,
#          meaning = case_when(diff > 0 ~ "source_unaccountedPWS",
#                              diff < 0 ~ "sink_unaccountedPWS",
#                              TRUE ~ "balanced"),
#          source = if_else(diff > 0, "unaccountedPWSsource", "publicWatSup"),
#          target = if_else(diff > 0, "publicWatSup", "discharge"),
#          value = abs(diff),
#          units = "MGD") # %>% select(county, year, source, target, value, units)

# plot_sankey_enhanced(pws_diff, reg = "Fulton", show_values_in_labels = TRUE, animate = T, label_units = "MGD")

# linear:
# this is truly the core table for water with all details
# (source water bodies, ww facilities, ww transfers, discharge water bodies etc)
df_sankey_county_pws_balanced <- df_sankey_ww_mgmt_C_ind %>%
  # remove original pws surface inflows; pws target where source is not groundwater
  filter(!(grepl("publicWatSup", target) & source != "groundwater")) %>%
  rbind(mgmtplan_surface_pws_scaled) %>% # add balanced pws inflows
  filter(year %in% YEARS_TO_ENSURE) %>%
  # collapse to one row per flow
  group_by(county, year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop")
validate_flows(df_sankey_county_pws_balanced, "water_county_flows", strict_years = TRUE)
# ExtraNotes: clamped to the study period at publication. The management-plan and connection
# tables extend well beyond it, and because the diagrams filter on year those extra rows are
# invisible in the artefacts while still bloating the published CSV. The wider series remains
# available upstream for trend work.
#
# ExtraNotes: collapsed to one row per flow. Several terms are derived per sector or per basin
# and then lose that disaggregating column, which leaves multiple rows for the same node pair
# and draws them as separate redundant ribbons. Summing preserves the total exactly.


if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value), .groups = "drop") %>% pretty_labels(),
                     animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Cobb")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Cobb", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Gwinnett", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Fulton", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "DeKalb", show_values_in_labels = TRUE, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Douglas", show_values_in_labels = TRUE, animate = T, label_units = "MGD")

if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Bartow", yr = 2024, animate = F, show_values_in_labels = TRUE, label_units = "MGD")

# loopy:
# water bodies as loops
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced %>%
                       # replace _ds in targets to get loops back
                       mutate(target = gsub("_ds", "", target))%>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value), .groups = "drop") %>% pretty_labels(),
                     animate = T, label_units = "MGD")

# basins as loops ----
mgmtplan_all_basin <- rbind(mgmtplan_surface_pws_scaled %>% # mgmtplan_surface is unbalanced table
                              # bring back basin info
                              left_join(mgmtplan_surface %>% select(county, year, basin, source), by = c("county", "source", "year")),
                            mgmtplan_self_c_s_y,
                            # remove the _GW part as that was just for tracking
                            mgmtplan_ground %>% mutate(basin = source)
                            ) %>%
  group_by(county, basin, year, target, units) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  select(county, year, source = basin, target, value, units) %>%
  # add ww basin as sinks
  rbind(ww_sink %>% group_by(county, basin, year, source, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          select(county, year, source, target = basin, value, units) )

df_sankey_water_county <- rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all_basin)

if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_water_county %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value), .groups = "drop") %>% pretty_labels(),
                     show_values_in_labels = TRUE, animate = T, label_units = "MGD")

if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_water_county %>% pretty_labels(), reg = "Cobb", show_values_in_labels = TRUE, animate = T, label_units = "MGD")


###############################################################################%

# METRO level----

## main metro diagrams ----
# for linear: these are all flows (fresh gw to pws and ind) FROM mgmt plan BUT ww sinks. we patch sinks next
mgmtplan_all_basin_metro_nosink <- mgmtplan_all_basin %>% filter(!grepl("Basin", target))

# for loopy: aggregate up facilities and transfers. all ww sinks to basins
mgmtplan_all_basin_agg <- mgmtplan_all_basin_metro_nosink %>%
  rbind(ww_sink %>% group_by(county, basin, year, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(source = "in-county treatment") %>%
          select(county, year, source, target = basin, value, units))

# # these show ww discharges and fresh water sources in one diagram.
# # BUT the fresh water source part is not complete (missing elec, ag etc; only has pws and ind)
# plot_sankey_enhanced(mgmtplan_all_basin_agg %>%
#                        group_by(year, source, target, units) %>%
#                        summarise(value = sum(value), .groups = "drop") %>% pretty_labels(),
#                      show_values_in_labels = TRUE, animate = T, label_units = "MGD")


# loopy diagram
df_water_metro_loopy <- rbind(df_sankey_wwtrade,
      industrial_discharge %>% select(-county),
      thermoelec_water_use %>% select(-county),
      mgmtplan_all_basin_agg %>% select(-county)) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD")

if (MAKE_PLOT) plot_sankey_enhanced(df_water_metro_loopy %>% pretty_labels(),
                     show_values_in_labels = TRUE, animate = T, label_units = "MGD")

# linear diagram
df_water_metro_linear_nosink <- rbind(df_sankey_wwtrade,
                             industrial_discharge %>% select(-county),
                             thermoelec_water_use %>% select(-county),
                             mgmtplan_all_basin_metro_nosink %>% select(-county)) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD")

if (MAKE_PLOT) plot_sankey_enhanced(df_water_metro_linear_nosink %>% pretty_labels(),
                     show_values_in_labels = T, animate = T, label_units = "MGD")


## add surface water as a source node ----
df_water_metro_linear_wSW <- df_water_metro_linear_nosink %>%
  # add surface water node
  rbind(df_water_metro_linear_nosink %>%
          filter(grepl("Basin", source)) %>%
          group_by(year, source, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(target = source, source = "surfaceWater")) %>%
  # change groundwater to groundwaterAllBasins for labeling
  mutate(source = if_else(source == "groundwater", "groundwaterAllBasins", source))


if (MAKE_PLOT) plot_sankey_enhanced(df_water_metro_linear_wSW %>% pretty_labels(),
                     show_values_in_labels = T, animate = T, label_units = "MGD")

# unique source-target pairs for documentation later
df_water_metro_linear_wSW %>% select(source, target) %>% distinct()


## add ww discharges sink ----

# All treated effluent is attributed to the in-county treatment node.
# ExtraNotes: effluent exported from one county is treated and discharged by a plant in another,
# and at metro scope that plant's discharge is already counted. Giving the export node its own
# discharge outflow would therefore count the same effluent twice, so the export node is
# terminal at metro scope: it represents sewage leaving the originating county, not leaving the
# metro system. Treatment inflow is (generated - exports) + imports, which equals the summed
# facility discharge by construction.
WW_DISCHARGE_SOURCE <- "in-county treatment"

### to discharge (except land and reuse) ----
# for facilities with _ds (because they are water bodies), keeping the rest as-is. merge Various with discharge
ww_sink_discharge <- ww_sink_downstream %>%
  mutate(sinktype = if_else(grepl("_ds|Various", target), "discharge", target)) %>%
  group_by(county, year, source, sinktype, units) %>% # leave the basin out
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(target = sinktype) %>%
  select(county, year, source, target, value, units)

# plot_sankey_enhanced(ww_sink_discharge %>% group_by(year, source, target, units) %>% summarise(value = sum(value), .groups = "drop") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "MGD")


# combine with metro diagram to get sources of in-county treatment and exports. the sinks will be sinktypes
df_water_metro_linear_wSW_discharge <- df_water_metro_linear_wSW %>%
  # add discharge sinks
  rbind(ww_sink_discharge %>%
          group_by(year, target, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(source = WW_DISCHARGE_SOURCE) %>%
          select(source, target, year, value, units))

if (MAKE_PLOT) plot_sankey_enhanced(df_water_metro_linear_wSW_discharge %>% pretty_labels(),
                     show_values_in_labels = T, animate = T, label_units = "MGD")

# plot_sankey_enhanced(df_water_metro_linear_wSW_discharge %>% pretty_labels(),
#                      show_values_in_labels = T, animate = F, yr = 2024, label_units = "MGD")

# for documentation later
df_water_metro_linear_wSW_discharge %>% select(source, target) %>% distinct()


### to receiving types ----
# ww discharge based on water body type using ww_facility_sink_map_s mapping
ww_sink_discharge_type <- ww_sink_downstream %>% mutate(target = gsub("_ds", "", target)) %>%
  left_join(ww_facility_sink_map_s %>%
              select(county, source = facility_name, target, receiving_type),
            by = c("county", "source", "target")) %>%
  group_by(county, source , year, receiving_type, units) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(target = receiving_type) %>%
  select(county, year, source, target, value, units)

if (MAKE_PLOT) plot_sankey_enhanced(ww_sink_discharge_type %>% group_by(year, source, target, units) %>% summarise(value = sum(value), .groups = "drop") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "MGD")


# combine with metro diagram. the sinks will be receiving types
df_water_metro_linear_wSW_discharge_type <- df_water_metro_linear_wSW %>%
  rbind(ww_sink_discharge_type %>%
          group_by(year, target, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(source = WW_DISCHARGE_SOURCE) %>%
          select(source, target, year, value, units)) %>%
  filter(year %in% YEARS_TO_ENSURE) %>%
  group_by(year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop")
validate_flows(df_water_metro_linear_wSW_discharge_type, "water_metro_flows",
               strict_years = TRUE)
# ExtraNotes: clamped and collapsed as for the county frame; see the note there.

if (MAKE_PLOT) plot_sankey_enhanced(df_water_metro_linear_wSW_discharge_type %>% pretty_labels(),
                     show_values_in_labels = T, animate = T, label_units = "MGD")

if (MAKE_PLOT) plot_sankey_enhanced(df_water_metro_linear_wSW_discharge_type %>% pretty_labels(),
                     show_values_in_labels = T, animate = F, yr = 2024, label_units = "MGD")

# filter all things touching PWS
high_level_pws_sw_demands_balance <- df_water_metro_linear_wSW_discharge_type_pws <- df_water_metro_linear_wSW_discharge_type %>%
  filter(grepl("publicWatSup|surfaceWater|Chattahoochee Basin|Coosa_", source) | grepl("publicWatSup", target)) %>%
  pretty_labels()

if (MAKE_PLOT) plot_sankey_enhanced(high_level_pws_sw_demands_balance,
                     show_values_in_labels = T, animate = T, label_units = "MGD")
if (MAKE_PLOT) plot_sankey_enhanced(high_level_pws_sw_demands_balance,
                     show_values_in_labels = T, animate = F, yr = 2024, label_units = "MGD")
# write_csv(high_level_pws_sw_demands_balance, paste0(SAVE_DIR, "high_level_pws_sw_demands_balance.csv"))

# write_csv(df_water_metro_linear_wSW_discharge_type, paste0(SAVE_DIR, "water_metro_linear_wSW_discharge_receivingtype.csv"))


if (SAVE_FILES) {
###############################################################################%
# SAVING METRO ----
###############################################################################%

message("Saving water outputs...")

write_csv(df_water_metro_linear_wSW_discharge_type,
          file.path(SAVE_DIR, "water/01_metro_water_flows.csv"))

save_metro_sankey(df_water_metro_linear_wSW_discharge_type,
                  "water", "01_metro_water", label_units = "MGD")

###############################################################################%
# SAVING COUNTY ----
###############################################################################%

write_csv(df_sankey_county_pws_balanced,
          file.path(SAVE_DIR, "water/02_county_water_flows.csv"))

save_county_sankeys(
  df_sankey_county_pws_balanced, "water", "02", "water",
  prep_fn = identity, label_units = "MGD")


}


##############################################################################%
# ENERGY FOR WATER ----
##############################################################################%

# This could go into energy-water script but keep it here due to data tables
# being here and downstream dependency to energy

# TODO: probably write out the balanced county level water flows df_sankey_county_pws_balanced
# to a csv and move this to a separate script for energy-for-water

# sample before E4W
if (MAKE_PLOT) plot_sankey_enhanced(df_sankey_county_pws_balanced %>% pretty_labels(),
                     reg = "Fulton", show_values_in_labels = TRUE, animate = T, label_units = "MGD")


en4swflows <- df_sankey_county_pws_balanced %>%
  filter((!grepl("ground", source) & # anything but groundwater
           grepl("publicWatSup|industrial|agricultural|Plant|plant", target) & # uses of SW
           !grepl("publicWatSup", source)) # but not PWS to industrial/ag to avoid double counting
  ) %>%
  mutate(watertype = "surfaceWater")
# plot_sankey(en4swflows)


en4gwflows <- df_sankey_county_pws_balanced %>%
  filter(grepl("groundwater", source)) %>%
  mutate(watertype = "groundwater")
# plot_sankey(en4gwflows)

# plot_sankey(rbind(en4swflows, en4gwflows))


###############################################################################%

## water extraction energy ----
## energy for surface water and groundwater extraction

## https://pnnl.github.io/interflow/public_water_sector.html
## Electricity (kWh/day) = ((Flow (gpm) x pumping head (ft)) / (3960 x pumping efficiency)) x 0.746 x 24
## 3960 water horsepower, 0.746 horsepower to kilowatts, 24 hours per day, kWh_to_EJ, eta 0.5,

# for depth to groundwater
# https://pubs.usgs.gov/fs/2022/3035/fs20223035.pdf
# https://ga.water.usgs.gov/www2/publications/ggs/ic-88/pdf/GGS-IC-88.pdf
# https://gmd.copernicus.org/articles/18/1737/2025/ or https://gmd.copernicus.org/articles/18/1737/2025/gmd-18-1737-2025.pdf
# surficial aquifer system typically is less than 100 feet thick.
# PWS wells can go up to 750 feet deep.
# domestic wells have intermediate in depth, usually between 50 and 150 feet deep.
# most wells are between 101 and 300 feet deep, with public supply wells typically being deeper (150 to 750 feet) and domestic wells being intermediate (50 to 150 feet)
# average depth to water table in GA is 85 feet

# params
# flow will be in MGD from the data
# PUMPING_HEAD_GW / PUMPING_HEAD_SW are defined in functions.R
# ExtraNotes: the fivefold difference between the two heads is what makes groundwater the more
# energy-intensive source per unit volume, and it is the mechanism behind the whole
# energy-for-water term. 125 ft is a domestic-dominated middle value: Georgia public supply wells
# run 150-750 ft while domestic wells run 50-150 ft, and self-supply here is assigned to
# households. Because metro Atlanta is overwhelmingly surface-supplied (~97.6%), the choice of
# groundwater head has little leverage on the metro total but matters for any county with a high
# groundwater share.

# EJ/year = (flow → gpm → HP → kW → kWh → J → EJ) × 365
# value × MGD_to_GPM	gpm
# gpm × ft	gpm·ft
# ÷ 3960	HP
# ÷ efficiency	HP
# × 0.746	kW
# × 24	kWh/day
# × 3.6e-12	EJ/day
# × 365	EJ/year

# solve electricity in EJ/yr
en4sw_extract <- en4swflows %>%
  mutate(elec = ((value * MGD_to_GPM) * PUMPING_HEAD_SW) / (3960 * PUMPING_EFFICIENCY) * HP_to_KW * HOURS_PER_YEAR * kWh_to_EJ)

en4gw_extract <- en4gwflows %>%
  mutate(elec = ((value * MGD_to_GPM) * PUMPING_HEAD_GW) / (3960 * PUMPING_EFFICIENCY) * HP_to_KW * HOURS_PER_YEAR * kWh_to_EJ)


# NOTE: setting sources of to watertype (SW or GW) BUT it could be basins or water bodies as well
# change the grouping based on that if needed

# energy for water extraction: source electricity, target water use, aggregated

en4water_extract <- rbind(en4sw_extract, en4gw_extract) %>%
  group_by(county, watertype, target, year) %>%
  summarise(value = sum(elec), .groups = "drop") %>%
  mutate(source = paste0("extract_", watertype),
         units = "EJ") %>%
  select(county, source, target, year, value, units)


###############################################################################%

## pws water treatment ----

# Fresh surface water treatment = 405 kWh/mg Fresh groundwater treatment = 205 kWh/mg
# saline surface water treatment = 12,000 kWh/mg saline groundwater treatment = 12,000 kWh/mg
# distribution 1040 kWh/mg

# FRESH_SW_TREAT_ENERGY_INT / FRESH_GW_TREAT_ENERGY_INT are defined in functions.R
# ExtraNotes: surface water costs about twice as much energy to treat as groundwater because it
# needs coagulation, flocculation, sedimentation and filtration, whereas groundwater arrives
# filtered by the aquifer and often needs only disinfection. Both intensities are national
# averages from PNNL interflow, not local measurements, so treatment energy is transferable
# rather than observed. Note the direction of the trade-off against pumping: groundwater is
# cheaper to treat but much more expensive to lift.

en4water_treat <- rbind(en4sw_extract, en4gw_extract) %>%
  # only pws and residential (new additions: industry and plants also) need treatment
  filter(grepl("publicWatSup|residential|industr|plant|Plant", target)) %>%
  mutate(elec_treat = case_when(
    watertype == "surfaceWater" ~ (value * DAYS_PER_YEAR) * FRESH_SW_TREAT_ENERGY_INT * kWh_to_EJ,
    watertype == "groundwater" ~ (value * DAYS_PER_YEAR) * FRESH_GW_TREAT_ENERGY_INT * kWh_to_EJ)) %>%
  group_by(county, watertype, target, year) %>%
  summarise(value = sum(elec_treat), .groups = "drop") %>%
  mutate(source = paste0("treat_", watertype), units = "EJ", value) %>%
  select(county, source, target, year, value, units)


###############################################################################%

## water distribution energy ----
# probably exclude self-supply, but since energy for water is so small, calculating distribution for all
# (it's not like industrial or other self use won't have the need to move water, so it's not unreasonable)

# DISTRIBUTION_ENERGY_INT is defined in functions.R
# ExtraNotes: distribution is the single largest term in the water-energy chain, roughly 2.5x
# surface treatment and 5x groundwater treatment, because pressurising a sprawling network over
# rolling terrain dominates. Metro Atlanta sits on the Piedmont with substantial relief, so this
# national average is more likely to understate than overstate the local figure. Power plants
# and agriculture are excluded: plants use water on site, and irrigation is not a pressurised
# municipal network.

en4water_distribute <- rbind(en4sw_extract, en4gw_extract) %>%
  # exclude powerplants from distribution because they likely use water on-site
  # and don't have distribution needs. also exclude ag because they likely use
  # irrigation which is not like typical distribution. keep industrial for now
  # because some of it is likely to be distributed like other non-ag self-supply
  filter(!grepl("Plant|plant|agricultural|Agricultural", target)) %>%
  # MGD to mg/yr to kWh/yr to EJ/yr
  mutate(elec_distribute = (value * DAYS_PER_YEAR) * DISTRIBUTION_ENERGY_INT * kWh_to_EJ) %>%
  group_by(county, watertype, target, year) %>%
  summarise(value = sum(elec_distribute), .groups = "drop") %>%
  mutate(source = paste0("distribute_", watertype), units = "EJ", value) %>%
  select(county, source, target, year, value, units)

# combine all energy for water
en4water_all <- rbind(en4water_extract, en4water_treat, en4water_distribute)

# ANALYSIS: decompose water-sector energy into its stages. The published diagram shows only the
# total reaching `en4water`, so the split between lifting, treating and distributing -- and
# therefore which intervention would matter -- is visible only here. Reported per water type as
# well, since the surface/groundwater contrast is the mechanism behind the intensity differences
# between counties.
if (ANALYSIS) {
  e4w_stages <- bind_rows(
    en4water_extract %>% mutate(stage = "extraction"),
    en4water_treat %>% mutate(stage = "treatment"),
    en4water_distribute %>% mutate(stage = "distribution")) %>%
    filter(year %in% YEARS_TO_ENSURE) %>%
    mutate(water_type = if_else(grepl("groundwater", source), "groundwater", "surface water")) %>%
    group_by(year, stage, water_type) %>%
    summarise(pj = sum(value) * EJ_to_PJ, .groups = "drop") %>%
    group_by(year) %>% mutate(share_pct = 100 * pj / sum(pj)) %>% ungroup()
  write_csv(e4w_stages, file.path(ANALYSIS_DIR, "P6_energy_for_water_by_stage.csv"))

  st <- e4w_stages %>% filter(year == max(YEARS_TO_ENSURE)) %>%
    group_by(stage) %>% summarise(pj = sum(pj), .groups = "drop") %>% arrange(desc(pj))
  message("  ANALYSIS: water-sector energy by stage, ", max(YEARS_TO_ENSURE), ": ",
          paste0(st$stage, " ", round(st$pj, 3), " PJ", collapse = ", "))
}

if (MAKE_PLOT) plot_sankey_enhanced(en4water_all %>% group_by(source, target, year) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>%
                       mutate(units = "PJ") %>%
                       pretty_labels(),
                     show_values_in_labels = TRUE, animate = T, label_units = "PJ")

if (MAKE_PLOT) plot_sankey_enhanced(en4water_all %>% mutate(value = value * EJ_to_PJ) %>% pretty_labels(),
                     reg = "Fulton", show_values_in_labels = TRUE, animate = T, label_units = "PJ")


###############################################################################%

## ww energy ----

# everything is treated as secondary so 2080 kWh/mg
# see the table here https://pnnl.github.io/interflow/wastewater_sector.html

# WW_TREATMENT_ENERGY_INT is defined in functions.R
# ExtraNotes: every plant is treated as secondary. Metro Atlanta plants are in practice mostly
# advanced/tertiary with nutrient removal, which is more energy intensive, so this is a
# deliberately conservative floor on wastewater treatment energy. Wastewater treatment is the
# most energy-intensive step per unit volume in the whole water chain -- twice distribution and
# five times surface treatment -- which is why I&I matters energetically as well as
# hydraulically: every leaked gallon is pumped and aerated at full cost.

# check facilities and flows (do !grepl to see which are not included)
# plot_sankey_enhanced(df_sankey_county_pws_balanced %>% filter(grepl("wastewater|inFrom", source)) %>% group_by(year, source, target, units) %>% summarise(value = sum(value), .groups = "drop") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "MGD")

# treatment energy
en4ww_treat_facility <- df_sankey_county_pws_balanced %>%
  # get all facilities and their treated volumes (value)
  filter(grepl("wastewater|inFrom", source)) %>%
  # MGD to mg/yr to kWh/yr to EJ/yr
  mutate(elec_ww_treat = (value * DAYS_PER_YEAR) * WW_TREATMENT_ENERGY_INT * kWh_to_EJ) %>%
  group_by(county, source, target, year) %>%
  summarise(value = sum(elec_ww_treat), .groups = "drop") %>%
  mutate(en_wwtype = "en_wwtreat", units = "EJ")

# distribution energy
en4ww_distribute_facility <- df_sankey_county_pws_balanced %>%
  # get all facilities and their treated volumes (value)
  filter(grepl("wastewater|inFrom", source)) %>%
  # assume the same distribution energy intensity as PWS for ww but 2x for exports due to moving more distances.
  # The intention is to account for energy to move ww before treatment
  mutate(elec_ww_distribute = case_when(
    grepl("inFrom", source) ~ (value * DAYS_PER_YEAR) * DISTRIBUTION_ENERGY_INT * 2 * kWh_to_EJ, # exports
    TRUE ~ (value * DAYS_PER_YEAR) * DISTRIBUTION_ENERGY_INT * kWh_to_EJ # in-county
  )) %>%
  group_by(county, source, target, year) %>%
  summarise(value = sum(elec_ww_distribute), .groups = "drop") %>%
  mutate(en_wwtype = "en_wwdist", units = "EJ")
# ExtraNotes: sewage conveyed across a county line is charged twice the collection energy, on the
# reasoning that an inter-county transfer travels further and needs more lift than a flow to the
# nearest in-county plant. This is the only place where the transfer network carries an explicit
# energy penalty, and it is what makes inter-county sewage sharing visible as an energy choice
# rather than a purely hydraulic one. The factor of 2 is a stated assumption, not a measurement.

en4ww_treat_dist_facility <- rbind(en4ww_treat_facility, en4ww_distribute_facility)

if (MAKE_PLOT) plot_sankey_enhanced(en4ww_treat_dist_facility %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "PJ")

# add treatment vs distribution node as a source
en4ww_treat_dist_facility_type <- en4ww_treat_dist_facility %>% select(-en_wwtype) %>%
  rbind(en4ww_treat_dist_facility %>%
          mutate(target = source, source = en_wwtype) %>%
          group_by(county, source, target, year, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          select(county, source, target, year, value, units))

if (MAKE_PLOT) plot_sankey_enhanced(en4ww_treat_dist_facility_type %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "PJ")


# lump in-county treatment vs exports
en4ww_treat_dist_cat <- en4ww_treat_dist_facility_type %>%
  mutate(source = if_else(grepl("inFrom", source), "ww_exports", source),
         target = if_else(grepl("inFrom", target), "ww_exports", target),
         # change labels to in-county treatment
         source = if_else(grepl("wastewater", source), "in-county treatment", source),
         target = if_else(grepl("wastewater", target), "in-county treatment", target)
         ) %>% # hack, actually inflows
  group_by(county, source, target, year, units) %>%
  summarise(value = sum(value), .groups = "drop")


if (MAKE_PLOT) plot_sankey_enhanced(en4ww_treat_dist_cat %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "PJ")


# combine water and ww energy
en4water_ww <- rbind(en4water_all, en4ww_treat_dist_cat)

if (MAKE_PLOT) plot_sankey_enhanced(en4water_ww %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "PJ")

# assign electricity as a source
en4water_ww_elec_facility <- en4water_ww %>%
  rbind(en4water_ww %>%
          filter(grepl("extract|treat|distribute|en_", source) & !grepl("in-county treatment", source)) %>%
          mutate(target = source, source = "electricity") %>%
          group_by(county, source, target, year, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          select(county, source, target, year, value, units))

###############################################################################%

## COUNTY E4W ----
# plot_sankey_enhanced(en4water_ww_elec_facility %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "PJ")

if (MAKE_PLOT) plot_sankey_enhanced(en4water_ww_elec_facility %>% pretty_labels() %>% mutate(value = value * EJ_to_TJ, units = "TJ", .groups = "drop"),
                     reg = "Cobb", show_values_in_labels = TRUE, animate = T, label_units = "TJ")

## METRO E4W ----
# without ww facilities: remove targets where sources are in-county treatment and ww_exports
en4water_ww_elec_use <- en4water_ww_elec_facility %>%
  filter(!(grepl("in-county treatment|ww_exports", source)))
validate_flows(en4water_ww_elec_use, "en4water_ww_elec_use")

if (MAKE_PLOT) plot_sankey_enhanced(en4water_ww_elec_use %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_TJ, .groups = "drop") %>% mutate(units = "TJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "TJ")

# simple table for energy calcs
# make elec flow direct into sectors
en4water <- en4water_ww_elec_use %>%
  filter(!grepl("electricity", source)) %>% # take out first elec node
  mutate(source = "electricity") %>% # reassgin all middle nodes to electricity
  group_by(county, source, target, year, units) %>%
  summarise(value = sum(value), .groups = "drop")

if (MAKE_PLOT) plot_sankey_enhanced(en4water %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_TJ, .groups = "drop") %>% mutate(units = "TJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = T, label_units = "TJ")


