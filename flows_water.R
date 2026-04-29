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


## commercial ----
# sum commercial, institutional, municipal and new_commercial into commercial_calc. Ignore missing values (NAs)
# adding muni here because Danny J said municipal is really government buildings

df_pws_com <- df_pws %>%
  select(county, year, commercial, institutional, municipal, new_commercial) %>%
  mutate(commercial_calc = rowSums(select(., commercial, institutional, municipal, new_commercial), na.rm = TRUE)) %>%
  select(county, year, commercial=commercial_calc)

## industrial ----
# sum industrial and anheuser_busch in industrial_calc. Ignore missing values (NAs)

df_pws_ind <- df_pws %>%
  select(county, year, industrial, anheuser_busch) %>%
  mutate(industrial_calc = rowSums(select(., industrial, anheuser_busch), na.rm = TRUE)) %>%
  select(county, year, industrial=industrial_calc)

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

## ag self supply ----
# the data in only for 2020 so let's write zeros for missing counties and extend for all years. the data should be complete

df_ag_self_supplied <- read_csv(paste0(DATA_DIR, "water_selfsupply_ag.csv")) %>% rename_all(tolower) %>%
  select(county, year, value = total) %>%
  complete(county = counties, year = unique(df_pws$year)) %>%  # add missing counties and years
  group_by(county) %>%
  mutate(value = value[year == 2020]) %>% # copy 2020 values forward to all years
  ungroup() %>% replace_na(list(value = 0)) %>%
  mutate(target = "agricultural")  %>%
  select(county, year, target, value)


# nrw / losses ----
# non-revenue water (NRW) is the difference between total water supplied and total water billed
# source is publicWatSup, target is NRW

df_pws_nrw <- df_pws %>%
  select(county, year, nrw) %>%
  mutate(value = replace_na(nrw, 0),
         source = "publicWatSup", target = "losses") %>%
  select(county, year, source, target, value)

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
# sum single_family and multifamily and self_supplied to residential_wastewater
# take out septic part from treated wastewater
df_wastewater_res <- df_wastewater %>%
  select(county, year, single_family, multi_family, self_supplied, vol_septic_generated_mg) %>%
  mutate(residentialww = rowSums(select(., single_family, multi_family, self_supplied), na.rm = TRUE),
         vol_septic_generated_mgd = vol_septic_generated_mg / 365, # convert mg to mgd
         residentialnoseptic = residentialww - replace_na(vol_septic_generated_mgd, 0)) %>%
  select(county, year, residential=residentialnoseptic)

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

df_ss_ <- rbind(df_pws_self_supplied, df_ag_self_supplied) %>% mutate(source = "groundwater")


## water supply ----
df_watersupp <- rbind(df_pws_, df_ss_, df_pws_nrw)

plot_sankey(df_watersupp)
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
plot_sankey(df_water_sup_waste, reg = "Hall") # losses not fully visible

###############################################################################%

# losses ----
# difference of total water supplied and total wastewater produced
total_water_supply <- df_water_sup_waste %>%
  filter(source %in% c("publicWatSup", "groundwater")) %>%
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
# # but the total wasterwater is still less than the total water supply
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

neg_losses <- df_water_losses %>% filter(value < 0) # should be none

# temp fix: add back negative losses into water supply for each category and remove negative losses or recalculate them
df_water_sup_waste_fix <- df_water_sup_waste %>%
  left_join(neg_losses %>% rename(negloss=value), by = c("county", "year", "target" = "source")) %>%
  mutate(supply_adj = if_else(!is.na(negloss), value + abs(negloss), value)) %>%
  select(county, year, source, target, value = supply_adj)

total_water_supply_fix <- df_water_sup_waste_fix %>%
  filter(source %in% c("publicWatSup", "groundwater")) %>%
  filter(target != "losses") %>% # losses are a terminal node, not requiring another losses calculation
  group_by(county, year, target) %>%
  summarise(total_supply = sum(value), .groups = "drop")

df_water_losses_fix <- total_water_supply_fix %>%
  left_join(total_wastewater, by = c("county", "year", "target")) %>%
  mutate(losses = total_supply - wastewater_generated,
         source = target, target = "losses") %>%
  select(county, year, source, target, value = losses)

df_water_losses_fix %>% filter(value < 0) # should be none
df_water_losses_fix <- df_water_losses_fix %>% mutate(value = if_else(value < 0, 0, value)) # fixing just 4 edge cases in Bartow

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

# check if any I/I values are negative or zero (should not be)
if (any(df_water_i_i$value <= 0)) {
  cat(paste0("\nI/I values are negative or zero in ",
              unique(paste(df_water_i_i$county[df_water_i_i$value <= 0])),
             " for flows: " , paste(unique(df_water_i_i$source[df_water_i_i$value <= 0]),
              " in year(s): " , paste(unique(df_water_i_i$year[df_water_i_i$value <= 0]),
                    collapse = ", "))))

  cat("\n => Zero I/I is OK, as some counties have no wastewater from agriculture and industry")
}

# diagnostic plot for I/I (histogram, x county, y i_i_factor)
if (F) {
  ggplot(df_water_i_i %>% select(county, i_i_factor) %>% unique(), aes(x = county, y = i_i_factor)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    # add an average horizontal line
    geom_hline(yintercept = mean(df_water_i_i$i_i_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
    # add text label for average line
    geom_text(aes(x = Inf, y = mean(df_water_i_i$i_i_factor, na.rm = TRUE), label = paste0("Average: ", round(mean(df_water_i_i$i_i_factor, na.rm = TRUE), 2))),
              hjust = 1.1, vjust = -0.5, color = "red2") +
    theme_minimal() +
    labs(title = "Infiltration and Inflow (I/I) Factor by County",
         x = "County",
         y = "I/I factor") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major = element_blank())

  # plot a 2nd plot as a map here's the sf: sf_counties_atlanta
  ggplot(data = sf_counties_atlanta %>% mutate(county=name) %>% left_join(df_water_i_i %>% select(county, i_i_factor) %>% unique(), by = "county")) +
    geom_sf(aes(fill = i_i_factor), color = "white") +
    geom_sf_text(aes(label = paste0(name, " ", i_i_factor)), size = 3, color = "yellow2") +
    # scale_fill_viridis_c(option = "C", na.value = "lightgrey", alpha = 0.9) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "lightgrey") +
    theme_void() +
    labs(title = "Infiltration and Inflow (I/I) Factor by County",
         fill = "I/I factor")

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

# sum of wastewater generated and I/I added
df_ww_tobetreated <- rbind(df_water_i_i, df_wastewat) %>%
  filter(target == "wastewater") %>% # just in case
  group_by(county, year, target) %>%
  summarise(value = sum(value), .groups = "drop")

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
plot_sankey(df_sankey)

# single county (all)
plot_sankey_enhanced(df_sankey, reg = "Bartow", show_values_in_labels = TRUE, animate = F)
plot_sankey_enhanced(df_sankey, reg = "Bartow")
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
plot_sankey(df_sankey, reg = "Rockdale")

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
plot_sankey(df_sankey_allc)


# just categories, no detail
df_sankey_agg <- rbind(df_water_sup_waste_fix, df_water_losses_fix, df_wastewater_septic, df_water_i_i, df_wastewater_treated_one_node) %>%
  select(-county) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD") %>% pretty_labels()


plot_sankey(df_sankey_agg)
plot_sankey_enhanced(df_sankey_agg, show_values_in_labels = TRUE, animate = F, label_units = "MGD")



###############################################################################%

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
plot_sankey(county2county)
county2facility <- df_ww_conn %>% select(county, year, source = fromcounty, target = tofacility, value) %>% unique() %>% replace_na(list(value= 0))
plot_sankey(county2facility)
place2facility <- df_ww_conn %>% select(county, year, source = fromplace, target = tofacility, value) %>% unique() %>% replace_na(list(value= 0))
plot_sankey(place2facility)

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

# check duplicates
dupes <- df_ww_conn_type %>%
  group_by(fromcounty, tocounty, fromplace, tofacility, year, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

df_ww_conn_dupes <- dupes %>% left_join(df_ww_conn_type, by = c("fromcounty", "tocounty", "fromplace", "tofacility", "year", "value")) %>%
  arrange(fromcounty, tocounty, fromplace, tofacility, year)

# TODO: remove duplicates later, just go back to notes and add "repeated or copied"


# trade flows
df_ww_conn_trade <- df_ww_conn_type %>%
  group_by(fromcounty, fromplace, tocounty, tofacility, year) %>%
  summarise(trade = sum(value, na.rm = TRUE), .groups = "drop")

# imports
ww_imports <- df_ww_conn_trade %>%
  mutate(fromcounty_fromplace = paste("inFrom", fromcounty, fromplace, sep = "_")) %>%
  select(county = tocounty, source = fromcounty_fromplace, target = tofacility, year, import = trade)

# plot_sankey(rbind(df_sankey, ww_imports %>% rename(value = import)), reg = "Cobb")

# exports
ww_exports <- df_ww_conn_trade %>%
  mutate(source = "wastewater") %>%
  # mutate(tocounty_tofacility = paste("outTo" , tocounty, tofacility, sep = "_")) %>%
  select(county = fromcounty, source, target = tofacility, year, export = trade) %>%
  unique() # some flows are reported by each county, so keep only one. difference was fromplace and reporting county

ww_exports_track <- df_ww_conn_trade %>%
  mutate(source = "wastewater") %>%
  mutate(tocounty_tofacility = paste("outTo" , tocounty, tofacility, sep = "_")) %>%
  select(county = fromcounty, source, target = tocounty_tofacility, year, export = trade) %>%
  unique() # some flows are reported by each county, so keep only one. difference was fromplace and reporting county

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

plot_sankey(df_sankey_wwtrade_c_f)
plot_sankey(df_sankey_wwtrade_c_f, reg = "Cobb")
# plot_sankey(df_sankey_wwtrade_c_f, reg = "DeKalb")
# plot_sankey(df_sankey_wwtrade_c_f, reg = "Fulton")
# plot_sankey(df_sankey_wwtrade_c_f, reg = "Douglas")
plot_sankey_enhanced(df_sankey_wwtrade_c_f, reg = "Douglas", show_values_in_labels = TRUE, animate = F, label_units = "MGD")

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
         target = if_else(trade_type == "ww_imports", "wastewater", "ww_exports")) %>%
  select(source, target, year, value)

df_sankey_wwtrade <- df_sankey_agg <- rbind(df_water_sup_waste_fix, df_water_losses_fix, df_wastewater_septic, df_water_i_i,
                                            df_wastewater_treated_one_node %>% mutate(target = "in-county treatment")) %>%
  select(-county) %>% rbind(ww_trade_agg) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD")

plot_sankey(df_sankey_wwtrade %>% pretty_labels())

plot_sankey_enhanced(df_sankey_wwtrade %>% pretty_labels(),
                     show_values_in_labels = TRUE, animate = F, label_units = "MGD")


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

plot_sankey_enhanced(rbind(df_sankey_wwtrade, thermoelec_water_use %>% select(!c(county))))


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


# no data on conveyance losses so all water leaving a water body is assumed to be used
# TODO: use each 2019 ratios to split up each year's sources. first compare if
# the supply data = use data above. if not we may want to just split up use
# using the basin ratios from here

plot_sankey_enhanced(rbind(df_sankey_wwtrade, mgmtplan_surface %>% select(!c(basin, county))))
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

plot_sankey(mgmtplan_ground, yr = 2020, animate = F)

# Notes: major difference between Atlanta region commission data for
# 'self-supply' use 45.9 , which is assumed to be from groundwater, and this
# data from the report for groundwater supply 3.6.
# Actually, this is just GW; I will bring in self-supply after this and compare
# again. Also, the 45.9 could include SW self-supply for Ag.



## self supply sources ----
# mostly industrial and golf irrigation -? going to assign all use to industrial
# only permitted data, not actual withdrawals, so will just assume 0.85 of permit of permitted is used
# need to breakout by counties -> based on industrial use by county
# need to determine surface to groundwater ratio for this - let's say 50 50 for now
p_water_mgmtplan_self <- read_csv(paste0(DATA_DIR, "water_mgmtplan_self.csv")) %>% clean_col_names()

PERMIT_USE_FACTOR <- 0.85

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
ww_facility_sink_map_s <- ww_facility_sink_map %>%
  group_by(facility_name) %>%
  # adjust permits between datasets to avoid zero permit per site
  mutate(permit = if_else(is.na(permit), 0, permit),
         permit_adj = max(permitted_capacity, permit)) %>%
  mutate(disposal_share =  permit_adj / sum(permit_adj),
         disposal_share = replace_na(disposal_share, 0)) %>%
  ungroup() %>% distinct()

# calculate wastewater sink flows
ww_sink <- df_sankey %>% filter(source == "wastewater") %>%
  rbind(ww_trade_comb %>% filter(grepl("inFrom", source))) %>% # bring in ww trade inflows
  select(county, year, source = target, value, units) %>%
  # to get sinks based on facility (and shares)
  left_join(ww_facility_sink_map_s, by = c("county", "source" = "facility_name")) %>%
  select(county, year, basin, source, target, value, units) %>%
  distinct() %>%
  group_by(county, basin, year, source, target, units) %>%
  summarise(value = sum(value), .groups = "drop")

plot_sankey(ww_sink)

# make it all linear by assigning downstream _ds to all sinks
ww_sink_downstream <- ww_sink %>%
  mutate(target = if_else(grepl("River|Stream|Creek|Lake|Reservoir|Basin|Branch", target),
                          paste0(target, "_ds"), target))

ww_sink_downstream %>% filter(!grepl("_ds", target)) %>% plot_sankey(yr = 2020, animate = F)

plot_sankey(ww_sink_downstream)


## all management plan data ----
mgmtplan_all <- rbind(mgmtplan_surface, mgmtplan_ground, mgmtplan_self_c_s_y, ww_sink)

mgmtplan_all_downstream <- rbind(mgmtplan_surface, mgmtplan_ground, mgmtplan_self_c_s_y, ww_sink_downstream)

plot_sankey(mgmtplan_all)
plot_sankey(mgmtplan_all_downstream)

# plot_sankey(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)))

# plot_sankey_enhanced(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)) %>% pretty_labels())
plot_sankey_enhanced(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)) %>% pretty_labels(), reg = "Cobb")
plot_sankey_enhanced(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all %>% select(-basin)) %>% pretty_labels(), reg = "Gwinnett")


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



###############################################################################%

# COUNTY level ----
df_sankey_ww_mgmt_C_ind <- rbind(df_sankey_ww_mgmt_C, industrial_discharge)

# plot_sankey(rbind(df_sankey, thermoelec_water_use, ww_trade_comb, mgmtplan_all_downstream %>% select(-basin)))
plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind)
plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Cobb")
plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Cobb", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Gwinnett", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Fulton", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "DeKalb", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_ww_mgmt_C_ind, reg = "Douglas", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")


# balance PWS ----

# scale PWS inflows based on total PWS outflows
# calculate total PWS outlflows by county, year
# calculate share of each inflow (basins) and multiply by total outflow to get scaled inflows

pws_out <- df_sankey_ww_mgmt_C_ind %>%
  filter(grepl("publicWatSup", source)) %>%
  group_by(county, year) %>%
  summarise(pws_out = sum(value), .groups = "drop")

mgmtplan_surface_pws_scaled <- df_sankey_ww_mgmt_C_ind %>%
  filter(year >= 2020) %>%
  filter(source != "groundwater") %>% # exclude groundwater for now
  filter(grepl("publicWatSup", target)) %>%
  group_by(county, year, target) %>%
  mutate(source_share = value / sum(value)) %>% ungroup() %>%
  left_join(pws_out, by = c("county", "year")) %>%
  mutate(pws_in_scaled = source_share * pws_out) %>%
  select(county, year, source, target, value = pws_in_scaled, units)


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

# plot_sankey_enhanced(pws_diff, reg = "Fulton", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")

# linear:
# this is truly the core table for water with all details
# (source water bodies, ww facilities, ww transfers, discharge water bodies etc)
df_sankey_county_pws_balanced <- df_sankey_ww_mgmt_C_ind %>%
  # remove original pws surface inflows; pws target where source is not groundwater
  filter(!(grepl("publicWatSup", target) & source != "groundwater")) %>%
  rbind(mgmtplan_surface_pws_scaled) # add balanced pws inflows


plot_sankey_enhanced(df_sankey_county_pws_balanced %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value), .groups = "drop") %>% pretty_labels(),
                     animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Cobb")
plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Cobb", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Gwinnett", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Fulton", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "DeKalb", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")
plot_sankey_enhanced(df_sankey_county_pws_balanced, reg = "Douglas", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")


# loopy:
# water bodies as loops
plot_sankey_enhanced(df_sankey_county_pws_balanced %>%
                       # replace _ds in targets to get loops back
                       mutate(target = gsub("_ds", "", target))%>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value), .groups = "drop") %>% pretty_labels(),
                     animate = F, yr = 2024, label_units = "MGD")

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

plot_sankey_enhanced(df_sankey_water_county %>%
                       group_by(year, source, target, units) %>%
                       summarise(value = sum(value), .groups = "drop") %>% pretty_labels(),
                     show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")

plot_sankey_enhanced(df_sankey_water_county %>% pretty_labels(), reg = "Cobb", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")


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
#                      show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")


# loopy diagram
df_water_metro_loopy <- rbind(df_sankey_wwtrade,
      industrial_discharge %>% select(-county),
      thermoelec_water_use %>% select(-county),
      mgmtplan_all_basin_agg %>% select(-county)) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD")

plot_sankey_enhanced(df_water_metro_loopy %>% pretty_labels(),
                     show_values_in_labels = TRUE, yr = 2024, animate = F, label_units = "MGD")

# linear diagram
df_water_metro_linear_nosink <- rbind(df_sankey_wwtrade,
                             industrial_discharge %>% select(-county),
                             thermoelec_water_use %>% select(-county),
                             mgmtplan_all_basin_metro_nosink %>% select(-county)) %>%
  group_by(source, target, year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(units = "MGD")

plot_sankey_enhanced(df_water_metro_linear_nosink %>% pretty_labels(),
                     show_values_in_labels = T, yr = 2024, animate = F, label_units = "MGD")


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


plot_sankey_enhanced(df_water_metro_linear_wSW %>% pretty_labels(),
                     show_values_in_labels = T, yr = 2024, animate = F, label_units = "MGD")

# unique source-target pairs for documentation later
df_water_metro_linear_wSW %>% select(source, target) %>% distinct()


## add ww discharges sink ----

# determine in-county treatment vs exports ratio by year to split the sources of ww discharges
# does it need to be by county? Current thinking is no because this recieving type viz is only for metro level. County level already has exact water body sinks
ww_discharge_source_shares <- df_water_metro_linear_wSW %>%
  filter(grepl("wastewater", source)) %>%
  group_by(source, year) %>%
  mutate(ww_src_share = value / sum(value)) %>% ungroup()

### to discharge (except land and reuse) ----
# for facilities with _ds (because they are water bodies), keeping the rest as-is. merge Various with discharge
ww_sink_discharge <- ww_sink_downstream %>%
  mutate(sinktype = if_else(grepl("_ds|Various", target), "discharge", target)) %>%
  group_by(county, year, source, sinktype, units) %>% # leave the basin out
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(target = sinktype) %>%
  select(county, year, source, target, value, units)

# plot_sankey_enhanced(ww_sink_discharge %>% group_by(year, source, target, units) %>% summarise(value = sum(value), .groups = "drop") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")


# combine with metro diagram to get sources of in-county treatment and exports. the sinks will be sinktypes
df_water_metro_linear_wSW_discharge <- df_water_metro_linear_wSW %>%
  # add discharge sinks
  rbind(ww_sink_discharge %>%
          group_by(year, target, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          left_join(ww_discharge_source_shares %>% select(year, target, ww_src_share),
                    by = c("year")) %>%
          mutate(value = value * ww_src_share, source = target.y, target = target.x) %>%
          select(source, target, year, value, units))

plot_sankey_enhanced(df_water_metro_linear_wSW_discharge %>% pretty_labels(),
                     show_values_in_labels = T, yr = 2024, animate = F, label_units = "MGD")

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

plot_sankey_enhanced(ww_sink_discharge_type %>% group_by(year, source, target, units) %>% summarise(value = sum(value), .groups = "drop") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")


# combine with metro diagram. the sinks will be receiving types
df_water_metro_linear_wSW_discharge_type <- df_water_metro_linear_wSW %>%
  rbind(ww_sink_discharge_type %>%
          group_by(year, target, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          left_join(ww_discharge_source_shares %>% select(year, target, ww_src_share),
                    by = c("year")) %>%
          mutate(value = value * ww_src_share, source = target.y, target = target.x) %>%
          select(source, target, year, value, units))

plot_sankey_enhanced(df_water_metro_linear_wSW_discharge_type %>% pretty_labels(),
                     show_values_in_labels = T, yr = 2024, animate = F, label_units = "MGD")

# write_csv(df_water_metro_linear_wSW_discharge_type, paste0(SAVE_DIR, "water_metro_linear_wSW_discharge_receivingtype.csv"))


##############################################################################%
# ENERGY FOR WATER ----
##############################################################################%

# TODO: probably write out the balanced county level water flows df_sankey_county_pws_balanced
# to a csv and move this to a separate script for energy-for-water

# sample before E4W
plot_sankey_enhanced(df_sankey_county_pws_balanced %>% pretty_labels(),
                     reg = "Fulton", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")


en4swflows <- df_sankey_county_pws_balanced %>%
  filter(!grepl("ground", source) & # anything but groundwater
           grepl("publicWatSup|industrial|Plant|plant", target) & # uses of SW
           !grepl("publicWatSup", source) # but not PWS to industrial to avoid double counting
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
PUMPING_HEAD_GW <- 125 # AVG_GW_DEPTH_FT, assuming a middle number (domestic dominated due to it's high share in volumes)
PUMPING_HEAD_SW <- 25  # typical for surface water

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

FRESH_SW_TREAT_ENERGY_INT <- 405  # kWh/mg
FRESH_GW_TREAT_ENERGY_INT <- 205  # kWh/mg

en4water_treat <- rbind(en4sw_extract, en4gw_extract) %>%
  # only pws and residential need treatment
  filter(grepl("publicWatSup|residential", target)) %>%
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

DISTRIBUTION_ENERGY_INT <- 1040 # kWh/mg

en4water_distribute <- rbind(en4sw_extract, en4gw_extract) %>%
  # MGD to mg/yr to kWh/yr to EJ/yr
  mutate(elec_distribute = (value * DAYS_PER_YEAR) * DISTRIBUTION_ENERGY_INT * kWh_to_EJ) %>%
  group_by(county, watertype, target, year) %>%
  summarise(value = sum(elec_distribute), .groups = "drop") %>%
  mutate(source = paste0("distribute_", watertype), units = "EJ", value) %>%
  select(county, source, target, year, value, units)

# combine all energy for water
en4water_all <- rbind(en4water_extract, en4water_treat, en4water_distribute)

plot_sankey_enhanced(en4water_all %>% group_by(source, target, year) %>%
                       summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>%
                       mutate(units = "PJ") %>%
                       pretty_labels(),
                     show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "PJ")

plot_sankey_enhanced(en4water_all %>% mutate(value = value * EJ_to_PJ) %>% pretty_labels(),
                     reg = "Fulton", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "PJ")


###############################################################################%

## ww energy ----

# everything is treated as secondary so 2080 kWh/mg
# see the table here https://pnnl.github.io/interflow/wastewater_sector.html

WW_TREATMENT_ENERGY_INT <- 2080 # kWh/mg

# check facilities and flows (do !grepl to see which are not included)
# plot_sankey_enhanced(df_sankey_county_pws_balanced %>% filter(grepl("wastewater|inFrom", source)) %>% group_by(year, source, target, units) %>% summarise(value = sum(value), .groups = "drop") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "MGD")

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

en4ww_treat_dist_facility <- rbind(en4ww_treat_facility, en4ww_distribute_facility)

plot_sankey_enhanced(en4ww_treat_dist_facility %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "PJ")

# add treatment vs distribution node as a source
en4ww_treat_dist_facility_type <- en4ww_treat_dist_facility %>% select(-en_wwtype) %>%
  rbind(en4ww_treat_dist_facility %>%
          mutate(target = source, source = en_wwtype) %>%
          group_by(county, source, target, year, units) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          select(county, source, target, year, value, units))

plot_sankey_enhanced(en4ww_treat_dist_facility_type %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "PJ")


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


plot_sankey_enhanced(en4ww_treat_dist_cat %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "PJ")


# combine water and ww energy
en4water_ww <- rbind(en4water_all, en4ww_treat_dist_cat)

plot_sankey_enhanced(en4water_ww %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "PJ")

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
# plot_sankey_enhanced(en4water_ww_elec_facility %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_PJ, .groups = "drop") %>% mutate(units = "PJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "PJ")

plot_sankey_enhanced(en4water_ww_elec_facility %>% pretty_labels() %>% mutate(value = value * EJ_to_TJ, units = "TJ", .groups = "drop"),
                     reg = "Cobb", show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "TJ")

## METRO E4W ----
# without ww facilities: remove targets where sources are in-county treatment and ww_exports
en4water_ww_elec_use <- en4water_ww_elec_facility %>%
  filter(!(grepl("in-county treatment|ww_exports", source)))

plot_sankey_enhanced(en4water_ww_elec_use %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_TJ, .groups = "drop") %>% mutate(units = "TJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "TJ")

# simple table for energy calcs
# make elec flow direct into sectors
en4water <- en4water_ww_elec_use %>%
  filter(!grepl("electricity", source)) %>% # take out first elec node
  mutate(source = "electricity") %>% # reassgin all middle nodes to electricity
  group_by(county, source, target, year, units) %>%
  summarise(value = sum(value), .groups = "drop")

plot_sankey_enhanced(en4water %>% group_by(source, target, year) %>% summarise(value = sum(value) * EJ_to_TJ, .groups = "drop") %>% mutate(units = "TJ") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024, label_units = "TJ")


###############################################################################%
# ENERGY-WATER ----
###############################################################################%

# # add thermoelectric water with EFW
# en_water_ww_elec_thermo <- rbind(en4water_ww_elec %>% mutate(value = value * EJ_to_PJ), thermoelec_water_use %>% mutate(value = value /10))
#
# plot_sankey_enhanced(en_water_ww_elec_thermo %>% group_by(source, target, year) %>% summarise(value = sum(value), .groups = "drop") %>% pretty_labels(), show_values_in_labels = TRUE, animate = F, yr = 2024)












###############################################################################%
# # archive ----
#
# # test, dummy data for imports and exports
# # unique((df_sankey %>% filter(county == "Bartow", source == "wastewater"))$target)
# test_ww_flows <- data.frame(
#   fromcounty = c("Bartow", "Bartow", "Cherokee", "Cherokee", "Fulton", "Fulton"),
#   tocounty = c("Cherokee", "Fulton", "Bartow", "Fulton", "Bartow", "Cherokee"),
#   fromplace = c("PlaceA", "PlaceB", "PlaceC", "PlaceD", "PlaceE", "PlaceF"),
#   tofacility = c("CARTERSVILLE JAMES R. STAFFORD WPCP", "EMERSON HENRY JORDAN WWTP", "ADAIRSVILLE NORTH WPCP", "ADAIRSVILLE SOUTH WPCP", "BARTOW SOUTHEAST WPCP", "BARTOW TWO RUN WPCP"),
#   year = c(2020, 2020, 2020, 2020, 2020, 2020),
#   value = c(10, 14, 15, 12, 13, 5)
# )
#
# imports <- test_ww_flows %>% filter(fromcounty != "Bartow", tocounty == "Bartow") %>%
#   select(county = tocounty, source = fromcounty, target = tofacility, year, value)
# exports <- test_ww_flows %>% filter(fromcounty == "Bartow", tocounty != "Bartow") %>% mutate(source = "wastewater") %>%
#   select(county = fromcounty, source, target = tofacility, year, value)
#
# plot_sankey(rbind(df_sankey, imports, exports), reg = "Bartow")
#
# # attempts
# a <- df_wastewater_treated_ %>% rename(total_treated_byfacility = value) %>%
#   left_join(df_ww_conn_type, by = c("county", "year", "target" = "tofacility")) %>% filter(!is.na(value)) %>%
#   group_by(tofacility, year) %>%
#   summarise(total_connected = sum(value), .groups = "drop"),
# by = c("tofacility", "year")) %>%
#   mutate(perc_connected = if_else(total_treated_byfacility > 0,
#                                   total_connected / total_treated_byfacility * 100, 0)) %>%
#   filter(perc_connected > 100) %>%
#   arrange(desc(perc_connected)) -> df_ww_conn_exceed
#
#
# # calculate imports, exports
# ww_imports <- df_ww_conn_type %>%
#   group_by(tocounty, tofacility, year) %>%
#   summarise(imports = sum(value, na.rm = TRUE), .groups = 'drop') %>%
#   rename(county = tocounty, facility = tofacility)
#
# # I think total treated does NOT include imports, given half the imports are larger than total treated
# # adding imports to total treated to get in-county treated
# df_wastewater_treated_imports_adj <- df_wastewater_treated_ %>%
#   left_join(ww_imports, by = c("county", "target" = "facility", "year")) %>%
#   mutate(imports = replace_na(imports, 0),
#          in_county_treated = value + imports) %>%
#   select(county, year, facility = target, in_county_treated, imports)
#
# b <- df_wastewater_treated_imports_adj %>% filter(imports > 0) # should be none
#
# ww_exports <- df_ww_conn_type %>%
#   group_by(fromcounty, tocounty, tofacility, year) %>%
#   summarise(ww_exports = sum(value), .groups = "drop") %>%
#   select(county = fromcounty, source = tocounty, target = tofacility, year, value = ww_exports)
#
# # total treated by a facility = df_wastewater_treated_
# ww_flow <- ww_imports %>%
#   left_join(ww_exports, by = c("fromcounty" = "tocounty", "tofacility", "year")) %>%
#   mutate(ww_exports = replace_na(ww_exports, 0),
#          net_flow = ww_imports - ww_exports) %>%
#   filter(net_flow != 0)
#
# # quick test for Cherokee county
# df_ww_conn_cher <- df_ww_conn %>% filter(tocounty == "Cherokee" | fromcounty == "Cherokee") %>% filter(tocounty != fromcounty) %>%
#   # replace "Little River WRF (SEE CHEROKEE CO)" with "Fulton Co Little River WRF" in tofacility
#   mutate(tofacility = if_else(tofacility == "Little River WRF (SEE CHEROKEE CO)", "Fulton Co Little River WRF", tofacility))
#
#
# # main workflow
# df_ww_conn_cher %>% rename(source = fromcounty, target = tofacility) %>%
#   select(county = source, year, source, target, value = flow) %>%
#   rbind(df_ww_conn_cher %>% rename(source = fromfacility, target = tocounty) %>%
#           select(county = target, year, source, target, value)) %>%
#   filter(county == "Cherokee") %>%
#   rbind(df_sankey %>% filter(county == "Cherokee")) %>%
#   plot_sankey(., reg = "Cherokee", title = "Cherokee County Wastewater Connections")
#
#
# df_ww_conn_cher %>% rename(source = fromcounty, target = tofacility) %>% mutate(county = "Cherokee") %>%
#   select(county, year, source, target, value = flow) %>% rbind(df_sankey) %>%
#   # if source = Cherokee, replace it "wastewater"
#   mutate(source = if_else(source == "Cherokee", "wastewater", source)) -> df_plotcher
# plot_sankey(df_plotcher %>% unique(), reg = "Cherokee", title = "Cherokee County Wastewater Connections")
#
#
#
# ## testing
#
# # Load required libraries
# library(dplyr)
# library(tidyr)
# library(networkD3)  # for Sankey diagrams
# library(ggplot2)
#
# # Step 1: Data Preparation and Validation
# prepare_data <- function(df_wastewater_treated_, df_ww_conn_type, target_year = 2020) {
#
#   # Filter for target year
#   treatment_data <- df_wastewater_treated_ %>%
#     filter(year == target_year) %>%
#     select(county, target, value) %>%
#     rename(facility = target, total_treatment = value)
#
#   connection_data <- df_ww_conn_type %>%
#     filter(year == target_year)
#
#   return(list(treatment = treatment_data, connections = connection_data))
# }
#
# # Step 2: Calculate Imports by Facility
# calculate_imports <- function(connection_data, treatment_data) {
#
#   imports <- connection_data %>%
#     filter(!is.na(tofacility)) %>%  # Only facility-level imports
#     group_by(tocounty, tofacility) %>%
#     summarise(imports = sum(value, na.rm = TRUE), .groups = 'drop') %>%
#     rename(county = tocounty, facility = tofacility)
#
#   return(imports)
# }
#
# # Step 3: Calculate Exports by County (limitation: not at facility level)
# calculate_exports <- function(connection_data) {
#
#   exports <- connection_data %>%
#     group_by(fromcounty) %>%
#     summarise(total_exports = sum(value, na.rm = TRUE), .groups = 'drop') %>%
#     rename(county = fromcounty)
#
#   return(exports)
# }
#
# # Step 4: Calculate In-County Treatment
# calculate_incounty_treatment <- function(treatment_data, imports) {
#
#   # Join treatment data with imports
#   facility_flows <- treatment_data %>%
#     left_join(imports, by = c("county", "facility")) %>%
#     mutate(
#       imports = coalesce(imports, 0),
#       incounty_treatment = total_treatment - imports,
#       # Flag potential issues
#       issue_flag = case_when(
#         incounty_treatment < 0 ~ "negative_incounty",
#         incounty_treatment == 0 ~ "zero_incounty",
#         TRUE ~ "normal"
#       )
#     )
#
#   return(facility_flows)
# }
#
# # Step 5: Create Sankey Data Structure
# prepare_sankey_data <- function(facility_flows, connection_data, target_county = NULL) {
#
#   # Filter for specific county if requested
#   if (!is.null(target_county)) {
#     facility_flows <- facility_flows %>% filter(county == target_county)
#     connection_data <- connection_data %>%
#       filter(fromcounty == target_county | tocounty == target_county)
#   }
#
#   # Create nodes and links for Sankey
#   nodes <- data.frame()
#   links <- data.frame()
#
#   # In-county treatment flows
#   incounty_flows <- facility_flows %>%
#     filter(incounty_treatment > 0) %>%
#     mutate(
#       source = paste0(county, "_generation"),
#       target = paste0(county, "_", facility),
#       value = incounty_treatment,
#       flow_type = "incounty"
#     ) %>%
#     select(source, target, value, flow_type)
#
#   # Import flows
#   import_flows <- connection_data %>%
#     filter(!is.na(tofacility), value > 0) %>%
#     mutate(
#       source = paste0(fromcounty, "_export"),
#       target = paste0(tocounty, "_", tofacility),
#       value = value,
#       flow_type = "import"
#     ) %>%
#     select(source, target, value, flow_type)
#
#   # Export flows (county level)
#   export_flows <- connection_data %>%
#     filter(!is.na(fromcounty), value > 0) %>%
#     mutate(
#       source = paste0(fromcounty, "_generation"),
#       target = paste0(tocounty, "_import"),
#       value = value,
#       flow_type = "export"
#     ) %>%
#     select(source, target, value, flow_type)
#
#   # Combine all flows
#   all_flows <- bind_rows(incounty_flows, import_flows, export_flows)
#
#   return(all_flows)
# }
#
# # Step 6: Data Quality Checks
# perform_quality_checks <- function(facility_flows, connection_data) {
#
#   # Check for negative in-county treatment
#   negative_incounty <- facility_flows %>%
#     filter(incounty_treatment < 0) %>%
#     select(county, facility, total_treatment, imports, incounty_treatment)
#
#   # Check mass balance by county
#   county_balance <- facility_flows %>%
#     group_by(county) %>%
#     summarise(
#       total_treatment = sum(total_treatment),
#       total_imports = sum(imports),
#       total_incounty = sum(incounty_treatment),
#       .groups = 'drop'
#     )
#
#   # Check for missing connections
#   facilities_in_treatment <- facility_flows %>%
#     distinct(county, facility)
#
#   facilities_in_connections <- connection_data %>%
#     filter(!is.na(tofacility)) %>%
#     distinct(tocounty, tofacility) %>%
#     rename(county = tocounty, facility = tofacility)
#
#   missing_facilities <- anti_join(facilities_in_treatment, facilities_in_connections,
#                                   by = c("county", "facility"))
#
#   return(list(
#     negative_incounty = negative_incounty,
#     county_balance = county_balance,
#     missing_facilities = missing_facilities
#   ))
# }
#
# # Step 7: Create Sankey Visualization
# create_sankey_plot <- function(sankey_data, title = "Wastewater Flow") {
#
#   # Create node list
#   nodes <- data.frame(
#     name = unique(c(sankey_data$source, sankey_data$target)),
#     stringsAsFactors = FALSE
#   ) %>%
#     mutate(id = row_number() - 1)
#
#   # Create links with node IDs
#   links <- sankey_data %>%
#     left_join(nodes, by = c("source" = "name")) %>%
#     rename(source_id = id) %>%
#     left_join(nodes, by = c("target" = "name")) %>%
#     rename(target_id = id) %>%
#     select(source_id, target_id, value, flow_type)
#
#   # Create Sankey plot
#   sankeyNetwork(
#     Links = links,
#     Nodes = nodes,
#     Source = "source_id",
#     Target = "target_id",
#     Value = "value",
#     NodeID = "name",
#     fontSize = 12,
#     nodeWidth = 30,
#     title = title
#   )
# }
#
# # Main execution function
# analyze_wastewater_flows <- function(df_wastewater_treated_, df_ww_conn_type,
#                                      target_year = 2020, target_county = NULL) {
#
#   # Step 1: Prepare data
#   data <- prepare_data(df_wastewater_treated_, df_ww_conn_type, target_year)
#
#   # Step 2-4: Calculate flows
#   imports <- calculate_imports(data$connections, data$treatment)
#   exports <- calculate_exports(data$connections)
#   facility_flows <- calculate_incounty_treatment(data$treatment, imports)
#
#   # Step 5: Quality checks
#   quality_checks <- perform_quality_checks(facility_flows, data$connections)
#
#   # Step 6: Prepare Sankey data
#   sankey_data <- prepare_sankey_data(facility_flows, data$connections, target_county)
#
#   # Step 7: Create visualization
#   sankey_plot <- create_sankey_plot(sankey_data,
#                                     paste("Wastewater Flows",
#                                           ifelse(is.null(target_county), "", target_county),
#                                           target_year))
#
#   return(list(
#     facility_flows = facility_flows,
#     quality_checks = quality_checks,
#     sankey_data = sankey_data,
#     sankey_plot = sankey_plot
#   ))
# }
#
# # Usage example:
# # results <- analyze_wastewater_flows(df_wastewater_treated_, df_ww_conn_type,
# #                                   target_year = 2020, target_county = "Bartow")
# # results$sankey_plot
# # View(results$quality_checks$negative_incounty)
