# MAWEI figures — publication figure suite
#
#   Rscript R/figures.R                       writes a NEW analysis/figures_v<n>/
#   MAWEI_FIG_REUSE=1 Rscript R/figures.R     overwrites the highest existing version
#   MAWEI_FIG_DIR=path Rscript R/figures.R    writes to a chosen directory
#
# Reads the tables from R/analysis.R plus the coordinate and basin layers from R/prep_spatial.R.
# Landscape, roughly 16:9, unless a panel grid needs otherwise.
#
# Design rules
#   - print the value in place; never rely on a colour bar alone
#   - one colour for categories that share a physical meaning
#   - state the finding on the panel, so a reader who skips the caption still gets it
#   - keep text off the data; put legends in dead space
#   - no empty panels: if a row has spare width, the panels widen to fill it
#
# Hassan Niazi, Aug 2026

source("functions.R")

FIG_DIR <- fig_dir(); message("== writing to ", FIG_DIR, " ==")
FIG_DIR_P <- "analysis/figures_paper/"
dir.create(FIG_DIR_P, showWarnings = FALSE, recursive = TRUE)

save_fig <- function(p, name, w, h, save_pdf = FALSE) {
  ggsave(paste0(FIG_DIR, name, ".png"), p, width = w, height = h, dpi = 300, bg = "white")
  if (save_pdf) {
    ggsave(paste0(FIG_DIR, name, ".pdf"), p, width = w, height = h, device = cairo_pdf, bg = "white")
  }
  message(sprintf("  %-38s %4.1f x %4.1f in", name, w, h))
}

message("== reading tables ==")
for (t in c("A1_water_balance_metro","A2_energy_balance_metro","A3_water_by_sector",
            "A4_energy_by_sector","A5_fuel_mix","A6_basin_withdrawals",
            "A6b_basin_concentration","A7_discharge_destinations","A8_county_profile",
            "B1b_nrw_scenarios","B2_ii_energy_penalty","B2b_ii_energy_metro",
            "C1_electricity_self_sufficiency","C2_efficiency_llnl_vs_corrected",
            "C3_generation_by_plant","D1_energy_intensity_of_water",
            "D2_water_intensity_of_electricity","D3_coupling_asymmetry",
            "E1b_transfer_network_nodes","G1_validation_permitted_capacity",
            "H1_datacentres_existing","H1b_datacentres_existing_by_county",
            "H2_datacentre_projections","H4_datacentre_projected_sites",
            "H4b_datacentre_sites_by_county","I1_cost_of_losses",
            "I2_leakage_recovery_payback","I3_datacentre_capital_vs_leakage",
            "J1_spatial_transfer_edges","J1b_spatial_network_summary",
            "J2_receiving_plant_loading","K1_basin_balance","K3_basin_burden",
            "K4_interbasin_sewage_transfer","L1_transfer_gross_net_pairs",
            "L1b_transfer_gross_net_summary","L2_transfer_by_county_gross_net",
            "M1_settlement_by_county","M2_population_by_basin",
            "M3_water_performance_vs_settlement","M3b_settlement_correlations",
            "P4_treatment_concentration",
            "P6_energy_for_water_by_stage","P8_plant_thermal_performance",
            "P9_downscaling_sensitivity")) {
  assign(str_extract(t, "^[A-Z][0-9]+[a-z]?"), tab(t))
}

xy <- read_csv(paste0(DATA_DIR, "spatial_facility_coords.csv"), show_col_types = FALSE)
YR <- max(A1$year)

sf_cty <- st_read(paste0(DATA_DIR, "geojson-counties-fips.json"), quiet = TRUE) %>%
  rename_with(tolower) %>% filter(id %in% fips) %>%
  mutate(county = name) %>% select(county, geometry) %>% st_set_crs(4326)
cent <- sf_cty %>% st_centroid() %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>% st_drop_geometry()
sf_basin <- st_read(paste0(DATA_DIR, "spatial_basins.geojson"), quiet = TRUE)
cty_basin <- read_csv(paste0(DATA_DIR, "spatial_county_basin_area.csv"), show_col_types = FALSE)

# Census tracts, for the settlement underlay and the density panels. The ACS extract carries
# MEASURED population, so density is real rather than the tract-area proxy an earlier version used.
# ExtraNotes: falls back to the cartographic boundary file if the ACS extract is absent, in which
# case `pop_density` is NA and the density panels degrade to an outline underlay rather than
# failing. Run Rscript R/prep_acs.R to build the extract.
ACS_GEO   <- paste0(DATA_DIR, "acs_tract_metro.geojson")
TRACT_SHP <- paste0(DATA_DIR, "cb_2025_13_tract_500k/cb_2025_13_tract_500k.shp")
tracts_sf <- if (file.exists(ACS_GEO)) {
  st_read(ACS_GEO, quiet = TRUE) %>% st_set_crs(4326) %>%
    mutate(land_sqkm = as.numeric(st_area(.)) / 1e6) %>%
    select(county, pop, pop_density, median_hh_income, mean_commute_min, land_sqkm)
} else if (file.exists(TRACT_SHP)) {
  st_read(TRACT_SHP, quiet = TRUE) %>%
    mutate(cfips = as.numeric(paste0(STATEFP, COUNTYFP))) %>%
    filter(cfips %in% fips) %>% st_transform(4326) %>%
    mutate(land_sqkm = as.numeric(ALAND) / 1e6,
           pop = NA_real_, pop_density = NA_real_,
           median_hh_income = NA_real_, mean_commute_min = NA_real_, county = NA_character_) %>%
    select(county, pop, pop_density, median_hh_income, mean_commute_min, land_sqkm)
} else NULL

# Choropleth with the value printed inside each county on its own line, so the map reads without
# the colour bar. The label colour is fixed dark rather than value-mapped, because a value-mapped
# label vanishes against its own fill at one end of the scale.
cmap <- function(df, col, title, sub = NULL, pal = "Blues", dir = 1, fmt = "%.1f",
                 unit = "", legend = NULL) {
  d <- sf_cty %>% left_join(df %>% select(county, v = all_of(col)), by = "county")
  ggplot(d) +
    geom_sf(aes(fill = v), colour = "white", linewidth = 0.35) +
    geom_sf_text(aes(label = paste0(county, "\n", sprintf(fmt, v), unit)),
                 size = 1.65, colour = "grey12", lineheight = 0.95, fontface = "bold") +
    scale_fill_distiller(palette = pal, direction = dir, na.value = "grey93",
                         guide = guide_colourbar(title.position = "top", barwidth = 5,
                                                 barheight = 0.28)) +
    labs(title = title, subtitle = sub, fill = legend %||% col) +
    theme_map()
}

# Common metro basemap: all facilities in light grey, so any highlighted subset is read against
# the whole system rather than in isolation.
base_layers <- function(basin = FALSE, all_fac = TRUE) {
  l <- list()
  if (basin) l <- c(l, list(
    geom_sf(data = sf_basin %>% filter(basin != "Broad"), aes(fill = basin),
            colour = "white", linewidth = 0.3, alpha = 0.18, inherit.aes = FALSE),
    scale_fill_manual(values = BASIN_COLS, name = "basin")))
  l <- c(l, list(geom_sf(data = sf_cty, fill = NA, colour = "grey55", linewidth = 0.35,
                         inherit.aes = FALSE)))
  if (all_fac) l <- c(l, list(
    geom_point(data = xy %>% filter(kind == "wastewater plant"),
               aes(lon, lat), colour = "grey72", size = 0.85, inherit.aes = FALSE)))
  l
}

###############################################################################%
# Fig 1: the system ----
message("\n== Fig 1: the system ==")

p1a <- ggplot(sf_cty) +
  geom_sf(fill = "grey94", colour = "white", linewidth = 0.45) +
  geom_point(data = xy %>% filter(kind == "wastewater plant"),
             aes(lon, lat, size = capacity), colour = C_WATER, alpha = 0.5) +
  geom_point(data = xy %>% filter(kind == "power plant", capacity > 300),
             aes(lon, lat, size = capacity / 45), colour = C_ENERGY, alpha = 0.9, shape = 17) +
  geom_sf_text(aes(label = county), size = 1.9, colour = "grey30", fontface = "bold") +
  scale_size_area(max_size = 8, guide = "none") + coord_sf(expand = FALSE) +
  labs(title = "Infrastructure of both systems",
       subtitle = paste0(sum(xy$kind == "wastewater plant"), " wastewater plants (circles) and ",
                         sum(xy$kind == "power plant" & xy$capacity > 300),
                         " major power plants (triangles).\nSymbol area is capacity. ",
                         "Generation sits at the edge; treatment is everywhere.")) +
  theme_map()

stage_bar <- function(d, fill, title, sub) {
  ggplot(d, aes(stage, v)) +
    geom_col(width = 0.6, fill = fill) +
    geom_text(aes(label = comma(round(v))), vjust = -0.4, size = 2.5, colour = "grey25") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.17))) +
    labs(x = NULL, y = NULL, title = title, subtitle = sub) + theme_mawei()
}
w24 <- A1 %>% filter(year == YR); e24 <- A2 %>% filter(year == YR)
p1b <- stage_bar(tibble(stage = factor(c("withdrawn","delivered","returned","consumed\nor leaked"),
                          levels = c("withdrawn","delivered","returned","consumed\nor leaked")),
                        v = c(w24$withdrawal, w24$pws_throughput, w24$ww_collected, w24$losses)),
                 C_WATER, paste0("Water, ", YR, " (MGD)"),
                 sprintf("%.0f%% of input never returns as flow", w24$loss_share_pct))
p1c <- stage_bar(tibble(stage = factor(c("primary\ninput","reaching\nend uses","useful\nservices",
                          "rejected\nor lost"),
                          levels = c("primary\ninput","reaching\nend uses","useful\nservices",
                                     "rejected\nor lost")),
                        v = c(e24$total_input, sum(A4$consumed[A4$year == YR]),
                              e24$services, e24$all_rejected)),
                 C_ENERGY, paste0("Energy, ", YR, " (PJ)"),
                 sprintf("%.0f%% ends as rejected heat or losses",
                         100 * e24$all_rejected / e24$total_input))

idx <- bind_rows(pop = A1 %>% select(year, v = pop), water = A1 %>% select(year, v = withdrawal),
                 energy = A2 %>% select(year, v = total_input),
                 elec = A2 %>% select(year, v = elec_supply),
                 ww = A1 %>% select(year, v = ww_collected), .id = "series") %>%
  group_by(series) %>% mutate(idx = 100 * v / first(v)) %>% ungroup() %>%
  mutate(series = recode(series, pop = "Population", water = "Water withdrawal",
                         energy = "Energy input", elec = "Electricity", ww = "Wastewater"))
# Values are printed at each point, not only the endpoint index, because a reader asked to judge a
# dip needs the magnitude. The 2023 electricity fall is annotated because it is REAL, not an
# artefact: Georgia residential electricity in SEDS fell 4.5% that year, local generation rose
# 6.8% as Bowen ran harder, and imports absorbed the difference by falling 14.8%.
p1d <- ggplot(idx, aes(year, idx, colour = series)) +
  geom_hline(yintercept = 100, linewidth = 0.3, colour = "grey85") +
  geom_line(linewidth = 0.85) + geom_point(size = 1.5) +
  geom_text(data = idx %>% filter(series == "Electricity"),
            aes(label = sprintf("%.0f", v)), vjust = 2.1, size = 1.9, colour = "#C05A12",
            show.legend = FALSE) +
  geom_text(data = idx %>% filter(series == "Water withdrawal"),
            aes(label = sprintf("%.0f", v)), vjust = -1.5, size = 1.9, colour = C_WATER,
            show.legend = FALSE) +
  annotate("segment", x = 2023, xend = 2023, y = 96, yend = 102.5,
           colour = "grey45", linewidth = 0.3, linetype = "dotted") +
  annotate("label", x = 2023, y = 95, label = "2023 dip is real:\nmild year, demand -2.1%",
           size = 1.95, colour = "grey30", lineheight = 0.95, label.size = 0,
           fill = alpha("white", 0.85)) +
  geom_text_repel(data = idx %>% filter(year == YR),
                  aes(label = sprintf("%s %+.1f%%", series, idx - 100)), size = 2.4,
                  hjust = 0, direction = "y", nudge_x = 0.12, segment.size = 0.2,
                  segment.colour = "grey75", min.segment.length = 0, box.padding = 0.12,
                  show.legend = FALSE) +
  scale_colour_manual(values = c("Energy input" = C_ENERGY, "Electricity" = "#C05A12",
                                 "Wastewater" = "#1F5F8B", "Water withdrawal" = C_WATER,
                                 "Population" = "grey45"), guide = "none") +
  scale_x_continuous(breaks = A1$year, limits = c(min(A1$year), YR + 2.1)) +
  scale_y_continuous(limits = c(93, NA)) +
  labs(x = NULL, y = paste0("index, ", min(A1$year), " = 100"),
       title = "Resource use is outgrowing population",
       subtitle = "Labels are absolute values: electricity in PJ, water in MGD") +
  theme_mawei()

save_fig((p1a | (p1b / p1c) | p1d) + plot_layout(widths = c(1.15, 0.9, 1.25)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig1_system_and_scale", 16, 5.8)

###############################################################################%
# Fig 2: water ----
message("\n== Fig 2: water ==")

b <- A6 %>% mutate(basin = str_remove(basin, " Basin")) %>% filter(mgd > 0) %>%
  mutate(basin = factor(basin, levels = A6 %>% filter(year == YR) %>%
                          mutate(basin = str_remove(basin, " Basin")) %>%
                          arrange(mgd) %>% pull(basin)))
blab <- b %>% filter(year == YR) %>% stack_label_y(basin, mgd)
p2a <- ggplot(b, aes(year, mgd, fill = basin)) +
  geom_area(colour = "white", linewidth = 0.3) +
  geom_text(data = blab %>% filter(mgd > 20),
            aes(x = YR - 0.06, y = ypos,
                label = sprintf("%s\n%.0f MGD (%.0f%%)", basin, mgd, share_pct)),
            inherit.aes = FALSE, hjust = 1, size = 2.4, colour = "white", fontface = "bold",
            lineheight = 0.95) +
  geom_text_repel(data = blab %>% filter(mgd <= 20),
                  aes(x = min(b$year), y = ypos, label = sprintf("%s %.0f MGD", basin, mgd)),
                  inherit.aes = FALSE, hjust = 0, size = 2.2, colour = "grey25", direction = "y",
                  nudge_x = 0.25, segment.size = 0.2, segment.colour = "grey70",
                  min.segment.length = 0) +
  scale_fill_manual(values = BASIN_COLS, guide = "none") +
  scale_x_continuous(breaks = A6$year) +
  labs(x = NULL, y = "surface withdrawal (MGD)",
       title = "One river supplies two thirds of the region",
       subtitle = sprintf("Herfindahl index %.2f. The metro sits near the Chattahoochee headwaters,\nwhere little upstream flow exists to draw on.",
                          A6b$hhi[A6b$year == YR])) +
  theme_mawei()

s24 <- A3 %>% filter(year == YR) %>% mutate(sector = str_to_title(sector))
p2b <- ggplot(s24 %>% pivot_longer(c(consumed, returned), names_to = "fate", values_to = "v") %>%
                mutate(fate = factor(fate, c("consumed","returned"),
                                     c("consumed or lost","returned as sewage"))),
              aes(reorder(sector, supplied), v, fill = fate)) +
  geom_col(width = 0.6) +
  geom_text(data = s24, aes(x = sector, y = supplied,
                            label = sprintf(" %.0f%% returned", 100 * return_ratio)),
            inherit.aes = FALSE, hjust = 0, size = 2.4, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("consumed or lost" = C_LOSS, "returned as sewage" = C_WATER),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(x = NULL, y = "MGD", title = paste0("Where supplied water ends up, ", YR),
       subtitle = "Irrigation-heavy sectors return least; the gap is consumptive use") +
  theme_mawei() + theme(legend.position = c(0.70, 0.22),
                        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))

d24 <- A7 %>% filter(year == YR) %>%
  mutate(destination = str_to_title(destination),
         class = case_when(destination %in% c("River","Creek") ~ "Flowing water",
                           destination %in% c("Lake","Reservoir","Wetland") ~ "Still water",
                           destination == "Reuse" ~ "Beneficial reuse",
                           TRUE ~ "Land application"))
p2c <- ggplot(d24, aes(reorder(destination, mgd), mgd, fill = class)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf(" %.2f (%.2f%%)", mgd, share_pct)), hjust = 0, size = 2.3,
            colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_log10(expand = expansion(mult = c(0, 0.40))) +
  scale_fill_manual(values = c("Flowing water" = C_FLOW, "Still water" = C_STILL,
                               "Beneficial reuse" = C_GOOD, "Land application" = C_LAND),
                    name = NULL) +
  labs(x = NULL, y = "MGD (log scale)",
       title = paste0("Treated effluent by receiving environment, ", YR),
       subtitle = "Still waters accumulate what flowing waters carry away") +
  theme_mawei() +
  theme(legend.position = c(0.74, 0.30),
        legend.background = element_rect(fill = alpha("white", 0.88), colour = "grey85",
                                         linewidth = 0.2))

gp <- A8 %>% filter(year == YR) %>% select(county, pws_gpcd, nrw_pct)
p2d <- ggplot(gp, aes(reorder(county, pws_gpcd), pws_gpcd, fill = nrw_pct)) +
  geom_col(width = 0.62) +
  geom_hline(yintercept = 85, linetype = "dashed", colour = C_LOSS, linewidth = 0.4) +
  geom_text(aes(label = sprintf(" %.0f", pws_gpcd)), hjust = 0, size = 2.3, colour = "grey30") +
  annotate("text", x = 2.4, y = 86, label = "US domestic average ~85 gpcd", hjust = 0,
           size = 2.3, colour = C_LOSS) +
  coord_flip(clip = "off") +
  scale_fill_distiller(palette = "Reds", direction = 1, name = "non-revenue\nwater (%)",
                       guide = guide_colourbar(barwidth = 0.3, barheight = 3.6)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "public supply delivered (gpcd)",
       title = "Low demand per person, uneven losses",
       subtitle = "Frugal use and a tight network are separate achievements") +
  theme_mawei() + theme(legend.position = c(0.86, 0.30))

# Basin burden: supply share against land share. A basin above the diagonal is doing more work
# than its footprint implies, which is the quantitative form of the headwater problem.
kb <- K3 %>% filter(total_withdrawal_mgd > 0)
p2e <- ggplot(kb, aes(land_share_pct, supply_share_pct)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(aes(size = total_withdrawal_mgd, colour = basin), alpha = 0.85) +
  geom_text_repel(aes(label = sprintf("%s\n%.2fx", basin, burden_ratio)), size = 2.3,
                  colour = "grey20", lineheight = 0.95, box.padding = 0.5,
                  segment.colour = "grey70", segment.size = 0.2) +
  annotate("text", x = 30, y = 12, label = "below the line:\nsupplies less than its area",
           size = 2.1, colour = "grey45", lineheight = 0.95) +
  scale_colour_manual(values = BASIN_COLS, guide = "none") +
  scale_size_area(max_size = 9, guide = "none") +
  labs(x = "share of metro land area (%)", y = "share of metro withdrawal (%)",
       title = "Which basins carry the load",
       subtitle = "The Chattahoochee supplies 1.7 times its share of the region's area") +
  theme_mawei()

# Sixth panel: the basin loop closed. The Sankeys break the cycle by suffixing downstream water
# bodies with _ds so the diagram stays acyclic, which hides the fact that a basin both supplies
# and receives. Plotting withdrawal against discharge per basin restores it, and the diagonal
# separates basins the metro takes FROM those it gives TO.
kl <- K1 %>% filter(year == YR, total_withdrawal_mgd + discharge_mgd > 0) %>%
  left_join(M2 %>% select(basin, population), by = "basin")
p2f <- ggplot(kl, aes(total_withdrawal_mgd, discharge_mgd)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_segment(aes(xend = total_withdrawal_mgd, yend = total_withdrawal_mgd, colour = basin),
               linewidth = 0.4, alpha = 0.6) +
  geom_point(aes(size = population, colour = basin), alpha = 0.9) +
  geom_text_repel(aes(label = sprintf("%s\n%.0f in, %.0f out", basin, total_withdrawal_mgd,
                                      discharge_mgd)),
                  size = 2.2, colour = "grey20", lineheight = 0.95, box.padding = 0.55,
                  segment.colour = "grey70", segment.size = 0.2) +
  annotate("text", x = 430, y = 480, label = "receives more\nthan it gives", size = 2.1,
           colour = "grey45", lineheight = 0.95) +
  annotate("text", x = 460, y = 60, label = "gives more\nthan it receives", size = 2.1,
           colour = "grey45", lineheight = 0.95) +
  scale_colour_manual(values = BASIN_COLS, guide = "none") +
  scale_size_area(max_size = 8, guide = "none") +
  scale_x_continuous(limits = c(-20, 560)) + scale_y_continuous(limits = c(-20, 560)) +
  labs(x = "withdrawn from the basin (MGD)", y = "discharged to the basin (MGD)",
       title = "The loop the Sankey has to break",
       subtitle = "Metro Atlanta takes from the Chattahoochee and gives to the Ocmulgee.\nSymbol area is the population living in each basin.") +
  theme_mawei()

save_fig((p2a | p2b | p2e) / (p2c | p2d | p2f) +
           plot_layout(heights = c(1, 0.95)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig2_water_structure", 16, 8.8)

###############################################################################%
# Fig 3: energy ----
message("\n== Fig 3: energy ==")

fm <- A5 %>% filter(pj > 0.05) %>%
  mutate(fuel = recode(fuel, "out_metro_elec_import" = "Imported electricity",
                       "Hydroelectric" = "Hydro", "onsiteBTM" = "On-site"))
fm <- fm %>% left_join(fm %>% filter(year == min(year)) %>% select(fuel, base = pj), by = "fuel")
p3a <- ggplot(fm, aes(year, pj, group = fuel, colour = fuel)) +
  geom_line(aes(y = base), linetype = "dotted", linewidth = 0.4, alpha = 0.8) +
  geom_line(linewidth = 0.85) + geom_point(size = 1.4) +
  geom_text_repel(data = fm %>% filter(year == YR),
                  aes(label = sprintf("%s %+.0f%%", fuel, 100 * (pj - base) / base)),
                  size = 2.3, hjust = 0, direction = "y", nudge_x = 0.3, segment.size = 0.2,
                  segment.colour = "grey70", min.segment.length = 0, box.padding = 0.12,
                  show.legend = FALSE) +
  scale_y_log10() +
  scale_x_continuous(breaks = A5$year, limits = c(min(A5$year), YR + 1.7)) +
  scale_colour_brewer(palette = "Dark2", guide = "none") +
  labs(x = NULL, y = "PJ (log scale)", title = "The fuel mix moved away from low carbon",
       subtitle = "Dotted line is each fuel's 2020 level. Coal rose while hydro fell.") +
  theme_mawei()

e4 <- A4 %>% filter(year == YR) %>% mutate(sector = str_to_title(sector),
                                           hi = grepl("Transport", sector))
p3b <- ggplot(e4, aes(reorder(sector, consumed), consumed, fill = hi)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf(" %.0f PJ (%.1f%%)", consumed, share_pct)), hjust = 0,
            size = 2.3, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30))) +
  scale_fill_manual(values = c(`TRUE` = C_ENERGY, `FALSE` = C_GREY), guide = "none") +
  labs(x = NULL, y = "PJ", title = paste0("End-use energy by sector, ", YR),
       subtitle = sprintf("Transport is half of demand and petroleum is %.0f%% of primary input:\nthe same fact seen twice",
                          A5$share_pct[A5$year == YR & A5$fuel == "Petroleum"])) +
  theme_mawei()

c24 <- C3 %>% filter(year == YR, !is.na(capacity_factor)) %>%
  mutate(fuel = case_when(grepl("Bowen", plant) ~ "coal",
                          grepl("McDonough|Yates", plant) ~ "natural gas", TRUE ~ ""),
         lab = sprintf("%s\n%.1f PJ | CF %.2f", fuel, pj, capacity_factor))
p3c <- ggplot(c24, aes(reorder(plant, pj), pj, fill = capacity_factor)) +
  geom_col(width = 0.58) +
  geom_text(aes(label = paste0(" ", lab)), hjust = 0, size = 2.2, colour = "grey25",
            lineheight = 0.95) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.48))) +
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(0, 0.8),
                       name = "capacity factor",
                       guide = guide_colourbar(barwidth = 4.6, barheight = 0.32,
                                               title.position = "top", title.hjust = 0)) +
  labs(x = NULL, y = "PJ generated", title = paste0("Generation by plant, ", YR),
       subtitle = "The gas plant runs hardest; the coal plant is largest") +
  theme_mawei() + theme(legend.position = c(0.70, 0.24),
                        legend.direction = "horizontal",
                        legend.background = element_rect(fill = alpha("white", 0.85),
                                                         colour = NA))

us <- A4 %>% filter(year == YR) %>%
  mutate(sector = str_to_lower(sector),
         eff = if_else(sector %in% names(SECTOR_EFFICIENCY), SECTOR_EFFICIENCY[sector],
                       DEFAULT_EFFICIENCY),
         eff = if_else(grepl("transport", sector), TRANSPORT_EFFICIENCY_REAL, eff),
         useful = consumed * eff, rejected = consumed - useful, sector = str_to_title(sector)) %>%
  filter(consumed > 1) %>%
  pivot_longer(c(useful, rejected), names_to = "k", values_to = "v") %>%
  mutate(k = factor(k, c("rejected","useful"), c("rejected","useful services")))
p3d <- ggplot(us, aes(reorder(sector, v, sum), v, fill = k)) +
  geom_col(width = 0.62) + coord_flip() +
  scale_fill_manual(values = c("rejected" = C_LOSS, "useful services" = C_GOOD), name = NULL) +
  labs(x = NULL, y = "PJ", title = paste0("Useful energy by sector, ", YR),
       subtitle = "Transport corrected to a real fleet. It consumes most and wastes most.") +
  theme_mawei() + theme(legend.position = c(0.74, 0.22),
                        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))

p3e <- ggplot(C2, aes(year)) +
  geom_ribbon(aes(ymin = eff_real_pct, ymax = eff_llnl_pct), fill = C_ENERGY, alpha = 0.13) +
  geom_line(aes(y = eff_llnl_pct), colour = C_ENERGY, linewidth = 0.85) +
  geom_line(aes(y = eff_real_pct), colour = "grey35", linewidth = 0.85) +
  geom_point(aes(y = eff_llnl_pct), colour = C_ENERGY, size = 1.5) +
  geom_point(aes(y = eff_real_pct), colour = "grey35", size = 1.5) +
  annotate("text", x = min(C2$year) + 0.08, y = 65.5, hjust = 0, size = 2.3, colour = C_ENERGY,
           label = "published convention (transport 0.65)") +
  annotate("text", x = min(C2$year) + 0.08, y = 33, hjust = 0, size = 2.3, colour = "grey35",
           label = "corrected for a real vehicle fleet (~0.225)") +
  annotate("text", x = mean(C2$year), y = mean(c(C2$eff_real_pct, C2$eff_llnl_pct)),
           label = sprintf("%.0f point gap", mean(C2$overstatement_pp)), size = 2.4,
           colour = "grey25") +
  scale_x_continuous(breaks = C2$year) + scale_y_continuous(limits = c(25, 70)) +
  labs(x = NULL, y = "useful share of end use (%)", title = "How much energy is actually useful",
       subtitle = "The convention flatters a region where transport dominates") +
  theme_mawei()

# Sixth panel: the electricity account, which the other five never show. Generation, imports and
# the two loss terms in one place, because the metro's dependence on outside power is a structural
# fact with a water consequence.
el <- A2 %>% select(year, elec_supply, elec_imports, td_losses, plant_own_use) %>%
  mutate(local_generation = elec_supply - elec_imports) %>%
  select(year, local_generation, elec_imports, td_losses, plant_own_use) %>%
  pivot_longer(-year, names_to = "k", values_to = "v") %>%
  mutate(k = recode(k, local_generation = "generated locally", elec_imports = "imported",
                    td_losses = "T&D losses", plant_own_use = "plant own use"),
         k = factor(k, c("generated locally","imported","T&D losses","plant own use")))
p3f <- ggplot(el, aes(year, v, fill = k)) +
  geom_col(width = 0.62) +
  # Label sits ABOVE the stack in dark grey, not inside it. Inside, it fell on the dark local
  # generation band and became unreadable.
  geom_text(data = A2, aes(x = year, y = elec_supply,
                           label = sprintf("%.0f%% imported", import_share_pct)),
            inherit.aes = FALSE, vjust = -0.6, size = 2.2, colour = "grey30") +
  scale_fill_manual(values = c("generated locally" = "#8C4A21", "imported" = C_ENERGY,
                               "T&D losses" = C_LOSS, "plant own use" = "grey60"), name = NULL) +
  scale_x_continuous(breaks = A2$year) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "PJ", title = "The electricity account",
       subtitle = "A third of supply is imported, so its cooling water lies outside the region") +
  theme_mawei() + theme(legend.position = "bottom")

save_fig((p3a | p3b | p3c) / (p3d | p3e | p3f) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig3_energy_structure", 16, 8.6)

###############################################################################%
# Fig 4: county atlas ----
message("\n== Fig 4: county atlas ==")

c24m <- A8 %>% filter(year == YR)
ss <- C1 %>% filter(year == YR) %>% mutate(ss = pmin(self_sufficiency_pct, 300))
d1m <- D1 %>% filter(year == YR)
nt <- E1b %>% filter(year == YR)
i1 <- I1 %>% mutate(cost_musd = total_loss_cost_usd_yr / 1e6)
ii <- B2 %>% filter(year == YR) %>% mutate(ii_pj = pj_total)

# Composite indices, each built from measures already in the tables, each answering a question no
# single measure can. Construction is written out so a reader can rebuild it.
comp <- c24m %>%
  select(county, nrw_pct, ii_share_pct, septic_share_pct, pws_gpcd, pws_out, collected,
         energy_pj, pop) %>%
  left_join(d1m %>% select(county, kwh_per_mg), by = "county") %>%
  left_join(ss %>% select(county, self_sufficiency_pct), by = "county") %>%
  left_join(nt %>% select(county, export_dependency_pct), by = "county") %>%
  left_join(P4 %>% select(county, treatment_hhi = hhi), by = "county") %>%
  left_join(i1 %>% select(county, cost_musd), by = "county") %>%
  left_join(ii %>% select(county, ii_pj), by = "county") %>%
  left_join(cty_basin %>% group_by(county) %>% slice_max(share_pct, n = 1) %>%
              ungroup() %>% select(county, main_basin = basin, basin_share = share_pct),
            by = "county") %>%
  left_join(cty_basin %>% group_by(county) %>%
              summarise(basins_spanned = n(),
                        basin_hhi = sum((share_pct / 100)^2), .groups = "drop"), by = "county") %>%
  mutate(export_dependency_pct = replace_na(export_dependency_pct, 0),
         r_nrw = percent_rank(nrw_pct), r_ii = percent_rank(ii_share_pct),
         r_int = percent_rank(kwh_per_mg), r_conc = percent_rank(treatment_hhi),
         r_dep = percent_rank(export_dependency_pct), r_ss = percent_rank(-self_sufficiency_pct),
         r_bas = percent_rank(basin_hhi),
         idx_water_stress = 100 * (r_nrw + r_ii + r_int) / 3,
         idx_exposure = 100 * (r_dep + r_conc + r_ss) / 3,
         # Source concentration: a county drawing from one basin has nowhere to turn if that basin
         # is constrained, which is a different vulnerability from either of the above.
         idx_source_conc = 100 * r_bas,
         water_per_capita = pws_out * 1e6 / pop,
         energy_per_capita_gj = energy_pj * 1e6 / pop,
         cost_per_capita = cost_musd * 1e6 / pop,
         # Water returned as sewage per unit supplied: high means an indoor-use county, low means
         # irrigation and evaporation dominate.
         return_rate_pct = 100 * collected / pws_out)

f <- list()
f$nrw  <- cmap(c24m, "nrw_pct", "Non-revenue water", "share of supply never billed",
               pal = "Reds", fmt = "%.0f", unit = "%", legend = "%")
f$ii   <- cmap(c24m, "ii_share_pct", "Infiltration and inflow", "share of collected flow",
               pal = "PuBu", fmt = "%.0f", unit = "%", legend = "%")
f$sep  <- cmap(c24m, "septic_share_pct", "Septic systems", "share of household wastewater",
               pal = "BuGn", fmt = "%.0f", unit = "%", legend = "%")
f$gpcd <- cmap(c24m, "pws_gpcd", "Public supply per capita", "gallons per person per day",
               pal = "Blues", fmt = "%.0f", legend = "gpcd")
f$wpc  <- cmap(comp, "water_per_capita", "All-sector water per capita",
               "gallons per person per day", pal = "GnBu", fmt = "%.0f", legend = "gpcd")
f$ret  <- cmap(comp, "return_rate_pct", "Return rate", "sewage collected per unit supplied",
               pal = "YlGnBu", fmt = "%.0f", unit = "%", legend = "%")
f$ss   <- cmap(ss, "ss", "Electricity self-sufficiency", "local generation against local use",
               pal = "Oranges", fmt = "%.0f", unit = "%", legend = "%")
f$int  <- cmap(d1m, "kwh_per_mg", "Energy intensity of water", "kWh per million gallons",
               pal = "Purples", fmt = "%.0f", legend = "kWh/MG")
f$epc  <- cmap(comp, "energy_per_capita_gj", "Energy use per capita", "GJ per person per year",
               pal = "YlOrBr", fmt = "%.0f", legend = "GJ")
f$iipj <- cmap(comp, "ii_pj", "Energy wasted on infiltration", "PJ per year",
               pal = "BuPu", fmt = "%.2f", legend = "PJ")
f$dep  <- cmap(nt, "export_dependency_pct", "Export dependency", "own sewage sent to a neighbour",
               pal = "YlOrRd", fmt = "%.0f", unit = "%", legend = "%")
f$conc <- cmap(P4, "hhi", "Treatment concentration", "Herfindahl index over a county's plants",
               pal = "OrRd", fmt = "%.2f", legend = "HHI")
f$cost <- cmap(i1, "cost_musd", "Annual cost of losses", "leakage and infiltration, USD million",
               pal = "Reds", fmt = "%.0f", unit = "M", legend = "USD M")
f$cpc  <- cmap(comp, "cost_per_capita", "Loss cost per capita", "USD per person per year",
               pal = "OrRd", fmt = "%.0f", unit = "", legend = "USD")
f$str  <- cmap(comp, "idx_water_stress", "Water-system stress",
               "leakage, infiltration, pumping energy", pal = "RdPu", fmt = "%.0f",
               legend = "index")
f$exp  <- cmap(comp, "idx_exposure", "Structural exposure",
               "transfer reliance, concentration, imports", pal = "PuRd", fmt = "%.0f",
               legend = "index")
f$src  <- cmap(comp, "idx_source_conc", "Source concentration",
               "reliance on a single river basin", pal = "BuPu", fmt = "%.0f", legend = "index")
f$bas  <- cmap(comp, "basins_spanned", "Basins spanned", "watersheds a county draws from",
               pal = "Greens", fmt = "%.0f", legend = "count")

# Kept as separate figures so each can be used on its own, AND combined into one overview.
save_fig((f$nrw | f$ii | f$sep) / (f$gpcd | f$wpc | f$ret) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
                  title = paste0("County atlas I: water use and losses, ", YR),
                  theme = theme(plot.title = element_text(face = "bold", size = 11))),
  "Fig4a_county_atlas_water", 14, 8.8)

save_fig((f$ss | f$int | f$epc) / (f$iipj | f$dep | f$conc) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
                  title = paste0("County atlas II: energy, structure and cost, ", YR),
                  theme = theme(plot.title = element_text(face = "bold", size = 11))),
  "Fig4b_county_atlas_energy", 14, 8.8)

save_fig((f$str | f$exp | f$src | f$bas) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
                  title = "County atlas III: composite indices",
                  subtitle = "Each index is the mean percentile rank of its components, so 100 is worst in region",
                  theme = theme(plot.title = element_text(face = "bold", size = 11),
                                plot.subtitle = element_text(size = 8.5, colour = "grey35"))),
  "Fig4c_county_composites", 17, 5.2)

# The overview: three rows, six water, six energy, six composite. Separate from 4a-4c by design,
# because a reader wants either the overview or the detail, never both at once.
save_fig(
  (f$nrw | f$ii | f$sep | f$gpcd | f$wpc | f$ret) /
  (f$ss | f$int | f$epc | f$iipj | f$dep | f$conc) /
  (f$str | f$exp | f$src | f$bas | f$cost | f$cpc) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
                  title = paste0("County atlas: eighteen measures of the same fifteen counties, ", YR),
                  subtitle = "Row 1 water use and losses; row 2 energy and structure; row 3 composite indices and cost",
                  theme = theme(plot.title = element_text(face = "bold", size = 12),
                                plot.subtitle = element_text(size = 9, colour = "grey35"))),
  "Fig4_county_atlas_overview", 22, 11)

###############################################################################%
# Fig 5: losses and opportunity ----
message("\n== Fig 5: losses and opportunity ==")

thermo_cons <- D2 %>% filter(year == YR) %>% pull(consumed_mgd) %>% sum()
loss_w <- tibble(term = c("Non-revenue water","Sector consumptive use","Thermoelectric evaporation",
                          "Septic (leaves the system)","Infiltration (enters sewers)"),
                 v = c(sum(I1$nrw), w24$losses - sum(I1$nrw) - thermo_cons, thermo_cons,
                       w24$septic, w24$inflow_infiltration), side = "Water (MGD)")
loss_e <- tibble(term = c("Rejected at end use","Rejected at power plants","T&D losses",
                          "Plant own use"),
                 v = c(e24$rejected_enduse, e24$plant_rejected, e24$td_losses,
                       e24$plant_own_use), side = "Energy (PJ)")
p5a <- ggplot(bind_rows(loss_w, loss_e), aes(reorder(term, v), v, fill = side)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf(" %.1f", v)), hjust = 0, size = 2.3, colour = "grey30") +
  coord_flip(clip = "off") + facet_wrap(~side, scales = "free", ncol = 1) +
  scale_fill_manual(values = c("Water (MGD)" = C_WATER, "Energy (PJ)" = C_ENERGY),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.24))) +
  labs(x = NULL, y = NULL, title = "Every loss term in both systems",
       subtitle = "Not all losses are avoidable. Evaporation and rejected heat are physics;\nleakage and infiltration are maintenance.") +
  theme_mawei()

sc <- B1b %>% mutate(lab = c("Every county at the\nmetro average (19.2%)",
                             "Every county at the\nbest county (8.6%)",
                             "Reference: all thermoelectric\nwithdrawal",
                             "Reference: all water reuse"),
                     grp = c("recoverable","recoverable","reference","reference"))
p5b <- ggplot(sc, aes(reorder(lab, mgd), mgd, fill = grp)) +
  geom_col(width = 0.58) +
  geom_text(aes(label = sprintf(" %.1f MGD", mgd)), hjust = 0, size = 2.4, colour = "grey25") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(recoverable = C_GOOD, reference = C_GREY), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.32))) +
  labs(x = NULL, y = "MGD", title = "Leakage is the largest available water source",
       subtitle = "Matching the best county frees more water than the region's plants withdraw") +
  theme_mawei()

st <- P6 %>% filter(year == YR) %>% group_by(stage) %>% summarise(pj = sum(pj), .groups = "drop") %>%
  mutate(share = 100 * pj / sum(pj))
p5c <- ggplot(st, aes(reorder(stage, pj), pj, fill = stage)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf(" %.2f PJ (%.0f%%)", pj, share)), hjust = 0, size = 2.3,
            colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(distribution = C_E4W, treatment = "#9575CD",
                               extraction = "#B39DDB"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.38))) +
  labs(x = NULL, y = "PJ", title = "Energy spent on water, by stage",
       subtitle = sprintf("Infiltration alone is %.0f%% of all water-sector energy",
                          B2b$share_of_water_energy_pct[B2b$year == YR])) +
  theme_mawei()

pb <- I2 %>% filter(recoverable_mgd > 0)
p5d <- ggplot(pb, aes(reorder(county, annual_saving_usd / 1e6), annual_saving_usd / 1e6)) +
  geom_col(width = 0.62, fill = C_GOOD, alpha = 0.9) +
  geom_text(aes(label = sprintf(" $%.1fM/yr, %.0f yr payback", annual_saving_usd / 1e6,
                                payback_years)), hjust = 0, size = 2.2, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.62))) +
  labs(x = NULL, y = "annual saving (USD million)",
       title = "What closing the leakage gap is worth",
       subtitle = sprintf("Metro losses cost about $%.0f million a year to produce and treat",
                          sum(I1$total_loss_cost_usd_yr) / 1e6)) +
  theme_mawei()

save_fig((p5a | (p5b / p5c) | p5d) + plot_layout(widths = c(1, 1, 1.08)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig5_losses_and_opportunity", 16, 7.6)

###############################################################################%
# Fig 6: the coupling ----
message("\n== Fig 6: the coupling ==")

p6a <- ggplot(d1m, aes(reorder(county, kwh_per_mg), kwh_per_mg)) +
  geom_col(width = 0.62, fill = C_E4W, alpha = 0.9) +
  geom_text(aes(label = paste0(" ", comma(round(kwh_per_mg)))), hjust = 0, size = 2.2,
            colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = comma) +
  labs(x = NULL, y = "kWh per million gallons",
       title = "Energy to deliver water", subtitle = "Twofold spread across adjacent counties") +
  theme_mawei()

d2w <- D2 %>% filter(year == YR)
p6b <- ggplot(d2w %>% select(plant, withdrawn = gal_per_kwh_withdrawn,
                             consumed = gal_per_kwh_consumed) %>%
                pivot_longer(-plant, names_to = "k", values_to = "v"),
              aes(plant, v, fill = k)) +
  geom_col(position = position_dodge(width = 0.68), width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", v)), position = position_dodge(width = 0.68),
            vjust = -0.4, size = 2.2, colour = "grey30") +
  geom_text(data = d2w, aes(x = plant, y = 0,
                            label = sprintf("%.0f%% evaporated", 100 * consumption_ratio)),
            inherit.aes = FALSE, vjust = 1.5, size = 2.0, colour = "grey40") +
  scale_fill_manual(values = c(withdrawn = C_W4E, consumed = C_LOSS), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.22, 0.16))) +
  labs(x = NULL, y = "gallons per kWh", title = paste0("Water intensity of generation, ", YR),
       subtitle = "The withdrawn-to-consumed ratio identifies the cooling system") +
  theme_mawei() + theme(legend.position = c(0.86, 0.88))

asym <- D3 %>% filter(year == YR)
p6c <- ggplot(tibble(k = c("Energy for water\n(% of metro energy)",
                           "Energy for water\n(% of metro electricity)",
                           "Water for energy\n(% of metro withdrawals)"),
                     v = c(asym$e4w_share_of_energy_pct, asym$e4w_share_of_electricity_pct,
                           asym$w4e_share_of_withdrawal_pct),
                     dir = c("energy for water","energy for water","water for energy")),
              aes(reorder(k, v), v, fill = dir)) +
  geom_col(width = 0.58) +
  geom_text(aes(label = sprintf(" %.2f%%", v)), hjust = 0, size = 2.4, colour = "grey25") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("energy for water" = C_E4W, "water for energy" = C_W4E),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.30))) +
  labs(x = NULL, y = "% of own system",
       title = sprintf("Asymmetric by %.0f times", asym$asymmetry_ratio),
       subtitle = "Water constrains energy far more than the reverse") +
  theme_mawei() + theme(legend.position = "bottom")

sec <- A3 %>% filter(year == YR) %>% select(sector, water_mgd = supplied) %>%
  left_join(A4 %>% filter(year == YR) %>% select(sector, energy_pj = consumed), by = "sector") %>%
  filter(!is.na(energy_pj), water_mgd > 0) %>% mutate(sector = str_to_title(sector))
p6d <- ggplot(sec, aes(energy_pj, water_mgd)) +
  geom_point(aes(size = water_mgd + energy_pj), colour = C_E4W, alpha = 0.5) +
  geom_text_repel(aes(label = sector), size = 2.4, colour = "grey20", box.padding = 0.45,
                  segment.colour = "grey75", segment.size = 0.2) +
  scale_size_area(max_size = 10, guide = "none") +
  scale_x_log10() + scale_y_log10() +
  labs(x = "energy consumed (PJ, log)", y = "water supplied (MGD, log)",
       title = "Every sector needs both systems",
       subtitle = "Symbol area is a sector's combined draw on the two systems") +
  theme_mawei()

# The county bar answers "how much", but not "on what". This panel decomposes the same energy by
# the stage that spends it and the water type it moves, which is what identifies the lever:
# distribution dominates, and it is almost entirely surface water.
p6e <- ggplot(P6 %>% filter(year == YR) %>%
                mutate(stage = factor(str_to_title(stage),
                                      levels = c("Extraction","Treatment","Distribution"))),
              aes(stage, pj, fill = water_type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = if_else(pj > 0.05, sprintf("%.2f", pj), "")),
            position = position_stack(vjust = 0.5), size = 2.2, colour = "white",
            fontface = "bold") +
  geom_text(data = P6 %>% filter(year == YR) %>% group_by(stage) %>%
              summarise(pj = sum(pj), share = 100 * pj / sum(P6$pj[P6$year == YR]),
                        .groups = "drop") %>%
              mutate(stage = factor(str_to_title(stage),
                                    levels = c("Extraction","Treatment","Distribution"))),
            aes(stage, pj, label = sprintf("%.0f%%", share)), inherit.aes = FALSE,
            vjust = -0.5, size = 2.4, colour = "grey30") +
  scale_fill_manual(values = c("surface water" = C_WATER, "groundwater" = "#8D6E63"),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "PJ",
       title = paste0("What the water system spends energy on, ", YR),
       subtitle = "Pressurising the network costs more than lifting and treating combined") +
  theme_mawei() + theme(legend.position = c(0.24, 0.86),
                        legend.background = element_rect(fill = alpha("white", 0.8),
                                                         colour = NA))

save_fig((p6a | p6e | p6b) / (p6d | p6c | plot_spacer()) +
           plot_layout(heights = c(1, 0.92)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig6_coupling", 16, 8.2)

###############################################################################%
# Fig 7: data centers ----
message("\n== Fig 7: data centers ==")

dc_ex <- H1 %>% filter(!is.na(lat))
dc_pr <- H4 %>% filter(growth_scenario == "higher", market_gravity_weight == 50)

p7a <- ggplot(sf_cty) +
  geom_sf(fill = "grey95", colour = "white", linewidth = 0.4) +
  geom_point(data = dc_pr, aes(lon, lat), colour = C_ENERGY, alpha = 0.32, size = 2.3,
             shape = 15) +
  geom_point(data = dc_ex, aes(lon, lat, size = sqft), colour = "grey15", alpha = 0.75) +
  geom_sf_text(aes(label = county), size = 1.8, colour = "grey40", fontface = "bold") +
  scale_size_area(max_size = 7, guide = "none") + coord_sf(expand = FALSE) +
  labs(title = "Existing and projected data centers",
       subtitle = paste0(nrow(dc_ex), " existing (dark circles, area is floor space) and ",
                         nrow(dc_pr), " projected\nsites (orange squares), highest growth and mid market gravity")) +
  theme_map()

ex_c <- H1b %>% mutate(sqft_M = sqft / 1e6)
p7b <- ggplot(ex_c, aes(reorder(county, sqft_M), sqft_M)) +
  geom_col(width = 0.6, fill = "grey30") +
  geom_text(aes(label = sprintf(" %.2fM sqft (%.0f%%)", sqft_M, sqft_share_pct)), hjust = 0,
            size = 2.3, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.45))) +
  labs(x = NULL, y = "million square feet", title = "Existing capacity is already concentrated",
       subtitle = sprintf("%.1f million square feet, about half the Georgia total",
                          sum(ex_c$sqft) / 1e6)) +
  theme_mawei()

h2 <- H2 %>% mutate(growth = factor(growth_scenario, c("low","moderate","high","higher")))
p7c <- ggplot(h2, aes(market_gravity_weight, pct_of_metro_electricity, colour = growth)) +
  geom_line(linewidth = 0.85) + geom_point(size = 1.8) +
  scale_colour_brewer(palette = "YlOrRd", name = "growth") +
  labs(x = "market gravity weight (%)", y = "% of current metro electricity",
       title = "New load against today's system",
       subtitle = "The metro attracts sites only when market proximity is weighted.\nAt zero gravity, siting goes elsewhere.") +
  theme_mawei() + theme(legend.position = c(0.17, 0.70),
                        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))

p7d <- ggplot(h2, aes(market_gravity_weight, metro_water_demand_mgd, colour = growth)) +
  geom_line(linewidth = 0.85) + geom_point(size = 1.8) +
  scale_colour_brewer(palette = "YlOrRd", guide = "none") +
  labs(x = "market gravity weight (%)", y = "cooling water demand (MGD)",
       title = "The water that load would need",
       subtitle = sprintf("At most %.1f MGD: %.0f%% of thermoelectric withdrawal but %.1f%% of all\nmetro withdrawals",
                          max(h2$metro_water_demand_mgd), max(h2$pct_of_thermo_withdrawal),
                          max(h2$pct_of_metro_withdrawal))) +
  theme_mawei()

county_dc <- H4b %>% filter(growth_scenario == "higher", market_gravity_weight == 50) %>%
  left_join(comp %>% select(county, idx_water_stress), by = "county")
p7e <- ggplot(county_dc, aes(reorder(county, it_mw), it_mw, fill = idx_water_stress)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf(" %.0f MW | %.1f MGD", it_mw, water_mgd)), hjust = 0,
            size = 2.2, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_distiller(palette = "RdPu", direction = 1, name = "water-system\nstress index",
                       guide = guide_colourbar(barwidth = 0.3, barheight = 3.2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.46))) +
  labs(x = NULL, y = "projected IT load (MW)", title = "Where the load would land",
       subtitle = "Siting does not avoid counties whose water systems are already strained") +
  theme_mawei() + theme(legend.position = c(0.80, 0.34))

cmp <- I3 %>% mutate(growth = factor(growth_scenario, c("low","moderate","high","higher"))) %>%
  select(growth, `data-center cooling water` = water_mgd,
         `leakage recoverable for the same capital` = leakage_mgd_for_same_capital) %>%
  pivot_longer(-growth, names_to = "k", values_to = "v")
p7f <- ggplot(cmp, aes(growth, v, fill = k)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.0f", v)), position = position_dodge(width = 0.7),
            vjust = -0.35, size = 2.2, colour = "grey30") +
  scale_y_log10(expand = expansion(mult = c(0, 0.20))) +
  scale_fill_manual(values = c("data-center cooling water" = C_W4E,
                               "leakage recoverable for the same capital" = C_GOOD), name = NULL) +
  labs(x = "growth scenario", y = "MGD (log scale)",
       title = "The same capital, two water outcomes",
       subtitle = "The same money spent on leakage would yield far more water than the\ncooling load consumes") +
  theme_mawei() + theme(legend.position = "bottom")

save_fig((p7a | p7b | p7c) / (p7d | p7e | p7f) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig7_datacentres", 16, 9)

###############################################################################%
# Fig 8: spatial Sankey ----
message("\n== Fig 8: spatial Sankey of the sewage network ==")

ed <- J1 %>% filter(year == YR)

# TRUE spatial Sankey. Each flow is a ribbon polygon whose width is proportional to volume, laid
# over the map at the real positions of its endpoints and tapering toward the destination so
# direction reads without an arrowhead. geom_curve cannot do this: it strokes a line of fixed
# millimetre thickness, so a large flow becomes a thick line rather than a band with area.
rib_fac <- sankey_ribbons(ed, from_lon, from_lat, fac_lon, fac_lat, mgd,
                          max_w = 0.075, taper = 0.4, curv = 0.16)

fac_in_trade <- ed %>% distinct(facility, fac_lon, fac_lat, fac_capacity, fac_county)

p8a <- ggplot() +
  base_layers(basin = TRUE, all_fac = TRUE) +
  # ExtraNotes: a single ggplot allows only ONE fill scale, and the basin shading already claims
  # it. The ribbons therefore carry magnitude through COLOUR with a fixed dark stroke-free
  # polygon, which also reads better than a fill against the pale basin wash.
  geom_polygon(data = rib_fac, aes(x, y, group = rib, colour = mgd), fill = NA,
               linewidth = 0.35) +
  geom_polygon(data = rib_fac, aes(x, y, group = rib), fill = "#1B5E8C", alpha = 0.55) +
  scale_colour_viridis_c(option = "mako", direction = -1, name = "flow (MGD)",
                         guide = guide_colourbar(barwidth = 5, barheight = 0.3,
                                                 title.position = "top")) +
  geom_point(data = fac_in_trade, aes(fac_lon, fac_lat, size = fac_capacity),
             shape = 21, fill = "white", colour = "grey20", stroke = 0.4) +
  geom_text_repel(data = fac_in_trade %>% group_by(fac_county) %>%
                    summarise(lon = mean(fac_lon), lat = mean(fac_lat), .groups = "drop"),
                  aes(lon, lat, label = fac_county), size = 2.1, fontface = "bold",
                  colour = "grey10", box.padding = 0.55, point.padding = 0.4,
                  segment.colour = "grey55", segment.size = 0.25, min.segment.length = 0,
                  seed = 1) +
  scale_size_area(max_size = 6, guide = "none") +
  coord_metro(sf_cty) +
  labs(title = "Spatial Sankey: county to receiving plant",
       subtitle = sprintf("Ribbon width is volume and tapers toward the destination. %d routes, %.0f MGD gross.\nGrey dots are all other plants; white circles receive transfers.",
                          nrow(ed), sum(ed$mgd))) +
  theme_map() +
  # Legends inside the frame, bottom right, where the map is empty. At the default position they
  # were pushed outside the panel and clipped.
  theme(legend.position = c(0.88, 0.13), legend.direction = "vertical",
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))

# The county-to-county view, kept alongside the facility view so either can be used. Ribbons show
# GROSS flow in each direction; the arrowheads mark the NET direction, so a pair that exchanges
# both ways is visibly different from one that only sends.
ed_cty <- ed %>% group_by(from_county, to_county, year) %>%
  summarise(mgd = sum(mgd), .groups = "drop") %>%
  left_join(cent %>% select(county, x0 = lon, y0 = lat), by = c("from_county" = "county")) %>%
  left_join(cent %>% select(county, x1 = lon, y1 = lat), by = c("to_county" = "county"))
rib_cty <- sankey_ribbons(ed_cty, x0, y0, x1, y1, mgd, max_w = 0.085, taper = 0.4, curv = 0.2)

# Net direction per pair, drawn as a short arrow at the destination end so direction is explicit
# rather than implied by the taper alone.
net_arrows <- L1 %>% filter(year == YR, net_mgd > 0.05) %>%
  left_join(cent %>% select(county, x0 = lon, y0 = lat), by = c("net_from" = "county")) %>%
  left_join(cent %>% select(county, x1 = lon, y1 = lat), by = c("net_to" = "county")) %>%
  # place the arrow at 80% along the chord so it sits near the destination without covering it
  mutate(ax = x0 + 0.72 * (x1 - x0), ay = y0 + 0.72 * (y1 - y0),
         bx = x0 + 0.86 * (x1 - x0), by = y0 + 0.86 * (y1 - y0))

p8b <- ggplot() +
  base_layers(basin = TRUE, all_fac = TRUE) +
  geom_polygon(data = rib_cty, aes(x, y, group = rib, colour = mgd), fill = NA,
               linewidth = 0.35) +
  geom_polygon(data = rib_cty, aes(x, y, group = rib), fill = "#1B5E8C", alpha = 0.55) +
  scale_colour_viridis_c(option = "mako", direction = -1, name = "gross flow (MGD)",
                         guide = guide_colourbar(barwidth = 0.3, barheight = 3.2)) +
  geom_segment(data = net_arrows, aes(x = ax, y = ay, xend = bx, yend = by),
               arrow = arrow(length = unit(0.11, "cm"), type = "closed"),
               colour = "grey15", linewidth = 0.4) +
  geom_sf_text(data = sf_cty, aes(label = county), size = 1.9, colour = "grey20",
               fontface = "bold") +
  coord_metro(sf_cty) +
  labs(title = "Spatial Sankey: county to county",
       subtitle = sprintf("%d directed links between %d pairs. Ribbons are gross flow; black arrows\nmark the NET direction. %d pairs exchange in both directions.",
                          nrow(ed_cty), nrow(L1 %>% filter(year == YR)),
                          sum(L1$bidirectional[L1$year == YR]))) +
  theme_map() +
  theme(legend.position = c(0.88, 0.13), legend.direction = "vertical",
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))

p8c <- ggplot(nt %>% filter(exported > 0 | imported > 0),
              aes(reorder(county, export_dependency_pct), export_dependency_pct,
                  fill = net_mgd > 0)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf(" %.1f%%", export_dependency_pct)), hjust = 0, size = 2.2,
            colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(`TRUE` = C_WATER, `FALSE` = C_LOSS),
                    labels = c(`TRUE` = "net importer", `FALSE` = "net exporter"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(x = NULL, y = "% of own collected flow sent away",
       title = "Reliance on a neighbour",
       subtitle = sprintf("%s sends %.0f%% of what it collects away",
                          nt$county[which.max(nt$export_dependency_pct)],
                          max(nt$export_dependency_pct))) +
  theme_mawei() + theme(legend.position = c(0.68, 0.22),
                        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))

p8d <- ggplot(ed, aes(distance_km, mgd)) +
  # Iso-work contours: equal volume-distance product, the quantity conveyance energy scales with.
  # A route's position relative to these says more than either axis alone.
  geom_line(data = expand_grid(w = c(50, 200, 800), distance_km = seq(10, 50, 2)) %>%
              mutate(mgd = w / distance_km),
            aes(distance_km, mgd, group = w), colour = "grey86", linewidth = 0.3,
            linetype = "dashed", inherit.aes = FALSE) +
  geom_point(aes(size = conveyance_kwh_yr / 1e6, fill = energy_cost_usd_yr / 1e6),
             shape = 21, colour = "grey25", stroke = 0.3, alpha = 0.9) +
  geom_text_repel(data = ed %>% slice_max(mgd, n = 6),
                  aes(label = sprintf("%s to %s\n%.1f MGD, %.0f km", from_county,
                                      str_trunc(facility, 18), mgd, distance_km)),
                  size = 1.9, colour = "grey20", lineheight = 0.95, box.padding = 0.5,
                  segment.colour = "grey65", segment.size = 0.2, max.overlaps = 15) +
  annotate("text", x = 47, y = 17, label = "equal transport work", size = 1.9,
           colour = "grey60", angle = -28) +
  scale_size_area(max_size = 8, name = "conveyance\nGWh per year") +
  scale_fill_viridis_c(option = "inferno", direction = -1, begin = 0.15, end = 0.92,
                       name = "electricity cost\nUSD million/yr") +
  scale_y_sqrt(breaks = c(0.1, 1, 5, 10, 20, 35)) +
  labs(x = "haul distance (km)", y = "volume (MGD, square-root scale)",
       title = "What moving sewage costs",
       subtitle = sprintf("%.0f GWh and about $%.1f million of electricity a year. Dashed lines join routes\nof equal transport work, the product energy actually scales with.",
                          J1b$conveyance_gwh_yr, J1b$energy_cost_musd_yr)) +
  theme_mawei() + theme(legend.position = "right", legend.box = "vertical")

p8e <- ggplot(J2 %>% slice_max(imported_mgd, n = 8) %>%
                mutate(util = if_else(is.finite(imported_share_of_capacity_pct) &
                                        imported_share_of_capacity_pct > 0,
                                      imported_share_of_capacity_pct, NA_real_),
                       n_lab = sprintf(" %.1f MGD from %d %s", imported_mgd, origin_counties,
                                       if_else(origin_counties == 1, "county", "counties"))),
              aes(reorder(str_trunc(facility, 24), imported_mgd), imported_mgd, fill = util)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = n_lab), hjust = 0, size = 2.2, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "% of plant\ncapacity",
                       na.value = "grey80",
                       guide = guide_colourbar(barwidth = 0.3, barheight = 2.8)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.6))) +
  labs(x = NULL, y = "imported flow (MGD)", title = "Plants doing regional work",
       subtitle = "Invisible in any county-level account") +
  theme_mawei() + theme(legend.position = c(0.78, 0.28))

# Inter-basin transfer, the finding the basin layer makes possible: sewage that crosses a divide
# leaves its watershed permanently.
k4 <- K4 %>% mutate(kind = if_else(crosses_divide, "crosses a basin divide",
                                   "stays within basin"))
p8f <- ggplot(k4 %>% filter(mgd > 0.01),
              aes(reorder(paste0(from_county, " to ", str_trunc(facility, 18)), mgd),
                  mgd, fill = kind)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf(" %.2f", mgd)), hjust = 0, size = 2.1, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("crosses a basin divide" = C_LOSS,
                               "stays within basin" = C_GREY), name = NULL) +
  scale_y_sqrt(expand = expansion(mult = c(0, 0.18))) +
  labs(x = NULL, y = "MGD (sqrt scale)", title = "Sewage that leaves its watershed",
       subtitle = sprintf("%.1f of %.1f MGD crosses a basin divide, a permanent inter-basin transfer",
                          sum(k4$mgd[k4$crosses_divide]), sum(k4$mgd))) +
  theme_mawei() + theme(legend.position = c(0.68, 0.20),
                        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA))

# Gross against net, per pair. A pair far above the diagonal exchanges heavily in both directions,
# so its infrastructure carries far more water than its dependency implies. Reporting only net
# would hide that pipe capacity; reporting only gross would overstate reliance.
lp <- L1 %>% filter(year == YR, gross_mgd > 0.02) %>%
  mutate(pair = paste0(a, " - ", b))
p8g <- ggplot(lp, aes(gross_mgd, net_mgd)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_segment(aes(xend = gross_mgd, yend = gross_mgd), colour = C_LOSS, linewidth = 0.4,
               alpha = 0.5) +
  geom_point(aes(colour = bidirectional), size = 2.6, alpha = 0.9) +
  geom_text_repel(aes(label = pair), size = 2.1, colour = "grey20", box.padding = 0.4,
                  segment.colour = "grey70", segment.size = 0.2, max.overlaps = 20) +
  annotate("text", x = 2, y = 30, hjust = 0, size = 2.2, colour = "grey40", lineheight = 0.95,
           label = "on the line: one-way transfer\nbelow it: two-way exchange,\nred segment is the offsetting volume") +
  scale_colour_manual(values = c(`TRUE` = C_LOSS, `FALSE` = C_GREY),
                      labels = c(`TRUE` = "two-way", `FALSE` = "one-way"), name = NULL) +
  scale_x_log10() + scale_y_log10() +
  labs(x = "gross transfer (MGD, log)", y = "net transfer (MGD, log)",
       title = "Gross and net are not the same question",
       subtitle = sprintf("%.1f MGD gross against %.1f MGD net: %.0f%% of movement offsets itself.\nGross sizes the pipes; net measures the dependency.",
                          L1b$gross_mgd[L1b$year == YR], L1b$net_mgd[L1b$year == YR],
                          L1b$offsetting_share_pct[L1b$year == YR])) +
  theme_mawei() + theme(legend.position = c(0.86, 0.16),
                        legend.background = element_rect(fill = alpha("white", 0.8),
                                                         colour = NA))

# Two rows: the two maps and the inter-basin finding on top, the quantitative panels below.
save_fig((p8a | p8b | p8f) / (p8c | p8g | p8d | p8e) +
           plot_layout(heights = c(1.25, 0.85)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig8_spatial_network", 21, 9.5)

###############################################################################%
# Fig 9: the region in one map ----
message("\n== Fig 9: the region in one map ==")

# Basin names are placed inside their own polygons rather than in a legend, so the eye never
# leaves the map. Labels go at the centroid of the part of each basin that lies INSIDE the view,
# because a basin centroid can easily fall outside the metro frame.
# ExtraNotes: s2 spherical geometry rejects the buffered union here (the WBD polygons contain
# slivers that are invalid on a sphere). Planar mode is switched off for this one operation, which
# is appropriate: at metro scale the planar approximation is irrelevant for a label position.
basin_lab <- local({
  old <- sf_use_s2(FALSE)
  on.exit(sf_use_s2(old), add = TRUE)
  suppressWarnings(
    st_intersection(st_make_valid(sf_basin %>% filter(basin != "Broad")),
                    st_union(st_buffer(sf_cty, 0.12)))) %>%
    st_make_valid() %>% st_centroid() %>%
    mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
    st_drop_geometry()
}) %>%
  left_join(K3 %>% select(basin, supply_share_pct), by = "basin") %>%
  mutate(lab = if_else(is.na(supply_share_pct) | supply_share_pct < 0.05,
                       str_replace(basin, "_", "-"),
                       sprintf("%s\n%.0f%% of supply", str_replace(basin, "_", "-"),
                               supply_share_pct)))

# Data-center symbol area carries a quantity, not just presence. Floor space is the main variant
# because it is the only measure the inventory reports for every facility; the intensity variants
# are computed and shown in the SI, where a proxy can be labelled as such.
# ExtraNotes: power is estimated at 150 W per square foot of IT load, a mid-range figure for a
# modern facility, so the derived MW and water columns are order-of-magnitude only.
dc_ex_m <- dc_ex %>%
  mutate(est_mw = sqft * 150 / 1e6,
         est_water_mgd = est_mw * 0.9 * HOURS_PER_YEAR * 1.8 / 3.785 / 1e6 / DAYS_PER_YEAR,
         mw_per_ksqft = 1000 * est_mw / sqft,
         water_per_ksqft = 1000 * est_water_mgd / sqft)

region_map <- function(underlay = FALSE) {
  p <- ggplot() +
    geom_sf(data = sf_basin %>% filter(basin != "Broad"), aes(fill = basin), colour = "white",
            linewidth = 0.35, alpha = if (underlay) 0.30 else 0.22) +
    scale_fill_manual(values = BASIN_COLS, guide = "none")
  if (underlay) {
    # A very dim context layer: every census tract, shaded by measured population density. Uses
    # colour rather than fill because the basin layer already owns the fill scale and two fill
    # scales cannot coexist in one ggplot.
    p <- p + geom_sf(data = tracts_sf, aes(colour = log10(pmax(pop_density, 1))), fill = NA,
                     linewidth = 0.12) +
      scale_colour_gradient(low = "grey88", high = "grey30", guide = "none")
  }
  p +
    geom_sf(data = sf_cty, fill = NA, colour = "grey40", linewidth = 0.45) +
    geom_polygon(data = rib_fac, aes(x, y, group = rib), fill = "#1F6FA8", alpha = 0.32) +
    geom_point(data = xy %>% filter(kind == "wastewater plant"),
               aes(lon, lat, size = capacity), shape = 21, fill = alpha(C_WATER, 0.5),
               colour = "grey25", stroke = 0.3) +
    geom_point(data = xy %>% filter(kind == "power plant", capacity > 300),
               aes(lon, lat, size = capacity / 40), shape = 24, fill = alpha(C_ENERGY, 0.85),
               colour = "grey20", stroke = 0.3) +
    geom_point(data = dc_ex_m, aes(lon, lat, size = sqft / 12000), shape = 22,
               fill = alpha("grey15", 0.8), colour = "white", stroke = 0.25) +
    geom_text(data = basin_lab, aes(lon, lat, label = lab), size = 2.5, fontface = "bold",
              colour = "grey25", lineheight = 0.95, alpha = 0.85) +
    geom_sf_text(data = sf_cty, aes(label = county), size = 2, colour = "grey12",
                 fontface = "bold") +
    scale_size_area(max_size = 9, guide = "none") +
    coord_metro(sf_cty) + theme_map()
}

save_fig(region_map(FALSE) +
  labs(title = "Metro Atlanta's water and energy system in one frame",
       subtitle = paste0("River basins shaded and named in place. ",
                         sum(xy$kind == "wastewater plant"),
                         " treatment plants (circles), major power plants (triangles),\n",
                         nrow(dc_ex), " data centers (squares, area is floor space). ",
                         "Ribbons are inter-county sewage transfers.")),
  "Fig9_regional_overview", 11, 9.5)

save_fig(region_map(TRUE) +
  labs(title = "The same system over its settlement pattern",
       subtitle = "Faint outlines are the 1,386 census tracts, shaded by measured population density.\nThe average resident lives at 1,130 per km2 while the region averages 450."),
  "Fig9b_regional_overview_settlement", 11, 9.5)

###############################################################################%
# Fig 10: settlement, demographics and basin population ----
message("\n== Fig 10: settlement, demographics and basin population ==")

# Counties are the unit the data arrives in, but neither water nor people respect them. These
# panels re-express the region on the two geographies that matter: settlement density, which is
# what drives demand, and the basin, which is what constrains supply. All quantities are measured
# ACS tract values, not proxies.
p10a <- cmap(M1, "pw_density", "Where the average resident lives",
             "population-weighted density",
             pal = "Greys", fmt = "%.0f", legend = "per km2")
p10b <- cmap(M1, "density_unevenness", "How unevenly people are spread",
             "weighted / area density; 1.0 = uniform",
             pal = "PuOr", dir = -1, fmt = "%.1f", unit = "x", legend = "ratio")

# ExtraNotes: the two densities are plotted against each other rather than either alone, because
# the GAP between them is the finding. A county on the 1:1 line is uniformly settled; one far above
# it holds a dense core and an empty fringe, and its county average describes neither.
p10c <- ggplot(M1, aes(area_density, pw_density)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(aes(size = acs_pop / 1e3), colour = C_WATER, alpha = 0.6) +
  geom_text_repel(aes(label = county), size = 2.3, colour = "grey20", box.padding = 0.4,
                  segment.colour = "grey70", segment.size = 0.2) +
  scale_size_area(max_size = 9, name = "population\n(thousands)") +
  annotate("text", x = 130, y = 1750, hjust = 0, size = 2.2, colour = "grey35",
           label = "above the line = a dense core\nwith an empty fringe") +
  labs(x = "area density (people per km2 of county)",
       y = "population-weighted density (per km2)",
       title = "One county can be two places",
       subtitle = "Metro-wide the average resident lives at 1130 per km2 while the region averages 450,\na factor of 2.5") +
  theme_mawei() + theme(legend.position = c(0.87, 0.22))

# ExtraNotes: only associations significant at p < 0.05 are drawn, and the panel is framed as
# association rather than mechanism -- fifteen counties is a small sample and the water-side inputs
# are themselves per-county assumptions. What makes it worth showing is that the signs are all
# physically sensible, which is evidence the county inputs encode something real.
m3 <- M3b %>% filter(p < 0.05) %>%
  mutate(lab = paste0(recode(y, nrw_pct = "non-revenue water", ii_share_pct = "infiltration share",
                             septic_share_pct = "septic share", pws_gpcd = "supply per person",
                             kwh_per_mg = "energy per MG"),
                      "  ~  ",
                      recode(x, pw_density = "density (pop-weighted)", area_density = "density (area)",
                             transit_pct = "transit commuting", poverty_pct = "poverty rate",
                             persons_per_hh = "household size",
                             median_hh_income = "median income",
                             mean_commute_min = "mean commute")))

p10d <- ggplot(m3, aes(reorder(lab, r), r, fill = r > 0)) +
  geom_col(width = 0.62) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", r), hjust = ifelse(r > 0, -0.25, 1.25)),
            size = 2.2, colour = "grey25") +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = C_WATER, `FALSE` = C_LOSS), guide = "none") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(x = NULL, y = "Spearman rho",
       title = "Settlement pattern predicts water performance",
       subtitle = "Only associations significant at p < 0.05, n = 15 counties. Septic falls with density\n(rho -0.74); infiltration and energy per gallon rise with it.") +
  theme_mawei()

p10e <- ggplot(M2, aes(reorder(basin, population), population / 1e6)) +
  geom_col(aes(fill = basin), width = 0.6) +
  geom_text(aes(label = sprintf(" %.2fM (%.0f%%)", population / 1e6, pop_share_pct)),
            hjust = 0, size = 2.3, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = BASIN_COLS, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(x = NULL, y = "population (millions)",
       title = "Where the people live, by basin",
       subtitle = "Measured tract population summed by centroid. Counties cannot answer this question.") +
  theme_mawei()

# ExtraNotes: withdrawal and discharge per person are plotted together because the COMPARISON is
# the finding. A basin discharging more per person than it withdraws is receiving water piped in
# from another basin -- an inter-basin transfer that supply-side accounting cannot see.
p10f <- M2 %>%
  select(basin, withdrawal_gpcd, discharge_gpcd) %>%
  pivot_longer(-basin, names_to = "k", values_to = "gpcd") %>%
  mutate(k = factor(recode(k, withdrawal_gpcd = "withdrawn", discharge_gpcd = "discharged"),
                    levels = c("withdrawn", "discharged"))) %>%
  ggplot(aes(reorder(basin, gpcd), gpcd, fill = k)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.66) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(withdrawn = C_WATER, discharged = C_E4W), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "gallons per person per day in the basin",
       title = "The Ocmulgee discharges more than it withdraws",
       subtitle = "78 gpcd out against 34 in: 1.6 million people are supplied from the Chattahoochee\nand return their sewage to a different river") +
  theme_mawei() + theme(legend.position = "bottom")

save_fig((p10a | p10b | p10c) / (p10d | p10e | p10f) +
           plot_layout(heights = c(1.05, 0.95)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig10_settlement_and_basins", 17, 9)

###############################################################################%
# SI ----
message("\n== SI ==")

si1 <- ggplot(G1, aes(epa_design_mgd, permitted_capacity)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(aes(colour = agrees_10pct), size = 2.2, alpha = 0.85) +
  # Only the disagreeing facilities are named. Labelling all sixteen would obscure the 1:1 line,
  # and the agreeing ones need no explanation.
  geom_text_repel(data = G1 %>% filter(!agrees_10pct),
                  aes(label = sprintf("%s\nstudy %.1f vs EPA %.2f",
                                      str_trunc(facility_name, 24), permitted_capacity,
                                      epa_design_mgd)),
                  size = 1.95, colour = C_LOSS, lineheight = 0.95, box.padding = 0.6,
                  segment.colour = "grey65", segment.size = 0.2, max.overlaps = 20) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c(`TRUE` = C_GOOD, `FALSE` = C_LOSS),
                      labels = c(`TRUE` = "within 10%", `FALSE` = "differs"), name = NULL) +
  labs(x = "EPA ECHO design flow (MGD, log)", y = "study permitted capacity (MGD, log)",
       title = "Independent validation of facility capacity",
       subtitle = sprintf("%d of %d agree within 10%%; median ratio %.2f. The three that differ are named.",
                          sum(G1$agrees_10pct), nrow(G1), median(G1$ratio))) +
  theme_mawei() + theme(legend.position = "bottom")

si2 <- ggplot(P8 %>% filter(year == YR),
              aes(reorder(plant, thermal_efficiency_pct), thermal_efficiency_pct)) +
  geom_col(width = 0.55, fill = C_ENERGY, alpha = 0.9) +
  geom_text(aes(label = sprintf(" %.1f%%, %s Btu/kWh", thermal_efficiency_pct,
                                comma(round(heat_rate_btu_per_kwh)))), hjust = 0, size = 2.3,
            colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.55))) +
  labs(x = NULL, y = "thermal efficiency (%)", title = "Plant thermal performance",
       subtitle = "Textbook-correct for a combined cycle and a coal unit, which validates\nthe closed fuel-to-generation balance") +
  theme_mawei()

si3 <- ggplot(P9 %>% filter(year == YR), aes(reorder(county, difference_pct), difference_pct)) +
  geom_col(width = 0.62, aes(fill = difference_pct > 0)) +
  geom_text(aes(label = sprintf("%+.0f%%", difference_pct),
                hjust = if_else(difference_pct > 0, -0.15, 1.15)), size = 2.2, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(`TRUE` = C_ENERGY, `FALSE` = C_WATER), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(x = NULL, y = "difference if weighted by water use rather than population (%)",
       title = "County downscaling is the main county-level uncertainty",
       subtitle = "Metro totals are unaffected; county energy is an allocation, not a measurement") +
  theme_mawei()

si4 <- ggplot(K1 %>% filter(total_withdrawal_mgd > 0), aes(year, return_ratio, colour = basin)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 0.8) + geom_point(size = 1.4) +
  scale_colour_manual(values = BASIN_COLS, name = NULL) +
  scale_x_continuous(breaks = unique(K1$year)) +
  labs(x = NULL, y = "discharge divided by withdrawal",
       title = "Which basins get their water back",
       subtitle = "Above one means a basin receives more effluent than the metro withdraws from it") +
  theme_mawei() + theme(legend.position = "bottom")

save_fig((si1 | si2) / (si3 | si4) +
           plot_annotation(tag_levels = "a", tag_prefix = "(S", tag_suffix = ")"),
         "FigS1_validation_and_uncertainty", 13, 9)

message("\n== ", length(list.files(FIG_DIR, pattern = "png$")), " figures in ", FIG_DIR, " ==")

# END of MAIN FIGURES -----
###############################################################################%



# PAPER FIGURES  -----
# fig_paper.R originally

# Composite figures for the two manuscripts
#
#   Rscript R/fig_paper.R
#
# Writes analysis/figures_paper/{Fig1_accounts, Fig2_form, Fig3_consequence}.{pdf,png}
# plus the standalone Sankeys used by the long article.
#
# ExtraNotes: kept separate from R/figures.R, which produces the fifteen exploratory analysis
# figures. Those are for reading the results; these are for the papers, and they differ in
# consequence -- panel sizes are set for a journal column, every panel has a caption commitment in
# the .tex, and nothing is drawn that the text does not reference.
#
# Hassan Niazi, Aug 2026

suppressMessages({library(patchwork); library(sf); library(scales); library(readr)})

save_fig_p <- function(p, name, w, h, save_pdf = FALSE) {
  ggsave(paste0(FIG_DIR_P, name, ".png"), p, width = w, height = h, dpi = 300, bg = "white")
  if (save_pdf) {
    ggsave(paste0(FIG_DIR_P, name, ".pdf"), p, width = w, height = h, device = cairo_pdf, bg = "white")
  }
  message(sprintf("  %-24s %4.1f x %4.1f in", name, w, h))
}

W <- "outputs/files/water/01_metro_water_flows.csv"
E <- "outputs/files/energy/01_metro_energy_flows.csv"
YR <- 2024

###############################################################################%
## Fig 1: the closed account and the asymmetry ----

# ExtraNotes: the two loss sinks are split here. The metro table carries `losses` (municipal
# non-revenue plus sectoral consumptive) and `Losses` (thermoelectric evaporation) and the display
# mapping would merge them. Merging is exactly wrong for a figure whose subject is the comparison
# between network loss and plant cooling.
p1a <- sankey_static(W, YR, relabel = c(losses = "Water Losses", Losses = "Plant Evaporation"),
                     unit = " MGD", label_size = 2.15) +
  labs(title = "a   Water, 2024") +
  theme(plot.title = element_text(face = "bold", size = 10, hjust = 0))

p1b <- sankey_static(E, YR, scale = EJ_to_PJ, unit = " PJ", label_size = 2.15) +
  labs(title = "b   Energy, 2024") +
  theme(plot.title = element_text(face = "bold", size = 10, hjust = 0))

d3 <- tab("D3_coupling_asymmetry")
# ExtraNotes: both directions on one axis, each as a share of its OWN domain. Plotting them on a
# shared absolute axis would be meaningless -- MGD and PJ are not comparable -- and the whole point
# is that the two shares differ by a factor the reader can see.
p1c <- d3 %>%
  select(year, `water to energy` = w4e_share_of_withdrawal_pct,
         `energy to water` = e4w_share_of_energy_pct) %>%
  pivot_longer(-year, names_to = "dir", values_to = "pct") %>%
  mutate(dir = factor(dir, levels = c("water to energy", "energy to water"))) %>%
  ggplot(aes(year, pct, colour = dir, group = dir)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.9) +
  geom_text(data = d3, aes(x = year, y = 11.2, label = sprintf("%.0f\u00d7", asymmetry_ratio)),
            inherit.aes = FALSE, size = 2.5, colour = "grey25") +
  annotate("text", x = 2022, y = 12.6, label = "asymmetry ratio", size = 2.4, colour = "grey45") +
  scale_colour_manual(values = c(`water to energy` = C_WATER, `energy to water` = C_E4W),
                      name = NULL) +
  scale_y_continuous(limits = c(0, 13.4), breaks = c(0, 2, 4, 6, 8)) +
  labs(title = "c   The coupling is asymmetric, and stable",
       x = NULL, y = "share of own domain (%)") +
  theme_mawei() +
  theme(legend.position = c(0.76, 0.62), plot.title = element_text(face = "bold", size = 10))

save_fig_p((p1a / p1b / p1c) + plot_layout(heights = c(1, 1, 0.62)),
         "Fig1_accounts", 9.6, 13.2)

# standalone versions for the long article, which shows the two accounts as separate figures
save_fig_p(p1a + labs(title = NULL), "SankeyWater2024", 9.5, 6.2)
save_fig_p(p1b + labs(title = NULL), "SankeyEnergy2024", 9.5, 6.2)

###############################################################################%
## Fig 2: settlement form and where losses sit ----

m1 <- tab("M1_settlement_by_county")
m3 <- tab("M3b_settlement_correlations")
fips <- read_csv(paste0(DATA_DIR, "common_county_fips.csv"), show_col_types = FALSE)$fip
sf_cty <- st_read(paste0(DATA_DIR, "geojson-counties-fips.json"), quiet = TRUE) %>%
  rename_with(tolower) %>% filter(id %in% fips) %>%
  mutate(county = name) %>% select(county, geometry) %>% st_set_crs(4326)

p2a <- sf_cty %>% left_join(m1 %>% select(county, pw_density), by = "county") %>%
  ggplot() +
  geom_sf(aes(fill = pw_density), colour = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = sprintf("%s\n%.0f", county, pw_density)),
               size = 1.75, colour = "grey12", lineheight = 0.95, fontface = "bold") +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "per km2",
                       guide = guide_colourbar(title.position = "top", barwidth = 4.6,
                                               barheight = 0.26)) +
  labs(title = "a   Where the average resident lives",
       subtitle = "population-weighted density") +
  theme_map() + theme(plot.title = element_text(face = "bold", size = 10))

met_pw <- weighted.mean(m1$pw_density, m1$acs_pop)
met_ar <- sum(m1$acs_pop) / sum(m1$land_km2)

p2b <- ggplot(m1, aes(area_density, pw_density)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey60") +
  geom_point(aes(size = acs_pop / 1e3), colour = C_WATER, alpha = 0.55) +
  geom_text_repel(aes(label = county), size = 2.1, colour = "grey20",
                  box.padding = 0.32, segment.colour = "grey70", segment.size = 0.2,
                  max.overlaps = 20, seed = 1L) +
  annotate("point", x = met_ar, y = met_pw, shape = 18, size = 3.4, colour = C_LOSS) +
  annotate("text", x = met_ar + 40, y = met_pw + 90, hjust = 0, size = 2.3, colour = C_LOSS,
           label = sprintf("metro: %.0f vs %.0f\n(%.2f\u00d7)", met_pw, met_ar, met_pw / met_ar)) +
  scale_size_area(max_size = 7.5, name = "population\n(thousands)") +
  labs(title = "b   Residents are concentrated, the region is not",
       x = "area density (per km2)", y = "population-weighted density (per km2)") +
  theme_mawei() +
  theme(legend.position = c(0.86, 0.24), plot.title = element_text(face = "bold", size = 10))

# ExtraNotes: only the nine associations significant at p < 0.05 are drawn, and the panel is framed
# as association. Showing all forty would imply a screen we did not correct for; showing none would
# hide the coherence of the signs, which is the actual argument.
lbl_y <- c(nrw_pct = "non-revenue water", ii_share_pct = "infiltration share",
           septic_share_pct = "septic share", pws_gpcd = "supply per person",
           kwh_per_mg = "energy per million gal")
lbl_x <- c(pw_density = "density (weighted)", area_density = "density (area)",
           transit_pct = "transit commuting", poverty_pct = "poverty rate",
           persons_per_hh = "household size", median_hh_income = "median income",
           mean_commute_min = "mean commute")

p2c <- m3 %>% filter(p < 0.05) %>%
  mutate(lab = paste0(lbl_y[y], "  ~  ", lbl_x[x])) %>%
  ggplot(aes(reorder(lab, r), r, fill = r > 0)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.2f", r), hjust = ifelse(r > 0, -0.22, 1.22)),
            size = 2.15, colour = "grey20") +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = C_WATER, `FALSE` = C_LOSS), guide = "none") +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(title = "c   Losses track settlement form",
       subtitle = "Spearman rho, only p < 0.05 of 40 tested; n = 15 counties",
       x = NULL, y = "rho") +
  theme_mawei() + theme(plot.title = element_text(face = "bold", size = 10))

save_fig_p((p2a | p2b) / p2c + plot_layout(heights = c(1, 0.78)),
         "Fig2_form", 10.6, 8.4)

###############################################################################%
## Fig 3: what the reframing changes ----

b1b <- tab("B1b_nrw_scenarios")
h2  <- tab("H2_datacentre_projections") %>% filter(market_gravity_weight == 50)
l1b <- tab("L1b_transfer_gross_net_summary")

# ExtraNotes: the counterfactuals and the two reference volumes go on ONE axis. Separate panels
# would let a reader miss the comparison, which is the single most important number in the paper.
p3a <- b1b %>%
  mutate(lab = recode(scenario,
           `all counties at best observed county` = "recoverable: all at best county (8.65%)",
           `all counties at metro average` = "recoverable: all at metro mean (19.2%)",
           `reference: thermoelectric withdrawal` = "power-plant cooling withdrawal",
           `reference: reuse volume` = "total water reuse"),
         kind = if_else(grepl("recoverable", lab), "recoverable loss", "reference")) %>%
  ggplot(aes(reorder(lab, mgd), mgd, fill = kind)) +
  geom_col(width = 0.58) +
  geom_text(aes(label = sprintf(" %.1f", mgd)), hjust = 0, size = 2.6, colour = "grey20") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(`recoverable loss` = C_WATER, reference = "grey65"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.17))) +
  labs(title = "a   Recoverable loss exceeds cooling withdrawal",
       subtitle = "million gallons per day, 2024", x = NULL, y = NULL) +
  theme_mawei() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 10))

# ExtraNotes: log axes because the two shares differ by a factor of 35 and a linear plot would
# render the water share as a flat line at zero. The 1:1 line makes the disproportion the visual
# subject rather than something the reader has to compute.
p3b <- h2 %>%
  mutate(sc = factor(growth_scenario, levels = c("low", "moderate", "high", "higher"))) %>%
  ggplot(aes(pct_of_metro_withdrawal, pct_of_metro_electricity)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey65") +
  annotate("text", x = 0.14, y = 0.19, label = "equal shares", size = 2.2, colour = "grey55",
           angle = 34, hjust = 0) +
  geom_point(aes(size = metro_it_mw, colour = sc), alpha = 0.85) +
  geom_text_repel(aes(label = sprintf("%s\n%.1f%% vs %.2f%%", sc, pct_of_metro_electricity,
                                      pct_of_metro_withdrawal)),
                  size = 2.05, colour = "grey20", lineheight = 0.94, box.padding = 0.4,
                  segment.colour = "grey70", segment.size = 0.2, seed = 1L) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c(low = "#A5D4E0", moderate = "#5AA9C7",
                                 high = "#2E7CB0", higher = "#1F4E79"), guide = "none") +
  scale_size_area(max_size = 6.5, name = "IT load (MW)") +
  labs(title = "b   Data centers are an electricity question",
       subtitle = "share of metro electricity vs share of metro water withdrawal",
       x = "% of metro water withdrawal (log)", y = "% of metro electricity (log)") +
  theme_mawei() +
  theme(legend.position = c(0.83, 0.22), plot.title = element_text(face = "bold", size = 10))

p3c <- l1b %>%
  ggplot(aes(year)) +
  geom_ribbon(aes(ymin = net_mgd, ymax = gross_mgd), fill = C_LOSS, alpha = 0.22) +
  geom_line(aes(y = gross_mgd, colour = "gross"), linewidth = 0.7) +
  geom_line(aes(y = net_mgd, colour = "net"), linewidth = 0.7) +
  geom_point(aes(y = gross_mgd, colour = "gross"), size = 1.8) +
  geom_point(aes(y = net_mgd, colour = "net"), size = 1.8) +
  geom_text(data = l1b %>% filter(year == max(year)),
            aes(y = (gross_mgd + net_mgd) / 2,
                label = sprintf("  %.1f MGD offsetting\n  (%.1f%%)", offsetting_mgd,
                                offsetting_share_pct)),
            hjust = 0, size = 2.3, colour = C_LOSS, lineheight = 0.95) +
  scale_colour_manual(values = c(gross = "grey25", net = C_W4E), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.30))) +
  labs(title = "c   Gross transfers exceed net",
       subtitle = "8 of 15 county pairs exchange sewage in both directions",
       x = NULL, y = "inter-county transfers (MGD)") +
  theme_mawei() +
  theme(legend.position = c(0.14, 0.55), plot.title = element_text(face = "bold", size = 10))

save_fig_p(p3a / (p3b | p3c) + plot_layout(heights = c(0.82, 1)),
         "Fig3_consequence", 10.6, 8.2)

message("== done: ", length(list.files(FIG_DIR, pattern = "pdf$")), " PDFs in ", FIG_DIR)

# STATIS SANKEYS (probably discard) ----
# originally fig_static_sankey.R

# Static, mass-proportional Sankey diagrams for publication figures
#
#   source("R/fig_sankey_static.R")
#   p <- sankey_static("outputs/files/water/01_metro_water_flows.csv", year = 2024)
#
# Writes nothing by itself; returns a ggplot. Use save_fig() from R/figures.R to export.
#
# ExtraNotes: this is deliberately NOT an export of the interactive plotly diagram. The interactive
# layout pins node y positions (SANKEY_NODE_Y) and applies a minimum-width floor so that small nodes
# stay clickable, both of which break proportionality on purpose. A journal figure has to be
# mass-proportional or the reader cannot verify the balance by eye, which is the whole reason to show
# a Sankey rather than a table. Same data, same layer assignment, different geometry and a different
# goal.
#
# ExtraNotes: node height is max(inflow, outflow), never inflow + outflow. A node that both receives
# and emits appears once as a target and again as a source, so summing the two double-counts every
# intermediate node and inflates the middle of the diagram. For a closed account the two are equal
# anyway, so max() reads as "the throughput of this node" and stays correct for sources and sinks
# where only one side exists.
#
# Hassan Niazi, Aug 2026

suppressMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr)
  library(ggplot2); library(readr); library(tibble); library(ggrepel)
})

###############################################################################%
## palette ----

# Semantic colours keyed on DISPLAY node names, reusing the constants in R/fig_helpers.R so these
# figures match the other fifteen.
# ExtraNotes: the loss sinks are deliberately the only warm reds in either diagram. A reader scanning
# the figure should find the loss terms without reading a label, because the size and position of
# those terms is the point being made. Everything else is desaturated so the reds carry.
SANKEY_PAL_WATER <- c(
  "Surface Water" = "#2E7CB0", "Groundwater" = "#5AA9C7",
  "Infiltration and Inflow" = "#7E6BA8", "Transfers In" = "#9E9E9E",
  "Chattahoochee Basin" = "#1F6FA8", "Coosa_Etowah Basin" = "#E8A33D",
  "Ocmulgee Basin" = "#4E9B6E", "Flint Basin" = "#B4577A",
  "Tallapoosa Basin" = "#7E6BA8", "Oconee Basin" = "#8C8C8C", "Basins" = "#4E8FB0",
  "Public Water Supply" = "#3E7EA0",
  "Residential Use" = "#4E9B6E", "Commercial Use" = "#6FAF8A",
  "Industrial Use" = "#9CBF6B", "Agricultural Use" = "#C2CE7A",
  "Bowen Plant" = "#B8892F", "Yates Plant" = "#D4A94A", "Jack McDonough Plant" = "#E0BC6A",
  "Wastewater Collection" = "#26A69A", "In-County Treatment" = "#3D8F86",
  # loss sinks -- the only warm reds
  "Water Losses" = "#B0413E", "Losses" = "#B0413E", "Plant Evaporation" = "#D2705C",
  "Septic Systems" = "#C58B6A", "Transfers Out" = "#9E9E9E",
  # receiving waters -- cool
  "Discharge" = "#4E8FB0", "River" = "#2E7CB0", "Creek" = "#5AA9C7",
  "Lake" = "#7FBFD4", "Reservoir" = "#A5D4E0", "Wetland" = "#8FB89E",
  "Land" = "#8D6E63", "Reuse" = "#2E8B57", "Disposal" = "#4E8FB0")

SANKEY_PAL_ENERGY <- c(
  "Coal" = "#4A4A4A", "Natural Gas" = "#E8A33D", "Petroleum" = "#8D6E63",
  "Biomass" = "#8A9A5B", "Hydroelectric" = "#2E7CB0", "Solar" = "#E8C547",
  "Wind" = "#26A69A", "Geothermal" = "#A1887F", "Energy Storage" = "#7E6BA8",
  "Renewables" = "#4E9B6E", "Other" = "#9E9E9E",
  "Onsite / BehindTheMeter" = "#B0BEC5", "Onsite Solar/DER" = "#CFD8DC",
  "Electricity Imports" = "#78909C", "Imports (out-metro)" = "#78909C",
  "Bowen Plant" = "#546E7A", "Yates Plant" = "#78909C", "Jack McDonough Plant" = "#90A4AE",
  "Utility-scale Gen." = "#A5B4BC", "Distributed Gen." = "#BFC9CE",
  "On-Site Gen." = "#D0D8DC", "Small-scale generation" = "#BFC9CE",
  "Grid Electricity" = "#2E7CB0",
  "Residential Use" = "#4E9B6E", "Commercial Use" = "#6FAF8A",
  "Industrial Use" = "#9CBF6B", "Government Use" = "#C2CE7A",
  "Transportation Use" = "#3D8F86", "Agricultural Use" = "#C2CE7A",
  "Water Services Energy" = "#7E57C2",
  # loss sinks -- the only warm reds
  "Plants Own Use" = "#C58B6A", "Efficiency Losses" = "#C4564C",
  "T&D Losses" = "#D2705C", "Energy Losses" = "#C4564C", "Rejected Energy" = "#B0413E",
  "Energy Services" = "#2E8B57",
  "Electricity Exports" = "#78909C", "Exports (out-metro)" = "#78909C")

# ExtraNotes: a node absent from the palette gets grey and a message, rather than an arbitrary hue
# from a ramp. A silent fallback colour hides the fact that a new node name has appeared.
sankey_pal <- function(nodes, extra = NULL) {
  base <- c(SANKEY_PAL_WATER, SANKEY_PAL_ENERGY, extra)
  base <- base[!duplicated(names(base), fromLast = TRUE)]
  out <- setNames(rep("grey70", length(nodes)), nodes)
  hit <- intersect(nodes, names(base))
  out[hit] <- base[hit]
  miss <- setdiff(nodes, names(base))
  if (length(miss)) message("  [fig] no palette entry, drawn grey: ", paste(miss, collapse = ", "))
  out
}

# Keep precision on small nodes: a 3.8 PJ node printed with "%.0f" reads as 0 beside a 571 PJ one.
fmt_val <- function(v) {
  ifelse(v >= 10, sprintf("%.0f", v),
  ifelse(v >= 1,  sprintf("%.1f", v), sprintf("%.2f", v)))
}

###############################################################################%
## geometry ----

# Cubic Bezier with HORIZONTAL tangents at both ends.
# ExtraNotes: bezier_path() in fig_helpers.R bows perpendicular to the chord, which is right for the
# geographic ribbon maps and wrong here -- a Sankey link has to leave its node face horizontally or
# the ribbon appears to peel off the bar. This is a sibling function rather than a change to that
# one, because the map figures depend on the existing behaviour.
sigmoid_path <- function(x0, y0, x1, y1, n = 60, flat = 0.42) {
  t <- seq(0, 1, length.out = n)
  dx <- x1 - x0
  c1x <- x0 + flat * dx; c2x <- x1 - flat * dx
  tibble(
    x = (1 - t)^3 * x0 + 3 * (1 - t)^2 * t * c1x + 3 * (1 - t) * t^2 * c2x + t^3 * x1,
    y = (1 - t)^3 * y0 + 3 * (1 - t)^2 * t * y0 + 3 * (1 - t) * t^2 * y1 + t^3 * y1)
}

# One link ribbon: a closed polygon bounded above and below by sigmoids.
# Width is exact at both faces, so a ribbon leaving a node covers exactly its share of the node bar.
link_ribbon <- function(x0, y0_top, x1, y1_top, w0, w1, id, n = 60, flat = 0.42) {
  top <- sigmoid_path(x0, y0_top, x1, y1_top, n, flat)
  bot <- sigmoid_path(x0, y0_top + w0, x1, y1_top + w1, n, flat)
  bind_rows(top, bot[rev(seq_len(nrow(bot))), ]) %>% mutate(rib = id)
}

###############################################################################%
## layout ----

# Assign every node a layer, then stack layers vertically in proportion to throughput.
# ExtraNotes: unnamed nodes are placed by topology -- source if the node never appears as a target,
# sink if it never appears as a source, otherwise mid-chain. Without this a single new node name in
# the flow table silently lands at x = 0.5 on top of whatever else is there.
sankey_layout_static <- function(d, gap_frac = 0.012, layer_x = NULL) {

  nodes <- union(d$source, d$target)
  doms  <- if (exists("sankey_detect_domains")) sankey_detect_domains(nodes) else names(SANKEY_LAYOUT)
  layouts <- SANKEY_LAYOUT[doms]

  lay_of <- vapply(nodes, function(n) {
    l <- sankey_layer_of(n, layouts)
    if (!is.na(l)) return(l)
    if (!n %in% d$target) return("source")
    if (!n %in% d$source) return("sink")
    "treat"
  }, character(1))

  inflow  <- d %>% group_by(node = target) %>% summarise(i = sum(value), .groups = "drop")
  outflow <- d %>% group_by(node = source) %>% summarise(o = sum(value), .groups = "drop")

  nd <- tibble(node = nodes, layer = unname(lay_of[nodes])) %>%
    left_join(inflow, by = "node") %>% left_join(outflow, by = "node") %>%
    mutate(i = coalesce(i, 0), o = coalesce(o, 0),
           value = pmax(i, o)) %>%
    filter(value > 0)

  # x: keep the declared layer order but rescale to the layers actually present, so the diagram
  # fills the panel instead of leaving a gap where an unused layer would have been.
  lx <- if (is.null(layer_x)) SANKEY_LAYER_X else layer_x
  used <- names(lx)[names(lx) %in% unique(nd$layer)]
  xs <- lx[used]; xs <- (xs - min(xs)) / (max(xs) - min(xs))
  nd$x <- unname(xs[nd$layer])

  # y: within each layer, order by the declared vertical order where one exists, then by descending
  # throughput for anything unnamed. Stack proportionally with a fixed gap between bars.
  declared <- unlist(lapply(layouts, function(l) unlist(l, use.names = FALSE)), use.names = FALSE)
  nd <- nd %>%
    mutate(ord = match(node, declared),
           ord = if_else(is.na(ord), 1000L + rank(-value, ties.method = "first"), ord)) %>%
    group_by(layer) %>%
    arrange(ord, .by_group = TRUE) %>%
    mutate(n_in_layer = n(),
           span = 1 - gap_frac * pmax(0, n_in_layer - 1),
           h = span * value / sum(value),
           y0 = cumsum(h) - h + gap_frac * (row_number() - 1),
           y1 = y0 + h) %>%
    ungroup() %>%
    select(node, layer, value, x, y0, y1, h)

  nd
}

###############################################################################%
## assemble ----

# ExtraNotes: link order at each node face is sorted by the y of the node at the other end. This is a
# deterministic crossing-reduction rule rather than an optimiser -- a greedy optimiser gave different
# answers for the same graph between runs, which is unacceptable in a figure that has to be
# reproducible.
sankey_ribbons_static <- function(d, nd, flat = 0.42, n = 60) {
  y_of <- setNames((nd$y0 + nd$y1) / 2, nd$node)
  d <- d %>% filter(source %in% nd$node, target %in% nd$node, value > 0)

  out <- d %>% mutate(ty = y_of[target]) %>%
    group_by(source) %>% arrange(ty, .by_group = TRUE) %>%
    mutate(off_out = cumsum(value) - value) %>% ungroup()

  inn <- d %>% mutate(sy = y_of[source]) %>%
    group_by(target) %>% arrange(sy, .by_group = TRUE) %>%
    mutate(off_in = cumsum(value) - value) %>% ungroup()

  ed <- out %>%
    left_join(inn %>% select(source, target, off_in), by = c("source", "target")) %>%
    left_join(nd %>% select(source = node, sx = x, sy0 = y0, s_val = value), by = "source") %>%
    left_join(nd %>% select(target = node, tx = x, ty0 = y0, t_val = value), by = "target") %>%
    left_join(nd %>% select(source = node, s_h = h), by = "source") %>%
    left_join(nd %>% select(target = node, t_h = h), by = "target") %>%
    mutate(id = row_number(),
           # a link's thickness is its share of the node bar it leaves, and of the one it enters
           w0 = s_h * value / s_val,
           w1 = t_h * value / t_val,
           y0 = sy0 + s_h * off_out / s_val,
           y1 = ty0 + t_h * off_in  / t_val)

  ribs <- pmap_dfr(list(ed$sx, ed$y0, ed$tx, ed$y1, ed$w0, ed$w1, ed$id),
                   function(a, b, c, e, f, g, i)
                     link_ribbon(a, b, c, e, f, g, i, n = n, flat = flat))
  ribs %>% left_join(ed %>% select(rib = id, source, target, value), by = "rib")
}

# Main entry point.
#   file      flow CSV with year, source, target, units, value
#   year      single study year
#   scale     multiply value by this (energy CSVs are in EJ; pass EJ_to_PJ for PJ)
#   relabel   named vector applied to RAW node keys before pretty_labels(), for splitting or
#             merging sinks that the display mapping would otherwise collapse
#   min_share drop links below this share of total throughput. Default 0 -- see note below
#
# ExtraNotes: min_share defaults to 0 because dropping links corrupts the node totals printed in the
# labels. A threshold of 0.15% removed 14 water links worth 0.87% of throughput, which sounds
# harmless and made the groundwater node read 12 MGD against a true 17.66. In a figure whose purpose
# is to let a reader verify a balance, a label that disagrees with the account is worse than a
# hairline ribbon.
sankey_static <- function(file, year, scale = 1, min_share = 0,
                          relabel = NULL,
                          label_size = 2.3, value_fmt = "%.1f", unit = "",
                          flat = 0.42, gap_frac = 0.012, pal = NULL,
                          label_nudge = 0.014) {

  d <- read_csv(file, show_col_types = FALSE, progress = FALSE) %>%
    filter(year == !!year) %>%
    mutate(value = value * scale)

  # ExtraNotes: relabel runs on the RAW keys, before pretty_labels(). The metro water table carries
  # both `losses` (municipal non-revenue plus sectoral consumptive use) and `Losses` (thermoelectric
  # evaporation), which the display mapping sends to the same string. Merging them is right for a
  # general overview and wrong for any figure comparing network loss against plant cooling, so the
  # split has to be available at the call site.
  if (!is.null(relabel)) {
    d <- d %>% mutate(source = if_else(source %in% names(relabel), relabel[source], source),
                      target = if_else(target %in% names(relabel), relabel[target], target))
  }

  d <- d %>%
    pretty_labels() %>%
    group_by(source, target) %>% summarise(value = sum(value), .groups = "drop") %>%
    filter(value > 0)

  if (min_share > 0) {
    tot <- sum(d$value)
    keep <- d$value / tot >= min_share
    if (any(!keep)) {
      message(sprintf("  [fig] dropped %d links below %.2f%% of throughput (%.3f%% of total)",
                      sum(!keep), 100 * min_share, 100 * sum(d$value[!keep]) / tot))
      d <- d[keep, ]
    }
  }

  nd <- sankey_layout_static(d, gap_frac = gap_frac)
  ribs <- sankey_ribbons_static(d, nd, flat = flat)

  # colour by source node, which is the convention the interactive diagrams use
  if (is.null(pal)) pal <- sankey_pal(nd$node)
  ribs$fill <- pal[ribs$source]
  nd$fill <- pal[nd$node]

  # labels sit outside the diagram at the extremes and above the bar in the middle, so text never
  # lands on a ribbon
  # ExtraNotes: labels are repelled vertically only (direction = "y"). Small nodes cluster at the
  # bottom of a proportional diagram -- three plants at 6-34 MGD, four discharge types under 10 --
  # and their labels collide. Free 2D repulsion would push text sideways out of its column and break
  # the reader's association between a label and its layer.
  nd <- nd %>%
    mutate(lab = if (unit == "") node else sprintf("%s\n%s%s", node, fmt_val(value), unit),
           side = case_when(x <= 0.001 ~ "left", x >= 0.999 ~ "right", TRUE ~ "mid"),
           lab_x = case_when(side == "left" ~ x - label_nudge,
                             side == "right" ~ x + label_nudge, TRUE ~ x),
           lab_y = (y0 + y1) / 2,
           hjust = case_when(side == "left" ~ 1, side == "right" ~ 0, TRUE ~ 0.5))

  bar_w <- 0.008
  # ExtraNotes: min.segment.length = 0 forces a leader line on every displaced label. In a
  # proportional diagram the small sinks bunch at the bottom, and without leaders a repelled label
  # sits nearer a neighbour's bar than its own.
  rep_args <- list(size = label_size, colour = "grey15", lineheight = 0.92,
                   direction = "y", min.segment.length = 0, box.padding = 0.16,
                   point.padding = 0, segment.colour = "grey55", segment.size = 0.2,
                   max.overlaps = Inf, seed = 1L)

  ggplot() +
    geom_polygon(data = ribs, aes(x, y, group = rib, fill = fill),
                 alpha = 0.42, colour = NA) +
    geom_rect(data = nd, aes(xmin = x - bar_w, xmax = x + bar_w,
                             ymin = y0, ymax = y1, fill = fill),
              colour = "grey25", linewidth = 0.2) +
    do.call(geom_text_repel, c(list(
      data = nd %>% filter(side == "left"),
      mapping = aes(lab_x, lab_y, label = lab), hjust = 1, xlim = c(NA, -0.02)), rep_args)) +
    do.call(geom_text_repel, c(list(
      data = nd %>% filter(side == "right"),
      mapping = aes(lab_x, lab_y, label = lab), hjust = 0, xlim = c(1.02, NA)), rep_args)) +
    do.call(geom_text_repel, c(list(
      data = nd %>% filter(side == "mid"),
      mapping = aes(lab_x, y0, label = lab), vjust = 1, nudge_y = -0.012), rep_args)) +
    scale_fill_identity() +
    scale_y_reverse(expand = expansion(mult = 0.04)) +
    scale_x_continuous(expand = expansion(mult = 0.19)) +
    theme_void() +
    theme(plot.margin = margin(4, 4, 4, 4))
}

# Balance check for the figure itself: does every mid-chain node close?
# ExtraNotes: run this before putting a Sankey in a paper. A rendering bug that mis-stacks link
# offsets produces a diagram that looks plausible and does not conserve, which is the one failure
# mode a reader would catch and an author would not.
# ExtraNotes: the default exemptions are the same two declared in BALANCE_EXEMPT in R/run_qc.R --
# small distributed and backup generation, whose conversion loss cannot be separated from their fuel
# input because gross generation is reported only for the three large plants. Combined magnitude is
# about 0.03% of the energy system. They are named here so the check does not raise a known and
# documented limitation as if it were a defect.
SANKEY_FIG_EXEMPT <- c("Distributed-scale Generation", "On-Site Backup Generation",
                       "Distributed Gen.", "On-Site Gen.")

sankey_static_check <- function(file, year, scale = 1, tol = 0.005,
                                exempt = SANKEY_FIG_EXEMPT) {
  d <- read_csv(file, show_col_types = FALSE, progress = FALSE) %>%
    filter(year == !!year) %>% mutate(value = value * scale) %>% pretty_labels()
  i <- d %>% group_by(node = target) %>% summarise(i = sum(value), .groups = "drop")
  o <- d %>% group_by(node = source) %>% summarise(o = sum(value), .groups = "drop")
  full_join(i, o, by = "node") %>%
    filter(!is.na(i), !is.na(o), !node %in% exempt) %>%
    mutate(resid = o - i, pct = 100 * resid / pmax(i, 1e-12)) %>%
    filter(abs(pct) > 100 * tol) %>%
    arrange(desc(abs(pct)))
}

# PLOTS.R
# old plots.R with only maps

# Extra analysis and plotting for Metro Atlanta
#
# Hassan Niazi, Sep 2025

# source("functions.R")

# # plot maps
# sf_counties <- st_read(paste0(DATA_DIR, "geojson-counties-fips.json")) %>% rename_with(tolower)

# sf_counties_GA <- sf_counties %>% filter(state == 13) # GA is 13
# sf_counties_atlanta <- sf_counties %>% filter(id %in% fips)

# plot(sf_counties_GA$geometry, col = "lightblue", border = "darkblue")
# plot(sf_counties_atlanta$geometry, col = "lightblue", border = "darkblue")

# county_colors <- rep(brewer.pal(11, "Spectral"), length.out = length(counties))
# # county_colors <- sample(colors(), length(counties))
# # county_colors <- viridis_discrete(length(counties), option = "plasma")

# ggplot() +
#   # geom_sf(data = sf_counties_GA, fill = "gray90", color = "gray") +
#   geom_sf(data = sf_counties_atlanta, aes(fill = name, color = "white"), alpha = 0.75) +
#   geom_sf_text(data = sf_counties_atlanta, aes(label = name), size = 3) +
#   scale_fill_manual(values = county_colors) +
#   theme_void() +
#   labs(title = "Atlanta metro-area Counties in Georgia",
#        caption = "", fill = "County", color = "County")

# # better one (used this)
# ggplot() +
#   # geom_sf(data = sf_counties_GA, fill = "gray90", color = "gray") +
#   geom_sf(data = sf_counties_atlanta, aes(fill = name), color = "white") +
#   geom_sf_text(data = sf_counties_atlanta, aes(label = name), size = 3) +
#   scale_fill_d3("category20", alpha = 0.75) +  # D3.js 20-color palette
#   # or scale_fill_npg() for Nature Publishing Group colors
#   # or scale_fill_aaas() for Science journal colors
#   theme_void() +
#   theme(legend.position = "none")

# # cobb and douglas
# ggplot() +
#   # geom_sf(data = sf_counties_GA, fill = "gray90", color = "gray") +
#   geom_sf(data = sf_counties_atlanta, color = "white") +
#   geom_sf(data = sf_counties_atlanta %>% filter(name %in% c("Douglas", "Cobb")), aes(fill = name), color = "white") +
#   geom_sf_text(data = sf_counties_atlanta, aes(label = name), size = 3) +
#   scale_fill_d3("category20", alpha = 0.75) +  # D3.js 20-color palette
#   # or scale_fill_npg() for Nature Publishing Group colors
#   # or scale_fill_aaas() for Science journal colors
#   theme_void() +
#   theme(legend.position = "none")

# PREVIEW STYLES ----
# Render style variants of the three metro diagrams for review.
#   Rscript R/preview_styles.R
#   open outputs/style_preview/index.html
#
# Node palette and link style are independent, so this writes the full grid. Both are
# ordinary arguments to plot_sankey_enhanced(), e.g.
#   plot_sankey_enhanced(df, color_scheme = "signature", link_style = "nexus")

# Sys.setenv(MAWEI_SAVE_FILES = "0", MAWEI_MAKE_PLOT = "0")
# suppressMessages(suppressWarnings(source("R/flows_energy_water.R")))

# unlink("outputs/style_preview", recursive = TRUE)

# # Energy and water are single-domain, so only the node palette is in question there;
# # the coupling classes only exist in the combined diagram.
# preview_sankey_styles(
#   en_fuel_gen_use_loss_all_trade_metro %>%
#     group_by(year, source, target, units) %>%
#     summarise(value = sum(value) * EJ_to_PJ, .groups = "drop"),
#   label = "energy", units = "PJ", styles = "node",
#   title = "Metro Atlanta energy")

# preview_sankey_styles(
#   df_water_metro_linear_wSW_discharge_type,
#   label = "water", units = "MGD", styles = "node",
#   title = "Metro Atlanta water")

# preview_sankey_styles(
#   energy_water, label = "energy-water", units = "auto", alt_units = ew_alt_units,
#   styles = c("node", "nexus", "domain", "class"),
#   title = "Metro Atlanta energy-water")
