# MAWEI figures — publication figure suite
#
#   Rscript R/figures.R                       writes a NEW docs_analysis/figures_v<n>/
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
# Hassan Niazi / MAWEI

source("functions.R")
source(paste0(SCRIPTS_DIR, "fig_helpers.R"))

FIG_DIR <- fig_dir()
TAB_DIR <- "docs_analysis/analysis_outputs/"
message("== writing to ", FIG_DIR, " ==")
tab <- function(n) read_csv(paste0(TAB_DIR, n, ".csv"), show_col_types = FALSE)

save_fig <- function(p, name, w, h) {
  ggsave(paste0(FIG_DIR, name, ".png"), p, width = w, height = h, dpi = 400, bg = "white")
  ggsave(paste0(FIG_DIR, name, ".pdf"), p, width = w, height = h, device = cairo_pdf, bg = "white")
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
            "K4_interbasin_sewage_transfer","P4_treatment_concentration",
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
p1d <- ggplot(idx, aes(year, idx, colour = series)) +
  geom_hline(yintercept = 100, linewidth = 0.3, colour = "grey85") +
  geom_line(linewidth = 0.85) + geom_point(size = 1.5) +
  geom_text_repel(data = idx %>% filter(year == YR),
                  aes(label = sprintf("%s %+.1f%%", series, idx - 100)), size = 2.4,
                  hjust = 0, direction = "y", nudge_x = 0.12, segment.size = 0.2,
                  segment.colour = "grey75", min.segment.length = 0, box.padding = 0.12,
                  show.legend = FALSE) +
  scale_colour_manual(values = c("Energy input" = C_ENERGY, "Electricity" = "#C05A12",
                                 "Wastewater" = "#1F5F8B", "Water withdrawal" = C_WATER,
                                 "Population" = "grey45"), guide = "none") +
  scale_x_continuous(breaks = A1$year, limits = c(min(A1$year), YR + 2.1)) +
  labs(x = NULL, y = paste0("index, ", min(A1$year), " = 100"),
       title = "Resource use is outgrowing population",
       subtitle = "Energy is decoupling from population twice as poorly as water") +
  theme_mawei()

save_fig((p1a | (p1b / p1c) | p1d) + plot_layout(widths = c(1.15, 0.9, 1.25)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig1_system_and_scale", 16, 5.8)

###############################################################################%
message("\n== Fig 2: water ==")

b <- A6 %>% mutate(basin = str_remove(basin, " Basin")) %>% filter(mgd > 0)
blab <- b %>% filter(year == YR) %>% arrange(desc(mgd)) %>%
  mutate(cum = cumsum(mgd), ypos = cum - mgd / 2)
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

save_fig((p2a | p2b | p2e) / (p2c | p2d | plot_spacer()) +
           plot_layout(heights = c(1, 0.95)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig2_water_structure", 16, 8.6)

###############################################################################%
message("\n== Fig 3: energy ==")

fm <- A5 %>% filter(pj > 0.05) %>%
  mutate(fuel = recode(fuel, "out_metro_elec_import" = "Imported electricity",
                       "Hydroelectric Water" = "Hydro", "onsiteBTM" = "On-site"))
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
                       guide = guide_colourbar(barwidth = 3.6, barheight = 0.28,
                                               title.position = "top")) +
  labs(x = NULL, y = "PJ generated", title = paste0("Generation by plant, ", YR),
       subtitle = "The gas plant runs hardest; the coal plant is largest") +
  theme_mawei() + theme(legend.position = c(0.76, 0.20))

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
  geom_text(data = A2, aes(x = year, y = elec_supply,
                           label = sprintf("%.0f%% imported", import_share_pct)),
            inherit.aes = FALSE, vjust = -0.5, size = 2.2, colour = "grey30") +
  scale_fill_manual(values = c("generated locally" = "#A0522D", "imported" = C_ENERGY,
                               "T&D losses" = C_LOSS, "plant own use" = "grey55"), name = NULL) +
  scale_x_continuous(breaks = A2$year) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(x = NULL, y = "PJ", title = "The electricity account",
       subtitle = "A third of supply is imported, so its cooling water lies outside the region") +
  theme_mawei() + theme(legend.position = "bottom")

save_fig((p3a | p3b | p3c) / (p3d | p3e | p3f) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig3_energy_structure", 16, 8.6)

###############################################################################%
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

save_fig((p6a | p6b | p6d | p6c) + plot_layout(widths = c(1, 1, 1, 1)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig6_coupling", 17, 4.8)

###############################################################################%
message("\n== Fig 7: data centres ==")

dc_ex <- H1 %>% filter(!is.na(lat))
dc_pr <- H4 %>% filter(growth_scenario == "higher", market_gravity_weight == 50)

p7a <- ggplot(sf_cty) +
  geom_sf(fill = "grey95", colour = "white", linewidth = 0.4) +
  geom_point(data = dc_pr, aes(lon, lat), colour = C_ENERGY, alpha = 0.32, size = 2.3,
             shape = 15) +
  geom_point(data = dc_ex, aes(lon, lat, size = sqft), colour = "grey15", alpha = 0.75) +
  geom_sf_text(aes(label = county), size = 1.8, colour = "grey40", fontface = "bold") +
  scale_size_area(max_size = 7, guide = "none") + coord_sf(expand = FALSE) +
  labs(title = "Existing and projected data centres",
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
  select(growth, `data-centre cooling water` = water_mgd,
         `leakage recoverable for the same capital` = leakage_mgd_for_same_capital) %>%
  pivot_longer(-growth, names_to = "k", values_to = "v")
p7f <- ggplot(cmp, aes(growth, v, fill = k)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.0f", v)), position = position_dodge(width = 0.7),
            vjust = -0.35, size = 2.2, colour = "grey30") +
  scale_y_log10(expand = expansion(mult = c(0, 0.20))) +
  scale_fill_manual(values = c("data-centre cooling water" = C_W4E,
                               "leakage recoverable for the same capital" = C_GOOD), name = NULL) +
  labs(x = "growth scenario", y = "MGD (log scale)",
       title = "The same capital, two water outcomes",
       subtitle = "The same money spent on leakage would yield far more water than the\ncooling load consumes") +
  theme_mawei() + theme(legend.position = "bottom")

save_fig((p7a | p7b | p7c) / (p7d | p7e | p7f) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig7_datacentres", 16, 9)

###############################################################################%
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
       subtitle = sprintf("Ribbon width is volume, tapering toward the destination. %d routes, %.0f MGD.\nGrey dots are all other treatment plants; white circles are those receiving transfers.",
                          nrow(ed), sum(ed$mgd))) +
  theme_map() + theme(legend.position = c(0.10, 0.20),
                      legend.direction = "horizontal")

# The county-to-county view, kept alongside the facility view so either can be used. Aggregating
# to the county pair is the conventional representation and makes the net balance legible; the
# facility view is the physical one.
ed_cty <- ed %>% group_by(from_county, to_county, year) %>%
  summarise(mgd = sum(mgd), .groups = "drop") %>%
  left_join(cent %>% select(county, x0 = lon, y0 = lat), by = c("from_county" = "county")) %>%
  left_join(cent %>% select(county, x1 = lon, y1 = lat), by = c("to_county" = "county"))
rib_cty <- sankey_ribbons(ed_cty, x0, y0, x1, y1, mgd, max_w = 0.085, taper = 0.4, curv = 0.2)

p8b <- ggplot() +
  base_layers(basin = TRUE, all_fac = TRUE) +
  geom_polygon(data = rib_cty, aes(x, y, group = rib, colour = mgd), fill = NA,
               linewidth = 0.35) +
  geom_polygon(data = rib_cty, aes(x, y, group = rib), fill = "#1B5E8C", alpha = 0.55) +
  scale_colour_viridis_c(option = "mako", direction = -1, name = "flow (MGD)",
                         guide = guide_colourbar(barwidth = 5, barheight = 0.3,
                                                 title.position = "top")) +
  geom_sf_text(data = sf_cty, aes(label = county), size = 1.9, colour = "grey20",
               fontface = "bold") +
  coord_metro(sf_cty) +
  labs(title = "Spatial Sankey: county to county",
       subtitle = sprintf("The same flows aggregated to the county pair. %d county-to-county links.\nBasin shading shows which watershed each transfer starts and ends in.",
                          nrow(ed_cty))) +
  theme_map() + theme(legend.position = c(0.10, 0.20),
                      legend.direction = "horizontal")

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
  geom_point(aes(size = conveyance_kwh_yr / 1e6, colour = energy_cost_usd_yr / 1e6), alpha = 0.8) +
  geom_text_repel(data = ed %>% slice_max(mgd, n = 4),
                  aes(label = paste0(from_county, " to\n", str_trunc(facility, 20))),
                  size = 1.95, colour = "grey25", lineheight = 0.95, box.padding = 0.45,
                  segment.colour = "grey70", segment.size = 0.2) +
  scale_size_area(max_size = 7, name = "conveyance\nGWh/yr") +
  scale_colour_distiller(palette = "YlOrRd", direction = 1, name = "cost\nUSD M/yr") +
  scale_y_sqrt() +
  labs(x = "haul distance (km)", y = "volume (MGD, sqrt scale)",
       title = "What moving sewage costs",
       subtitle = sprintf("%.0f GWh and about $%.1f million of electricity a year",
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

save_fig((p8a | p8b) / (p8c | p8d | p8e) / (p8f | plot_spacer()) +
           plot_layout(heights = c(1.35, 0.8, 0.85)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")"),
         "Fig8_spatial_network", 16, 14)

###############################################################################%
message("\n== Fig 9: the region in one map ==")

# A single layered map: basins, counties, both kinds of plant, data centres and the transfer
# network together. This is the figure that shows the two systems occupy the same space, which no
# single-theme map can.
p9 <- ggplot() +
  geom_sf(data = sf_basin %>% filter(basin != "Broad"), aes(fill = basin), colour = "white",
          linewidth = 0.35, alpha = 0.22) +
  scale_fill_manual(values = BASIN_COLS, name = "river basin") +
  geom_sf(data = sf_cty, fill = NA, colour = "grey45", linewidth = 0.4) +
  geom_polygon(data = rib_fac, aes(x, y, group = rib), fill = "#1F6FA8", alpha = 0.35) +
  geom_point(data = xy %>% filter(kind == "wastewater plant"),
             aes(lon, lat, size = capacity), shape = 21, fill = alpha(C_WATER, 0.5),
             colour = "grey25", stroke = 0.3) +
  geom_point(data = xy %>% filter(kind == "power plant", capacity > 300),
             aes(lon, lat, size = capacity / 40), shape = 24, fill = alpha(C_ENERGY, 0.85),
             colour = "grey20", stroke = 0.3) +
  geom_point(data = dc_ex, aes(lon, lat), shape = 22, size = 2, fill = alpha("grey20", 0.8),
             colour = "white", stroke = 0.25) +
  geom_sf_text(data = sf_cty, aes(label = county), size = 2.1, colour = "grey15",
               fontface = "bold") +
  scale_size_area(max_size = 9, guide = "none") +
  coord_metro(sf_cty) +
  labs(title = "Metro Atlanta's water and energy system in one frame",
       subtitle = paste0("Basins shaded; ", sum(xy$kind == "wastewater plant"),
                         " treatment plants (circles), major power plants (triangles), ",
                         nrow(dc_ex), " data centres (squares).\nRibbons are inter-county sewage transfers, width proportional to volume.")) +
  theme_map() + theme(legend.position = "right")

save_fig(p9, "Fig9_regional_overview", 11, 9)

###############################################################################%
message("\n== SI ==")

si1 <- ggplot(G1, aes(epa_design_mgd, permitted_capacity)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey55") +
  geom_point(aes(colour = agrees_10pct), size = 2.2, alpha = 0.85) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(values = c(`TRUE` = C_GOOD, `FALSE` = C_LOSS),
                      labels = c(`TRUE` = "within 10%", `FALSE` = "differs"), name = NULL) +
  labs(x = "EPA ECHO design flow (MGD, log)", y = "study permitted capacity (MGD, log)",
       title = "Independent validation of facility capacity",
       subtitle = sprintf("%d of %d agree within 10%%; median ratio %.2f",
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
