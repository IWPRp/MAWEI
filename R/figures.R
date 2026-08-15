# MAWEI figures — publication-quality figure suite
#
#   Rscript R/figures.R
#
# Reads the analysis tables written by R/analysis.R plus the coordinate file from
# R/prep_spatial.R, and writes multi-panel figures to docs_analysis/figures/ as PNG and PDF.
# Each figure is a self-contained argument; each panel carries one message.
#
# Figure sequence for the manuscript
#   Fig 1  study system, scale and growth
#   Fig 2  water system structure
#   Fig 3  energy system structure
#   Fig 4  county heterogeneity (choropleths)
#   Fig 5  the efficiency opportunity
#   Fig 6  the water-energy coupling
#   Fig 7  data-centre load scenario
#   Fig 8  inter-county wastewater transfer network
#   SI     supporting panels
#
# Hassan Niazi / MAWEI

source("functions.R")
suppressMessages({library(patchwork); library(sf); library(scales)})

FIG_DIR <- "docs_analysis/figures/"
TAB_DIR <- "docs_analysis/analysis_outputs/"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

tab <- function(n) read_csv(paste0(TAB_DIR, n, ".csv"), show_col_types = FALSE)

# One consistent look for every figure. Defined here rather than relying on the interactive
# `mytheme` so a figure rendered in a batch run is identical to one rendered in a session.
theme_mawei <- function(base = 9) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.title = element_text(size = base, colour = "grey20"),
      axis.text = element_text(size = base - 1, colour = "grey30"),
      plot.title = element_text(size = base + 1, face = "bold", colour = "grey10"),
      plot.subtitle = element_text(size = base - 0.5, colour = "grey35"),
      plot.caption = element_text(size = base - 2, colour = "grey45", hjust = 0),
      legend.title = element_text(size = base - 1),
      legend.text = element_text(size = base - 1),
      legend.key.height = unit(0.35, "cm"),
      strip.text = element_text(size = base, face = "bold", colour = "grey15"),
      plot.tag = element_text(size = base + 2, face = "bold")
    )
}
theme_map <- function(base = 9) {
  theme_void(base_size = base) +
    theme(plot.title = element_text(size = base + 1, face = "bold", colour = "grey10"),
          plot.subtitle = element_text(size = base - 0.5, colour = "grey35"),
          legend.title = element_text(size = base - 1),
          legend.text = element_text(size = base - 1),
          legend.key.height = unit(0.35, "cm"),
          plot.tag = element_text(size = base + 2, face = "bold"))
}

# Domain colours reused from the Sankey palette so figures and diagrams agree.
C_WATER <- "#2E7CB0"; C_ENERGY <- "#E07A2F"
C_E4W <- "#7E57C2";   C_W4E <- "#26A69A"
C_LOSS <- "#B0413E";  C_GOOD <- "#2E8B57"; C_GREY <- "grey65"

save_fig <- function(p, name, w, h) {
  ggsave(paste0(FIG_DIR, name, ".png"), p, width = w, height = h, dpi = 400, bg = "white")
  ggsave(paste0(FIG_DIR, name, ".pdf"), p, width = w, height = h, device = cairo_pdf, bg = "white")
  message(sprintf("  %-28s %4.1f x %4.1f in", paste0(name, ".png/.pdf"), w, h))
}

# Wrap long labels so a categorical axis never truncates.
wrap10 <- function(x) str_wrap(x, 10)

message("== reading analysis tables ==")
A1 <- tab("A1_water_balance_metro");  A2 <- tab("A2_energy_balance_metro")
A3 <- tab("A3_water_by_sector");      A4 <- tab("A4_energy_by_sector")
A5 <- tab("A5_fuel_mix");             A5b <- tab("A5b_fuel_shift_2020_2024")
A6 <- tab("A6_basin_withdrawals");    A6b <- tab("A6b_basin_concentration")
A7 <- tab("A7_discharge_destinations"); A8 <- tab("A8_county_profile")
A9 <- tab("A9_growth_and_decoupling")
B1 <- tab("B1_nrw_counterfactual");   B1b <- tab("B1b_nrw_scenarios")
B2 <- tab("B2_ii_energy_penalty");    B2b <- tab("B2b_ii_energy_metro")
B3 <- tab("B3_circularity");          B4 <- tab("B4_septic_structure")
C1 <- tab("C1_electricity_self_sufficiency")
C2 <- tab("C2_efficiency_llnl_vs_corrected"); C3 <- tab("C3_generation_by_plant")
D1 <- tab("D1_energy_intensity_of_water"); D2 <- tab("D2_water_intensity_of_electricity")
D3 <- tab("D3_coupling_asymmetry")
E1 <- tab("E1_transfer_edges");       E1b <- tab("E1b_transfer_network_nodes")
F1 <- tab("F1_datacentre_scenarios")
G1 <- tab("G1_validation_permitted_capacity")

xy <- read_csv(paste0(DATA_DIR, "spatial_facility_coords.csv"), show_col_types = FALSE)
YR <- max(A1$year)

# County geometry, for every choropleth and the point maps.
sf_cty <- st_read(paste0(DATA_DIR, "geojson-counties-fips.json"), quiet = TRUE) %>%
  rename_with(tolower) %>% filter(id %in% fips) %>%
  mutate(county = name) %>% select(county, geometry)

# Merging an sf object with a data frame that shares column names creates .x/.y suffixes and
# breaks aes(); joining only the columns needed avoids that entirely.
cmap <- function(df, col, title, sub, pal = "Blues", lab = waiver(), rev = FALSE, dir = 1) {
  d <- sf_cty %>% left_join(df %>% select(county, v = all_of(col)), by = "county")
  ggplot(d) +
    geom_sf(aes(fill = v), colour = "white", linewidth = 0.3) +
    geom_sf_text(aes(label = county), size = 1.9, colour = "grey20") +
    scale_fill_distiller(palette = pal, direction = dir, labels = lab, na.value = "grey92") +
    labs(title = title, subtitle = sub, fill = NULL) +
    theme_map()
}

###############################################################################%
message("\n== Fig 1: study system, scale and growth ==")

# 1a. Where the infrastructure is. This is the panel the new coordinate work makes possible:
# treatment plants sized by permitted capacity, power plants by nameplate capacity.
p1a <- ggplot(sf_cty) +
  geom_sf(fill = "grey93", colour = "grey99", linewidth = 0.5) +
  geom_point(data = xy %>% filter(kind == "wastewater plant"),
             aes(lon, lat, size = capacity), colour = C_WATER, alpha = 0.5) +
  geom_point(data = xy %>% filter(kind == "power plant", capacity > 300),
             aes(lon, lat, size = capacity / 45), colour = C_ENERGY, alpha = 0.9,
             shape = 17) +
  geom_sf_text(aes(label = county), size = 2.1, colour = "grey30", fontface = "bold") +
  scale_size_area(max_size = 8, guide = "none") +
  coord_sf(expand = FALSE) +
  labs(title = "Water and energy infrastructure",
       subtitle = paste0(sum(xy$kind == "wastewater plant"), " wastewater plants (circles), ",
                         sum(xy$kind == "power plant" & xy$capacity > 300),
                         " major generating plants (triangles);\nsymbol area is capacity")) +
  theme_map()

# 1b/1c. Each system's own stages, named for what they physically are. Forcing one shared set of
# labels onto both was misleading: "delivered to users" meant public supply for water but only
# grid electricity for energy, which omitted the direct fuel that is most of energy end use.
stage_plot <- function(d, fill, title, sub) {
  ggplot(d, aes(stage, v)) +
    geom_col(width = 0.62, fill = fill) +
    geom_text(aes(label = sprintf("%.0f", v)), vjust = -0.45, size = 2.7, colour = "grey25") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = NULL, y = NULL, title = title, subtitle = sub) +
    theme_mawei()
}

w24 <- A1 %>% filter(year == YR)
wbar <- tibble(stage = factor(c("withdrawn", "delivered by\nutilities",
                               "returned as\nsewage", "consumed\nor leaked"),
                             levels = c("withdrawn", "delivered by\nutilities",
                                        "returned as\nsewage", "consumed\nor leaked")),
               v = c(w24$withdrawal, w24$pws_throughput, w24$ww_collected, w24$losses))
p1b <- stage_plot(wbar, C_WATER, paste0("Water account, ", YR, " (MGD)"),
                  sprintf("%.0f%% of system input is consumed or leaked", w24$loss_share_pct))

e24b <- A2 %>% filter(year == YR)
ebar <- tibble(stage = factor(c("primary\ninput", "reaching\nend uses",
                               "useful\nservices", "rejected\nor lost"),
                             levels = c("primary\ninput", "reaching\nend uses",
                                        "useful\nservices", "rejected\nor lost")),
               v = c(e24b$total_input, sum(A4$consumed[A4$year == YR]),
                     e24b$services, e24b$all_rejected))
p1c <- stage_plot(ebar, C_ENERGY, paste0("Energy account, ", YR, " (PJ)"),
                  sprintf("%.0f%% of primary input ends as rejected heat or losses",
                          100 * e24b$all_rejected / e24b$total_input))

# 1d. Indexed growth. The message is the ORDER of the lines, not their level: energy is
# outpacing population growth twice as fast as water is.
idx <- bind_rows(
  pop = A1 %>% select(year, v = pop),
  water = A1 %>% select(year, v = withdrawal),
  energy = A2 %>% select(year, v = total_input),
  electricity = A2 %>% select(year, v = elec_supply),
  .id = "series") %>%
  group_by(series) %>% mutate(idx = 100 * v / first(v)) %>% ungroup() %>%
  mutate(series = factor(series, levels = c("energy", "electricity", "water", "pop"),
                         labels = c("Energy input", "Electricity", "Water withdrawal",
                                    "Population")))

p1d <- ggplot(idx, aes(year, idx, colour = series)) +
  geom_hline(yintercept = 100, linewidth = 0.3, colour = "grey80") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  geom_text(data = idx %>% filter(year == YR),
            aes(label = sprintf("%+.1f%%", idx - 100)), hjust = -0.15, size = 2.6,
            show.legend = FALSE) +
  scale_colour_manual(values = c("Energy input" = C_ENERGY, "Electricity" = "#C46210",
                                 "Water withdrawal" = C_WATER, "Population" = "grey45")) +
  scale_x_continuous(breaks = A1$year, expand = expansion(mult = c(0.02, 0.16))) +
  labs(x = NULL, y = paste0("index, ", min(A1$year), " = 100"), colour = NULL,
       title = "Resource use is growing faster than population",
       subtitle = "Energy demand is decoupling from population twice as poorly as water") +
  theme_mawei() + theme(legend.position = c(0.22, 0.82),
                        legend.background = element_rect(fill = alpha("white", 0.7),
                                                         colour = NA))

fig1 <- (p1a | (p1b / p1c)) / p1d +
  plot_layout(heights = c(1.45, 1)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
save_fig(fig1, "Fig1_system_and_scale", 10.5, 9)

###############################################################################%
message("\n== Fig 2: water system structure ==")

# 2a. Source concentration. The Herfindahl index is annotated because a single number carries
# the finding better than the stack alone.
p2a <- ggplot(A6 %>% mutate(basin = str_remove(basin, " Basin")),
              aes(year, mgd, fill = reorder(basin, mgd))) +
  geom_area(colour = "white", linewidth = 0.25) +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  scale_x_continuous(breaks = A6$year) +
  annotate("text", x = min(A6$year) + 0.1, y = 690,
           label = sprintf("Chattahoochee %.0f%% of withdrawals\nHerfindahl index %.2f",
                           A6b$top_share_pct[A6b$year == YR], A6b$hhi[A6b$year == YR]),
           hjust = 0, size = 2.6, colour = "grey20", lineheight = 1.1) +
  labs(x = NULL, y = "surface withdrawal (MGD)", fill = NULL,
       title = "One river supplies two thirds of the region",
       subtitle = "Metro Atlanta sits near the headwaters of the Chattahoochee") +
  theme_mawei()

# 2b. Sector supply against return. The gap is consumptive use; the ratio distinguishes an
# irrigation-heavy sector from an indoor-water sector.
s24 <- A3 %>% filter(year == YR) %>%
  mutate(sector = str_to_title(sector)) %>%
  pivot_longer(c(returned, consumed), names_to = "fate", values_to = "v") %>%
  mutate(fate = factor(fate, levels = c("consumed", "returned"),
                       labels = c("consumed / lost", "returned as sewage")))

p2b <- ggplot(s24, aes(reorder(sector, supplied), v, fill = fate)) +
  geom_col(width = 0.65) +
  geom_text(data = A3 %>% filter(year == YR) %>% mutate(sector = str_to_title(sector)),
            aes(x = sector, y = supplied, label = sprintf("%.0f%% returned", 100 * return_ratio)),
            inherit.aes = FALSE, hjust = -0.1, size = 2.5, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("consumed / lost" = C_LOSS, "returned as sewage" = C_WATER)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(x = NULL, y = "MGD", fill = NULL,
       title = paste0("Where supplied water ends up, ", YR),
       subtitle = "The unreturned share is consumptive use plus data inconsistency") +
  theme_mawei() + theme(legend.position = "bottom")

# 2c. Discharge destinations on a log scale, because reuse is three orders of magnitude below
# river discharge and a linear axis would render it invisible.
d24 <- A7 %>% filter(year == YR) %>% mutate(destination = str_to_title(destination),
                                            hi = destination %in% c("Reuse", "Land"))
p2c <- ggplot(d24, aes(reorder(destination, mgd), mgd, fill = hi)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.2f (%.2f%%)", mgd, share_pct)), hjust = -0.1,
            size = 2.4, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_log10(expand = expansion(mult = c(0, 0.35))) +
  scale_fill_manual(values = c(`TRUE` = C_GOOD, `FALSE` = C_GREY), guide = "none") +
  labs(x = NULL, y = "MGD (log scale)",
       title = paste0("Treated effluent by receiving environment, ", YR),
       subtitle = "Reuse is 0.6% of effluent despite a national exemplar in the region") +
  theme_mawei()

# 2d. Per-capita demand against published benchmarks, which is what makes the low value a
# finding rather than a number.
bench <- tibble(label = c("US domestic average", "Metro Atlanta"),
                v = c(85, A1$pws_gpcd <- NULL) )
gpcd <- A8 %>% filter(year == YR) %>% select(county, pws_gpcd)
p2d <- ggplot(gpcd, aes(reorder(county, pws_gpcd), pws_gpcd)) +
  geom_col(width = 0.65, fill = C_WATER, alpha = 0.85) +
  geom_hline(yintercept = 85, linetype = "dashed", colour = C_LOSS, linewidth = 0.4) +
  annotate("text", x = 1.5, y = 87, label = "US domestic average ~85 gpcd",
           hjust = 0, size = 2.5, colour = C_LOSS) +
  coord_flip() +
  labs(x = NULL, y = "public supply delivered (gallons per capita per day)",
       title = "Per-capita demand is low by national standards",
       subtitle = "Consistent with Georgia's post-drought conservation regime") +
  theme_mawei()

fig2 <- (p2a | p2b) / (p2c | p2d) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
save_fig(fig2, "Fig2_water_structure", 10.5, 8)

###############################################################################%
message("\n== Fig 3: energy system structure ==")

# 3a. Fuel shift as a slope chart. A grouped bar hides the direction of change; a slope makes
# coal rising and hydro falling immediately legible.
fs <- A5 %>% filter(year %in% range(A5$year), pj > 0.05) %>%
  mutate(fuel = recode(fuel, "out_metro_elec_import" = "Imported electricity",
                       "Hydroelectric Water" = "Hydro", "onsiteBTM" = "On-site"))
p3a <- ggplot(fs, aes(factor(year), pj, group = fuel, colour = fuel)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  geom_text(data = fs %>% filter(year == YR), aes(label = fuel),
            hjust = -0.12, size = 2.6, show.legend = FALSE) +
  scale_y_log10() +
  scale_x_discrete(expand = expansion(mult = c(0.08, 0.55))) +
  scale_colour_brewer(palette = "Dark2", guide = "none") +
  labs(x = NULL, y = "PJ (log scale)",
       title = "The fuel mix moved away from low carbon",
       subtitle = sprintf("Coal %+.0f%%, hydro %+.0f%% over %d-%d",
                          A5b$pj_change_pct[A5b$fuel == "Coal"],
                          A5b$pj_change_pct[A5b$fuel == "Hydroelectric Water"],
                          min(A5$year), YR)) +
  theme_mawei()

# 3b. End-use shares. Transport at half of demand is the structural fact behind the
# petroleum share, so the two panels are adjacent by design.
e24 <- A4 %>% filter(year == YR) %>% mutate(sector = str_to_title(sector),
                                            hi = grepl("Transport", sector))
p3b <- ggplot(e24, aes(reorder(sector, consumed), consumed, fill = hi)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.0f PJ (%.1f%%)", consumed, share_pct)),
            hjust = -0.08, size = 2.5, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  scale_fill_manual(values = c(`TRUE` = C_ENERGY, `FALSE` = C_GREY), guide = "none") +
  labs(x = NULL, y = "PJ",
       title = paste0("End-use energy by sector, ", YR),
       subtitle = "Half of all metro energy moves people and goods") +
  theme_mawei()

# 3c. Generation with capacity factor, which separates "large" from "hard-working".
c24 <- C3 %>% filter(year == YR, !is.na(capacity_factor))
p3c <- ggplot(c24, aes(reorder(plant, pj), pj, fill = capacity_factor)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f PJ | CF %.2f", pj, capacity_factor)),
            hjust = -0.08, size = 2.5, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.45))) +
  scale_fill_distiller(palette = "Oranges", direction = 1, limits = c(0, 0.8)) +
  labs(x = NULL, y = "PJ generated", fill = "capacity\nfactor",
       title = paste0("Generation by plant, ", YR),
       subtitle = "The gas plant runs hardest; the coal plant is largest") +
  theme_mawei()

# 3d. The efficiency correction, stated as a range rather than a point, because the LLNL
# transport coefficient is the single largest source of optimism in the accounting.
ef <- C2 %>% select(year, eff_llnl_pct, eff_real_pct) %>%
  pivot_longer(-year, names_to = "k", values_to = "v") %>%
  mutate(k = if_else(k == "eff_llnl_pct", "LLNL convention (transport = 0.65)",
                     "transport corrected (~0.225)"))
p3d <- ggplot(ef, aes(year, v, colour = k)) +
  geom_ribbon(data = C2, aes(x = year, ymin = eff_real_pct, ymax = eff_llnl_pct),
              inherit.aes = FALSE, fill = C_ENERGY, alpha = 0.12) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.8) +
  scale_colour_manual(values = c(C_ENERGY, "grey35")) +
  scale_x_continuous(breaks = C2$year) +
  scale_y_continuous(limits = c(0, 70)) +
  annotate("text", x = mean(C2$year), y = 50,
           label = sprintf("convention overstates\nuseful energy by %.0f points",
                           mean(C2$overstatement_pp)),
           size = 2.6, colour = "grey25", lineheight = 1.1) +
  labs(x = NULL, y = "useful share of end-use energy (%)", colour = NULL,
       title = "How much energy is actually useful",
       subtitle = "The published convention is optimistic where transport dominates") +
  theme_mawei() + theme(legend.position = "bottom")

fig3 <- (p3a | p3b) / (p3c | p3d) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
save_fig(fig3, "Fig3_energy_structure", 10.5, 8)

###############################################################################%
message("\n== Fig 4: county heterogeneity ==")

c24p <- A8 %>% filter(year == YR)
ss24 <- C1 %>% filter(year == YR) %>%
  mutate(ss = pmin(self_sufficiency_pct, 300))

f4a <- cmap(c24p, "nrw_pct", "Non-revenue water",
            sprintf("%.1f%% metro; %.1f-%.1f%% across counties",
                    100 * sum(c24p$nrw) / sum(c24p$pws_out),
                    min(c24p$nrw_pct), max(c24p$nrw_pct)),
            pal = "Reds", lab = label_percent(scale = 1, accuracy = 1), dir = 1)
f4b <- cmap(c24p, "ii_share_pct", "Infiltration and inflow",
            sprintf("%.0f%% of collected flow metro-wide",
                    100 * sum(c24p$ii) / sum(c24p$collected)),
            pal = "PuBu", lab = label_percent(scale = 1, accuracy = 1), dir = 1)
f4c <- cmap(c24p, "septic_share_pct", "Septic share of household wastewater",
            "Water that leaves the utility system entirely",
            pal = "BuGn", lab = label_percent(scale = 1, accuracy = 1), dir = 1)
f4d <- cmap(ss24, "ss", "Electricity self-sufficiency",
            "Three plant hosts export; twelve counties generate almost nothing",
            pal = "Oranges", lab = label_percent(scale = 1, accuracy = 1), dir = 1)

fig4 <- (f4a | f4b) / (f4c | f4d) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
                  title = paste0("County heterogeneity in ", YR),
                  subtitle = "Adjacent counties differ by factors of three to four on every measure",
                  theme = theme(plot.title = element_text(face = "bold", size = 11),
                                plot.subtitle = element_text(size = 9, colour = "grey35")))
save_fig(fig4, "Fig4_county_heterogeneity", 10, 8.5)

###############################################################################%
message("\n== Fig 5: the efficiency opportunity ==")

# 5a. The counter-factual against physical reference points. Comparing recoverable leakage with
# thermoelectric withdrawal is what turns a percentage into a decision.
sc <- B1b %>%
  mutate(scenario = c("If every county reached\nthe metro average (19.2%)",
                      "If every county matched\nthe best county (8.6%)",
                      "Reference: all thermoelectric\nwithdrawal",
                      "Reference: all water reuse"),
         grp = c("recoverable", "recoverable", "reference", "reference"))
p5a <- ggplot(sc, aes(reorder(scenario, mgd), mgd, fill = grp)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f MGD", mgd)), hjust = -0.1, size = 2.7,
            colour = "grey25") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(recoverable = C_GOOD, reference = C_GREY), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(x = NULL, y = "MGD",
       title = "Leakage reduction is the largest available water source",
       subtitle = "Matching the best-performing county would free more water than the region's\npower plants withdraw") +
  theme_mawei()

# 5b. The energetic cost of infiltration, per county, expressed in households so the magnitude
# is interpretable outside the energy literature.
ii <- B2 %>% filter(year == YR) %>%
  mutate(homes = (kwh_total) / 10500)
p5b <- ggplot(ii, aes(reorder(county, homes), homes)) +
  geom_col(width = 0.65, fill = C_E4W, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.0f", homes)), hjust = -0.15, size = 2.4, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = comma) +
  labs(x = NULL, y = "households of equivalent annual electricity",
       title = "Pumping and treating water that leaked into sewers",
       subtitle = sprintf("%.2f PJ metro-wide, %.0f%% of all water-sector energy",
                          B2b$pj[B2b$year == YR],
                          B2b$share_of_water_energy_pct[B2b$year == YR])) +
  theme_mawei()

# 5c. Circularity in context. Reuse is plotted against the volume that could be recovered from
# leakage, because the comparison shows which lever is larger.
circ <- tibble(
  what = c("Water reused", "Discharged to rivers and streams", "Recoverable from leakage"),
  v = c(B3$reuse[B3$year == YR],
        A7 %>% filter(year == YR, !str_to_lower(destination) %in% c("reuse", "land")) %>%
          pull(mgd) %>% sum(),
        B1b$mgd[2]),
  grp = c("circular", "linear", "opportunity"))
p5c <- ggplot(circ, aes(reorder(what, v), v, fill = grp)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f MGD", v)), hjust = -0.1, size = 2.7, colour = "grey25") +
  coord_flip(clip = "off") +
  scale_y_log10(expand = expansion(mult = c(0, 0.35))) +
  scale_fill_manual(values = c(circular = C_GOOD, linear = C_GREY, opportunity = C_WATER),
                    guide = "none") +
  labs(x = NULL, y = "MGD (log scale)",
       title = "The system is close to linear",
       subtitle = "Reuse is two orders of magnitude below the recoverable leakage volume") +
  theme_mawei()

fig5 <- p5a / (p5b | p5c) +
  plot_layout(heights = c(0.8, 1)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
save_fig(fig5, "Fig5_efficiency_opportunity", 10.5, 8)

###############################################################################%
message("\n== Fig 6: the water-energy coupling ==")

# 6a. Energy intensity of delivered water. The spread is the finding: a county-level metric
# comparable with published benchmarks rather than only with its neighbours.
d1 <- D1 %>% filter(year == YR)
p6a <- ggplot(d1, aes(reorder(county, kwh_per_mg), kwh_per_mg)) +
  geom_col(width = 0.65, fill = C_E4W, alpha = 0.9) +
  geom_text(aes(label = comma(round(kwh_per_mg))), hjust = -0.12, size = 2.4, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = comma) +
  labs(x = NULL, y = "kWh per million gallons delivered",
       title = "Energy to deliver water varies twofold across counties",
       subtitle = sprintf("%s highest at %s kWh/MG; %s lowest at %s",
                          d1$county[which.max(d1$kwh_per_mg)], comma(round(max(d1$kwh_per_mg))),
                          d1$county[which.min(d1$kwh_per_mg)], comma(round(min(d1$kwh_per_mg))))) +
  theme_mawei()

# 6b. Water intensity of generation. Withdrawal and consumption are shown together because the
# RATIO between them is what identifies the cooling technology.
d2 <- D2 %>% filter(year == YR) %>%
  select(plant, withdrawn = gal_per_kwh_withdrawn, consumed = gal_per_kwh_consumed) %>%
  pivot_longer(-plant, names_to = "k", values_to = "v")
p6b <- ggplot(d2, aes(plant, v, fill = k)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%.2f", v)), position = position_dodge(width = 0.7),
            vjust = -0.4, size = 2.4, colour = "grey30") +
  scale_fill_manual(values = c(withdrawn = C_W4E, consumed = C_LOSS),
                    labels = c(consumed = "consumed (evaporated)", withdrawn = "withdrawn")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(x = NULL, y = "gallons per kWh", fill = NULL,
       title = paste0("Water intensity of generation, ", YR),
       subtitle = "The withdrawn-to-consumed ratio identifies the cooling system") +
  theme_mawei() + theme(legend.position = "bottom")

# 6c. The asymmetry. Each direction is expressed as a share of its own system, which is the
# only way the two can be compared honestly.
asym <- D3 %>% filter(year == YR) %>%
  transmute(`Energy used by the water system\n(share of metro energy)` = e4w_share_of_energy_pct,
            `Energy used by the water system\n(share of metro electricity)` = e4w_share_of_electricity_pct,
            `Water used by the energy system\n(share of metro withdrawals)` = w4e_share_of_withdrawal_pct) %>%
  pivot_longer(everything(), names_to = "k", values_to = "v") %>%
  mutate(dir = if_else(grepl("^Water", k), "water for energy", "energy for water"))
p6c <- ggplot(asym, aes(reorder(k, v), v, fill = dir)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", v)), hjust = -0.12, size = 2.7, colour = "grey25") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("energy for water" = C_E4W, "water for energy" = C_W4E)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(x = NULL, y = "% of own system", fill = NULL,
       title = sprintf("The coupling is asymmetric by a factor of %.0f",
                       D3$asymmetry_ratio[D3$year == YR]),
       subtitle = "Water constrains energy far more than energy constrains water") +
  theme_mawei() + theme(legend.position = "bottom")

fig6 <- (p6a | p6b) / p6c +
  plot_layout(heights = c(1, 0.75)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
save_fig(fig6, "Fig6_coupling", 10.5, 8)

###############################################################################%
message("\n== Fig 7: data-centre scenario ==")

f1l <- F1 %>%
  select(new_load_mw, pct_of_metro_electricity, onsite_cooling_mgd,
         upstream_consumption_mgd, total_water_mgd, pct_of_nrw_recoverable)

p7a <- ggplot(f1l, aes(factor(new_load_mw), pct_of_metro_electricity)) +
  geom_col(width = 0.6, fill = C_ENERGY, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.0f%%", pct_of_metro_electricity)), vjust = -0.4,
            size = 2.7, colour = "grey25") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = "new continuous load (MW)", y = "% of current metro electricity supply",
       title = "Data-centre load against the existing electricity system",
       subtitle = "Metro Atlanta is among the fastest-growing US data-centre markets") +
  theme_mawei()

wl <- f1l %>%
  select(new_load_mw, `on-site cooling` = onsite_cooling_mgd,
         `upstream generation` = upstream_consumption_mgd) %>%
  pivot_longer(-new_load_mw, names_to = "k", values_to = "v")
p7b <- ggplot(wl, aes(factor(new_load_mw), v, fill = k)) +
  geom_col(width = 0.6) +
  geom_text(data = f1l, aes(x = factor(new_load_mw), y = total_water_mgd,
                            label = sprintf("%.0f MGD\n%.0f%% of recoverable\nleakage",
                                            total_water_mgd, pct_of_nrw_recoverable)),
            inherit.aes = FALSE, vjust = -0.2, size = 2.4, colour = "grey25", lineheight = 1) +
  scale_fill_manual(values = c("on-site cooling" = C_W4E, "upstream generation" = C_LOSS)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(x = "new continuous load (MW)", y = "water demand (MGD)", fill = NULL,
       title = "The water cost of that load",
       subtitle = "Recovered leakage could cover it, which makes the two policies substitutes") +
  theme_mawei() + theme(legend.position = "bottom")

fig7 <- (p7a | p7b) + plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
save_fig(fig7, "Fig7_datacentre_scenario", 10.5, 4.6)

###############################################################################%
message("\n== Fig 8: inter-county transfer network ==")

# 8a. Spatial network. Centroids are used rather than plant locations because an edge is a
# county-to-county relationship, not a pipe between two named plants.
cent <- sf_cty %>% st_centroid() %>% mutate(lon = st_coordinates(.)[, 1],
                                            lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()
ed <- E1 %>% filter(year == YR) %>%
  left_join(cent, by = c("from_county" = "county")) %>%
  rename(x1 = lon, y1 = lat) %>%
  left_join(cent, by = c("to_county" = "county")) %>%
  rename(x2 = lon, y2 = lat)

p8a <- ggplot() +
  geom_sf(data = sf_cty, fill = "grey97", colour = "white", linewidth = 0.4) +
  geom_curve(data = ed, aes(x = x1, y = y1, xend = x2, yend = y2, linewidth = mgd),
             curvature = 0.22, colour = C_WATER, alpha = 0.55,
             arrow = arrow(length = unit(0.10, "cm"), type = "closed")) +
  geom_sf_text(data = sf_cty, aes(label = county), size = 1.9, colour = "grey35") +
  scale_linewidth(range = c(0.2, 2.6), name = "MGD") +
  labs(title = "Wastewater crosses county lines",
       subtitle = paste0(nrow(ed), " transfer routes in ", YR)) +
  theme_map() + theme(legend.position = "right")

# 8b. Export dependency. The share of a county's own collected flow that it sends away is the
# measure of structural reliance on a neighbour.
nt <- E1b %>% filter(year == YR, exported > 0 | imported > 0)
p8b <- ggplot(nt, aes(reorder(county, export_dependency_pct), export_dependency_pct)) +
  geom_col(width = 0.65, aes(fill = net_mgd > 0)) +
  geom_text(aes(label = sprintf("%.1f%%", export_dependency_pct)), hjust = -0.12,
            size = 2.4, colour = "grey30") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c(`TRUE` = C_WATER, `FALSE` = C_LOSS),
                    labels = c(`TRUE` = "net importer", `FALSE` = "net exporter"),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(x = NULL, y = "% of own collected flow sent to another county",
       title = "Dependence on a neighbour's treatment capacity",
       subtitle = sprintf("%s exports %.0f%% of everything it collects",
                          nt$county[which.max(nt$export_dependency_pct)],
                          max(nt$export_dependency_pct))) +
  theme_mawei() + theme(legend.position = "bottom")

fig8 <- (p8a | p8b) + plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
save_fig(fig8, "Fig8_transfer_network", 10.5, 5.2)

###############################################################################%
message("\n== SI figures ==")

# SI1. Independent validation of permitted capacity against EPA ECHO design flow. A 1:1 line
# with points on it is the most direct way to show two sources agreeing.
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

# SI2. Sectoral loss shares, the term carrying most of the water-side uncertainty.
si2 <- ggplot(A3, aes(year, 100 * (1 - return_ratio), colour = str_to_title(sector))) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  scale_x_continuous(breaks = A3$year) +
  scale_colour_brewer(palette = "Set2", name = NULL) +
  labs(x = NULL, y = "unreturned share of supply (%)",
       title = "Sectoral consumptive-use residual",
       subtitle = "Derived as supply minus return, so it absorbs cross-dataset inconsistency") +
  theme_mawei() + theme(legend.position = "bottom")

# SI3. Electricity trade, the term that makes the metro dependent on outside cooling water.
si3 <- ggplot(A2, aes(year)) +
  geom_col(aes(y = elec_supply), fill = "grey85", width = 0.6) +
  geom_col(aes(y = elec_imports), fill = C_ENERGY, width = 0.6) +
  geom_text(aes(y = elec_imports, label = sprintf("%.0f%%", import_share_pct)),
            vjust = -0.4, size = 2.6, colour = "grey25") +
  scale_x_continuous(breaks = A2$year) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "PJ",
       title = "Electricity imported from outside the metro",
       subtitle = "About a third of supply, and its cooling water lies outside the accounting") +
  theme_mawei()

save_fig((si1 | si2) / (si3 | plot_spacer()) +
           plot_annotation(tag_levels = "a", tag_prefix = "(S", tag_suffix = ")"),
         "FigS1_supporting", 10.5, 8)

message("\n== ", length(list.files(FIG_DIR, pattern = "png$")), " figures in ", FIG_DIR, " ==")
