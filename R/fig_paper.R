# Composite figures for the two manuscripts
#
#   Rscript R/fig_paper.R
#
# Writes docs_analysis/figures_paper/{Fig1_accounts, Fig2_form, Fig3_consequence}.{pdf,png}
# plus the standalone Sankeys used by the long article.
#
# ExtraNotes: kept separate from R/figures.R, which produces the fifteen exploratory analysis
# figures. Those are for reading the results; these are for the papers, and they differ in
# consequence -- panel sizes are set for a journal column, every panel has a caption commitment in
# the .tex, and nothing is drawn that the text does not reference.
#
# Hassan Niazi / MAWEI

source("functions.R")
source("R/fig_helpers.R")
source("R/fig_sankey_static.R")
suppressMessages({library(patchwork); library(sf); library(scales)})

FIG_DIR <- "docs_analysis/figures_paper/"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)
OUT <- "docs_analysis/analysis_outputs/"
tab <- function(n) read_csv(paste0(OUT, n, ".csv"), show_col_types = FALSE, progress = FALSE)

save_fig <- function(p, name, w, h) {
  ggsave(paste0(FIG_DIR, name, ".png"), p, width = w, height = h, dpi = 400, bg = "white")
  ggsave(paste0(FIG_DIR, name, ".pdf"), p, width = w, height = h, device = cairo_pdf, bg = "white")
  message(sprintf("  %-24s %4.1f x %4.1f in", name, w, h))
}

W <- "outputs/files/water/01_metro_water_flows.csv"
E <- "outputs/files/energy/01_metro_energy_flows.csv"
YR <- 2024

###############################################################################%
## Fig 1 -- the closed account and the asymmetry ----

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

save_fig((p1a / p1b / p1c) + plot_layout(heights = c(1, 1, 0.62)),
         "Fig1_accounts", 9.6, 13.2)

# standalone versions for the long article, which shows the two accounts as separate figures
save_fig(p1a + labs(title = NULL), "SankeyWater2024", 9.5, 6.2)
save_fig(p1b + labs(title = NULL), "SankeyEnergy2024", 9.5, 6.2)

###############################################################################%
## Fig 2 -- settlement form and where losses sit ----

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

save_fig((p2a | p2b) / p2c + plot_layout(heights = c(1, 0.78)),
         "Fig2_form", 10.6, 8.4)

###############################################################################%
## Fig 3 -- what the reframing changes ----

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
  labs(title = "b   Data centres are an electricity question",
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

save_fig(p3a / (p3b | p3c) + plot_layout(heights = c(0.82, 1)),
         "Fig3_consequence", 10.6, 8.2)

message("== done: ", length(list.files(FIG_DIR, pattern = "pdf$")), " PDFs in ", FIG_DIR)
