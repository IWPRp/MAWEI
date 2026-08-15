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
# Hassan Niazi / MAWEI

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
  "Biomass" = "#8A9A5B", "Hydroelectric Water" = "#2E7CB0", "Solar" = "#E8C547",
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
