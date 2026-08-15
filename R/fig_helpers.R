# MAWEI figure helpers — theme, palette, versioned output, spatial primitives
#
# Sourced by R/figures.R. Kept separate so the figure script stays readable and so the spatial
# Sankey machinery can be reused.
#
# Hassan Niazi / MAWEI

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
