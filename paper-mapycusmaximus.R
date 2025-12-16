## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(tidyverse)
library(mapycusmaximus)
library(geogrid)
library(sf)
library(units)
library(ggthemes)
library(cowplot)
library(RColorBrewer)
library(cartogram)
library(patchwork)
library(ggrepel)
library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)
library(stringr)
library(janitor)
library(plotly)
<<<<<<< HEAD
=======
library(sugarbag)
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73


## ----intro-snippet, eval=FALSE----------------------------------------------------------------
# # Minimal usage: warp an sf layer around a focal polygon
# melb <- vic |> dplyr::filter(LGA_NAME == "MELBOURNE")
# vic_fish <- sf_fisheye(vic, center = melb, r_in = 0.33, r_out = 0.60,
#                        zoom = 1.8, squeeze = 0.35)


<<<<<<< HEAD
## ----data-cart, include=FALSE-----------------------------------------------------------------
=======
## ----data-preparation-pop-----------------------------------------------------
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
# Library
# Load the population per states (source: https://www.abs.gov.au/methodologies/data-region-methodology/2011-25#data-downloads)
pop <- readxl::read_xlsx("data/14100DO0001_2011-25.xlsx",sheet = 3, skip = 6)
pop <- pop |>
  mutate(across(where(is.character), ~ na_if(.x, "-")))
pop <- pop |>
  clean_names() |>
  rename_with(~ str_replace_all(.x, c(
    "estimated_resident_population" = "erp",
    "population_density" = "pop_density",
    "persons_km2" = "per_km2",
    "years" = "yr",
    "percentage" = "pct",
    "percent" = "pct",
    "no" = "n",
    "aged_" = "age_",
    "and_over" = "plus"
  )))

pop <- pop |>
  mutate(across(-c(label, year), ~ parse_number(.x)))

clean_lga <- function(x) {
  x |>
    str_to_upper() |>
    str_replace_all("\\s*\\(.*\\)$", "") |>   # drop trailing "(NSW)" etc.
    str_squish()
}

pop2 <- pop |> 
  mutate(lga_key = clean_lga(pop$label))

vic2 <- vic |> 
  mutate(lga_key = clean_lga(vic$LGA_NAME))

my_sf <- left_join(vic2, pop2, by = "lga_key")


# Compute the cartogram, using this population information
# First we need to change the projection, we use Mercator (AKA Google Maps, EPSG 3857)
my_sf_merc <- st_transform(my_sf, 3857)
my_sf_merc <- my_sf_merc |>
  mutate(erp_n = erp_n / 100000)
my_sf_merc_2024 <- my_sf_merc |> filter(year == 2024)
my_sf_merc_2024 <- my_sf_merc_2024 |>
  mutate(erp_round = factor(round(erp_n)))
my_sf <- my_sf_merc_2024 |>
  select(LGA_NAME, erp_n, erp_round, geometry) |>
  st_make_valid() |>
  st_transform(7844)

<<<<<<< HEAD
# Back to original projection
cartogram <- st_transform(cartogram, st_crs(my_sf))


## ----plot-cart, echo=FALSE, fig.cap="Population of Victorian LGAs shown as a diffusion cartogram. Colour (and size) indicates population. We can see that the LGAs for greater Melbourne have the highest population, and these are massively exploded."----
ggplot(cartogram) +
  geom_sf(aes(fill = erp_n), linewidth = 0.05, alpha = 0.9, color = "black") +
  scale_fill_gradientn(
    colours = brewer.pal(7, "BuPu"), name = "population (in 100,000)",
    labels = scales::label_comma(),
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = "top",
      label.position = "bottom"
    )
  ) +
  geom_sf_text(aes(label = LGA_NAME), color = "white", size = 2, alpha = 0.8) +
  theme_void() +
  ggtitle("Another look on the Victoria population") +
  theme(
    legend.position = c(0.5, 0.9),
    legend.direction = "horizontal",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f9", color = NA),
    panel.background = element_rect(fill = "#f5f5f9", color = NA),
    legend.background = element_rect(fill = "#f5f5f9", color = NA),
    plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )


## ----sugarbag-prepare, include=FALSE----------------------------------------------------------
library(sugarbag)

centroids <- create_centroids(vic, "LGA_NAME")
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)
hex_allocated <- allocate(
  centroids = centroids,
  hex_grid = grid,
  hex_size = 0.2, # same size used in create_grid
  hex_filter = 3,
  focal_points = capital_cities,
  width = 30, 
  verbose = TRUE
)


## ----sugarbag-plot, echo=FALSE, fig.cap="Sugarbag example: thematic distortion changes shapes and sizes to encode population."----
hexagons <- fortify_hexagon(data = hex_allocated, sf_id = "LGA_NAME", hex_size = 0.2)

polygons <- fortify_sfc(vic) %>% 
  mutate(poly_type = "geo")

ggplot(mapping = aes(fill = LGA_NAME)) +
  geom_polygon(data = polygons, 
               aes(x=long, lat, 
    group = interaction(LGA_NAME, polygon)), 
               alpha = 0.4) +
  geom_polygon(data = hexagons, 
               aes(x=long, lat, 
    group = interaction(LGA_NAME))) +
  scale_fill_viridis_d() +
  theme_map() +
  theme(legend.position = "none")
=======
pal <- brewer.pal(nlevels(my_sf$erp_round), "BuPu")
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73


## ----data-cow-plot, include=FALSE-------------------------------------------------------------
greater_melbourne <- c(
  "BANYULE", "BAYSIDE", "BOROONDARA", "BRIMBANK", "CARDINIA", "CASEY",
  "DAREBIN", "FRANKSTON", "GLEN EIRA", "GREATER DANDENONG",
  "HOBSONS BAY", "HUME", "KINGSTON", "KNOX", "MARIBYRNONG",
  "MANNINGHAM", "MAROONDAH", "MELBOURNE", "MELTON", "MERRI-BEK",
  "MONASH", "MOONEE VALLEY", "MORNINGTON PENINSULA",
  "NILLUMBIK", "PORT PHILLIP", "STONNINGTON", "WHITEHORSE",
  "WHITTLESEA", "WYNDHAM", "YARRA", "YARRA RANGES"
)

lga_inset <- my_sf[my_sf$LGA_NAME %in% greater_melbourne, ]
main_map <- ggplot() +
 geom_sf(data = my_sf, aes(fill = erp_round), color = "grey65") +
 geom_sf(data = lga_inset, aes(fill = erp_round), color = "black") + # Highlight the inset area
  scale_fill_manual(
    values = pal,
    name   = "population (in 100,000)",
    drop   = TRUE,              # don't show unused levels
    na.translate = FALSE,       # don't show NA in legend
    guide  = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keyheight = unit(3, "mm"),
      keywidth  = unit(12, "mm")
    )
  ) +
  ggtitle("Another look on the Victoria population with inset map") +
  theme_map() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    plot.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )
inset_map <- ggplot() +
  geom_sf(data = lga_inset, fill = "lightblue", color = "darkblue", linewidth = 0.2) +
  labs(title = "Greater Melbourne") +
  theme_map() +
  theme(
    plot.title = element_text(size = 9, face = "bold", hjust = 0.5)
  )


## ----cow-plot-plot, echo=FALSE, fig.cap="Overview map with inset showing Greater Melbourne. The main panel displays Victoria, while a secondary inset zooms into metropolitan Melbourne. The separation highlights local detail but requires the reader to mentally integrate focus and context across panels."----
final_map <- ggdraw() +
 draw_plot(main_map, x = 0, y = 0, width = 1, height = 1) + # Main map occupies the whole canvas
 draw_plot(inset_map, x = 0.7, y = 0.7, width = 0.25, height = 0.25) # Inset map position and size

print(final_map)


<<<<<<< HEAD
## ----preparing-data, inMMOlude=FALSE----------------------------------------------------------
=======
## ----sugarbag-prepare, include=FALSE------------------------------------------
centroids <- create_centroids(vic, "LGA_NAME")
grid <- create_grid(centroids = centroids, hex_size = 0.2, buffer_dist = 1.2)
hex_allocated <- allocate(
  centroids = centroids,
  hex_grid = grid,
  hex_size = 0.2, # same size used in create_grid
  hex_filter = 3,
  focal_points = capital_cities,
  width = 30, 
  verbose = TRUE
)



## ----sugarbag-plot, echo=FALSE, fig.cap="Sugarbag hex tile map illustrating thematic spatial abstraction. Original LGA polygons are replaced by uniform hexagons, with fill indicating poplulation and original geography shown faintly beneath. This representation removes area bias but sacrifices precise geographic location."----
hexagons <- fortify_hexagon(
  data    = hex_allocated,
  sf_id   = "LGA_NAME",
  hex_size = 0.2
) %>%
  left_join(
    my_sf %>%
      st_drop_geometry() %>%
      select(LGA_NAME, erp_round, erp_n),
    by = "LGA_NAME", 
    relationship = "many-to-many"
  ) %>%
  mutate(
    erp_round = factor(
      erp_round,
      levels = sort(unique(erp_round))
    )
  ) |>
  filter(!is.na(erp_round))

polygons <- fortify_sfc(vic) %>% 
  mutate(poly_type = "geo") |> 
  mutate(layer = "base")


plot_2 <- ggplot() +
  geom_polygon(
    data = polygons,
    aes(x = long, y = lat, group = interaction(LGA_NAME, polygon)),
    fill = "grey85", colour = "grey70", linewidth = 0.2
  ) +
  geom_polygon(
    data = hexagons,
    aes(x = long, y = lat, group = interaction(LGA_NAME), fill = erp_round,
        text = paste0(
          "<b>", LGA_NAME, "</b><br>",
          "Population: ", scales::comma(erp_n * 1e5)
        )),
    colour = "grey20", linewidth = 0.3
  ) +
  scale_fill_manual(
    values = pal,
    name   = "population (in 100,000)",
    drop   = TRUE,              # don't show unused levels
    na.translate = FALSE,       # don't show NA in legend
    guide  = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keyheight = unit(3, "mm"),
      keywidth  = unit(12, "mm")
    )
  ) +
  ggtitle("Another look on the Victoria population with sugarbag") +
  coord_equal(expand = FALSE) +
  theme_map() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  )

ggplotly(plot_2, tooltip = "text") |> layout(
  title = list(text = "Another look on the Victoria population", x = 0.5),
  legend = list(
    orientation = "h",
    x = 0.5, xanchor = "center",
    y = 1.07, yanchor = "top"  # inside the plot area
  ),
  margin = list(t = 60)
)


## ----data-cart, include=FALSE-------------------------------------------------
cartogram <- cartogram_cont(my_sf_merc_2024, "erp_n")
# Back to original projection
cartogram <- st_transform(cartogram, st_crs(my_sf))


## ----plot-cart, echo=FALSE, fig.cap="Population of Victorian LGAs shown as a diffusion cartogram. Colour (and size) indicates population. We can see that the LGAs for greater Melbourne have the highest population, and these are massively exploded.", eval=knitr::is_latex_output(), fig.align="center", out.width="80%"----
ggplot(cartogram) +
  geom_sf(aes(fill = erp_n), linewidth = 0.05, alpha = 0.9, color = "black") +
  scale_fill_gradientn(
    colours = brewer.pal(7, "BuPu"), name = "population (in 100,000)",
    labels = scales::label_comma(),
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      title.position = "top",
      label.position = "bottom"
    )
  ) +
  theme_void() +
  ggtitle("Another look on the Victoria population") +
  theme(
    legend.position = c(0.5, 0.9),
    legend.direction = "horizontal",
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f9", color = NA),
    panel.background = element_rect(fill = "#f5f5f9", color = NA),
    legend.background = element_rect(fill = "#f5f5f9", color = NA),
    plot.title = element_text(size = 20, hjust = 0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )


## ----plot-cart-html, echo=FALSE, fig.cap="Population of Victorian LGAs shown as a diffusion cartogram. Colour (and size) indicates population. We can see that the LGAs for greater Melbourne have the highest population, and these are massively exploded.", eval=knitr::is_html_output(), fig.align="center", out.width="100%"----
# 
# cartogram2 <- cartogram |>
#   st_make_valid() |>
#   st_collection_extract("POLYGON") |>
#   st_cast("MULTIPOLYGON")
# 
# plot_1 <- ggplot(cartogram2) +
#   geom_sf(
#     aes(
#       fill = erp_round,
#       text = paste0(
#         "<b>", LGA_NAME, "</b><br>",
#         "Population: ", scales::comma(erp_n * 1e5)
#       )
#     ),
#     linewidth = 0.05, alpha = 0.9, color = "black"
#   ) +
#   scale_fill_manual(
#     values = brewer.pal(nlevels(cartogram2$erp_round), "BuPu"),
#     name   = "population (in 100,000)",
#     guide  = guide_legend(
#       title.position = "top",
#       label.position = "bottom",
#       keyheight = unit(3, "mm"),
#       keywidth  = unit(12, "mm")
#     )
#   ) +
#   theme_void() +
#   theme(
#     legend.position = c(0.5, 0.9),
#     legend.direction = "horizontal"
#   ) +
#   coord_sf(expand = FALSE)
# 
# p <- ggplotly(plot_1, tooltip = "text")
# 
# p <- p |>
#   layout(
#     title = list(text = "Another look on the Victoria population", x = 0.5, font = list(size = 14)),
#      legend = list(
#     orientation = "h",
#     x = 0.5, xanchor = "center",
#     y = 1.10, yanchor = "top",
#     title = list(font = list(size = 12)),
#     font  = list(size = 9)
#   ),
#     margin = list(l = 0, r = 0, b = 0, t = 90, pad = 0),
#     xaxis = list(autorange = TRUE),
#     yaxis = list(autorange = TRUE)
#     ) |>
#   config(responsive = TRUE)
# 
# p


## ----preparing-data, include=FALSE--------------------------------------------
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73

plot_fisheye_fgc <- function(original_coords, transformed_coords, 
  cx = 0, cy = 0, r_in = 0.34, r_out = 0.5) {

# Create data frames for plotting
  zones <- classify_zones(original_coords, cx, cy, r_in, r_out)

  original_df <- data.frame(
    x = original_coords[, 1],
    y = original_coords[, 2],
    zone = zones,
    type = "Original"
  )

  transformed_df <- data.frame(
    x = transformed_coords[, 1],
    y = transformed_coords[, 2], 
    zone = zones,
    type = "Transformed"
  )

  combined_df <- rbind(original_df, transformed_df)

  # Create the plot
  p <- ggplot(combined_df, aes(x = x, y = y, color = zone)) +
    geom_point(size = 1.5, alpha = 0.8) +
    scale_color_manual(values = c("focus" = "#c60000ff", 
      "glue" = "#a5a9e8ff", 
      "context" = "#FFCC00")) +
    facet_wrap(~type) +
    coord_fixed() +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.title = element_blank()
    ) +
    labs(title = "Fisheye FGC Transformation",
    subtitle = paste("r_in =", r_in, ", r_out =", r_out))

  # Add zone boundary circles
  if (r_in > 0) {
    circle_in <- data.frame(
      x = cx + r_in * cos(seq(0, 2*pi, length.out = 100)),
      y = cy + r_in * sin(seq(0, 2*pi, length.out = 100))
    )
  p <- p + geom_path(data = circle_in, aes(x = x, y = y), 
    color = "red", linetype = "dashed", inherit.aes = FALSE)
  }

  circle_out <- data.frame(
    x = cx + r_out * cos(seq(0, 2*pi, length.out = 100)),
    y = cy + r_out * sin(seq(0, 2*pi, length.out = 100))
  )
  p <- p + geom_path(data = circle_out, aes(x = x, y = y), 
    color = "blue", linetype = "dashed", inherit.aes = FALSE)

    return(p)
}


## ----fgc-zones, echo=FALSE, fig.cap="Illustration of Focus-Glue-Context zones in a fisheye transformation. Original grid points are shown alongside their transformed positions, coloured by zone, with arrows indicating displacement. Points expand in the focus, compress smoothly in the glue, and remain fixed in the context."----

grid <- create_test_grid()
grid_df <- as_tibble(grid)
transform <- fisheye_fgc(grid, cx = 0, cy = 0, r_in = 0.34, r_out = 0.5)
transform_df <- as_tibble(transform) |>
  dplyr::mutate(
    zone = attr(transform, "zones"),
    r_orig = attr(transform, "original_radius"),
    r_new  = attr(transform, "new_radius")
  )
arrows_df <- cbind(grid_df, transform_df)

ggplot() +
  # Draw arrows showing movement
  # Original points
  geom_point(
    data = grid_df, aes(x = x, y = y),
    size = 0.6, alpha = 0.7, color = "#8080e0ff"
  ) +
  # Transformed points
  geom_point(
    data = transform_df, aes(x = x_new, y = y_new, color = zone),
    size = 0.6, alpha = 0.7
  ) +
  scale_color_manual(values = c("focus" = "#c60000ff", 
  "glue" = "#070737ff", 
  "context" = "#FFCC00")) +
    geom_segment(
  data = arrows_df |>
    filter(
      zone != "context",
      sqrt((x_new - x)^2 + (y_new - y)^2) > 1e-6  # only draw if moved
    ),
  aes(x = x, y = y, xend = x_new, yend = y_new, color = zone),
  arrow = arrow(length = unit(0.02, "npc")),
  alpha = 0.6,
  size = 0.5
) +
  scale_color_manual(values = c("focus" = "#c60000ff", 
     "glue" = "#070737ff", 
     "context" = "#FFCC00")) +
  coord_equal() +
  theme_minimal(base_size = 14) +
  labs(title = "Fisheye Transformation: Point Movement", x = "x", y = "y")



## ----diagnostics-output, echo=TRUE------------------------------------------------------------
# Inspect diagnostics returned by fisheye_fgc()
head(transform_df[, c("x_new", "y_new", "zone", "r_orig", "r_new")])
table(transform_df$zone)


## ----radial-curve, fig.cap="Radial mapping function of the FGC fisheye. The plot shows original radius r against warped radius r', with shaded focus, glue, and context regions and a reference identity line. The curve demonstrates expansion in the focus, smooth compression in the glue, and identity mapping outside.", out.width="120%"----
r_in  <- 0.34
r_out <- 0.55

rad <- tibble::tibble(r = seq(0, 1, length.out = 400))
xy  <- cbind(rad$r, 0)
xy2 <- fisheye_fgc(xy, r_in = r_in, r_out = r_out,
                    zoom_factor = 3, squeeze_factor = 0.35,
                    method = "outward")
xy2 <- xy2 |>
  as_tibble() |>
  mutate(zone = attr(xy2, "zones"),
          r1   = attr(xy2, "original_radius"),
          r2   = attr(xy2, "new_radius"))

# Points on the curve closest to r_in and r_out (for targeted labels)
i_in  <- which.min(abs(xy2$r1 - r_in))
i_out <- which.min(abs(xy2$r1 - r_out))
pt_in  <- xy2[i_in,  c("r1", "r2")]
pt_out <- xy2[i_out, c("r1", "r2")]
i_maxF <- which.max(if_else(xy2$r1 <= r_in, xy2$r2, NA_real_))

ggplot(xy2, aes(r1, r2, color = zone)) +
  # Light zone shading by r (x-axis)
  annotate("rect", xmin = 0,     xmax = r_in,  ymin = -Inf, ymax = Inf,
            fill = "#c60000", alpha = 0.05) +
  annotate("rect", xmin = r_in,  xmax = r_out, ymin = -Inf, ymax = Inf,
            fill = "#6c8ae6", alpha = 0.05) +
  annotate("rect", xmin = r_out, xmax = 1,     ymin = -Inf, ymax = Inf,
            fill = "#ffb000", alpha = 0.05) +
  # Curve + references
  geom_line(size = 1.1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = c(r_in, r_out), linetype = c("dashed", "dotted"),
              color = "grey40") +
  geom_hline(yintercept = c(r_in, r_out), linetype = c("dashed", "dotted"),
              color = "grey40") +
  annotate("label", x = r_in,  y = 0.05, label = "r_in",  angle = 90, vjust = -0.5) +
  annotate("label", x = r_out, y = 0.05, label = "r_out", angle = 90, vjust = -0.5) +
  annotate("label", y = r_in + 0.03,  x = 0.05, label = "r_in") +
  annotate("label", y = r_out  + 0.03, x = 0.05, label = "r_out") +
  annotate("label", x = 0.88, y = 0.88, label = "identity: r' = r",
            color = "steelblue", fill = "white") +
  # Arrows to the curve at r_in / r_out
  annotate(
    "curve",
    x = r_in + 0.07, y = r_in + 0.02,
    xend = pt_in$r1, yend = pt_in$r2,
    curvature = 0.2,
    arrow = arrow(type = "closed", length = unit(6, "pt"))
  ) +
  annotate("label", x = r_in + 0.08, y = r_in + 0.04, label = "expansion at r_in",
            color = "steelblue", fill = "white") +
  annotate(
    "curve",
    x = r_out - 0.12, y = r_out + 0.08,
    xend = pt_out$r1, yend = pt_out$r2,
    curvature = -0.2,
    arrow = arrow(type = "closed", length = unit(6, "pt"))
  ) +
  annotate("label", x = r_out - 0.13, y = r_out + 0.09, label = "compression at r_out",
            color = "steelblue", fill = "white") +
  # Arrow to the inner radius
  geom_point(aes(x = r1, y = r2), data = xy2[i_maxF, ], size = 1, color = "black") +
  annotate(
    "curve",
    x = r_in * 0.55, y = r_in * 0.40,
    xend = xy2$r1[i_maxF], yend = xy2$r2[i_maxF],
    curvature = 0.2,
    arrow = arrow(type = "closed", length = unit(4, "pt"))
  ) +
  annotate("label", x = r_in, y = r_in * 0.5,
            label = "shifted radius (expansion) in focus \n then flattened at r_in",
            color = "steelblue", fill = "white") +
  # Visual cue for flattening at r_in (horizontal tangent/cap)
  scale_color_manual(values = c(focus = "#c60000", glue = "#6c8ae6", context = "#ffb000"),
                      name = "Zone") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), ratio = 1) +
  labs(x = "r (original)", y = "r' (warped)") +
  theme_minimal(base_size = 10)


<<<<<<< HEAD
## ----norm-diagram, fig.cap="Diagram of the normalization step", eval=knitr::is_html_output(), fig.align="center", out.width="100%"----
grViz("
=======
## ----norm-diagram, fig.cap="Workflow diagram of the normalization and CRS handling pipeline. The flowchart depicts CRS selection, normalization, center resolution, fisheye application, and CRS restoration. This highlights the staged design ensuring projection awareness and parameter stability.", eval=knitr::is_html_output(), fig.align="center", out.width="100%"----
# grViz("
# digraph fgc {
#   rankdir=TB; nodesep=0.25; ranksep=0.4;
#   node [shape=rect, style=rounded, fontsize=10, fontname=Helvetica, fillcolor='#f5f5f9', color='#4e4d47', style=filled];
#   edge [color='#4e79a7', arrowsize=.7];
# 
#   A [label='Input sf/sfc or matrix\\noptions: center, preserve_aspect, normalized_center'];
#   B [shape=diamond, label='Projected?', fillcolor='#eef3ff'];
#   C [label='Use layer CRS'];
#   D [label='Transform to projected CRS\\n(UTM / GDA2020)'];
#   E [label='BBox (xmin,xmax,ymin,ymax)'];
#   F [label='Compute mid + half-spans'];
#   G [shape=diamond, label='Preserve aspect?', fillcolor='#eef3ff'];
#   H [label='Use s=max(sx,sy)'];
#   I [label='Use sx,sy independently'];
#   J [shape=diamond, label='Resolve center', fillcolor='#eef3ff'];
#   K [label='sf/sfc -> centroid'];
#   L [label='numeric + CRS -> transform'];
#   M [label='numeric only -> lon/lat heuristic'];
#   N [label='normalized_center -> (mx,my)+center*scale'];
#   O [label='Normalize to unit space'];
#   P [label='Apply fisheye'];
#   Q [label='Denormalize'];
#   R [label='Restore CRS'];
# 
#   A -> B;
#   B -> C [label='yes'];
#   B -> D [label='no'];
#   C -> E;
#   D -> E;
#   E -> F;
#   F -> G;
#   G -> H [label='yes'];
#   G -> I [label='no'];
#   H -> J;
#   I -> J;
#   J -> K;
#   J -> L;
#   J -> M;
#   J -> N;
#   K -> O;
#   L -> O;
#   M -> O;
#   N -> O;
#   O -> P;
#   P -> Q;
#   Q -> R;
# }
# ")


## ----norm-diagram-pdf, eval=knitr::is_latex_output()--------------------------

# Static version for PDF using DiagrammeR's export

diagram <- grViz("
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
digraph fgc {
  rankdir=TB; nodesep=0.25; ranksep=0.4;
  node [shape=rect, style=rounded, fontsize=10, fontname=Helvetica, fillcolor='#f5f5f9', color='#4e4d47', style=filled];
  edge [color='#4e79a7', arrowsize=.7];

  A [label='Input sf/sfc or matrix\\noptions: center, preserve_aspect, normalized_center'];
  B [shape=diamond, label='Projected?', fillcolor='#eef3ff'];
  C [label='Use layer CRS'];
  D [label='Transform to projected CRS\\n(UTM / GDA2020)'];
  E [label='BBox (xmin,xmax,ymin,ymax)'];
  F [label='Compute mid + half-spans'];
  G [shape=diamond, label='Preserve aspect?', fillcolor='#eef3ff'];
  H [label='Use s=max(sx,sy)'];
  I [label='Use sx,sy independently'];
  J [shape=diamond, label='Resolve center', fillcolor='#eef3ff'];
  K [label='sf/sfc -> centroid'];
  L [label='numeric + CRS -> transform'];
  M [label='numeric only -> lon/lat heuristic'];
  N [label='normalized_center -> (mx,my)+center*scale'];
  O [label='Normalize to unit space'];
  P [label='Apply fisheye'];
  Q [label='Denormalize'];
  R [label='Restore CRS'];

  A -> B; 
  B -> C [label='yes']; 
  B -> D [label='no'];
  C -> E; 
  D -> E; 
  E -> F; 
  F -> G;
  G -> H [label='yes']; 
  G -> I [label='no'];
  H -> J; 
  I -> J;
  J -> K; 
  J -> L; 
  J -> M; 
  J -> N;
  K -> O; 
  L -> O; 
  M -> O; 
  N -> O;
  O -> P; 
  P -> Q; 
  Q -> R;
}
")


## ----norm-diagram-pdf, fig.cap="Diagram of the normalization step", eval=knitr::is_latex_output(), fig.align="center", out.width="80%"----
# 
# # Static version for PDF using DiagrammeR's export
# 
# diagram <- grViz("
# digraph fgc {
#   rankdir=TB; nodesep=0.25; ranksep=0.4;
#   node [shape=rect, style=rounded, fontsize=10, fontname=Helvetica, fillcolor='#f5f5f9', color='#4e4d47', style=filled];
#   edge [color='#4e79a7', arrowsize=.7];
# 
#   A [label='Input sf/sfc or matrix\\noptions: center, preserve_aspect, normalized_center'];
#   B [shape=diamond, label='Projected?', fillcolor='#eef3ff'];
#   C [label='Use layer CRS'];
#   D [label='Transform to projected CRS\\n(UTM / GDA2020)'];
#   E [label='BBox (xmin,xmax,ymin,ymax)'];
#   F [label='Compute mid + half-spans'];
#   G [shape=diamond, label='Preserve aspect?', fillcolor='#eef3ff'];
#   H [label='Use s=max(sx,sy)'];
#   I [label='Use sx,sy independently'];
#   J [shape=diamond, label='Resolve center', fillcolor='#eef3ff'];
#   K [label='sf/sfc → centroid'];
#   L [label='numeric + CRS → transform'];
#   M [label='numeric only → lon/lat heuristic'];
#   N [label='normalized_center → (mx,my)+center*scale'];
#   O [label='Normalize to unit space'];
#   P [label='Apply fisheye'];
#   Q [label='Denormalize'];
#   R [label='Restore CRS'];
# 
#   A -> B;
#   B -> C [label='yes'];
#   B -> D [label='no'];
#   C -> E;
#   D -> E;
#   E -> F;
#   F -> G;
#   G -> H [label='yes'];
#   G -> I [label='no'];
#   H -> J;
#   I -> J;
#   J -> K;
#   J -> L;
#   J -> M;
#   J -> N;
#   K -> O;
#   L -> O;
#   M -> O;
#   N -> O;
#   O -> P;
#   P -> Q;
#   Q -> R;
# }
# ")
# 
# diagram |>
#   export_svg() |>
#   charToRaw() |>
#   rsvg::rsvg_png("diagram.png")


<<<<<<< HEAD
## ----norm-diagram-gg, fig.cap="Diagram of the normalization step", eval=knitr::is_latex_output(), fig.align="center", out.width="80%"----
# knitr::include_graphics("diagram.png")


## ----basic-example----------------------------------------------------------------------------
=======
## ----norm-diagram-gg, fig.cap="Workflow diagram of the normalization and CRS handling pipeline. The flowchart depicts CRS selection, normalization, center resolution, fisheye application, and CRS restoration. This highlights the staged design ensuring projection awareness and parameter stability.", eval=knitr::is_latex_output(), fig.align="center", out.width="80%"----
knitr::include_graphics("diagram.png")


## ----basic-example, fig.cap="Basic numeric example of an FGC fisheye transformation. A synthetic grid is shown before and after warping. The example isolates the core radial mapping independent of spatial geometry reconstruction.", fig.align="center", out.width="80%"----
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
original <- create_test_grid()
fisheye_org <- fisheye_fgc(original, r_in = 0.34, r_out = 0.55)
plot_fisheye_fgc(original, fisheye_org)


## ----fisheye-attributes-----------------------------------------------------------------------
fisheye_org |> head()
fisheye_org |> attributes() |> names()


## ----bench-data, eval=FALSE-------------------------------------------------------------------
# library(microbenchmark)
# 
# gen_xy <- function(n) matrix(runif(2L*n, -1, 1), ncol = 2)
# 
# bench_core <- function(ns = c(1e3, 2e3, 5e3, 1e4, 2e4, 5e4, 1e5), times = 10L) {
#   r_in <- 0.34; r_out <- 0.55
#   res <- lapply(ns, function(n) {
#     xy <- gen_xy(n)
#     invisible(fisheye_fgc(xy, r_in = r_in, r_out = r_out, zoom_factor = 1.6, squeeze_factor = 0.35)) # warm-up
#     b <- microbenchmark(
#       fisheye_fgc(xy, r_in = r_in, r_out = r_out, zoom_factor = 1.6, squeeze_factor = 0.35),
#       times = times, unit = "ms"
#     )
#     data.frame(
#       n = n,
#       median_ms = median(b$time),
#       per_vertex_ns = 1e6 * median(b$time) / n
#     )
#   })
#   df <- bind_rows(res) |>
#     mutate(logn = log(n), logt = log(median_ms))
#   slope <- coef(lm(logt ~ logn, data = df))[2]
#   list(data = df, slope = slope)
# }
# 
# core <- bench_core()
# 
# # sf_fisheye end-to-end (vertex-count scaling)
# 
# library(sf)
# 
# nverts <- function(sfc_or_sf) nrow(sf::st_coordinates(sfc_or_sf))
# 
# gen_points_sf <- function(n, crs = 3857) {
#   st_as_sf(data.frame(x = runif(n, -1, 1)*1e4, y = runif(n, -1, 1)*1e4),
#             coords = c("x", "y"), crs = crs)
# }
# 
# bench_sf <- function(ns = c(5e3, 1e4, 2e4, 5e4), times = 5L) {
#   res <- lapply(ns, function(n) {
#     s <- gen_points_sf(n)
#     center <- st_centroid(st_union(s))
#     invisible(sf_fisheye(s, center = center, r_in = 0.35, r_out = 0.60, zoom = 1.8, squeeze = 0.35)) # warm-up
#     b <- microbenchmark(
#       sf_fisheye(s, center = center, r_in = 0.35, r_out = 0.60, zoom = 1.8, squeeze = 0.35),
#       times = times, unit = "ms"
#     )
#     data.frame(
#       vertices = nverts(s),
#       median_ms = median(b$time),
#       per_vertex_ns = 1e6 * median(b$time) / nverts(s)
#     )
#   })
#   df <- bind_rows(res) |>
#     mutate(logv = log(vertices), logt = log(median_ms))
#   slope <- coef(lm(logt ~ logv, data = df))[2]
#   list(data = df, slope = slope)
# }
# 
# sfb <- bench_sf()
# sf_data <- sfb$data
# save(sf_data, file = "data/sf_data.rda")
# save(core, file = "data/core.rda")


## ----bench-plot-data--------------------------------------------------------------------------
load("data/sf_data.rda")
load("data/core.rda")
plot_2 <- ggplot(sf_data, aes(vertices, median_ms)) +
  geom_point() + geom_line() +
  scale_x_log10() + scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed(ratio = 1)

plot_1 <- ggplot(core$data, aes(n, median_ms)) +
  geom_point() + geom_line() +
  scale_x_log10() + scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE)+
  coord_fixed(ratio = 1)


## ----bench-plot, fig.cap="Benchmark performance of fisheye transformations. Log-log plots show runtime scaling for fisheye_fgc() (numeric coordinates) and sf_fisheye() (sf geometries) against input size. Both exhibit near-linear scaling, indicating efficient per-vertex computation.", echo=FALSE----

plot_1 + plot_2


## ----prepare-table, echo=FALSE----------------------------------------------------------------
# Table 1: Coordinate transformation across zones for selected points
samples <- transform_df |>
  group_by(zone) |>
  slice_head(n = 3) |> 
  ungroup()
idx <- as.numeric(rownames(samples))

orig_samples <- grid_df |>
  slice(idx)

samples <- samples |>
  cbind(orig_samples) |>
  select(x, y, x_new, y_new, zone, r_orig, r_new)


## ----transformation-table, echo=FALSE---------------------------------------------------------
samples |>
  knitr::kable(
  caption = "Coordinate transformation across zones for selected points",
  digits = 3
  )


## ----multi-layer-example, eval=FALSE, echo=TRUE, caption = "Handling of multi-layer maps in sf_fisheye()"----
# # Multi-layer example
# bind <- dplyr::bind_rows(
#   object_1 |> dplyr::mutate(.layer="object_1"),
#   object_2 |> dplyr::mutate(.layer="object_2"))
# 
# bind_w <- sf_fisheye(
#   bind,
#   center = melb,
#   r_in = 0.34,
#   r_out = 0.55,
#   zoom = 1.8,
#   squeeze = 0.35)
# 
# object_1_transformed   <- bind_w |>
#   dplyr::filter(.layer == "object_1") |>
#   dplyr::select(-.layer)
# 
# object_2_transformed   <- bind_w |>
#   dplyr::filter(.layer == "object_2") |>
#   dplyr::select(-.layer)


## ----zoom-factor-data, echo=FALSE-------------------------------------------------------------

grid_zoom_base <- create_test_grid(range = c(-1, 1), spacing = 0.05)

grid_zoom_1 <- fisheye_fgc(grid_zoom_base, zoom_factor = 1.5)
grid_zoom_2 <- fisheye_fgc(grid_zoom_base, zoom_factor = 2)

zones_1 <- attr(grid_zoom_1, "zones")
zones_2 <- attr(grid_zoom_2, "zones")

grid_zoom_1 <- as_tibble(grid_zoom_1) |> mutate(zones = zones_1)
grid_zoom_2 <- as_tibble(grid_zoom_2) |> mutate(zones = zones_2)


## ----zoom-factor-plot, echo=FALSE, fig.cap="ZEffect of zoom factor on fisheye distortion. Two panels compare zoom factors 1.5 and 2 applied to a synthetic grid, with points coloured by zone. Higher zoom increases magnification in the focus while preserving context stability.", out.width="80%"----
zone_scale <- scale_color_discrete(name = "Zone")

plot_3 <- ggplot(grid_zoom_1) +
  geom_point(aes(x = x_new, y = y_new, color = zones),
             size = 0.6, alpha = 0.8) +
  zone_scale +
  coord_fixed() +
  ggtitle("Zoom factor: 1.5") +
  theme_map()

plot_4 <- ggplot(grid_zoom_2) +
  geom_point(aes(x = x_new, y = y_new, color = zones),
             size = 0.6, alpha = 0.8) +
  zone_scale +
  coord_fixed() +
  ggtitle("Zoom factor: 2") +
  theme_map()

(plot_3 + plot_4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")


## ----prepare-revo-data, echo=FALSE------------------------------------------------------------
test_grid <- create_test_grid()
transform_grid_1 <- fisheye_fgc(test_grid, zoom_factor = 1.7)
transform_grid_2 <- fisheye_fgc(test_grid, zoom_factor = 1.7, revolution = pi/8)
zones_1 <- attr(transform_grid_1,"zones")
zones_2 <- attr(transform_grid_2,"zones")
transform_grid_1 <- as_tibble(transform_grid_1)
transform_grid_2 <- as_tibble(transform_grid_2)
transform_grid_1$zones <- zones_1
transform_grid_2$zones <- zones_2

transform_grid_1 <- transform_grid_1 %>%
  mutate(seg = ceiling(row_number() / 21))
transform_grid_2 <- transform_grid_2 %>%
  mutate(seg = ceiling(row_number() / 21))


<<<<<<< HEAD
## ----echo=FALSE, fig.cap="Revolution effect", out.width="80%"---------------------------------
=======
## ----echo=FALSE, fig.cap="Effect of angular revolution on line geometries. Line paths are shown with revolution set to 0 and pi/8, coloured by zone. Introducing revolution produces a visible rotational flow in the glue region without affecting the focus or context radii.", out.width="80%"----
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
zone_scale <- scale_color_discrete(name = "Zone")

plot_3 <- ggplot(transform_grid_1, aes(x_new, y_new)) +
  geom_path(aes(group = seg, colour = zones), size = 1) +
  coord_equal() +
  zone_scale +
  labs(title = "Revolution: 0") +
  theme_void()

plot_4 <- ggplot(transform_grid_2, aes(x_new, y_new)) +
  geom_path(aes(group = seg, colour = zones), size = 1) +
  labs(title = "Revolution: pi/8") +
  coord_equal() +
  zone_scale +
  theme_void()

plot_3 + plot_4 +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "right")


<<<<<<< HEAD
## ----prepare-poly-data, echo=FALSE------------------------------------------------------------
library(dplyr)

add_blocks <- function(df, ncol_grid = 21, cell_size = 2) {
  df %>%
    mutate(
      idx = row_number(),
      row = (idx - 1) %/% ncol_grid + 1,
      col = (idx - 1) %%  ncol_grid + 1,
      brow = (row - 1) %/% cell_size,
      bcol = (col - 1) %/% cell_size,
      block = brow * ceiling(ncol_grid / cell_size) + bcol + 1,
      # position inside the 2x2 cell:
      r_in = (row - 1) %% cell_size,
      c_in = (col - 1) %% cell_size,
      # order corners: TL -> TR -> BR -> BL -> TL
      corner = case_when(
        r_in == 0 & c_in == 0 ~ 1L,  # top-left
        r_in == 0 & c_in == 1 ~ 2L,  # top-right
        r_in == 1 & c_in == 1 ~ 3L,  # bottom-right
        r_in == 1 & c_in == 0 ~ 4L   # bottom-left
      )
    )
}

transform_grid_1b <- add_blocks(transform_grid_1, 21, 2)
transform_grid_2b <- add_blocks(transform_grid_2, 21, 2)

grid1_poly <- transform_grid_1b %>%
  arrange(block, corner)

grid2_poly <- transform_grid_2b %>%
  arrange(block, corner)


## ----poly-plot, echo=FALSE, fig.cap="Revolution effect with polygon", out.width="80%"---------
zone_scale <- scale_color_discrete(name = "Zone")

plot_3 <- ggplot(grid1_poly, aes(x_new, y_new)) +
  geom_polygon(
    aes(group = block, colour = zones),
    fill = NA,        # outline only
    linewidth = 0.6
  ) +
  coord_equal() +
  zone_scale +
  labs(title = "Revolution: 0") +
  theme_minimal()

plot_4 <- ggplot(grid2_poly, aes(x_new, y_new)) +
  geom_polygon(
    aes(group = block, colour = zones),
    fill = NA,
    linewidth = 0.6
  ) +
  coord_equal() +
  zone_scale +
  labs(title = "Revolution: pi/8") +
  theme_minimal()

plot_3 + plot_4 +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "right")


## ---------------------------------------------------------------------------------------------
=======
## ----glue-method, fig.cap="Comparison of glue compression methods. Polygon grids are shown under expand and outward glue modes. The outward method concentrates distortion closer to the focus, while expand distributes compression symmetrically across the glue.", out.width="80%"----
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
transform_grid_1 <- fisheye_fgc(test_grid, zoom_factor = 1.7, method = "expand")
transform_grid_2 <- fisheye_fgc(test_grid, zoom_factor = 1.7, method = "outward")
zones_1 <- attr(transform_grid_1,"zones")
zones_2 <- attr(transform_grid_2,"zones")
transform_grid_1 <- as_tibble(transform_grid_1)
transform_grid_2 <- as_tibble(transform_grid_2)
transform_grid_1$zones <- zones_1
transform_grid_2$zones <- zones_2

zone_scale <- scale_color_discrete(name = "zones")

plot_3 <- ggplot(transform_grid_1, aes(x_new, y_new, color = zones)) +
  geom_point() +
  coord_equal() +
  zone_scale +
  labs(title = "Method: expand") +
  theme_void()

plot_4 <- ggplot(transform_grid_2, aes(x_new, y_new, color = zones)) +
  geom_point() +
  coord_equal() +
  zone_scale +
  labs(title = "Method: outward") +
  theme_void()


plot_3 + plot_4 +
  plot_layout(guides = "collect", axes = "collect") &
  theme(legend.position = "right")


<<<<<<< HEAD
## ----preparing-hosp-data----------------------------------------------------------------------
=======
## ----shiny-screenshot, fig.cap= "Interactive Focus-Glue-Context Shiny application. Users control lens centre, radii, zoom, and compression via sliders and drag the lens directly on the map, with points, lines, and polygons warped together in real time. The app enables rapid exploration of fisheye parameters before committing to static figures.", out.width="80%"----

knitr::include_graphics("figures/screenshot.jpeg")


## ----preparing-hosp-data------------------------------------------------------

metro_names <- c("MELBOURNE", "PORT PHILLIP", "STONNINGTON", "YARRA", "MARIBYRNONG", "MOONEE VALLEY", "BOROONDARA", "GLEN EIRA", "BAYSIDE")

metro_center <- st_union(vic[vic$LGA_NAME %in% metro_names, ]) |> st_centroid() |> st_transform(st_crs(4326))
vic <- vic |> st_transform(st_crs(4326))
vic <- vic |> mutate(distance = as.numeric(st_distance(metro_center, vic)))

conn_fish <- st_transform(conn_fish, 4326)
metro_poly <- vic %>%
  arrange(distance) %>% head(10) %>%
  st_union()

outer_poly <- vic %>%
  arrange(desc(distance)) %>% head(50) %>%
  st_union()

conn_pts <- conn_fish |> st_drop_geometry() |>
  select(long_racf, lat_racf, long_hosp, lat_hosp) 

hosp_pts <- st_as_sf(conn_pts, coords = c("long_hosp","lat_hosp"), crs = 4326, remove = FALSE)
racf_pts <- st_as_sf(conn_pts, coords = c("long_racf","lat_racf"), crs = 4326, remove = FALSE)

in_metro_h <- st_within(hosp_pts, metro_poly, sparse = FALSE)[,1]
in_metro_r <- st_within(racf_pts, metro_poly, sparse = FALSE)[,1]

in_outer_h <- st_within(hosp_pts, outer_poly, sparse = FALSE)[,1]
in_outer_r <- st_within(racf_pts, outer_poly, sparse = FALSE)[,1]

conn_metro <- conn_fish %>% filter(in_metro_h & in_metro_r)
conn_outer <- conn_fish %>% filter(in_outer_h & in_outer_r)

>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
set.seed(1092)
conn_metro_10 <- conn_metro %>% slice_sample(n = 5)
conn_outer_10 <- conn_outer %>% slice_sample(n = 5)

hosp_pts <- bind_rows(conn_metro_10, conn_outer_10) |> st_drop_geometry() |> st_as_sf(coords = c("long_hosp","lat_hosp"), crs = 4326, remove = FALSE)
racf_pts <- bind_rows(conn_metro_10, conn_outer_10) |> st_drop_geometry() |> st_as_sf(coords = c("long_racf","lat_racf"), crs = 4326, remove = FALSE)


## ----hospitals-point-plot, fig.cap="Standard maps of hospitals and RACFs in Victoria. Hospitals (red) and residential aged care facilities (blue) are plotted separately over a grey basemap. The figure shows their true spatial distribution prior to any fisheye distortion."----
# Standard plot
plot_hosp <- ggplot(vic) + 
  geom_sf(fill = "grey90") +
  geom_sf(data = hosp_pts, color = "red", size = 0.3, alpha = 1) +
  ggtitle("Melbourne hospitals") +
  theme(
  plot.title = element_text(size = 8) # adjust here
  ) +
  theme_map() 
plot_racf <- ggplot(vic) + 
  geom_sf(fill = "grey90") +
  geom_sf(data = racf_pts, color = "blue", size = 0.3, alpha = 1) +
  ggtitle("Melbourne RACFs") +
  theme(
  plot.title = element_text(size = 8) # adjust here
  ) +
  theme_map()

plot_hosp + plot_racf


<<<<<<< HEAD
## ----preparing-hosp-data-2--------------------------------------------------------------------
metro_names <- c("MELBOURNE", "PORT PHILLIP", "STONNINGTON", "YARRA", "MARIBYRNONG", "MOONEE VALLEY", "BOROONDARA", "GLEN EIRA", "BAYSIDE")

center_metro <- vic |> filter(LGA_NAME %in% metro_names) |>
  st_union() |>
  st_centroid()

transfers <- conn_sample |>
  mutate(
    geometry = pmap(
      list(long_racf, long_hosp, lat_racf, lat_hosp),
      ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = FALSE))
    ),
    transfer_n = sample(20:80, n(), replace = TRUE)
  ) |>
  st_as_sf(crs = 4326) |>
  st_transform(st_crs(vic))


## ----hospitals-basic-plot, fig.cap="Standard Victorian map with sampled hospitals (red), RACFs (blue), and simulated transfer lines on a grey background."----
=======
## ----hospitals-basic-plot, fig.cap="Standard Victorian map of a sampled hospital-RACF network. Points represent hospitals and RACFs, and lines indicate simulated transfer counts. At state scale, metropolitan structure is visually compressed and difficult to discern."----
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
ggplot() +
  geom_sf(data = vic, fill = "grey65", color = "white", linewidth = 0.5) +
  geom_sf(data = conn_metro_10, color = "black", linewidth = 0.3) +
  geom_sf(data = conn_outer_10, color = "black", linewidth = 0.3) +
  scale_size(range = c(0.2, 1.2), guide = "none") +
  geom_sf(data = hosp_pts, color = "red", size = 0.5,) +
  geom_sf(data = racf_pts, color = "blue", size = 0.5) +
  coord_sf() +
  ggtitle("Victorian hospital-RACF network (sampled)") +
  theme_map() +
  theme(panel.background = element_rect(fill = "white", color = NA))


<<<<<<< HEAD
## ----fisheye-preparation, echo = FALSE--------------------------------------------------------
hosp_point2 <- hosp_point |>
=======
## ----fisheye-preparation, echo = FALSE----------------------------------------
hosp_point2 <- hosp_pts |>
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73
  mutate(type = "hospital") |>
  rename(id = destination)

racf_point2 <- racf_pts |>
  mutate(type = "racf") |>
  rename(id = source)

total_trans <- bind_rows(conn_metro_10, conn_outer_10) |> st_transform(st_crs(vic))
hosp_point2 <- hosp_point2 |> st_transform(st_crs(vic))
racf_point2 <- racf_point2 |> st_transform(st_crs(vic))
all_points <- bind_rows(hosp_point2, racf_point2)
bind <- dplyr::bind_rows(
  vic |> dplyr::mutate(.layer="vic"),
  all_points |> dplyr::mutate(.layer="pts"),
  total_trans |> dplyr::mutate(.layer="transfers"))
bind_w <- sf_fisheye(bind, center = metro_center, r_in = 0.2, r_out = 0.3, zoom = 7, squeeze = 0.35)
vic_w   <- bind_w |> dplyr::filter(.layer == "vic") |> dplyr::select(-.layer)
pts_w   <- bind_w |> dplyr::filter(.layer == "pts") |> dplyr::select(-.layer)
transfers_w <- bind_w |> dplyr::filter(.layer == "transfers") |> dplyr::select(-.layer)


<<<<<<< HEAD
## ----fisheye-plot, fig.cap="Fisheye view magnifies greater Melbourne while keeping statewide context; lines, points, and polygons stay aligned.", eval=knitr::is_latex_output()----
# focus <- ggplot() +
#   geom_sf(data = vic_w, fill = "grey92", color = "grey65") +
#   geom_sf(data = transfers_w, aes(alpha = transfer_n), color = "grey40") +
#   geom_sf(data = pts_w, aes(color = type), size = 1) +
#   scale_size(range = c(0.2, 1.4), guide = "none") +
#   ggtitle("Fisheye on greater Melbourne") +
#   theme_map() +
#   theme(panel.background = element_rect(fill = "grey98", color = NA))
# 
# overview <- ggplot() +
#   geom_sf(data = vic, fill = NA, color = "grey40", linewidth = 0.2) +
#   geom_sf(data = transfers, aes(size = transfer_n), color = "grey50", alpha = 0.45, linewidth = 0.3) +
#   scale_size(range = c(0.2, 1.2), guide = "none") +
#   geom_sf(data = hosp_point, color = "red", size = 0.3, alpha = 1) +
#   geom_sf(data = racf_point, color = "blue", size = 0.3, alpha = 1) +
#   theme_void() +
#   ggtitle("Original size")
# 
# cowplot::ggdraw(focus) +
#   cowplot::draw_plot(overview, x = 0.62, y = 0.62, width = 0.33, height = 0.33, scale = 1)


## ----fisheye-plotly, cho = TRUE, out.width="100%", fig.width = 6, fig.height=5, layout="l-body", fig.cap="A basic interactive fisheye plot into Greater Melbourne region, with points, lines, and polygons aligned.", eval=knitr::is_html_output(), fig.alt = "A fisheye view of Greater Melbourne, with points, lines, and polygons aligned."----
focus2 <- ggplot() +
  geom_sf(data = vic_w, fill = "grey92", color = "grey65") +
  geom_sf(data = transfers_w, aes(linewidth = transfer_n), color = "grey40", alpha = 0.6) +
  scale_linewidth(range = c(0.1, 1.2), guide = "none") +
  geom_sf(data = pts_w, aes(color = type), size = 1) +
  ggtitle("Fisheye on Greater Melbourne") +
  theme_map() 

ggplotly(focus2) |>
  layout(hovermode = "closest")
=======
## ----fisheye-plot-prepare-----------------------------------------------------
focus <- ggplot() +
  geom_sf(data = vic_w, fill = "grey65", color = "white") +
  geom_sf(data = transfers_w, color = "black", size = 0.3) +
  geom_sf(data = pts_w, aes(color = type), size = 1) +
  scale_color_manual(values = c("hospital" = "red", "racf" = "blue")) +
  ggtitle("Fisheye on greater Melbourne") +
  theme_map() +
  theme(panel.background = element_rect(fill = "grey98", color = NA))

overview <- ggplot() +
  geom_sf(data = vic, fill = NA, color = "grey", linewidth = 0.5) +
  geom_sf(data = conn_metro_10, color = "black", linewidth = 0.3) +
  geom_sf(data = conn_outer_10, color = "black", linewidth = 0.3) +
  scale_size(range = c(0.2, 1.2), guide = "none") +
  geom_sf(data = hosp_pts, color = "red", size = 0.5,) +
  geom_sf(data = racf_pts, color = "blue", size = 0.5) +
  coord_sf() +
  theme_void() +
  ggtitle("Original size")

## ----fisheye-plot, fig.cap="Fisheye magnification of Greater Melbourne with statewide context. A fisheye lens enlarges metropolitan Melbourne while retaining Victoria's outline, with an inset showing the original scale. All layers remain aligned, revealing dense urban structure without losing context.", eval=knitr::is_latex_output()----
cowplot::ggdraw(focus) +
  cowplot::draw_plot(overview, x = 0.62, y = 0.62, width = 0.33, height = 0.33, scale = 1)


## ----fisheye-plotly, cho = TRUE, out.width="100%", fig.width = 6, fig.height=5, layout="l-body", fig.cap="Fisheye magnification of Greater Melbourne with statewide context. A fisheye lens enlarges metropolitan Melbourne while retaining Victoria's outline, with an inset showing the original scale. All layers remain aligned, revealing dense urban structure without losing context.", eval=knitr::is_html_output(), fig.alt = "A fisheye view of Greater Melbourne, with points, lines, and polygons aligned."----
# ggplotly(focus) |>
#   layout(hovermode = "closest",
#   xaxis = list(autorange = TRUE),
#   yaxis = list(autorange = TRUE))
>>>>>>> b8c8e2ec4ff4cb713f6212a0dd6a982f4f7f6d73

