library(shiny)
library(tidyverse)
library(sf)
library(ggthemes)
library(plotly)
library(mapycusmaximus)

# Load data once for responsiveness
vic <- mapycusmaximus::vic
conn <- mapycusmaximus::conn_fish

# Helper to prepare sample hospitals/RACFs and connections
prepare_network <- function(n_hosp = 10, n_racf = 10) {
  conn_sample <- conn |>
    st_drop_geometry() |>
    select(source, destination, long_racf, lat_racf, long_hosp, lat_hosp) |>
    distinct() |>
    slice_sample(n = max(n_hosp, n_racf)) |>
    mutate(transfer_n = sample(20:80, n(), replace = TRUE))

  hospitals <- conn_sample |>
    select(destination, long_hosp, lat_hosp) |>
    distinct() |>
    slice_head(n = n_hosp) |>
    st_as_sf(coords = c("long_hosp", "lat_hosp"), crs = 4326) |>
    st_transform(st_crs(vic)) |>
    mutate(type = "hospital")

  racfs <- conn_sample |>
    select(source, long_racf, lat_racf) |>
    distinct() |>
    slice_head(n = n_racf) |>
    st_as_sf(coords = c("long_racf", "lat_racf"), crs = 4326) |>
    st_transform(st_crs(vic)) |>
    mutate(type = "racf")

  transfers <- conn_sample |>
    mutate(
      geometry = pmap(
        list(long_racf, long_hosp, lat_racf, lat_hosp),
        ~ st_linestring(matrix(c(..1, ..2, ..3, ..4), ncol = 2, byrow = FALSE))
      )
    ) |>
    st_as_sf(crs = 4326) |>
    st_transform(st_crs(vic))

  list(hospitals = hospitals, racfs = racfs, transfers = transfers)
}

ui <- fluidPage(
  titlePanel("Focus–Glue–Context lens explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("centre", "Focus centre (LGA)", choices = sort(unique(vic$LGA_NAME)), selected = "MELBOURNE"),
      sliderInput("r_in", "Inner radius (focus)", min = 0.1, max = 0.5, value = 0.33, step = 0.01),
      sliderInput("r_out", "Outer radius (glue)", min = 0.4, max = 0.9, value = 0.6, step = 0.01),
      sliderInput("zoom", "Zoom factor", min = 1, max = 25, value = 12, step = 1),
      sliderInput("squeeze", "Squeeze", min = 0.1, max = 0.9, value = 0.35, step = 0.01),
      sliderInput("n_fac", "Sample size per layer", min = 5, max = 40, value = 10, step = 1),
      checkboxInput("show_lines", "Show transfer lines", value = TRUE),
      checkboxInput("use_plotly", "Interactive plotly view", value = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Fisheye", plotOutput("fisheye_plot"), plotlyOutput("fisheye_plotly", height = "600px")),
        tabPanel("Original", plotOutput("original_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  centre_geom <- reactive({
    req(input$centre)
    vic |> filter(LGA_NAME == input$centre)
  })

  sampled_layers <- reactive({
    prepare_network(n_hosp = input$n_fac, n_racf = input$n_fac)
  })

  lens_args <- reactive({
    list(
      center = centre_geom(),
      r_in = input$r_in,
      r_out = input$r_out,
      zoom = input$zoom,
      squeeze = input$squeeze,
      normalized_center = TRUE
    )
  })

  transformed <- reactive({
    layers <- sampled_layers()
    args <- lens_args()
    list(
      vic = do.call(sf_fisheye, c(list(vic), args)),
      hospitals = do.call(sf_fisheye, c(list(layers$hospitals), args)),
      racfs = do.call(sf_fisheye, c(list(layers$racfs), args)),
      transfers = do.call(sf_fisheye, c(list(layers$transfers), args))
    )
  })

  fisheye_gg <- reactive({
    layers <- transformed()
    metro <- layers$vic |> filter(LGA_NAME %in% c("MELBOURNE", "PORT PHILLIP", "STONNINGTON", "YARRA", "MARIBYRNONG", "MOONEE VALLEY", "BOROONDARA", "GLEN EIRA", "BAYSIDE"))
    ggplot() +
      geom_sf(data = layers$vic, fill = "grey92", color = "grey65", linewidth = 0.25) +
      { if (isTRUE(input$show_lines)) geom_sf(data = layers$transfers, aes(size = transfer_n), color = "grey40", alpha = 0.45) } +
      geom_sf(data = layers$racfs, color = "#2c7fb8", size = 1.2, alpha = 0.9) +
      geom_sf(data = layers$hospitals, color = "#d7191c", size = 1.4, alpha = 0.9) +
      geom_sf(data = metro, fill = NA, color = "black", linewidth = 0.4, linetype = "dashed") +
      geom_sf_label(data = centre_geom(), aes(label = input$centre), size = 3) +
      scale_size(range = c(0.2, 1.4), guide = "none") +
      ggtitle("Fisheye view") +
      theme_map() +
      theme(panel.background = element_rect(fill = "grey98", color = NA))
  })

  output$original_plot <- renderPlot({
    layers <- sampled_layers()
    ggplot() +
      geom_sf(data = vic, fill = "grey95", color = "grey70", linewidth = 0.2) +
      { if (isTRUE(input$show_lines)) geom_sf(data = layers$transfers, aes(size = transfer_n), color = "grey50", alpha = 0.45, linewidth = 0.3) } +
      geom_sf(data = layers$racfs, color = "#2c7fb8", size = 1, alpha = 0.9) +
      geom_sf(data = layers$hospitals, color = "#d7191c", size = 1.2, alpha = 0.9) +
      scale_size(range = c(0.2, 1.2), guide = "none") +
      ggtitle("Original Victoria") +
      theme_map() +
      theme(panel.background = element_rect(fill = "grey98", color = NA))
  }, res = 110)

  output$fisheye_plot <- renderPlot({
    fisheye_gg()
  }, res = 110)

  output$fisheye_plotly <- renderPlotly({
    req(input$use_plotly)
    ggplotly(fisheye_gg())
  })
}

shinyApp(ui, server)
