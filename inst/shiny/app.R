# Init ------------------------------------------------------------

library(shiny)
library(pfs)
library(ggplot2)

# Functions -------------------------------------------------------

# GGplot Theme for PFS Shiny App
PFSGGplotTheme <-
  function (
    size = 8,
    family = 'sans',
    scaler = 1,
    axis = 'x',
    panel_border = FALSE,
    background_color = 'grey95',
    grid = 'y',
    minor_grid = '',
    show_legend = TRUE,
    ar = NA,
    axis_title_just = 'rt',
    axis_ticks = TRUE
  ) {

    size_med = size*scaler
    size_sml = round(size*0.7)*scaler
    base_linesize = 0.3*scaler

    # justification of axis titles
    xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0,
                 l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
    yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0,
                 l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

    list(
      theme_minimal(base_size = size_med, base_family = family),
      theme(
        # basic
        text = element_text(color = 'black'),
        line = element_line(linewidth = base_linesize, lineend = 'square'),
        # axis
        axis.title = element_text(size = size_med, face = 'bold'),
        axis.title.x = element_text(hjust = xj),
        axis.title.y = element_text(hjust = yj),
        axis.title.y.right = element_text(hjust = yj, angle = 90),
        axis.text = element_text(size = size_med, color = 'black'),
        # strips
        strip.text = element_text(color = 'black', size = size_med),
        strip.background = element_blank(),
        # plot
        title = element_text(face = 'bold'),
        plot.subtitle = element_text(color = 'black', size = size_med, face = 'bold'),
        plot.caption = element_text(color = 'black', size = size_sml, face = 'plain'),
        plot.background = element_blank(),
        panel.background = element_rect(fill = background_color, colour = NA),
        #plot.margin = unit(c(1, 0.1, 0.5, 0.5), units = 'mm'),
        # grid
        panel.grid = element_blank()
      ),
      if (isTRUE(axis_ticks)) {
        theme(axis.ticks = element_line(linewidth = rel(0.5), color = 'black'))
      },
      if (identical(grid, 'y')) {
        theme(panel.grid.major.y =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(grid, 'x')) {
        theme(panel.grid.major.x =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(grid, 'xy') | identical(grid, 'yx')) {
        theme(panel.grid.major.y =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'),
              panel.grid.major.x =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(minor_grid, 'y')) {
        theme(panel.grid.minor.y =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(minor_grid, 'x')) {
        theme(panel.grid.minor.x =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (identical(minor_grid, 'xy') | identical(grid, 'yx')) {
        theme(panel.grid.minor.y =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'),
              panel.grid.minor.x =
                element_line(linewidth = base_linesize, linetype = 3, color = 'grey80'))
      },
      if (isTRUE(panel_border)) {
        theme(
          panel.border =
            element_rect(fill = NA)
        )
      },
      if (!isTRUE(show_legend)) {
        theme(legend.position = 'none')
      },
      if (axis == 'x') {
        theme(
          axis.line.x = element_line(linetype = 1, color = 'black')
        )
      },
      if (axis == 'y') {
        theme(
          axis.line.y = element_line(linetype = 1, color = 'black')
        )
      },
      if (axis == 'xy') {
        theme(
          axis.line = element_line(linetype = 1, color = 'black')
        )
      },
      if (!is.na(ar)) {
        theme(
          aspect.ratio = ar
        )
      }
    )
  }

# Data ------------------------------------------------------------

cnst <- list(
  forecast_horizon = 30,
  # starting ages of age groups
  ages = c(`15-20` = 15, `20-25` = 20, `25-30` = 25,
           `30-35` = 30, `35-40` = 35, `40-45` = 40, `45+` = 45),
  # width of last age group
  wlast = 5,
  # initial ASFRs
  jumpoff_asfrs = c(0.0031, 0.0269, 0.0671, 0.0888,
                    0.0512, 0.0134, 0.0012),
  nsim = 150
)

# UI --------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Fertility Model Inputs"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("target_tfr", "Target TFR:",
                  min = 1.2, max = 2.1, value = 1.51, step = 0.01),
      sliderInput("target_mab", "Target mean age at birth:",
                  min = 25, max = 35, value = 32.9, step = 0.1),
      sliderInput("asfr_growth_rate", "Pace of fertility change:",
                  min = 0.0, max = 2.0, value = 0.3, step = 0.01),
      sliderInput("timestep_of_steepest_growth", "Timing of steepest growth:",
                  min = 2, max = 50, value = 5, step = 1),
      actionButton("reset", "Reset to MPIDR forecast")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("TFR/ASFRs",
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"),
                               plotOutput("tfrPlot"),
                               plotOutput("asfrPlot"))
                 )
        ),
        tabPanel("Table",
                 tableOutput("valuesTable")
        )
      )
    )
  )
)

# Server ----------------------------------------------------------

server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateSliderInput(session, "target_tfr", value = 1.51)
    updateSliderInput(session, "target_mab", value = 32.9)
    updateSliderInput(session, "asfr_growth_rate", value = 0.3)
    updateSliderInput(session, "timestep_of_steepest_growth", value = 5)
  })

  predictions <- reactive({

    our_forecast_definition <- DefineForecast(
      jumpoff_asfrs = cnst$jumpoff_asfrs,
      forecast_horizon = cnst$forecast_horizon,
      ages = cnst$ages,
      wlast = cnst$wlast,
      target_tfr = input$target_tfr,
      target_mab = input$target_mab,
      asfr_growth_rate = input$asfr_growth_rate,
      timestep_of_steepest_growth = input$timestep_of_steepest_growth,
      randomness = 'finland1995-2024'
    )

    our_forecast <- MakeForecast(our_forecast_definition,
                                 nsim = cnst$nsim)

    list(asfr_paths = our_forecast$asfr_sim,
         mean_asfr_paths = our_forecast$asfr_central,
         tfr_paths = our_forecast$tfr_sim,
         mean_tfr_paths = our_forecast$tfr_central)

  })

  # Output for TFR tab: simply shows the Target TFR value.
  output$tfrPlot <- renderPlot({
    pred <- predictions()
    ggplot(pred$tfr_paths) +
      geom_line(aes(x = as.integer(h), y = tfr, group = sim),
                alpha = 0.2, linewidth = 0.7) +
      geom_line(aes(x = as.integer(h), y = tfr),
                data = pred$mean_tfr_paths, color = 'red', linewidth = 2) +
      coord_cartesian(ylim = c(1, 3)) +
      PFSGGplotTheme() +
      labs(x = 'Years into forecast', y = 'TFR')
  })

  # Output for ASFRs tab: shows both the Target mean age at birth and the Pace of fertility change.
  output$asfrPlot <- renderPlot({
    pred <- predictions()
    ggplot(pred$asfr_paths) +
      geom_line(aes(x = as.integer(h), y = asfr, group = sim),
                alpha = 0.2, linewidth = 0.5) +
      geom_line(aes(x = as.integer(h), y = asfr),
                data = pred$mean_asfr_paths, color = 'red', linewidth = 1) +
      facet_wrap(~age, scale = 'free_y') +
      PFSGGplotTheme() +
      labs(x = 'Years into forecast', y = 'ASFR')
  })

  # Output for Table tab: a table summarizing all the input values.
  output$valuesTable <- renderTable({
    pred <- predictions()
    tab <-
      data.frame(
        h = 1:cnst$forecast_horizon,
        tfr = pred$mean_tfr_paths$tfr
      )
    colnames(tab) <- c('Years into forecast', 'TFR')
    tab
  })
}

shinyApp(ui = ui, server = server)
