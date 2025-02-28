# app.R
# A Shiny app for the SWDSFODR package with an added area plot

library(shiny)
library(ggplot2)
library(DT)
library(SWDSFODR)  # Use the new package name

ui <- fluidPage(
  titlePanel("SWDSFODR Shiny App - Tool 04 (v08.1) FOD Model"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Approach"),
      radioButtons("approach", "Calculation Approach:",
                   choices = c("Annual" = "annual",
                               "Monthly" = "monthly",
                               "Simplified" = "simplified")),
      hr(),
      h4("Parameters"),
      numericInput("phi", "phi_y (model correction factor)", 1, min = 0, max = 1, step = 0.01),
      numericInput("f", "f_y (fraction of CH4 captured)", 0, min = 0, max = 1, step = 0.01),
      numericInput("gwpslid", "GWP_CH4", 28, min = 1, max = 100),
      numericInput("ox", "OX (oxidation factor)", 0.1, min = 0, max = 1, step = 0.01),
      numericInput("Fval", "F (fraction CH4 in gas)", 0.5, min = 0, max = 1, step = 0.1),
      numericInput("docf", "DOCf (decomposable fraction)", 0.5, min = 0, max = 1, step = 0.1),
      numericInput("mcf", "MCF (methane correction factor)", 1, min = 0, max = 1, step = 0.1),
      numericInput("yearMonthTarget", "Year or Month Target", 5, min = 1, step = 1),
      selectInput("climate_zone", "Climate Zone (Simplified Approach)", 
                  choices = c("tropical_wet", "tropical_dry",
                              "boreal_temperate_wet", "boreal_temperate_dry"),
                  selected = "tropical_wet"),
      fileInput("file1", "Upload Waste Data (.csv)", accept = ".csv"),
      helpText("For annual approach, columns: year, waste_type, mass_tonnes.
                For monthly approach, columns: month, waste_type, mass_tonnes.
                For simplified approach, columns: year, mass_tonnes."),
      actionButton("calcBtn", "Calculate Emissions!")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results Table", DTOutput("emissionsTable")),
        tabPanel("Bar Plot", plotOutput("emissionsPlot")),
        tabPanel("Area Plot", plotOutput("areaPlot"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Load the uploaded CSV data
  wasteData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })

  # Compute a single-value result (used in table and bar plot)
  emissionsResult <- eventReactive(input$calcBtn, {
    df <- wasteData()
    approach <- input$approach
    phi_val <- input$phi
    f_val   <- input$f
    gwp     <- input$gwpslid
    ox_val  <- input$ox
    F_      <- input$Fval
    docf    <- input$docf
    mcf     <- input$mcf
    target  <- input$yearMonthTarget
    cz      <- input$climate_zone

    if (approach == "annual") {
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
      res_val <- calcSWDSEmissionsYearly(
        W = df, DOCj = DOCj, k_j = k_j,
        phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
        OX = ox_val, F = F_, DOCf_y = docf,
        MCF_y = mcf, year_target = target
      )
      data.frame(Emissions_tCO2e = res_val)
    } else if (approach == "monthly") {
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
      res_val <- calcSWDSEmissionsMonthly(
        W = df, DOCj = DOCj, k_j = k_j,
        phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
        OX = ox_val, F = F_, DOCf_m = docf,
        MCF_y = mcf, month_target = target
      )
      data.frame(Emissions_tCO2e = res_val)
    } else {
      res_val <- calcSWDSEmissionsSimplified(
        W = df,
        phi_y = phi_val,
        f_y = f_val,
        GWP_CH4 = gwp,
        climate_zone = cz,
        year_target = target
      )
      data.frame(Emissions_tCO2e = res_val)
    }
  })

  # Compute a time series of emissions (cumulative and incremental)
  timeSeriesData <- eventReactive(input$calcBtn, {
    df <- wasteData()
    approach <- input$approach
    phi_val <- input$phi
    f_val   <- input$f
    gwp     <- input$gwpslid
    ox_val  <- input$ox
    F_      <- input$Fval
    docf    <- input$docf
    mcf     <- input$mcf
    cz      <- input$climate_zone

    if (approach %in% c("annual", "simplified")) {
      years <- seq(min(df$year), input$yearMonthTarget)
      if (approach == "annual") {
        DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
        k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
        cumulative <- sapply(years, function(y) {
          calcSWDSEmissionsYearly(W = df, DOCj = DOCj, k_j = k_j,
                                  phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
                                  OX = ox_val, F = F_, DOCf_y = docf,
                                  MCF_y = mcf, year_target = y)
        })
      } else {
        # Simplified approach using years column
        cumulative <- sapply(years, function(y) {
          calcSWDSEmissionsSimplified(W = df, phi_y = phi_val,
                                      f_y = f_val, GWP_CH4 = gwp,
                                      climate_zone = cz, year_target = y)
        })
      }
      incremental <- c(cumulative[1], diff(cumulative))
      ts_df <- data.frame(Time = years, Incremental = incremental, Cumulative = cumulative)
    } else if (approach == "monthly") {
      months <- seq(min(df$month), input$yearMonthTarget)
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
      cumulative <- sapply(months, function(m) {
        calcSWDSEmissionsMonthly(W = df, DOCj = DOCj, k_j = k_j,
                                 phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
                                 OX = ox_val, F = F_, DOCf_m = docf,
                                 MCF_y = mcf, month_target = m)
      })
      incremental <- c(cumulative[1], diff(cumulative))
      ts_df <- data.frame(Time = months, Incremental = incremental, Cumulative = cumulative)
    }
    ts_df
  })

  output$emissionsTable <- renderDT({
    req(emissionsResult())
    datatable(emissionsResult(), options = list(pageLength = 5))
  })

  output$emissionsPlot <- renderPlot({
    req(emissionsResult())
    # A simple bar plot of the single calculated emission value
    df_val <- emissionsResult()
    approach <- input$approach
    ggplot(data = data.frame(Approach = approach, Emissions = df_val$Emissions_tCO2e), 
           aes(x = Approach, y = Emissions)) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      labs(title = "Estimated CH4 Emissions (t CO2e)", x = "", y = "t CO2e")
  })

  output$areaPlot <- renderPlot({
    req(timeSeriesData())
    ts_df <- timeSeriesData()
    period_label <- ifelse(input$approach == "monthly", "Month", "Year")
    ggplot(ts_df, aes(x = Time)) +
      geom_area(aes(y = Incremental), fill = "lightblue", alpha = 0.7) +
      geom_line(aes(y = Cumulative), color = "red", size = 1.5) +
      labs(title = "Emissions Over Time",
           subtitle = paste("Incremental", period_label, "Emissions & Cumulative Emissions"),
           x = period_label,
           y = "Emissions (t CO2e)") +
      theme_minimal()
  })
}

shinyApp(ui, server)
