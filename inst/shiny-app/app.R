dir.create("SWDSFODR/inst", showWarnings=FALSE)
dir.create("SWDSFODR/inst/shiny-app", showWarnings=FALSE)

shiny_app_R <- '
# app.R
# A Shiny app for SWDSFOD package

library(shiny)
library(ggplot2)
library(DT)
library(SWDSFODR)  # so we can use the exported functions

ui <- fluidPage(
  titlePanel("SWDSFOD Shiny App - Tool 04 (v08.1) FOD Model"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Approach"),
      radioButtons("approach", "Calculation Approach:",
                   choices=c("Annual"="annual","Monthly"="monthly","Simplified"="simplified")),
      hr(),
      h4("Parameters"),
      numericInput("phi", "phi_y (model correction factor)", 1, min=0, max=1, step=0.01),
      numericInput("f", "f_y (fraction of CH4 captured)", 0, min=0, max=1, step=0.01),
      numericInput("gwpslid", "GWP_CH4", 28, min=1, max=100),
      numericInput("ox", "OX (oxidation factor)", 0.1, min=0, max=1, step=0.01),
      numericInput("Fval", "F (fraction CH4 in gas)", 0.5, min=0, max=1, step=0.1),
      numericInput("docf", "DOCf (decomposable fraction)", 0.5, min=0, max=1, step=0.1),
      numericInput("mcf", "MCF (methane correction factor)", 1, min=0, max=1, step=0.1),
      numericInput("yearMonthTarget", "Year or Month Target", 5, min=1, step=1),
      selectInput("climate_zone", "Climate Zone (Simplified Approach)", 
                  choices=c("tropical_wet","tropical_dry","boreal_temperate_wet","boreal_temperate_dry"),
                  selected="tropical_wet"),
      fileInput("file1", "Upload Waste Data (.csv)", accept=".csv"),
      helpText("For annual approach, columns: year,waste_type,mass_tonnes.
                For monthly approach, columns: month,waste_type,mass_tonnes.
                For simplified approach, columns: year,mass_tonnes."),
      actionButton("calcBtn", "Calculate Emissions!")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results Table", DTOutput("emissionsTable")),
        tabPanel("Plot", plotOutput("emissionsPlot"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive expression to load data
  wasteData <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors=FALSE)
    df
  })

  # Reactive expression to compute emissions
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

    if(approach=="annual") {
      # example usage: we need DOCj, k_j. Hardcode sample?
      # Real usage: user would provide it. We'll guess for a demo:
      DOCj <- c(food=0.15, paper=0.4, other=0.2)
      k_j  <- c(food=0.06, paper=0.04, other=0.05)

      # run function
      res_val <- calcSWDSEmissionsYearly(
        W = df, DOCj = DOCj, k_j = k_j,
        phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
        OX = ox_val, F = F_, DOCf_y = docf,
        MCF_y = mcf, year_target = target
      )
      data.frame(Emissions_tCO2e = res_val)
    } else if(approach=="monthly") {
      DOCj <- c(food=0.15, paper=0.4, other=0.2)
      k_j  <- c(food=0.06, paper=0.04, other=0.05)
      res_val <- calcSWDSEmissionsMonthly(
        W = df, DOCj = DOCj, k_j = k_j,
        phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
        OX = ox_val, F = F_, DOCf_m = docf,
        MCF_y = mcf, month_target = target
      )
      data.frame(Emissions_tCO2e = res_val)
    } else {
      # simplified
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

  output$emissionsTable <- renderDT({
    req(emissionsResult())
    datatable(emissionsResult(), options=list(pageLength=5))
  })

  output$emissionsPlot <- renderPlot({
    req(emissionsResult())
    df <- emissionsResult()
    val <- df$Emissions_tCO2e[1]
    approach <- input$approach
    # We'll just plot a bar or single column if only one value
    # Could expand for multi-year plotting if user wants to do more advanced logic.
    ggplot(data=data.frame(Approach=approach, Emissions=val), aes(x=Approach, y=Emissions)) +
      geom_col(fill="steelblue") +
      theme_minimal() +
      labs(title="Estimated CH4 Emissions (t CO2e)", x="", y="t CO2e")
  })
}

shinyApp(ui, server)
'

writeLines(shiny_app_R, "SWDSFOD/inst/shiny-app/app.R")
