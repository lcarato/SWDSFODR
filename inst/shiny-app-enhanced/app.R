# app.R
# Enhanced Shiny app for the SWDSFODR package with multi-region support
# and waste estimation functionality

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(SWDSFODR)

ui <- fluidPage(
  titlePanel("SWDSFODR Tool 04 (v08.1) FOD Model with Multi-Region Support"),
  
  navlistPanel(
    id = "mainNav",
    
    # Tab 1: Waste Estimation
    tabPanel(
      "Waste Estimation", 
      value = "estimation",
      fluidRow(
        column(4,
               wellPanel(
                 h4("Demographic Data Import"),
                 radioButtons("demo_data_source", "Data Source:",
                             choices = c("Upload File" = "file",
                                        "Manual Entry" = "manual"),
                             selected = "file"),
                 conditionalPanel(
                   condition = "input.demo_data_source == 'file'",
                   fileInput("demo_file", "Upload Demographics (.csv)", accept = ".csv"),
                   checkboxInput("demo_has_header", "File has header", TRUE)
                 ),
                 conditionalPanel(
                   condition = "input.demo_data_source == 'manual'",
                   numericInput("num_regions", "Number of Regions", 1, min = 1, max = 10),
                   uiOutput("manual_regions_ui")
                 ),
                 hr(),
                 h4("Estimation Model"),
                 radioButtons("est_model", "Estimation Model:",
                             choices = c("Simple (Population-based)" = "simple",
                                        "Economic (GDP-based)" = "economic",
                                        "Comprehensive" = "comprehensive"),
                             selected = "simple"),
                 hr(),
                 h4("Model Parameters"),
                 sliderInput("base_gen_rate", "Base Generation Rate (tonnes/capita/year)", 
                            min = 0.1, max = 1.0, value = 0.3, step = 0.01),
                 conditionalPanel(
                   condition = "input.est_model != 'simple'",
                   sliderInput("gdp_factor", "GDP Influence Factor", 
                              min = 0.00001, max = 0.001, value = 0.0001, step = 0.00001),
                   sliderInput("gdp_elasticity", "GDP Elasticity", 
                              min = 0.1, max = 1.0, value = 0.5, step = 0.05)
                 ),
                 conditionalPanel(
                   condition = "input.est_model == 'comprehensive'",
                   sliderInput("urban_factor", "Urbanization Factor", 
                              min = 0.001, max = 0.01, value = 0.005, step = 0.001)
                 ),
                 hr(),
                 h4("Waste Composition"),
                 checkboxInput("use_default_comp", "Use Default Compositions", TRUE),
                 conditionalPanel(
                   condition = "!input.use_default_comp",
                   fileInput("comp_file", "Upload Compositions (.csv)", accept = ".csv")
                 ),
                 actionButton("estimate_btn", "Estimate Waste Generation", class = "btn-primary")
               )
        ),
        column(8,
               tabsetPanel(
                 tabPanel("Results Table", 
                          h4("Estimated Waste Generation"),
                          DTOutput("est_waste_table"),
                          hr(),
                          h4("Waste Composition Distribution"),
                          DTOutput("est_comp_table"),
                          downloadButton("download_est_waste", "Download Waste Estimates"),
                          downloadButton("download_est_comp", "Download Waste Composition")
                 ),
                 tabPanel("Visualization", 
                          h4("Waste Generation by Region"),
                          plotOutput("est_waste_plot", height = "400px"),
                          hr(),
                          h4("Waste Composition by Region"),
                          plotOutput("est_comp_plot", height = "400px")
                 ),
                 tabPanel("Projection", 
                          h4("Waste Projection Settings"),
                          fluidRow(
                            column(4,
                                  numericInput("proj_start_year", "Start Year", 2025, min = 2000, max = 2100),
                                  numericInput("proj_end_year", "End Year", 2035, min = 2000, max = 2100)
                            ),
                            column(4,
                                  selectInput("proj_model", "Growth Model", 
                                             choices = c("Linear" = "linear", 
                                                        "Exponential" = "exponential",
                                                        "Logistic" = "logistic")),
                                  numericInput("proj_growth_rate", "Annual Growth Rate", 0.02, min = -0.1, max = 0.2, step = 0.01)
                            ),
                            column(4,
                                  conditionalPanel(
                                    condition = "input.proj_model == 'logistic'",
                                    numericInput("proj_saturation", "Saturation Level (tonnes)", 1000000, min = 1000)
                                  ),
                                  checkboxInput("proj_by_type", "Project by Waste Type", TRUE),
                                  actionButton("run_projection", "Run Projection", class = "btn-primary")
                            )
                          ),
                          hr(),
                          h4("Waste Projection Results"),
                          plotOutput("proj_plot", height = "500px"),
                          hr(),
                          DTOutput("proj_table"),
                          downloadButton("download_projection", "Download Projection")
                 )
               )
        )
      )
    ),
    
    # Tab 2: Emissions Calculation
    tabPanel(
      "Emissions Calculation", 
      value = "emissions",
      sidebarLayout(
        sidebarPanel(
          h4("Select Approach"),
          radioButtons("approach", "Calculation Approach:",
                       choices = c("Annual" = "annual",
                                   "Monthly" = "monthly",
                                   "Simplified" = "simplified")),
          hr(),
          h4("Region Selection"),
          checkboxInput("use_multi_region", "Enable Multi-Region Mode", FALSE),
          conditionalPanel(
            condition = "input.use_multi_region",
            uiOutput("region_selector")
          ),
          hr(),
          h4("Global Parameters"),
          numericInput("gwpslid", "GWP_CH4", 28, min = 1, max = 100),
          hr(),
          conditionalPanel(
            condition = "!input.use_multi_region",
            h4("Parameters"),
            numericInput("phi", "phi_y (model correction factor)", 1, min = 0, max = 1, step = 0.01),
            numericInput("f", "f_y (fraction of CH4 captured)", 0, min = 0, max = 1, step = 0.01),
            numericInput("ox", "OX (oxidation factor)", 0.1, min = 0, max = 1, step = 0.01),
            numericInput("Fval", "F (fraction CH4 in gas)", 0.5, min = 0, max = 1, step = 0.1),
            numericInput("docf", "DOCf (decomposable fraction)", 0.5, min = 0, max = 1, step = 0.1),
            numericInput("mcf", "MCF (methane correction factor)", 1, min = 0, max = 1, step = 0.1),
            numericInput("yearMonthTarget", "Year or Month Target", 
                         value = 25, 
                         min = min(waste_data()$year), 
                         max = min(waste_data()$year) + 100),  # Limit to 100 years projection
            conditionalPanel(
              condition = "input.approach == 'simplified'",
              selectInput("climate_zone", "Climate Zone (Simplified Approach)", 
                          choices = c("tropical_wet", "tropical_dry",
                                      "boreal_temperate_wet", "boreal_temperate_dry"),
                          selected = "tropical_wet")
            )
          ),
          conditionalPanel(
            condition = "input.use_multi_region",
            h4("Region Parameters"),
            uiOutput("region_params")
          ),
          hr(),
          fileInput("file1", "Upload Waste Data (.csv)", accept = ".csv"),
          checkboxInput("use_estimated_waste", "Use Estimated Waste Data", FALSE),
          helpText("For annual approach, columns: year, waste_type, mass_tonnes, region (optional).
                   For monthly approach, columns: month, waste_type, mass_tonnes, region (optional).
                   For simplified approach, columns: year, mass_tonnes, region (optional)."),
          actionButton("calcBtn", "Calculate Emissions!", class = "btn-primary")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Results Table", 
                     conditionalPanel(
                       condition = "input.use_multi_region",
                       h4("Results by Region"),
                       DTOutput("emissionsTableRegions"),
                       downloadButton("downloadRegionsTable", "Export to CSV")
                     ),
                     h4("Total Emissions"),
                     DTOutput("emissionsTable"),
                     downloadButton("downloadEmissionsTable", "Export to CSV")),
            
            tabPanel("Bar Plot", 
                     conditionalPanel(
                       condition = "input.use_multi_region",
                       plotOutput("emissionsPlotRegions"),
                       h4("Values by Region"),
                       DTOutput("barPlotTableRegions"),
                       downloadButton("downloadBarPlotTableRegions", "Export to CSV")
                     ),
                     h4("Total Emissions"),
                     plotOutput("emissionsPlot"),
                     h4("Annual Values"),
                     DTOutput("barPlotTable"),
                     downloadButton("downloadBarPlotTable", "Export to CSV")),
            
            tabPanel("Area Plot", 
                     conditionalPanel(
                       condition = "input.use_multi_region",
                       selectInput("region_for_area", "Select Region for Area Plot", choices = NULL),
                       plotOutput("areaPlotRegion")
                     ),
                     h4("Time Series Data"),
                     plotOutput("areaPlot"),
                     DTOutput("areaPlotTable"),
                     downloadButton("downloadAreaPlotTable", "Export to CSV")),
            
            tabPanel("Decay Chart", 
                     conditionalPanel(
                       condition = "input.use_multi_region",
                       selectInput("region_for_decay", "Select Region for Decay Chart", choices = NULL),
                       plotOutput("decayChartRegion")
                     ),
                     h4("Decay Chart (All Regions)"),
                     plotOutput("decayChart"),
                     h4("Annual Decay Values"),
                     DTOutput("decayTable"),
                     downloadButton("downloadDecayTable", "Export to CSV"))
          )
        )
      )
    ),
    
    # Tab 3: Documentation & Help
    tabPanel(
      "Documentation & Help",
      value = "help",
      tabsetPanel(
        tabPanel("Overview",
                 h2("SWDSFODR Package Documentation"),
                 p("This Shiny application is the interface for the SWDSFODR package, which implements 
                   the Clean Development Mechanism (CDM) Tool 04 (version 08.1) for calculating 
                   greenhouse gas emissions from solid waste disposal sites."),
                 h3("Features"),
                 p("This enhanced version includes:"),
                 tags$ul(
                   tags$li(strong("Multi-region support:"), "Calculate emissions for multiple regions with 
                          region-specific parameters"),
                   tags$li(strong("Waste estimation module:"), "Estimate waste generation based on demographic 
                          and economic data"),
                   tags$li(strong("Projection capabilities:"), "Project future waste generation based on various 
                          growth models")
                 ),
                 h3("Basic Usage"),
                 p("1. Start with 'Waste Estimation' if you need to estimate waste quantities based on 
                   demographic data."),
                 p("2. Go to 'Emissions Calculation' to calculate GHG emissions using the First Order Decay model."),
                 p("3. Use 'Documentation & Help' for reference on parameters and methodologies.")
        ),
        tabPanel("Parameters", 
                 h3("CDM Tool 04 Parameters"),
                 HTML("
                 <table class='table table-striped'>
                   <thead>
                     <tr>
                       <th>Parameter</th>
                       <th>Description</th>
                       <th>Default Value</th>
                     </tr>
                   </thead>
                   <tbody>
                     <tr>
                       <td>phi_y</td>
                       <td>Model correction factor to account for model uncertainties</td>
                       <td>0.75-0.85 (depending on application and climate)</td>
                     </tr>
                     <tr>
                       <td>f_y</td>
                       <td>Fraction of methane captured and destroyed</td>
                       <td>0 (no capture)</td>
                     </tr>
                     <tr>
                       <td>GWP_CH4</td>
                       <td>Global Warming Potential of methane</td>
                       <td>28 (AR5 value)</td>
                     </tr>
                     <tr>
                       <td>OX</td>
                       <td>Oxidation factor (amount of methane oxidized in cover material)</td>
                       <td>0.1</td>
                     </tr>
                     <tr>
                       <td>F</td>
                       <td>Fraction of methane in the SWDS gas</td>
                       <td>0.5</td>
                     </tr>
                     <tr>
                       <td>DOCf</td>
                       <td>Fraction of degradable organic carbon that decomposes</td>
                       <td>0.5</td>
                     </tr>
                     <tr>
                       <td>MCF</td>
                       <td>Methane correction factor</td>
                       <td>1.0 for managed anaerobic, 0.5 for semi-aerobic, 0.8 for unmanaged-deep, 0.4 for unmanaged-shallow</td>
                     </tr>
                     <tr>
                       <td>DOCj</td>
                       <td>Fraction of degradable organic carbon in waste type j</td>
                       <td>Varies by waste type: food (15%), paper (40%), garden (20%), wood (43%), textiles (24%)</td>
                     </tr>
                     <tr>
                       <td>k_j</td>
                       <td>Decay rate for waste type j</td>
                       <td>Varies by waste type and climate: food (0.06-0.4), paper (0.04-0.07), garden (0.05-0.17), wood (0.02-0.035)</td>
                     </tr>
                   </tbody>
                 </table>
                 ")
        ),
        tabPanel("Input Formats",
                 h3("Waste Data Format"),
                 p("The application accepts CSV files with the following formats:"),
                 h4("1. Annual Approach"),
                 HTML("
                 <pre>
year,waste_type,mass_tonnes,region
2020,food,1000,region1
2020,paper,500,region1
2020,garden,300,region1
2021,food,1100,region1
2021,paper,520,region1
2021,garden,320,region1
2020,food,800,region2
2020,paper,400,region2
...
                 </pre>
                 "),
                 h4("2. Monthly Approach"),
                 HTML("
                 <pre>
month,waste_type,mass_tonnes,region
1,food,100,region1
1,paper,50,region1
2,food,110,region1
2,paper,55,region1
...
                 </pre>
                 "),
                 h4("3. Simplified Approach"),
                 HTML("
                 <pre>
year,mass_tonnes,region
2020,1800,region1
2021,1940,region1
2020,1200,region2
...
                 </pre>
                 "),
                 h3("Demographic Data Format (for Waste Estimation)"),
                 HTML("
                 <pre>
year,region,population,gdp_per_capita,urban_pct
2020,region1,1000000,15000,80
2021,region1,1010000,15300,81
2022,region1,1020000,15600,82
2020,region2,500000,12000,65
...
                 </pre>
                 ")
        ),
        tabPanel("Methodology",
                 h3("CDM Tool 04 Methodology"),
                 p("The application implements the First Order Decay (FOD) model as specified in the Clean Development Mechanism (CDM) Tool 04, Version 08.1."),
                 h4("Annual Model Equation:"),
                 withMathJax(),
                 helpText("$$BE_{CH4,SWDS,y} = \\phi_y \\times (1 - f_y) \\times GWP_{CH4} \\times (1 - OX) \\times \\frac{16}{12} \\times F \\times DOC_{f,y} \\times MCF_y \\times \\sum_j \\sum_{x=1}^y (W_{j,x} \\times DOC_j \\times e^{-k_j \\times (y-x)} \\times (1 - e^{-k_j}))$$"),
                 h4("Monthly Model Equation:"),
                 helpText("$$BE_{CH4,SWDS,m} = \\phi_y \\times (1 - f_y) \\times GWP_{CH4} \\times (1 - OX) \\times \\frac{16}{12} \\times F \\times DOC_{f,m} \\times MCF_y \\times \\sum_j \\sum_{i=1}^m (W_{j,i} \\times DOC_j \\times e^{-\\frac{k_j}{12} \\times (m-i)} \\times (1 - e^{-\\frac{k_j}{12}}))$$"),
                 h3("Waste Estimation Methodology"),
                 p("The waste estimation module implements several models for estimating waste generation based on demographic and economic factors:"),
                 h4("Simple Population-Based Model:"),
                 helpText("$$Waste = Population \\times BaseGenerationRate$$"),
                 h4("Economic Model:"),
                 helpText("$$Waste = Population \\times (BaseRate + GDPfactor \\times GDP^{GDPelasticity})$$"),
                 h4("Comprehensive Model:"),
                 helpText("$$Waste = Population \\times (BaseRate + GDPfactor \\times GDP^{GDPelasticity} + UrbanFactor \\times UrbanPercentage)$$")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  options(shiny.sanitize.errors = FALSE)  # Shows actual error messages

  # -- REACTIVE VALUES --
  values <- reactiveValues(
    # Waste estimation
    demographic_data = NULL,
    estimated_total_waste = NULL,
    estimated_waste_composition = NULL,
    waste_projection = NULL,
    
    # Emissions calculation
    regions = NULL,
    region_params = NULL,
    waste_data = NULL,
    emissions_results = NULL,
    time_series_data = NULL,
    decay_data = NULL
  )
  
  # -- WASTE ESTIMATION MODULE --
  
  # Dynamic UI for manual region data entry
  output$manual_regions_ui <- renderUI({
    req(input$num_regions, input$demo_data_source == "manual")
    regions <- paste0("Region", 1:input$num_regions)
    
    ui_elements <- list()
    
    for(i in 1:input$num_regions) {
      region_name <- regions[i]
      
      ui_elements[[length(ui_elements) + 1]] <- h4(region_name)
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("pop_", region_name),
        label = "Population",
        value = 100000 * i,
        min = 1000
      )
      
      if(input$est_model != "simple") {
        ui_elements[[length(ui_elements) + 1]] <- numericInput(
          inputId = paste0("gdp_", region_name),
          label = "GDP per capita (USD)",
          value = 10000 + (2000 * i),
          min = 100
        )
      }
      
      if(input$est_model == "comprehensive") {
        ui_elements[[length(ui_elements) + 1]] <- sliderInput(
          inputId = paste0("urban_", region_name),
          label = "Urban population (%)",
          min = 0,
          max = 100,
          value = 40 + (5 * i)
        )
      }
    }
    
    do.call(tagList, ui_elements)
  })
  
  # Process demographic data
  observeEvent(input$demo_file, {
    req(input$demo_file)
    
    # Read the file
    demographics <- read.csv(input$demo_file$datapath, header = input$demo_has_header)
    
    # Validate the data
    required_cols <- "region"
    if(!all(required_cols %in% names(demographics))) {
      showNotification("Uploaded file must have 'region' column", type = "error")
      return(NULL)
    }
    
    # Add additional required columns if they don't exist
    if(!"year" %in% names(demographics)) {
      demographics$year <- 2024  # Default current year
    }
    
    if(!"population" %in% names(demographics)) {
      showNotification("Uploaded file must have 'population' column", type = "error")
      return(NULL)
    }
    
    if(input$est_model != "simple" && !"gdp_per_capita" %in% names(demographics)) {
      showNotification("Economic model requires 'gdp_per_capita' column", type = "error")
      return(NULL)
    }
    
    if(input$est_model == "comprehensive" && !"urban_pct" %in% names(demographics)) {
      showNotification("Comprehensive model requires 'urban_pct' column", type = "error")
      return(NULL)
    }
    
    # Store the data
    values$demographic_data <- demographics
  })
  
  # Create demographic data from manual input
  observeEvent(input$estimate_btn, {
    if(input$demo_data_source == "manual") {
      req(input$num_regions)
      
      regions <- paste0("Region", 1:input$num_regions)
      demographics <- data.frame(
        region = regions,
        year = 2024,  # Default current year
        population = NA_real_,
        gdp_per_capita = NA_real_,
        urban_pct = NA_real_
      )
      
      # Fill in the data from inputs
      for(i in 1:input$num_regions) {
        region_name <- regions[i]
        demographics$population[i] <- input[[paste0("pop_", region_name)]]
        
        if(input$est_model != "simple") {
          demographics$gdp_per_capita[i] <- input[[paste0("gdp_", region_name)]]
        } else {
          demographics$gdp_per_capita[i] <- NA
        }
        
        if(input$est_model == "comprehensive") {
          demographics$urban_pct[i] <- input[[paste0("urban_", region_name)]]
        } else {
          demographics$urban_pct[i] <- NA
        }
      }
      
      # Store the data
      values$demographic_data <- demographics
    }
  })
  
  # Estimate waste generation when button clicked
  observeEvent(input$estimate_btn, {
    req(values$demographic_data)
    
    # Get estimation parameters
    params <- list(
      base_generation_rate = input$base_gen_rate
    )
    
    if(input$est_model != "simple") {
      params$gdp_factor <- input$gdp_factor
      params$gdp_elasticity <- input$gdp_elasticity
    }
    
    if(input$est_model == "comprehensive") {
      params$urban_factor <- input$urban_factor
    }
    
    # Estimate total waste generation
    total_waste <- estimateMSWGeneration(
      population = values$demographic_data$population,
      gdp_per_capita = values$demographic_data$gdp_per_capita,
      urban_pct = values$demographic_data$urban_pct,
      years = values$demographic_data$year,
      regions = values$demographic_data$region,
      model = input$est_model,
      params = params
    )
    
    # Store the results
    values$estimated_total_waste <- total_waste
    
    # Distribute by waste type
    if(input$use_default_comp) {
      composition_profiles <- NULL  # Use defaults
    } else {
      req(input$comp_file)
      composition_profiles <- read.csv(input$comp_file$datapath)
    }
    
    waste_composition <- distributeMSWByType(
      msw_data = total_waste,
      composition_profiles = composition_profiles
    )
    
    # Store the composition results
    values$estimated_waste_composition <- waste_composition
    
    # Show success notification
    showNotification("Waste estimation completed successfully", type = "message")
  })
  
  # Run waste projection
  observeEvent(input$run_projection, {
    req(values$estimated_waste_composition)
    
    # Get projection parameters
    projection_years <- input$proj_start_year:input$proj_end_year
    growth_rate <- input$proj_growth_rate
    growth_model <- input$proj_model
    saturation_level <- if(input$proj_model == "logistic") input$proj_saturation else NULL
    by_waste_type <- input$proj_by_type
    
    # Run the projection
    projected_waste <- projectWasteGeneration(
      historical_data = values$estimated_waste_composition,
      projection_years = projection_years,
      growth_rate = growth_rate,
      growth_model = growth_model,
      saturation_level = saturation_level,
      by_region = TRUE,
      by_waste_type = by_waste_type
    )
    
    # Store the projection results
    values$waste_projection <- projected_waste
    
    # Show success notification
    showNotification("Waste projection completed successfully", type = "message")
  })
  
  # Display estimated waste table
  output$est_waste_table <- renderDT({
    req(values$estimated_total_waste)
    datatable(values$estimated_total_waste, options = list(pageLength = 10))
  })
  
  # Display waste composition table
  output$est_comp_table <- renderDT({
    req(values$estimated_waste_composition)
    datatable(values$estimated_waste_composition, options = list(pageLength = 10))
  })
  
  # Plot estimated waste by region
  output$est_waste_plot <- renderPlot({
    req(values$estimated_total_waste)
    
    ggplot(values$estimated_total_waste, aes(x = region, y = estimated_waste_tonnes, fill = region)) +
      geom_col() +
      theme_minimal() +
      labs(
        title = "Estimated Waste Generation by Region",
        x = "Region",
        y = "Waste (tonnes)",
        fill = "Region"
      ) +
      theme(legend.position = "bottom")
  })
  
  # Plot waste composition by region
  output$est_comp_plot <- renderPlot({
    req(values$estimated_waste_composition)
    
    ggplot(values$estimated_waste_composition, 
           aes(x = region, y = mass_tonnes, fill = waste_type)) +
      geom_col(position = "stack") +
      theme_minimal() +
      labs(
        title = "Waste Composition by Region",
        x = "Region",
        y = "Waste (tonnes)",
        fill = "Waste Type"
      ) +
      theme(legend.position = "bottom")
  })
  
  # Plot waste projection
  output$proj_plot <- renderPlot({
    req(values$waste_projection)
    
    if(input$proj_by_type) {
      # Plot by region and waste type
      ggplot(values$waste_projection, 
             aes(x = year, y = mass_tonnes, color = region, linetype = waste_type)) +
        geom_line() +
        theme_minimal() +
        labs(
          title = "Waste Generation Projection by Region and Type",
          x = "Year",
          y = "Waste (tonnes)",
          color = "Region",
          linetype = "Waste Type"
        ) +
        theme(legend.position = "bottom")
    } else {
      # Aggregate by region and year
      agg_data <- values$waste_projection %>%
        group_by(year, region) %>%
        summarize(mass_tonnes = sum(mass_tonnes, na.rm = TRUE), .groups = 'drop')
      
      ggplot(agg_data, aes(x = year, y = mass_tonnes, color = region)) +
        geom_line(size = 1) +
        theme_minimal() +
        labs(
          title = "Waste Generation Projection by Region",
          x = "Year",
          y = "Waste (tonnes)",
          color = "Region"
        ) +
        theme(legend.position = "bottom")
    }
  })
  
  # Display projection table
  output$proj_table <- renderDT({
    req(values$waste_projection)
    datatable(values$waste_projection, options = list(pageLength = 10))
  })
  
  # Download handlers for waste estimation
  output$download_est_waste <- downloadHandler(
    filename = function() {
      paste("estimated-waste-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$estimated_total_waste, file, row.names = FALSE)
    }
  )
  
  output$download_est_comp <- downloadHandler(
    filename = function() {
      paste("waste-composition-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$estimated_waste_composition, file, row.names = FALSE)
    }
  )
  
  output$download_projection <- downloadHandler(
    filename = function() {
      paste("waste-projection-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$waste_projection, file, row.names = FALSE)
    }
  )
  
  # -- EMISSIONS CALCULATION MODULE --
  
  # Load regions from waste data
  observe({
    if(!is.null(input$file1) || (input$use_estimated_waste && !is.null(values$estimated_waste_composition))) {
      if(input$use_estimated_waste && !is.null(values$estimated_waste_composition)) {
        waste_data <- values$estimated_waste_composition
        if("region" %in% names(waste_data)) {
          regions <- unique(waste_data$region)
          values$regions <- regions
          updateSelectInput(session, "region_for_area", choices = regions)
          updateSelectInput(session, "region_for_decay", choices = regions)
        }
      } else if(!is.null(input$file1)) {
        waste_data <- read.csv(input$file1$datapath)
        if("region" %in% names(waste_data)) {
          regions <- unique(waste_data$region)
          values$regions <- regions
          updateSelectInput(session, "region_for_area", choices = regions)
          updateSelectInput(session, "region_for_decay", choices = regions)
        }
      }
    }
  })
  
  # Region selector UI
  output$region_selector <- renderUI({
    req(values$regions)
    checkboxGroupInput("selected_regions", "Select Regions to Include:",
                      choices = values$regions,
                      selected = values$regions)
  })
  
  # Region-specific parameters UI
  output$region_params <- renderUI({
    req(input$use_multi_region, values$regions, input$selected_regions)
    
    selected_regions <- input$selected_regions
    
    ui_elements <- list()
    
    for(region in selected_regions) {
      ui_elements[[length(ui_elements) + 1]] <- h4(region)
      
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("phi_", region),
        label = "phi_y (model correction factor)",
        value = 1,
        min = 0,
        max = 1,
        step = 0.01
      )
      
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("f_", region),
        label = "f_y (fraction of CH4 captured)",
        value = 0,
        min = 0,
        max = 1,
        step = 0.01
      )
      
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("ox_", region),
        label = "OX (oxidation factor)",
        value = 0.1,
        min = 0,
        max = 1,
        step = 0.01
      )
      
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("Fval_", region),
        label = "F (fraction CH4 in gas)",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.1
      )
      
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("docf_", region),
        label = "DOCf (decomposable fraction)",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.1
      )
      
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("mcf_", region),
        label = "MCF (methane correction factor)",
        value = 1,
        min = 0,
        max = 1,
        step = 0.1
      )
      
      ui_elements[[length(ui_elements) + 1]] <- numericInput(
        inputId = paste0("yearMonthTarget_", region),
        label = "Year or Month Target",
        value = 25,
        min = 1,
        step = 1
      )
      
      if(input$approach == "simplified") {
        ui_elements[[length(ui_elements) + 1]] <- selectInput(
          inputId = paste0("climate_zone_", region),
          label = "Climate Zone",
          choices = c("tropical_wet", "tropical_dry",
                      "boreal_temperate_wet", "boreal_temperate_dry"),
          selected = "tropical_wet"
        )
      }
      
      ui_elements[[length(ui_elements) + 1]] <- hr()
    }
    
    do.call(tagList, ui_elements)
  })
  
  # Collect region-specific parameters
  observe({
    req(input$use_multi_region, values$regions, input$selected_regions)
    
    selected_regions <- input$selected_regions
    
    params <- list(
      phi_y = numeric(length(selected_regions)),
      f_y = numeric(length(selected_regions)),
      OX = numeric(length(selected_regions)),
      F = numeric(length(selected_regions)),
      DOCf = numeric(length(selected_regions)),
      MCF_y = numeric(length(selected_regions)),
      year_month_target = numeric(length(selected_regions)),
      climate_zone = character(length(selected_regions))
    )
    
    names(params$phi_y) <- selected_regions
    names(params$f_y) <- selected_regions
    names(params$OX) <- selected_regions
    names(params$F) <- selected_regions
    names(params$DOCf) <- selected_regions
    names(params$MCF_y) <- selected_regions
    names(params$year_month_target) <- selected_regions
    names(params$climate_zone) <- selected_regions
    
    for(region in selected_regions) {
      if(!is.null(input[[paste0("phi_", region)]])) {
        params$phi_y[region] <- input[[paste0("phi_", region)]]
      } else {
        params$phi_y[region] <- 1  # Default
      }
      
      if(!is.null(input[[paste0("f_", region)]])) {
        params$f_y[region] <- input[[paste0("f_", region)]]
      } else {
        params$f_y[region] <- 0  # Default
      }
      
      if(!is.null(input[[paste0("ox_", region)]])) {
        params$OX[region] <- input[[paste0("ox_", region)]]
      } else {
        params$OX[region] <- 0.1  # Default
      }
      
      if(!is.null(input[[paste0("Fval_", region)]])) {
        params$F[region] <- input[[paste0("Fval_", region)]]
      } else {
        params$F[region] <- 0.5  # Default
      }
      
      if(!is.null(input[[paste0("docf_", region)]])) {
        params$DOCf[region] <- input[[paste0("docf_", region)]]
      } else {
        params$DOCf[region] <- 0.5  # Default
      }
      
      if(!is.null(input[[paste0("mcf_", region)]])) {
        params$MCF_y[region] <- input[[paste0("mcf_", region)]]
      } else {
        params$MCF_y[region] <- 1  # Default
      }
      
      if(!is.null(input[[paste0("yearMonthTarget_", region)]])) {
        params$year_month_target[region] <- input[[paste0("yearMonthTarget_", region)]]
      } else {
        params$year_month_target[region] <- 25  # Default
      }
      
      if(input$approach == "simplified" && !is.null(input[[paste0("climate_zone_", region)]])) {
        params$climate_zone[region] <- input[[paste0("climate_zone_", region)]]
      } else {
        params$climate_zone[region] <- "tropical_wet"  # Default
      }
    }
    
    values$region_params <- params
  })
  
  # Load waste data
  observe({
    if(input$use_estimated_waste && !is.null(values$estimated_waste_composition)) {
      values$waste_data <- values$estimated_waste_composition
      if(input$approach == "simplified") {
        # For simplified approach, aggregate by year and region
        values$waste_data <- values$waste_data %>%
          group_by(year, region) %>%
          summarize(mass_tonnes = sum(mass_tonnes, na.rm = TRUE), .groups = 'drop')
      }
    } else if(!is.null(input$file1)) {
      values$waste_data <- read.csv(input$file1$datapath)
    } else {
      values$waste_data <- NULL
    }
  })
  
  # Calculate emissions when button is clicked
  observeEvent(input$calcBtn, {
    req(values$waste_data)
    
    waste_data <- values$waste_data
    approach <- input$approach
    
    # Filter by selected regions if in multi-region mode
    if(input$use_multi_region && "region" %in% names(waste_data)) {
      waste_data <- waste_data[waste_data$region %in% input$selected_regions, ]
    }
    
    # Get parameters (either global or region-specific)
    if(input$use_multi_region) {
      params <- values$region_params
      phi_val <- params$phi_y
      f_val <- params$f_y
      gwp <- input$gwpslid
      ox_val <- params$OX
      F_ <- params$F
      docf <- params$DOCf
      mcf <- params$MCF_y
      target <- params$year_month_target
      cz <- params$climate_zone
    } else {
      phi_val <- input$phi
      f_val <- input$f
      gwp <- input$gwpslid
      ox_val <- input$ox
      F_ <- input$Fval
      docf <- input$docf
      mcf <- input$mcf
      target <- input$yearMonthTarget
      cz <- input$climate_zone
    }
    
    # Calculate emissions based on approach
    if(approach == "annual") {
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j <- c(food = 0.06, paper = 0.04, other = 0.05)
      
      # Check if using multi-region
      if(input$use_multi_region) {
        emissions <- calcSWDSEmissionsYearly(
          W = waste_data, DOCj = DOCj, k_j = k_j,
          phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
          OX = ox_val, F = F_, DOCf_y = docf,
          MCF_y = mcf, year_target = target,
          by_region = TRUE
        )
        
        # Convert to data frame for display
        emissions_df <- data.frame(
          Region = names(emissions),
          Emissions_tCO2e = as.numeric(emissions),
          Emissions_GgCH4 = as.numeric(emissions) / (gwp * 1000)
        )
        
        # Store results
        values$emissions_results <- list(
          by_region = emissions_df,
          total = data.frame(
            Emissions_tCO2e = sum(emissions),
            Emissions_GgCH4 = sum(emissions) / (gwp * 1000)
          )
        )
      } else {
        emissions <- calcSWDSEmissionsYearly(
          W = waste_data, DOCj = DOCj, k_j = k_j,
          phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
          OX = ox_val, F = F_, DOCf_y = docf,
          MCF_y = mcf, year_target = target
        )
        
        # Store results
        values$emissions_results <- list(
          total = data.frame(
            Emissions_tCO2e = emissions,
            Emissions_GgCH4 = emissions / (gwp * 1000)
          )
        )
      }
    } else if(approach == "monthly") {
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j <- c(food = 0.06, paper = 0.04, other = 0.05)
      
      # Check if using multi-region
      if(input$use_multi_region) {
        emissions <- calcSWDSEmissionsMonthly(
          W = waste_data, DOCj = DOCj, k_j = k_j,
          phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
          OX = ox_val, F = F_, DOCf_m = docf,
          MCF_y = mcf, month_target = target,
          by_region = TRUE
        )
        
        # Convert to data frame for display
        emissions_df <- data.frame(
          Region = names(emissions),
          Emissions_tCO2e = as.numeric(emissions),
          Emissions_GgCH4 = as.numeric(emissions) / (gwp * 1000)
        )
        
        # Store results
        values$emissions_results <- list(
          by_region = emissions_df,
          total = data.frame(
            Emissions_tCO2e = sum(emissions),
            Emissions_GgCH4 = sum(emissions) / (gwp * 1000)
          )
        )
      } else {
        emissions <- calcSWDSEmissionsMonthly(
          W = waste_data, DOCj = DOCj, k_j = k_j,
          phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
          OX = ox_val, F = F_, DOCf_m = docf,
          MCF_y = mcf, month_target = target
        )
        
        # Store results
        values$emissions_results <- list(
          total = data.frame(
            Emissions_tCO2e = emissions,
            Emissions_GgCH4 = emissions / (gwp * 1000)
          )
        )
      }
    } else {
      # Simplified approach
      
      # Check if using multi-region
      if(input$use_multi_region) {
        emissions <- calcSWDSEmissionsSimplified(
          W = waste_data, phi_y = phi_val,
          f_y = f_val, GWP_CH4 = gwp,
          climate_zone = cz, year_target = target,
          by_region = TRUE
        )
        
        # Convert to data frame for display
        emissions_df <- data.frame(
          Region = names(emissions),
          Emissions_tCO2e = as.numeric(emissions),
          Emissions_GgCH4 = as.numeric(emissions) / (gwp * 1000)
        )
        
        # Store results
        values$emissions_results <- list(
          by_region = emissions_df,
          total = data.frame(
            Emissions_tCO2e = sum(emissions),
            Emissions_GgCH4 = sum(emissions) / (gwp * 1000)
          )
        )
      } else {
        emissions <- calcSWDSEmissionsSimplified(
          W = waste_data, phi_y = phi_val,
          f_y = f_val, GWP_CH4 = gwp,
          climate_zone = cz, year_target = target
        )
        
        # Store results
        values$emissions_results <- list(
          total = data.frame(
            Emissions_tCO2e = emissions,
            Emissions_GgCH4 = emissions / (gwp * 1000)
          )
        )
      }
    }
    
    # Calculate time series data
    # This part would implement the time series calculations similar to original app
    # with added multi-region support
    
    # Show success notification
    showNotification("Emissions calculation completed successfully", type = "message")
  })
  
  # Display emission results
  output$emissionsTable <- renderDT({
    req(values$emissions_results$total)
    datatable(values$emissions_results$total, options = list(pageLength = 5))
  })
  
  # Display emission results by region
  output$emissionsTableRegions <- renderDT({
    req(values$emissions_results$by_region)
    datatable(values$emissions_results$by_region, options = list(pageLength = 10))
  })
  
  # Bar plot of emissions
  output$emissionsPlot <- renderPlot({
    req(values$emissions_results$total)
    df_val <- values$emissions_results$total
    approach <- input$approach
    
    ggplot(
      data = data.frame(
        Approach = approach,
        Emissions_tCO2e = df_val$Emissions_tCO2e,
        Emissions_GgCH4 = df_val$Emissions_GgCH4
      ),
      aes(x = Approach, y = Emissions_tCO2e)
    ) +
      geom_col(fill = "steelblue") +
      geom_text(
        aes(label = paste0("Gg CH4: ", round(Emissions_GgCH4, 3))),
        vjust = -0.5, size = 5
      ) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / (input$gwpslid * 1000),
                            name = "Emissions (Gg CH4)")
      ) +
      theme_minimal() +
      labs(
        title = "Estimated CH4 Emissions",
        x = "",
        y = "Emissions (t CO2e)"
      )
  })
  
  # Bar plot of emissions by region
  output$emissionsPlotRegions <- renderPlot({
    req(values$emissions_results$by_region)
    df_val <- values$emissions_results$by_region
    
    ggplot(
      data = df_val,
      aes(x = Region, y = Emissions_tCO2e, fill = Region)
    ) +
      geom_col() +
      geom_text(
        aes(label = paste0("Gg CH4: ", round(Emissions_GgCH4, 3))),
        vjust = -0.5, size = 4
      ) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / (input$gwpslid * 1000),
                            name = "Emissions (Gg CH4)")
      ) +
      theme_minimal() +
      labs(
        title = "Estimated CH4 Emissions by Region",
        x = "",
        y = "Emissions (t CO2e)"
      ) +
      theme(legend.position = "bottom")
  })
  
  # Add other plot implementations (area plots, decay charts)
  # Time series data and decay charts would be implemented here
  # similar to the original app but with multi-region support
  
  # Add download handlers for results
  output$downloadEmissionsTable <- downloadHandler(
    filename = function() {
      paste("emissions-results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$emissions_results$total, file, row.names = FALSE)
    }
  )
  
  output$downloadRegionsTable <- downloadHandler(
    filename = function() {
      paste("emissions-by-region-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$emissions_results$by_region, file, row.names = FALSE)
    }
  )
  
  # Other download handlers would be implemented here for other outputs
  
  wasteData <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    # Validate data format
    if(input$approach == "annual") {
      validate(
        need(all(c("year", "waste_type", "mass_tonnes") %in% colnames(df)), 
             "Annual approach requires columns: year, waste_type, mass_tonnes")
      )
      validate(
        need(all(df$waste_type %in% c("food", "paper", "other")), 
             "Waste types must be 'food', 'paper', or 'other'")
      )
    } else if(input$approach == "monthly") {
      validate(
        need(all(c("month", "waste_type", "mass_tonnes") %in% colnames(df)), 
             "Monthly approach requires columns: month, waste_type, mass_tonnes")
      )
    } else if(input$approach == "simplified") {
      validate(
        need(all(c("year", "mass_tonnes") %in% colnames(df)), 
             "Simplified approach requires columns: year, mass_tonnes")
      )
    }
    
    # Ensure numeric columns are numeric
    if("year" %in% colnames(df)) df$year <- as.numeric(df$year)
    if("month" %in% colnames(df)) df$month <- as.numeric(df$month)
    df$mass_tonnes <- as.numeric(df$mass_tonnes)
    
    return(df)
  })
  
  # For long calculations
  withProgress(message = 'Calculating emissions...', {
    # Your calculation code
    incProgress(0.5)  # Update halfway
    # More calculation
  })
}

shinyApp(ui, server)