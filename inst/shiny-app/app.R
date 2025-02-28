dir.create("SWDSFODR/inst", showWarnings=FALSE)
dir.create("SWDSFODR/inst/shiny-app", showWarnings=FALSE)

# Shiny app.R
shiny_app_R <- "# app.R
# A Shiny app for SWDSFOD package

library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel(\"SWDSFOD Tool - First Order Decay Model for SWDS Emissions\"),
  sidebarLayout(
    sidebarPanel(
      h4(\"Select Approach\"),
      radioButtons(\"approach\", \"Calculation Approach:\",
                   choices=c(\"Annual\"=\"annual\",\"Monthly\"=\"monthly\",\"Simplified\"=\"simplified\")),
      hr(),
      h4(\"Parameters\"),
      numericInput(\"phi\", \"phi_y (model correction factor)\", 1, min=0, max=1, step=0.01),
      numericInput(\"f\", \"f_y (fraction of CH4 captured)\", 0, min=0, max=1, step=0.01),
      numericInput(\"gwpslid\", \"GWP_CH4\", 28, min=1, max=100),
      numericInput(\"ox\", \"OX (oxidation factor)\", 0.1, min=0, max=1, step=0.01),
      numericInput(\"Fval\", \"F (fraction CH4 in gas)\", 0.5, min=0, max=1, step=0.1),
      numericInput(\"docf\", \"DOCf (decomposable fraction)\", 0.5, min=0, max=1, step=0.1),
      numericInput(\"mcf\", \"MCF (methane correction factor)\", 1, min=0, max=1, step=0.1),
      numericInput(\"yearMonthTarget\", \"Year or Month Target\", 5, min=1, step=1),
      conditionalPanel(
        condition = \"input.approach == 'simplified'\",
        selectInput(\"climate_zone\", \"Climate Zone\", 
                    choices=c(\"tropical_wet\",\"tropical_dry\",\"boreal_temperate_wet\",\"boreal_temperate_dry\"),
                    selected=\"tropical_wet\")
      ),
      fileInput(\"file1\", \"Upload Waste Data (.csv)\", accept=\".csv\"),
      helpText(\"For annual approach, columns: year,waste_type,mass_tonnes.
                For monthly approach, columns: month,waste_type,mass_tonnes.
                For simplified approach, columns: year,mass_tonnes.\"),
      actionButton(\"calcBtn\", \"Calculate Emissions!\")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(\"Results\", 
                 h4(\"Estimated Emissions\"),
                 verbatimTextOutput(\"emissionsText\"),
                 DTOutput(\"emissionsTable\")),
        tabPanel(\"Plot\", 
                 plotOutput(\"emissionsPlot\"),
                 downloadButton(\"downloadPlot\", \"Download Plot\")),
        tabPanel(\"Help\", 
                 h4(\"SWDSFODR Package Help\"),
                 p(\"This tool implements the First Order Decay (FOD) model for estimating methane emissions from Solid Waste Disposal Sites (SWDS) as specified in the CDM Tool 04 (Version 08.1).\"),
                 h5(\"Approaches:\"),
                 tags$ul(
                   tags$li(strong(\"Annual:\"), \"Standard FOD model with yearly resolution\"),
                   tags$li(strong(\"Monthly:\"), \"Higher resolution FOD model with monthly calculations\"),
                   tags$li(strong(\"Simplified:\"), \"Approach for situations with limited waste composition data\")
                 ),
                 h5(\"Key Parameters:\"),
                 tags$ul(
                   tags$li(strong(\"phi_y:\"), \"Model correction factor\"),
                   tags$li(strong(\"f_y:\"), \"Fraction of CH4 captured and flared or used\"),
                   tags$li(strong(\"GWP_CH4:\"), \"Global Warming Potential of methane\"),
                   tags$li(strong(\"OX:\"), \"Oxidation factor\"),
                   tags$li(strong(\"F:\"), \"Fraction of CH4 in generated landfill gas\"),
                   tags$li(strong(\"DOCf:\"), \"Fraction of degradable organic carbon that decomposes\"),
                   tags$li(strong(\"MCF:\"), \"Methane correction factor\")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive expression to load data
  wasteData <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors=FALSE)
    
    # Validate based on approach
    if(input$approach == \"annual\") {
      req(all(c(\"year\", \"waste_type\", \"mass_tonnes\") %in% names(df)))
    } else if(input$approach == \"monthly\") {
      req(all(c(\"month\", \"waste_type\", \"mass_tonnes\") %in% names(df)))
    } else if(input$approach == \"simplified\") {
      req(all(c(\"year\", \"mass_tonnes\") %in% names(df)))
    }
    
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
    
    if(approach==\"annual\") {
      # Define sample DOCj and k_j values (would be user inputs in real app)
      DOCj <- c(food=0.15, paper=0.4, garden=0.2, wood=0.43, textile=0.24, other=0.15)
      k_j  <- c(food=0.4, paper=0.07, garden=0.17, wood=0.035, textile=0.07, other=0.09)
      
      # Ensure all waste types in data have parameters
      waste_types <- unique(df$waste_type)
      missing_doc <- setdiff(waste_types, names(DOCj))
      missing_k <- setdiff(waste_types, names(k_j))
      
      if(length(missing_doc) > 0 || length(missing_k) > 0) {
        return(list(
          error = TRUE,
          message = paste(\"Missing parameters for waste types:\", 
                         paste(unique(c(missing_doc, missing_k)), collapse=\", \"))
        ))
      }
      
      # Call the annual function
      tryCatch({
        res_val <- calcSWDSEmissionsYearly(
          W = df, DOCj = DOCj, k_j = k_j,
          phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
          OX = ox_val, F = F_, DOCf_y = docf,
          MCF_y = mcf, year_target = target
        )
        return(list(
          result = res_val,
          error = FALSE
        ))
      }, error = function(e) {
        return(list(
          error = TRUE,
          message = paste(\"Error in annual calculation:\", e$message)
        ))
      })
    } else if(approach==\"monthly\") {
      # Define sample DOCj and k_j values (would be user inputs in real app)
      DOCj <- c(food=0.15, paper=0.4, garden=0.2, wood=0.43, textile=0.24, other=0.15)
      k_j  <- c(food=0.4, paper=0.07, garden=0.17, wood=0.035, textile=0.07, other=0.09)
      
      # Ensure all waste types in data have parameters
      waste_types <- unique(df$waste_type)
      missing_doc <- setdiff(waste_types, names(DOCj))
      missing_k <- setdiff(waste_types, names(k_j))
      
      if(length(missing_doc) > 0 || length(missing_k) > 0) {
        return(list(
          error = TRUE,
          message = paste(\"Missing parameters for waste types:\", 
                         paste(unique(c(missing_doc, missing_k)), collapse=\", \"))
        ))
      }
      
      # Call the monthly function
      tryCatch({
        res_val <- calcSWDSEmissionsMonthly(
          W = df, DOCj = DOCj, k_j = k_j,
          phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
          OX = ox_val, F = F_, DOCf_m = docf,
          MCF_y = mcf, month_target = target
        )
        return(list(
          result = res_val,
          error = FALSE
        ))
      }, error = function(e) {
        return(list(
          error = TRUE,
          message = paste(\"Error in monthly calculation:\", e$message)
        ))
      })
    } else {
      # simplified
      tryCatch({
        res_val <- calcSWDSEmissionsSimplified(
          W = df,
          phi_y = phi_val,
          f_y = f_val,
          GWP_CH4 = gwp,
          climate_zone = cz,
          year_target = target
        )
        return(list(
          result = res_val,
          error = FALSE
        ))
      }, error = function(e) {
        return(list(
          error = TRUE,
          message = paste(\"Error in simplified calculation:\", e$message)
        ))
      })
    }
  })
  
  # Show the emissions text result
  output$emissionsText <- renderText({
    res <- emissionsResult()
    if(res$error) {
      return(paste(\"Error:\", res$message))
    } else {
      return(paste(\"Total CH4 emissions (t CO2e):\", round(res$result, 2)))
    }
  })

  # Show the emissions in a table
  output$emissionsTable <- renderDT({
    res <- emissionsResult()
    if(!res$error) {
      approach <- input$approach
      target <- input$yearMonthTarget
      
      # Create result table
      if(approach == \"annual\") {
        time_label <- \"Year\"
        time_value <- target
      } else if(approach == \"monthly\") {
        time_label <- \"Month\"
        time_value <- target
      } else {
        time_label <- \"Year\"
        time_value <- target
      }
      
      result_df <- data.frame(
        Approach = approach,
        TimeLabel = time_label,
        TimeValue = time_value,
        Emissions_tCO2e = round(res$result, 2)
      )
      
      datatable(result_df, options=list(pageLength=10, dom='t'))
    }
  })

  # Create the emissions plot
  output$emissionsPlot <- renderPlot({
    res <- emissionsResult()
    if(!res$error) {
      approach <- input$approach
      val <- res$result
      
      # Create plot data
      plot_data <- data.frame(
        Approach = factor(approach, levels=c(\"annual\", \"monthly\", \"simplified\"),
                        labels=c(\"Annual\", \"Monthly\", \"Simplified\")),
        Emissions = val
      )
      
      # Create plot
      ggplot(plot_data, aes(x=Approach, y=Emissions)) +
        geom_col(fill=\"steelblue\", width=0.5) +
        theme_minimal() +
        labs(title=\"Estimated CH4 Emissions\", 
             subtitle=paste(\"Target:\", input$yearMonthTarget, 
                           ifelse(approach==\"monthly\", \"(month)\", \"(year)\")),
             x=\"\", y=\"t CO2e\") +
        geom_text(aes(label=round(Emissions, 1)), vjust=-0.5, size=5) +
        theme(axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=10),
              plot.title = element_text(size=16, face=\"bold\"),
              plot.subtitle = element_text(size=12))
    }
  })
  
  # Download handler for the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(\"SWDS_emissions_\", Sys.Date(), \".png\", sep=\"\")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = \"png\", width = 8, height = 6, dpi = 300)
    }
  )
}

shinyApp(ui, server)
"
writeLines(shiny_app_R, "SWDSFODR/inst/shiny-app/app.R")
