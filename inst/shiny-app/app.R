# app.R
# A Shiny app for the SWDSFODR package with stacked lines for Decay Chart

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
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
      numericInput("yearMonthTarget", "Year or Month Target", 25, min = 1, step = 1),
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
        tabPanel("Results Table", 
                 DTOutput("emissionsTable"),
                 downloadButton("downloadEmissionsTable", "Export to CSV")),
        
        tabPanel("Bar Plot", 
                 plotOutput("emissionsPlot"),
                 h4("Annual Values"),
                 DTOutput("barPlotTable"),
                 downloadButton("downloadBarPlotTable", "Export to CSV")),
        
        tabPanel("Area Plot", 
                 plotOutput("areaPlot"),
                 h4("Time Series Data"),
                 DTOutput("areaPlotTable"),
                 downloadButton("downloadAreaPlotTable", "Export to CSV")),
        
        tabPanel("Decay Chart", 
                 plotOutput("decayChart"),
                 h4("Annual Decay Values"),
                 DTOutput("decayTable"),
                 downloadButton("downloadDecayTable", "Export to CSV"))
      )
    )
  )
)

server <- function(input, output, session) {

  # --- 1) Load CSV data ---
  wasteData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = FALSE)
  })

  # --- 2) Single-value Emissions Calculation ---
  emissionsResult <- eventReactive(input$calcBtn, {
    df <- wasteData()
    approach <- input$approach
    phi_val  <- input$phi
    f_val    <- input$f
    gwp      <- input$gwpslid
    ox_val   <- input$ox
    F_       <- input$Fval
    docf     <- input$docf
    mcf      <- input$mcf
    target   <- input$yearMonthTarget
    cz       <- input$climate_zone

    if (approach == "annual") {
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
      res_val <- calcSWDSEmissionsYearly(
        W = df, DOCj = DOCj, k_j = k_j,
        phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
        OX = ox_val, F = F_, DOCf_y = docf,
        MCF_y = mcf, year_target = target
      )
    } else if (approach == "monthly") {
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
      res_val <- calcSWDSEmissionsMonthly(
        W = df, DOCj = DOCj, k_j = k_j,
        phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
        OX = ox_val, F = F_, DOCf_m = docf,
        MCF_y = mcf, month_target = target
      )
    } else {
      res_val <- calcSWDSEmissionsSimplified(
        W = df,
        phi_y = phi_val,
        f_y = f_val,
        GWP_CH4 = gwp,
        climate_zone = cz,
        year_target = target
      )
    }
    # Convert to both t CO2e and Gg CH4
    data.frame(
      Emissions_tCO2e = res_val,
      Emissions_GgCH4 = res_val / (gwp * 1000)
    )
  })

  # --- 3) Time Series (Cumulative & Incremental) ---
  timeSeriesData <- eventReactive(input$calcBtn, {
    df <- wasteData()
    approach <- input$approach
    phi_val  <- input$phi
    f_val    <- input$f
    gwp      <- input$gwpslid
    ox_val   <- input$ox
    F_       <- input$Fval
    docf     <- input$docf
    mcf      <- input$mcf
    cz       <- input$climate_zone

    if (approach %in% c("annual", "simplified")) {
      years <- seq(min(df$year), input$yearMonthTarget)
      if (approach == "annual") {
        DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
        k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
        incremental <- sapply(years, function(y) {
          calcSWDSEmissionsYearly(W = df, DOCj = DOCj, k_j = k_j,
                                  phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
                                  OX = ox_val, F = F_, DOCf_y = docf,
                                  MCF_y = mcf, year_target = y)
        })

        incremental <- pmax(incremental, 0)  # Ensure incremental emissions are non-negative
        cumulative <- cumsum(incremental)    # Correct cumulative emissions calculation
      } else {
        # For simplified approach
        cumulative <- sapply(years, function(y) {
          calcSWDSEmissionsSimplified(W = df, phi_y = phi_val,
                                      f_y = f_val, GWP_CH4 = gwp,
                                      climate_zone = cz, year_target = y)
        })
        # Calculate incremental from cumulative
        incremental <- c(cumulative[1], diff(cumulative))
        incremental <- pmax(incremental, 0)  # Ensure non-negative
      }
      ts_df <- data.frame(Time = years, Incremental = incremental, Cumulative = cumulative)
      ts_df$Incremental_GgCH4 <- ts_df$Incremental / (gwp * 1000)
      ts_df$Cumulative_GgCH4  <- ts_df$Cumulative  / (gwp * 1000)
      ts_df
    } else if (approach == "monthly") {
      months <- seq(min(df$month), input$yearMonthTarget)
      DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
      k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)
      incremental <- sapply(months, function(m) {
        calcSWDSEmissionsMonthly(W = df, DOCj = DOCj, k_j = k_j,
                                 phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
                                 OX = ox_val, F = F_, DOCf_m = docf,
                                 MCF_y = mcf, month_target = m)
      })
      
      incremental <- pmax(incremental, 0)  # Ensure non-negative values
      incremental <- pmax(incremental, 0)  # Ensure non-negative values
      cumulative <- cumsum(incremental)    # Calculate cumulative values
      
      ts_df <- data.frame(Time = months, Incremental = incremental, Cumulative = cumulative)
      ts_df$Incremental_GgCH4 <- ts_df$Incremental / (gwp * 1000)
      ts_df$Cumulative_GgCH4  <- ts_df$Cumulative  / (gwp * 1000)
      ts_df
    }
  })

  # --- 4) Decay Data for Each Deposit (Annual Only) ---
  decayData <- eventReactive(input$calcBtn, {
    validate(need(input$approach == "annual", "Decay chart available only for Annual Approach"))
    df <- wasteData()

    # Hardcode sample DOCj/k_j (user can adapt for real usage)
    DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
    k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)

    outside_factor <- input$phi * (1 - input$f) * input$gwpslid * (1 - input$ox) *
                      (16/12) * input$Fval * input$docf * input$mcf

    raw <- data.frame()
    for (i in seq_len(nrow(df))) {
      deposit_year <- df$year[i]
      waste_type   <- as.character(df$waste_type[i])
      mass         <- df$mass_tonnes[i]
      if (!waste_type %in% names(DOCj)) next  # skip unknown types

      d_j   <- DOCj[[waste_type]]
      k_val <- k_j[[waste_type]]

      # Evaluate from deposit_year to target
      for (y in deposit_year:input$yearMonthTarget) {
        age <- y - deposit_year
        # Emissions from this deposit in year y:
        emission_tCO2e <- mass * d_j * exp(-k_val * age) * (1 - exp(-k_val)) * outside_factor

        raw <- rbind(raw, data.frame(
          EvaluationYear   = y,
          DepositYear      = deposit_year,
          Emission_tCO2e   = emission_tCO2e
        ))
      }
    }
    raw$Emission_GgCH4 <- raw$Emission_tCO2e / (input$gwpslid * 1000)
    raw
  })

  # Helper function to transform raw per‐deposit data into stacked lines
  stackedDecayData <- function(df_decay, gwpslid) {
    # Pivot wide so each DepositYear is a column
    df_wide <- df_decay %>%
      pivot_wider(names_from = DepositYear, values_from = Emission_tCO2e, values_fill = 0) %>%
      arrange(EvaluationYear)

    deposit_years <- sort(unique(df_decay$DepositYear))

    # Create cumulative "stacked_" columns
    for (i in seq_along(deposit_years)) {
      col_i <- as.character(deposit_years[i])
      if (i == 1) {
        df_wide[[paste0("stacked_", col_i)]] <- df_wide[[col_i]]
      } else {
        prev_col <- paste0("stacked_", deposit_years[i - 1])
        df_wide[[paste0("stacked_", col_i)]] <- df_wide[[col_i]] + df_wide[[prev_col]]
      }
    }

    # Pivot longer to get stacked lines
    df_long <- df_wide %>%
      pivot_longer(
        cols = starts_with("stacked_"),
        names_to = "StackedDepositYear",
        values_to = "Stacked_Emission_tCO2e"
      ) %>%
      mutate(
        # Extract the numeric deposit year from "stacked_XX"
        DepositYear = as.numeric(gsub("stacked_", "", StackedDepositYear))
      ) %>%
      select(EvaluationYear, DepositYear, Stacked_Emission_tCO2e)

    # Also create a stacked GgCH4 version
    # We'll do that by referencing the GWP in the environment. 
    # Easiest is to left_join the raw wide data or recalc here if needed.
    # For clarity, let's do the ratio after we pivot. 
    # We can guess the same ratio as original: Emission_GgCH4 = Emission_tCO2e / (gwp * 1000).
    # But we need the GWP from input$gwpslid. We'll store that in the function or pass as an argument.
    df_long
  }

  # --- 5) Outputs ---

  # Table of single-value results
  output$emissionsTable <- renderDT({
    req(emissionsResult())
    datatable(emissionsResult(), options = list(pageLength = 5))
  })

  # Bar Plot of single-value results
  output$emissionsPlot <- renderPlot({
    req(emissionsResult())
    df_val   <- emissionsResult()
    approach <- input$approach

    ggplot(
      data = data.frame(
        Approach         = approach,
        Emissions_tCO2e  = df_val$Emissions_tCO2e,
        Emissions_GgCH4  = df_val$Emissions_GgCH4
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

  # Area Plot of incremental & cumulative time series
  output$areaPlot <- renderPlot({
    req(timeSeriesData())
    ts_df       <- timeSeriesData()
    period_label <- ifelse(input$approach == "monthly", "Month", "Year")

    ggplot(ts_df, aes(x = Time)) +
      geom_area(aes(y = Incremental), fill = "lightblue", alpha = 0.7) +
      geom_line(aes(y = Cumulative), color = "red", size = 1.5) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / (input$gwpslid * 1000),
                            name = "Emissions (Gg CH4)")
      ) +
      labs(
        title = "Emissions Over Time",
        subtitle = paste("Incremental", period_label, "Emissions & Cumulative Emissions"),
        x = period_label,
        y = "Emissions (t CO2e)"
      ) +
      theme_minimal()
  })

  # Decay Chart (stacked columns) for each deposit year (Annual only)
  output$decayChart <- renderPlot({
    if (input$approach != "annual") {
      # If not annual, show a placeholder
      ggplot() +
        annotate("text", x = 1, y = 1,
                 label = "Decay chart available only for Annual Approach",
                 size = 6) +
        theme_void()
    } else {
      req(decayData())
      df_decay <- decayData()
      if (nrow(df_decay) == 0) {
        ggplot() +
          annotate("text", x = 1, y = 1,
                   label = "No valid deposit data found.",
                   size = 6) +
          theme_void()
      } else {
        # Group by evaluation year and deposit year, then sum emissions
        df_grouped <- df_decay %>%
          group_by(EvaluationYear, DepositYear) %>%
          summarise(Emission_tCO2e = sum(Emission_tCO2e), .groups = 'drop') %>%
          mutate(Emission_GgCH4 = Emission_tCO2e / (input$gwpslid * 1000))
        
        # Create stacked column chart
        ggplot(df_grouped,
               aes(x = EvaluationYear,
                   y = Emission_tCO2e,
                   fill = factor(DepositYear))) +
          geom_col(position = "stack") +
          scale_y_continuous(
            sec.axis = sec_axis(~ . / (input$gwpslid * 1000),
                              name = "Emissions (Gg CH4)")
          ) +
          labs(
            title = "Stacked Columns: Decay of Emissions by Deposit Year",
            subtitle = "Each column shows emissions from waste deposited in different years",
            x = "Evaluation Year",
            y = "Emissions (t CO2e)",
            fill = "Deposit Year"
          ) +
          theme_minimal()
      }
    }
  })

  # Add these after the existing plot outputs

  # Table for bar plot values
  output$barPlotTable <- renderDT({
    req(emissionsResult())
    
    # Get the data and approach
    df_val <- emissionsResult()
    approach <- input$approach
    
    # Create a table with more details
    table_data <- data.frame(
      Approach = approach,
      Year_or_Month = input$yearMonthTarget,
      Emissions_tCO2e = df_val$Emissions_tCO2e,
      Emissions_GgCH4 = df_val$Emissions_GgCH4
    )
    
    datatable(table_data, 
              options = list(pageLength = 5),
              rownames = FALSE) %>%
      formatRound(columns = c("Emissions_tCO2e", "Emissions_GgCH4"), digits = 4)
  })

  # Table for decay chart values
  output$decayTable <- renderDT({
    req(input$approach == "annual")
    req(decayData())
    
    df_decay <- decayData()
    
    if (nrow(df_decay) == 0) {
      return(NULL)
    }
    
    # Group by evaluation year and deposit year for the table
    df_table <- df_decay %>%
      group_by(EvaluationYear, DepositYear) %>%
      summarise(
        Emission_tCO2e = sum(Emission_tCO2e),
        Emission_GgCH4 = sum(Emission_GgCH4),
        .groups = 'drop'
      ) %>%
      arrange(EvaluationYear, DepositYear)
    
    # Create a pivot table with deposit years as columns
    df_pivot <- df_table %>%
      pivot_wider(
        id_cols = EvaluationYear,
        names_from = DepositYear,
        values_from = c(Emission_tCO2e, Emission_GgCH4),
        values_fill = 0
      )
    
    # Add a total column
    df_pivot$Total_tCO2e <- rowSums(select(df_pivot, starts_with("Emission_tCO2e")))
    df_pivot$Total_GgCH4 <- rowSums(select(df_pivot, starts_with("Emission_GgCH4")))
    
    datatable(df_pivot, 
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatRound(columns = grep("Emission|Total", names(df_pivot)), digits = 4)
  })

  # Table for area plot time series data
  output$areaPlotTable <- renderDT({
    req(timeSeriesData())
    
    # Get the time series data
    ts_df <- timeSeriesData()
    period_label <- ifelse(input$approach == "monthly", "Month", "Year")
    
    # Rename the Time column to be more descriptive
    names(ts_df)[names(ts_df) == "Time"] <- period_label
    
    # Format and display the table
    datatable(ts_df, 
              options = list(
                pageLength = 10,
                scrollX = TRUE
              ),
              rownames = FALSE) %>%
      formatRound(columns = c("Incremental", "Cumulative", 
                             "Incremental_GgCH4", "Cumulative_GgCH4"), 
                  digits = 4)
  })

  # Add these download handlers to the server function

  # Download handler for emissions table
  output$downloadEmissionsTable <- downloadHandler(
    filename = function() {
      paste("emissions-results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(emissionsResult(), file, row.names = FALSE)
    }
  )

  # Download handler for bar plot table
  output$downloadBarPlotTable <- downloadHandler(
    filename = function() {
      paste("bar-plot-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create the table data
      df_val <- emissionsResult()
      approach <- input$approach
      
      table_data <- data.frame(
        Approach = approach,
        Year_or_Month = input$yearMonthTarget,
        Emissions_tCO2e = df_val$Emissions_tCO2e,
        Emissions_GgCH4 = df_val$Emissions_GgCH4
      )
      
      write.csv(table_data, file, row.names = FALSE)
    }
  )

  # Download handler for area plot time series data
  output$downloadAreaPlotTable <- downloadHandler(
    filename = function() {
      paste("time-series-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Prepare time series data
      ts_df <- timeSeriesData()
      period_label <- ifelse(input$approach == "monthly", "Month", "Year")
      
      # Rename the Time column
      names(ts_df)[names(ts_df) == "Time"] <- period_label
      
      write.csv(ts_df, file, row.names = FALSE)
    }
  )

  # Download handler for decay table
  output$downloadDecayTable <- downloadHandler(
    filename = function() {
      paste("decay-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(input$approach == "annual")
      df_decay <- decayData()
      
      if (nrow(df_decay) == 0) {
        # Create an empty dataframe with message
        empty_df <- data.frame(Message = "No decay data available")
        write.csv(empty_df, file, row.names = FALSE)
      } else {
        # Group by evaluation year and deposit year for the table
        df_table <- df_decay %>%
          group_by(EvaluationYear, DepositYear) %>%
          summarise(
            Emission_tCO2e = sum(Emission_tCO2e),
            Emission_GgCH4 = sum(Emission_GgCH4),
            .groups = 'drop'
          ) %>%
          arrange(EvaluationYear, DepositYear)
        
        # Create a pivot table with deposit years as columns
        df_pivot <- df_table %>%
          pivot_wider(
            id_cols = EvaluationYear,
            names_from = DepositYear,
            values_from = c(Emission_tCO2e, Emission_GgCH4),
            values_fill = 0
          )
        
        # Add a total column
        df_pivot$Total_tCO2e <- rowSums(select(df_pivot, starts_with("Emission_tCO2e")))
        df_pivot$Total_GgCH4 <- rowSums(select(df_pivot, starts_with("Emission_GgCH4")))
        
        write.csv(df_pivot, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui, server)
