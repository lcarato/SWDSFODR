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
        tabPanel("Results Table", DTOutput("emissionsTable")),
        tabPanel("Bar Plot", plotOutput("emissionsPlot")),
        tabPanel("Area Plot", plotOutput("areaPlot")),
        tabPanel("Decay Chart", plotOutput("decayChart"))
      )
    )
  )
)

server <- function(input, output, session) {

  # --- 1) Load CSV data ---
  wasteData <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
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
        cumulative <- sapply(years, function(y) {
          calcSWDSEmissionsYearly(W = df, DOCj = DOCj, k_j = k_j,
                                  phi_y = phi_val, f_y = f_val, GWP_CH4 = gwp,
                                  OX = ox_val, F = F_, DOCf_y = docf,
                                  MCF_y = mcf, year_target = y)
        })
      } else {
        cumulative <- sapply(years, function(y) {
          calcSWDSEmissionsSimplified(W = df, phi_y = phi_val,
                                      f_y = f_val, GWP_CH4 = gwp,
                                      climate_zone = cz, year_target = y)
        })
      }
      incremental <- c(cumulative[1], diff(cumulative))
      ts_df <- data.frame(Time = years, Incremental = incremental, Cumulative = cumulative)
      ts_df$Incremental_GgCH4 <- ts_df$Incremental / (gwp * 1000)
      ts_df$Cumulative_GgCH4  <- ts_df$Cumulative  / (gwp * 1000)
      ts_df
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
      ts_df$Incremental_GgCH4 <- ts_df$Incremental / (gwp * 1000)
      ts_df$Cumulative_GgCH4  <- ts_df$Cumulative  / (gwp * 1000)
      ts_df
    }
  })

  # --- 4) Decay Data for Each Deposit (Annual Only) ---
  decayData <- eventReactive(input$calcBtn, {
    if (input$approach != "annual") return(NULL)
    df <- wasteData()

    # Hardcode sample DOCj/k_j (user can adapt for real usage)
    DOCj <- c(food = 0.15, paper = 0.4, other = 0.2)
    k_j  <- c(food = 0.06, paper = 0.04, other = 0.05)

    # Factor outside sums (same as in calcSWDSEmissionsYearly):
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
  stackedDecayData <- function(df_decay) {
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
      labs(title = "Estimated CH4 Emissions",
           x = "",
           y = "Emissions (t CO2e)")
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

  # Decay Chart (stacked lines) for each deposit year (Annual only)
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
        # Convert to stacked lines
        df_stacked <- stackedDecayData(df_decay)

        # Convert to GgCH4
        df_stacked <- df_stacked %>%
          mutate(Stacked_Emission_GgCH4 = Stacked_Emission_tCO2e / (input$gwpslid * 1000))

        ggplot(df_stacked,
               aes(x = EvaluationYear,
                   y = Stacked_Emission_tCO2e,
                   color = factor(DepositYear))) +
          geom_col() +
          scale_y_continuous(
            sec.axis = sec_axis(~ . / (input$gwpslid * 1000),
                                name = "Emissions (Gg CH4)")
          ) +
          labs(
            title = "Stacked Lines: Decay of Emissions by Deposit Year",
            subtitle = "Each line is cumulative up to that deposit year",
            x = "Evaluation Year",
            y = "Emissions (t CO2e)",
            color = "Deposit Year"
          ) +
          theme_minimal()
      }
    }
  })
}

shinyApp(ui, server)

shinyApp(ui, server)
