
# load in libraries, helper files, and dataset #####

library(shiny)
library(bslib)
library(readr) # read_csv
library(DT) # interactive data table
library(texreg)
library(xml2)

source("dashboard_funcs.R")

# Load default dataset
default_dat = read_csv("buses_with_price_per_seat_and_converted_dates.csv")

# footer #####
footer <- tags$footer(
  style = "
    margin-top: 24px;
    padding: 12px 0;
    font-size: 12px;
    color: #666;
    text-align: center;
    border-top: 1px solid #eee;
  ",
  HTML(
    paste(
      "Created by Louise Smith, Sonia Su, Karishni Veerabahu Pillai, Raveena Kumari, and Siva Selvam",
      "© 2025",
      "Last updated: January 22, 2026",
      sep = " • "
    )
  )
)


# ui #####
ui <- navbarPage(
  title = "EV Bus Cost Simulator",

  ## home #####
  tabPanel(
    icon("home"), 
    fluidPage(
      # Add file upload widget at the top
      fluidRow(
        column(
          12,
          wellPanel(
            style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
            fluidRow(
              column(
                6,
                fileInput(
                  inputId = "file_upload",
                  label = "Upload Your CSV File (optional):",
                  accept = c(".csv"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                )
              ),
              column(
                6,
                div(
                  style = "padding-top: 8px;",
                  p(strong("Instructions:"), "Upload a CSV file with the same structure as the default dataset. Required columns include: source_type, purchase_year, bus_manufacturer, bus_model, bus_type, seating_capacity, base_price, special_needs_bus, state, vehicle_dealer, source, source_url, date_published_or_updated."),
                  actionButton("reset_data", "Reset to Default Data", class = "btn-secondary btn-sm")
                )
              )
            )
          )
        )
      ),

      h3("Project Objectives"),
      p("The primary objective is to apply methods to identify the key drivers of electric school bus procurement price variation on a per-seat basis and determine which controllable factors most effectively influence price variability across manufacturers, bus types, and states. A secondary objective is to develop a framework grounded in statistical analysis that helps us understand market behavior in relation to procurement."),
      p("By using ANOVA, we will test for significant differences in mean procurement prices across categorical variables such as manufacturer, bus type, and dealer. By leveraging regression, we will quantify the effect of continuous variables on the dependent variable, the price per seat. Bootstrapped simulations will be used to model uncertainty in market composition and estimate the impact of varying manufacturer dominance and procurement conditions."),

      h3("Dataset"),
      p("We are using the Electric School Bus Price Tracker — State-Level Base Prices", 
      a("dataset,", href = "https://github.com/timothyfraser/sts/tree/3week/data/electric_school_buses"),
      "which contains state-level base prices for electric school buses compiled from publicly available state contracts and procurement sources and was developed by ElectricSchoolBusInitiative.org."
      ),
      p("Dates were converted from Excel serial dates to YYYY-MM-DD. Price per seat was calculated by dividing the base price by the seating capacity. For analyses done based on price per seat, entries with no seating capacity data were omitted."),
      br(),
      fluidRow(
        column(
          12, # full page width
          DTOutput("bus_data")
        )
      )

    )
  ),

  ## background #####
  tabPanel(
    title = "Background",
    fluidPage(
      h3("Project Background"),
      p("Electric school buses are expanding across the United States. However, their procurement prices vary widely for similar models. Districts must make long-term investment decisions without clear pricing standards or predictable cost behavior. Procurement records from 2020 to 2023 show significant differences across states, manufacturers, and dealers. These inconsistencies create equity challenges for districts that rely on stable budgets and transparent pricing. Base prices average about $347,232, so even minor variations represent considerable financial consequences. The problem appears during procurement milestones when districts evaluate bids and select vendors. The variation spans 16 states, 12 manufacturers, and 45 dealers, showing broad national inconsistency. Districts risk overspending when they cannot identify which factors drive cost differences. These uncertainties slow the adoption of clean transportation solutions and weaken planning efforts. This project aims to understand why prices vary and how data can support fairer procurement."),
      h3("Why this matters"),
      p("Electrification of school bus fleets is a primary national goal for decarbonization and public health. Districts need clear pricing insights to budget responsibly and avoid overpaying for similar buses. Understanding price drivers helps policymakers design better incentives and procurement rules. Improved pricing clarity can speed up adoption and reduce financial barriers for early adopters. Procurement decisions made today will shape fleet composition and cost savings for many years. Ignoring these discrepancies would allow inconsistent pricing to persist across states and vendors. Districts may continue paying uneven prices that strain budgets and reduce access to clean buses. This project supports clean energy goals, equity in public procurement, and waste reduction. Lean Six Sigma tools help identify and reduce unwanted variation in the procurement process. The work is timely because large-scale electrification efforts are underway now."),
      h3("Research Questions"),
      h4("Primary Question"),
      tags$ul(
        tags$li("What factors drive variation in electric school bus procurement prices on a per-seat basis?"),
      ),

      h4("Sub-Questions"),
      tags$ul(
        tags$li("Do manufacturer, bus type, state, or dealer create significant differences in mean prices?"),
        tags$li("Which continuous variables meaningfully affect price when modeled with regression?"),
        tags$li("How much total variation can be explained using ANOVA, SPC, and Regression tools?"),
        tags$li("What level of uncertainty remains after statistical modeling? Can these findings help districts make more consistent and cost-effective purchasing decisions?")
      ),

      h3("Literature Review"),
      p("Electric school buses are technologically mature but still face barriers related to durability, cold-climate performance, and long-term service infrastructure (Lee & Chard, 2023). Although upfront prices remain two to three times higher than those of diesel buses, lower maintenance costs and substantial health and climate benefits can offset this premium over a vehicle’s lifetime (ChargEVC Study Group, 2023; Choma et al., 2024). Adoption remains uneven, with low-income and minority communities disproportionately exposed to diesel pollution and receiving fewer clean transportation benefits (Moses & Brown, 2022). Studies show that purchase cost is the most influential driver of cost-effectiveness for battery-electric buses, making procurement pricing a central barrier to electrification (Avenali et al., 2023). Research also finds that combining distributed energy resources, smart charging, and potential V2G services can reduce operational costs and improve financial outcomes for districts (Becker et al., 2019; Noel & McCormack, 2014; Chen et al., 2023). Successful deployment requires early planning, workforce training, depot space readiness, charger interoperability, and reliable parts availability (Center for Urban Transportation Research, 2023). Best-practice studies highlight the need to match routes with battery range, grid capacity, and community priorities to optimize total cost of ownership (C40 Cities Climate Leadership Group, n.d.; Panta et al., 2024). Institutional constraints and outdated procurement norms limit the effectiveness of electric bus contracts, requiring new frameworks that align responsibilities with emerging technologies (Aslund et al., 2025; Kapatsila et al., 2024). Innovative financing models such as battery leasing, bus-as-a-service, joint procurement, and negotiated contracting can reduce upfront prices and stabilize long-term fleet costs (Li et al., 2018; Hensher, 2021; Hensher, 2022). Across case studies, aggregation, standardization, cooperative purchasing, and risk-sharing partnerships consistently lower costs and accelerate adoption while delivering major health and economic benefits (Dolman & Madden, 2018; Smyth et al., 2020; Plotnick & Pierce, 2021; Oester & Woodrum, 2024; Bursey & Kalisa, 2025).")
    )
  ),

  ## anova #####
  tabPanel(
    title = "ANOVA",
    div(
      style = "padding-left: 15px",
      tabsetPanel(
        tabPanel(
          "Single ANOVA",
          sidebarLayout(
            div(
              style = "padding-top: 15px",
              sidebarPanel(
                # select factor col
                selectInput(
                  inputId = "factor_col",
                  label = "Select a Category:",
                  choices = c(
                    "Purchase Year",
                    "Bus Manufacturer",
                    "Bus Model",
                    "Bus Type",
                    "Special Needs",
                    "State",
                    "Vehicle Dealer"
                  ),
                  selected = "State"
                ),
                h3("How to interpret:"),
                p("A significant one-way ANOVA result (p < 0.05) indicates that at least one group mean differs from the others. A non-significant p-value suggests no evidence of meaningful price differences across the tested categories."),
                tags$ul(
                  tags$li("P-value < 0.05 → The categorical factor has a statistically significant effect on the bus base price."),
                  tags$li("P-value ≥ 0.05 → No evidence of the category having a meaningful effect on bus base price.")
                )
              )
            ),
            mainPanel(
              h2("Single ANOVA Charts"),
              p("One-way Analysis of Variance (ANOVA) tests whether the mean bus prices differed across levels of a single categorical variable (e.g., state, bus type, manufacturer). For each factor, the response variable (base_price) was modeled as a function of the categorical groups. ANOVA compares the variability between group means to the variability within groups to determine whether any observed differences exceed what would be expected by random variation alone. Further post-hoc tests can be run to determine which groups specifically differed from one another."),
              plotOutput("single_anova")
            )
          )
        ),

        tabPanel(
          "Grouped ANOVA",
          sidebarLayout(
            div(
              style = "padding-top: 15px",
              sidebarPanel(
                # select factor col
                selectInput(
                  inputId = "factor_col_g",
                  label = "Select a Category:",
                  choices = c(
                    "Purchase Year",
                    "Bus Manufacturer",
                    "Bus Model",
                    "Special Needs",
                    "State",
                    "Vehicle Dealer"
                  ),
                  selected = "State"
                ),
                h3("How to interpret:"),
                p("A significant one-way ANOVA result (p < 0.05) indicates that at least one group mean differs from the others. A non-significant p-value suggests no evidence of meaningful price differences across the tested categories."),
                p("P-value < 0.05 → The categorical factor has a statistically significant effect on the bus base price."),
                p("P-value ≥ 0.05 → No evidence of the category having a meaningful effect on bus base price.")
              )
            ),
            mainPanel(
              h2("Grouped ANOVA Charts"),
              p("The dataset was first split according to a grouping variable (such as bus type), and a separate one-way ANOVA was conducted within each subgroup. For every subgroup, the model tested whether mean differences across levels of the factor were larger than would be expected by random variation."),
              plotOutput("grouped_anova")
            )
          )
        ),
      )
    )
  ),

  ## spc #####
  tabPanel(
    title = "SPC Charts",
    fluidPage(
      
      sidebarPanel(
        # select bus type
        selectInput(
          inputId = "bus_type",
          label = "Select a Bus Type:",
          choices = c(
            "Type A",
            "Type C",
            "Type D"
          ),
          selected = "Type C"
        ),

        # select categorical variable
        selectInput(
          inputId = "cat_var",
          label = "Select a Category:",
          choices = c(
            "Purchase Year",
            "Bus Manufacturer",
            "Bus Model",
            "Special Needs",
            "State",
            "Vehicle Dealer"
          ),
          selected = "Bus Manufacturer"
        ),

        # select base price vs price per seat
        selectInput(
          inputId = "bp_vs_pps",
          label = "Select a Pricing Structure:",
          choices = c(
            "Base Price",
            "Price Per Seat"
          ),
          selected = "Base Price"
        ),
        h3("How to interpret:"),
        tags$ul(
          tags$li("Points below the mean - Manufacturer has a lower-than-average price per seat for that bus type; this represents better cost efficiency relative to the population average."),
          tags$li("Points above the mean - Manufacturer has a higher-than-average price per seat for that bus type; this represents higher costs relative to the population average."),
          tags$li("Points below the lower control limit - Manufacturer is an outlier with a lower average price per seat"),
          tags$li("Points above the upper control limit - Manufacturer is an outlier with a higher average price per seat"),
        )
      ),
      mainPanel(
        h2("SPC Charts"),
        p("Statistical Process Control (SPC) charts were used to visualize and compare the average price per seat across different bus manufacturers within each bus type category. For each bus type (Type A, Type C, and Type D), the average price per seat was calculated for each manufacturer and plotted as individual points. Control limits were established at ±3 sigma (standard deviation) around the grand mean (overall average price per seat for all manufacturers within that bus type). These control limits define the expected range of variation. Manufacturers with average prices per seat falling outside these control limits are outliers, indicating performance that deviates from the typical variation observed across the population and suggesting they are more or less expensive than the average. Points above the upper control limit indicate higher-than-expected costs, while points below the lower control limit indicate lower-than-expected costs. For each bus type, these charts help us identify which manufacturers are, on average, more cost-efficient in terms of price per seat."),
        plotOutput("spc_chart")
      )
    )
  ),

  ## regression #####
  tabPanel(
    title = "Regression",
    fluidPage(
      column(
        width = 5, # left column (4/12 of the width)

        h2("Multivariate Regression"),

        # select bus type
        selectInput(
          inputId = "bus_type_reg",
          label = "Select a Bus Type:",
          choices = c(
            "Type A",
            "Type C",
            "Type D"
          ),
          selected = "Type C"
        ),
        p("Multivariable linear regression was used to assess how several categorical and numerical predictors jointly influence bus base price. By modeling all variables simultaneously (e.g., bus type, manufacturer, seating capacity, model year), the analysis isolates each predictor’s unique effect while controlling for the others. Regression coefficients (β) indicate the direction and size of these effects, positive values reflect higher predicted prices, negative values reflect lower prices, and associated p-values determine whether each relationship is statistically significant."),
        h3("How to interpret:"),
        h4("Coefficient sign (β)"),
        tags$ul(
          tags$li("⁠Positive β: Predictor is associated with higher bus prices"),
          tags$li("⁠Negative β: Predictor is associated with lower bus prices"),
        ),
        h4("Effect size"),
        tags$ul(
          tags$li("⁠Larger absolute β values indicate stronger influence on bus price")
        ),
        h4("P-values"),
        tags$ul(
          tags$li("⁠p < 0.05: The predictor has a statistically significant effect on bus pricing, controlling for all other variables."),
          tags$li("p ≥ 0.05: No evidence of a meaningful independent effect in the multivariable context.")
        )
      ),
      column(
        width = 7,
        uiOutput("html_reg")
      )
    )
  ),

  ## bootstrapping #####
  tabPanel(
    title = "Bootstrapping",
    fluidPage(
      
      sidebarPanel(
        numericInput(
          inputId = "boot_reps",
          label   = "Simulation Size:",
          value   = 500,   # default
          min     = 100,
          max     = 5000,
        ),

        numericInput(
          inputId = "seed",
          label   = "Random Seed",
          value   = 50,   # default
        ),

        # bus type for bootstrapping
        selectInput(
          inputId = "bus_type_boot",
          label   = "Bus type:",
          choices = c("Type A", "Type C", "Type D"),
          selected = "Type C"
        ),

        # manufacturer depends on bus_type_boot
        selectInput(
          inputId = "bus_manuf",
          label   = "Bus manufacturer:",
          choices = NULL,  # Will be populated dynamically
          selected = NULL
        ),
        h3("How to interpret:"),
        p("The plot shows the shows the relationship between average price per seat and percentage of buses made by by selected manufacturer for a given bus type."),
        p("The R^2 value for the line of best fit describes the percentage of the variation in the dataset that the model accounts for. ")
      ),

      mainPanel(
        h2("Bootstrapping Simulations"),
        p("Bootstrapping is a statistical method that involves resampling a given dataset to create simulated samples. Performing this technique multiple times and evaluating descriptive statistics for a dataset can yield insights into the sampling distribution of a statistic of interest, and can also be used to simulate what the data might look like under different scenarios."),
        p("To evaluate how the average price per seat changes as the percentage of different bus manufacturers within the school bus fleet changes, simulated data were created by sampling from the original dataset based on manufacturers of interest."),
        p("Example: We were interested in evaluating what the average price per seat for Type D buses would be if 33% of the school bus fleet were made by GreenPower. There are 21 Type D bus contracts in the original dataset, so we would sample from the pool of Type D GreenPower contracts 7 times with replacement, and sample from the pool of Type D non-GreenPower contracts 14 times with replacement to get new sample of 21 contract, giving us a single bootstrapped sample, from which we can calculate the percentage of GreenPower buses and the average price per seat. We can repeat this process many times to find the sampling distribution for average price per seat in this scenario."),
        plotOutput("boot_chart")
      )
    )
  ),

  ## discussion #####
  tabPanel(
    title = "Discussion",
    fluidPage(
      h3("Financial Impacts"),
      p("Each bus type was analysed through bootstrapping simulations to understand the financial impact of increasing the percentage of different bus manufacturers within the electrified school bus fleet. For each simulated scenario, bootstrapping was performed by sampling the original dataset with replacement to create a new simulated dataset. To evaluate the change in average bus price for the scenarios of interest, bootstrapping simulations were generated from the original dataset to estimate the baseline distribution of the average bus price per seat. To simulate an increase in the market share of a given bus manufacturer, another set of bootstrapping simulations was generated by randomly sampling from the pool of contracts involving the bus manufacturer of interest. The difference in average price per seat between the two scenarios was calculated, and 90% confidence intervals were computed."),
      h4("Type A Buses"),
      p("There are 9 manufacturers that make Type A buses. Increasing the percentage of buses made by Lightning eMotors/Collins Bus from 5% to 30% resulted in a decrease in average price per seat of $1655. The 90% confidence interval for the decrease in average price per seat is $2999 to $399."),
      h4("Type C and Type D Buses"),
      p("There are four manufacturers that make Type C buses, and three for Type D buses. The difference between each manufacturer’s average price per seat is much smaller for Type C and Type D buses than for Type A buses. When evaluating the impacts of increasing the percentage of the bus manufacturers with the lowest price per seat, a much greater increase must occur before the difference is statistically significant. For Type C buses, Lion Electric has the lowest price per seat. Increasing the percentage of Lion Electric buses from 3% to 70% resulted in a decrease in average price per seat of $303, with a 90% confidence interval of $556 to $62. For Type D buses, increasing the percentage of GreenPower from 14% to 90% resulted in a decrease in average price per seat of $317 with a 90% confidence interval of $631 to $10."),
      h3("Recommendations"),
      p("From our analysis, focusing on procuring Type A buses from Lightning eMotors/Collins Bus has the greatest potential to reduce cost per seat for the electric school bus fleet. This analysis took into account , bus manufacturer, base price, and seating capacity across the three types of buses (Type A, Type C, Type D). Our analysis reveals that a statistically significant change in average price per seat can be achieved for Type A buses with only a 25% increased allocation. As such,school districts should focus on procuring less expensive Type A buses from Lightning eMotors/Collins Bus."),
      p("To mitigate the risks of overreliance on one supplier, Pegasus Zeus is an alternate supplier for Type A; however, since the price per seat for Type A buses made by Pegasus Zeus is greater than the price per seat for Type A buses made by Lightning eMotors/Collins Bus, a larger change in purchasing trends will be required to lead to a statistically significant difference in average price per seat."),
      p("From a different perspective of Type C and Type D buses, we can conclude that for Type C, Lion Electric is the preferred supplier followed by Blue Bird in order to mitigate the overreliance. From a Type D perspective, Green Power is the preferred supplier followed by Lion Electric in order to mitigate the overreliance on one supplier.")
    )
  ),
  footer
)

# server #####
server <- function(input, output, session) {

  # Reactive value to store the current dataset
  current_data <- reactiveVal(default_dat)
  
  # Make bus_manuf_choices reactive based on current data
  bus_manuf_choices <- reactive({
    req(current_data())
    
    dat <- current_data()
    
    list(
      "Type A" = dat[dat$bus_type == "Type A", ]$bus_manufacturer %>% 
                   unique() %>% 
                   na.omit() %>% 
                   as.character(),
      "Type C" = dat[dat$bus_type == "Type C", ]$bus_manufacturer %>% 
                   unique() %>% 
                   na.omit() %>% 
                   as.character(),
      "Type D" = dat[dat$bus_type == "Type D", ]$bus_manufacturer %>% 
                   unique() %>% 
                   na.omit() %>% 
                   as.character()
    )
  })
  
  # Initialize manufacturer choices on startup
  observe({
    req(bus_manuf_choices())
    
    choices <- bus_manuf_choices()[["Type C"]]  # Default to Type C
    choices <- choices[!is.na(choices)]
    
    updateSelectInput(
      session,
      "bus_manuf",
      choices  = choices,
      selected = if(length(choices) > 0) choices[1] else NULL
    )
  })
  
  # Handle file upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      # Read the uploaded CSV
      uploaded_dat <- read_csv(input$file_upload$datapath)
      
      # Calculate price_per_seat if it doesn't exist
      if (!"price_per_seat" %in% names(uploaded_dat)) {
        uploaded_dat$price_per_seat <- uploaded_dat$base_price / uploaded_dat$seating_capacity
      }
      
      # Update the reactive value
      current_data(uploaded_dat)
      
      # Show success message
      showNotification("File uploaded successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(
        paste("Error uploading file:", e$message), 
        type = "error", 
        duration = 5
      )
    })
  })
  
  # Handle reset button
  observeEvent(input$reset_data, {
    current_data(default_dat)
    showNotification("Data reset to default dataset.", type = "message", duration = 3)
  })
  
  # Create reactive subsets based on current data
  dat_A <- reactive({
    current_data()[current_data()$bus_type == "Type A", ]
  })
  
  dat_C <- reactive({
    current_data()[current_data()$bus_type == "Type C", ]
  })
  
  dat_D <- reactive({
    current_data()[current_data()$bus_type == "Type D", ]
  })

  ## bus data table #####
  output$bus_data = renderDT({
    datatable(current_data(), options = list(scrollX = TRUE))
  })

  ## single anova #####
  output$single_anova <- renderPlot({

    # x variable
    if (input$factor_col == "Purchase Year"){
      x = "purchase_year"
    } else if (input$factor_col == "Bus Manufacturer"){
      x = "bus_manufacturer"
    } else if (input$factor_col == "Bus Model"){
      x = "bus_model"
    } else if (input$factor_col == "Bus Type"){
      x = "bus_type"
    } else if (input$factor_col == "Special Needs"){
      x = "special_needs_bus"
    } else if (input$factor_col == "State"){
      x = "state"
    } else if (input$factor_col == "Vehicle Dealer"){
      x = "vehicle_dealer"
    } 

    run_single_anova(current_data(), 
      response_col = "base_price", factor_col = x,
      response_lab = "Base Price", factor_lab = input$factor_col
    )
  })

  ## grouped anova #####

  output$grouped_anova <- renderPlot({

    # x variable
    if (input$factor_col_g == "Purchase Year"){
      x = "purchase_year"
    } else if (input$factor_col_g == "Bus Manufacturer"){
      x = "bus_manufacturer"
    } else if (input$factor_col_g == "Bus Model"){
      x = "bus_model"
    } else if (input$factor_col_g == "Special Needs"){
      x = "special_needs_bus"
    } else if (input$factor_col_g == "State"){
      x = "state"
    } else if (input$factor_col_g == "Vehicle Dealer"){
      x = "vehicle_dealer"
    }

    run_grouped_anova(
      current_data(),
      group_by = "bus_type",
      factor_col = x,
      response_col = "base_price",
      group_by_lab = "Bus Type",
      factor_lab = input$factor_col_g,
      response_lab = "Base Price"
    )

  })


  ## spc charts #####
  output$spc_chart <- renderPlot({
    
    # bus type
    if (input$bus_type == "Type A"){
      data = dat_A()
    } else if (input$bus_type == "Type C"){
      data = dat_C()
    } else {
      data = dat_D()
    }
  
    # y variable
    if (input$bp_vs_pps == "Base Price"){
      y = data$base_price

      # x variable
      if (input$cat_var == "Purchase Year"){
        x = data$purchase_year
      } else if (input$cat_var == "Bus Manufacturer"){
        x = data$bus_manufacturer
      } else if (input$cat_var == "Bus Model"){
        x = data$bus_model
      } else if (input$cat_var == "Special Needs"){
        x = data$special_needs_bus
      } else if (input$cat_var == "State"){
        x = data$state
      } else if (input$cat_var == "Vehicle Dealer"){
        x = data$vehicle_dealer
      }


    } else {
      # remove entries where price_per_seat is NA
      data2 = data[!is.na(data$price_per_seat), ]

      y = data2$price_per_seat

      # x variable
      if (input$cat_var == "Purchase Year"){
        x = data2$purchase_year
      } else if (input$cat_var == "Bus Manufacturer"){
        x = data2$bus_manufacturer
      } else if (input$cat_var == "Bus Model"){
        x = data2$bus_model
      } else if (input$cat_var == "Special Needs"){
        x = data2$special_needs_bus
      } else if (input$cat_var == "State"){
        x = data2$state
      } else if (input$cat_var == "Vehicle Dealer"){
        x = data2$vehicle_dealer
      }
    }

    ggxbar_cat(x,y, 
      xlab = input$cat_var, ylab = paste0("Average ", input$bp_vs_pps), 
      subtitle = paste0(input$bus_type, " (n = ", data %>% nrow() %>% as.character(), ")"))
    
  })
  
  ## html regs #####
  output$html_reg <- renderUI({
    result <- regression_analysis_formatted(
      data = current_data(),
      bus_type_filter = input$bus_type_reg,
      sci_digits = 2  # Adjust as needed
    )
    HTML(result$html)
  })

  ## bootstrapped simulations #####

  ### dynamically update list of bus manufacturers #####
  observeEvent(input$bus_type_boot, {
    req(input$bus_type_boot)

    choices <- bus_manuf_choices()[[input$bus_type_boot]]  # Added () for reactive
    choices <- choices[!is.na(choices)]  # just in case

    updateSelectInput(
      session,
      "bus_manuf",
      choices  = choices,
      selected = if(length(choices) > 0) choices[1] else NULL
    )
  })
  
  # Update manufacturer choices when data changes
  observeEvent(current_data(), {
    req(input$bus_type_boot)
    
    choices <- bus_manuf_choices()[[input$bus_type_boot]]
    choices <- choices[!is.na(choices)]
    
    updateSelectInput(
      session,
      "bus_manuf",
      choices  = choices,
      selected = if(length(choices) > 0) choices[1] else NULL
    )
  })

  ### create bootstrapped charts #####
  output$boot_chart <- renderPlot({

    # remove NAs
    dat2 = current_data()[!is.na(current_data()$price_per_seat), ]
    boot_size = round(input$boot_reps/100)
    weights = seq(from = 0, to = 0.99, by = 0.01)

    # set seed
    set.seed(input$seed)

    # create scatter
    getScatter(dat2, boot_size, input$bus_type_boot, input$bus_manuf, weights)


  })


}

# run the app #####
shinyApp(ui = ui, server = server)