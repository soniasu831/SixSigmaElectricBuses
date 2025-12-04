# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
# https://shiny.posit.co/r/gallery/#user-showcase

# we liked this example!! 
# https://shiny.posit.co/r/gallery/education/didacting-modeling/

# install.packages("shiny")
# install.packages("DT")

# load in libraries, helper files, and dataset #####

library(shiny)
library(bslib)
library(readr) # read_csv
library(DT) # interactive data table

source("dashboard_funcs.R")

dat = read_csv("buses_with_price_per_seat_and_converted_dates.csv")

dat_A = dat[dat$bus_type == "Type A", ]
dat_C = dat[dat$bus_type == "Type C", ]
dat_D = dat[dat$bus_type == "Type D", ]

# valid manufacturers by bus type
bus_manuf_choices <- list(
  "Type A" = c(
    "Lightning eMotors/Collins Bus",
    "Micro Bird",
    "Collins Bus",
    "GreenPower",
    "Trans Tech",
    "Endera",
    "Thomas Built Buses",
    "Magellan",
    "Pegasus/Zeus Electric"
    # NA omitted on purpose
  ),
  "Type C" = c(
    "Thomas Built Buses",
    "Blue Bird",
    "IC Bus",
    "Lion Electric"
  ),
  "Type D" = c(
    "Blue Bird",
    "GreenPower",
    "Lion Electric"
  )
)

# ui #####
ui <- navbarPage(
  title = "Team 18",

  ## home #####
  tabPanel(
    icon("home"), 
    fluidPage(

      h3("Project Objectives"),
      p("The primary objective is to apply methods to identify the key drivers of electric school bus procurement price variation on a per-seat basis and determine which controllable factors most effectively influence price variability across manufacturers, bus types, and states. A secondary objective is to develop a framework grounded in statistical analysis that helps us understand market behavior in relation to procurement."),
      p("By using ANOVA, we will test for significant differences in mean procurement prices across categorical variables such as manufacturer, bus type, and dealer. By leveraging regression, we will quantify the effect of continuous variables on the dependent variable, the price per seat. Bootstrapped simulations will be used to model uncertainty in market composition and estimate the impact of varying manufacturer dominance and procurement conditions."),

      h3("Dataset"),
      p("We are using the Electric School Bus Price Tracker — State-Level Base Prices dataset", 
      a("(Fraser 2022),", href = "https://github.com/timothyfraser/sts/tree/3week/data/electric_school_buses"),
      "which contains state-level base prices for electric school buses compiled from publicly available state contracts and procurement sources."
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
                p("P-value < 0.05 → The categorical factor has a statistically significant effect on the bus base price."),
                p("P-value ≥ 0.05 → No evidence of the category having a meaningful effect on bus base price.")
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
    sidebarLayout(
      
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
        p("Categorical charts inspired by statistical process control (SPC) are used to compare subgroup averages. For each category of interest, subgroup means are calculated and plotted against the grand mean. Evaluating average cost metrics across different categories enables the team to identify the factors that contribute to lower costs."),
      ),
      mainPanel(
        h2("SPC Charts"),
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

        p("To quantify the impacts of different variables on bus pricing, regression models can be developed. By comparing beta coefficients, the team will be able to discern the direction and magnitude of the effect of different variables on bus pricing. Associated p-values can be used to determine the statistical significance of the identified relationships."),
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
    div(
      style = "padding-left: 15px",
      tabsetPanel(
 
        ### methodology #####
        tabPanel(
          "Methodology",
          h2("Bootstrapping Methodology"),
          p("Bootstrapping is a statistical method that involves resampling a given dataset to create simulated samples. Performing this technique multiple times and evaluating descriptive statistics for a dataset can yield insights into the sampling distribution of a statistic of interest, and can also be used to simulate what the data might look like under different scenarios. In this investigation, the team employed bootstrapping simulations to assess the impact of various purchasing strategies on the costs associated with school bus procurement."),
          p("In particular, the team evaluated how the average price per seat changes as the percentage of different bus manufacturers within the school bus fleet changes. For these scenarios, simulated data were created by selectively sampling from the original dataset based on manufacturers of interest. For example, if we were interested in evaluating what the average price per seat for Type D buses would be if 33% of the school bus fleet were made by GreenPower, we would split the dataset into two pools – one made up of Type D bus contracts for GreenPower buses, and one with all of the other Type D bus contracts. We would then examine the total number of Type D buses (21) and sample from the GreenPower pool 7 times with replacement, and sample from the non-GreenPower pool 14 times with replacement. That would give us a single bootstrapped sample. From these single bootstrapped samples, we can calculate the percentage of GreenPower buses and the average price per seat. We can repeat this process many times to find the sampling distribution for average price per seat in this scenario.")
        ),

        ### simulations #####
        tabPanel(
          "Simulations",

          sidebarLayout(
            
            div(
              style = "padding-top: 15px",
              # sidebar for selections
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
                  choices = bus_manuf_choices[["Type C"]],  # default matches selected type
                  selected = bus_manuf_choices[["Type C"]][1]
                ),
                h3("How to interpret:"),
                p("The plot shows the shows the relationship between average price per seat and percentage of buses made by by selected manufacturer for a given bus type.")
              )
            ),
            mainPanel(
              h2("Simulation Results"),
              plotOutput("boot_chart")
            )
          )
        )
      )
    )
  ),

  tabPanel(
    title = "Discussion",
    fluidPage(
      h3("Financial Impacts"),
      p("Each bus type was analysed through bootstrapping simulations to understand the financial impact of increasing the percentage of different bus manufacturers within the electrified school bus fleet. For each simulated scenario, bootstrapping was performed by sampling the original dataset with replacement to create a new simulated dataset. To evaluate the change in average bus price for the scenarios of interest, bootstrapping simulations were generated from the original dataset to estimate the baseline distribution of the average bus price per seat. To simulate an increase in the market share of a given bus manufacturer, another set of bootstrapping simulations was generated by selectively sampling contracts involving the bus manufacturer of interest. The difference in average price per seat between the two scenarios was calculated, and 90% confidence intervals were computed."),
      p("There are 9 manufacturers that make Type A buses. Increasing the percentage of buses made by Lightning eMotors/Collins Bus from 5% to 30% resulted in a decrease in average price per seat of $1655. The 90% confidence interval for the decrease in average price per seat is $2999 to $399."),
      p("There are four manufacturers that make Type C buses, and three for Type D buses. The difference between each manufacturer’s average price per seat is much smaller for Type C and Type D buses than for Type A buses. When evaluating the impacts of increasing the percentage of the bus manufacturers with the lowest price per seat, a much greater increase must occur before the difference is statistically significant. For Type C buses, Lion Electric has the lowest price per seat. Increasing the percentage of Lion Electric buses from 3% to 70% resulted in a decrease in average price per seat of $303, with a 90% confidence interval of $556 to $62. For Type D buses, increasing the percentage of GreenPower from 14% to 90% resulted in a decrease in average price per seat of $317 with a 90% confidence interval of $631 to $10."),
      h3("Recommendations"),
      p("From our analysis, focusing on procuring Type A buses from Lightning eMotors/Collins Bus has the greatest potential to reduce cost per seat for the electric school bus fleet. This analysis took into account , bus manufacturer, base price, and seating capacity across the three types of buses (Type A, Type C, Type D). Our analysis reveals that a statistically significant change in average price per seat can be achieved for Type A buses with only a 25% increased allocation. As such,school districts should focus on procuring less expensive Type A buses from Lightning eMotors/Collins Bus."),
      p("To mitigate the risks of overreliance on one supplier, Pegasus Zeus is an alternate supplier for Type A; however, since the price per seat for Type A buses made by Pegasus Zeus is greater than the price per seat for Type A buses made by Lightning eMotors/Collins Bus, a larger change in purchasing trends will be required to lead to a statistically significant difference in average price per seat."),
      p("From a different perspective of Type C and Type D buses, we can conclude that for Type C, Lion Electric is the preferred supplier followed by Blue Bird in order to mitigate the overreliance. From a Type D perspective, Green Power is the preferred supplier followed by Lion Electric in order to mitigate the overreliance on one supplier.")
    )
  )
)

# server #####
server <- function(input, output, session) {

  ## bus data table #####
  output$bus_data = renderDT({
    datatable(dat, options = list(scrollX = TRUE))
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

    run_single_anova(dat, 
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
      dat,
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
      data = dat_A
    } else if (input$bus_type == "Type C"){
      data = dat_C
    } else {
      data = dat_D
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

    # bus type
    if (input$bus_type_reg == "Type A"){
      chunk = "regression_type_a_sci.html"
    } else if (input$bus_type_reg == "Type C"){
      chunk = "regression_type_c_sci.html"
    } else {
      chunk = "regression_type_d_sci.html"
    }

    includeHTML(chunk)
  })

  ## bootstrapped simulations #####

  ### dynamically update list of bus manufactureres #####
  observeEvent(input$bus_type_boot, {
    req(input$bus_type_boot)

    choices <- bus_manuf_choices[[input$bus_type_boot]]
    choices <- choices[!is.na(choices)]  # just in case

    updateSelectInput(
      session,
      "bus_manuf",
      choices  = choices,
      selected = choices[1]
    )
  })

  ### create bootstrapped charts #####
  output$boot_chart <- renderPlot({

    # remove NAs
    dat2 = dat[!is.na(dat$price_per_seat), ]
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