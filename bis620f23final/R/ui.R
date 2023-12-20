library(shinythemes)


shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  titlePanel("CDC Social Vulnerability Index + Covid Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "cdc_covid_st_cases_data_url", label = "CDC State Cases Data URL to CSV", value=""),
      textInput(inputId = "cdc_covid_st_vax_data_url", label = "CDC State Vax Data URL to CSV", value=""),
      selectInput(inputId = "state", label = "State:", choices = CDC_STATES, selected="Massachusetts"),
      selectInput(input = "svi_theme", label = "CDC SVI Measure", choices = SVI_MEASURES, selected="rpl_themes"),
      selectInput(inputId = "svi_agg_measure", label = "SVI County Aggregate Measure:", choices = c("mean", "med"), selected="mean"),
      selectInput(inputId = "covid_agg_measure", label = "Covid Aggregate Measure:", choices = c("mean", "med", "ub", "iqr"), selected="mean"),
      selectInput(input = "start_date", label = "Start Date", choices = CDC_DATES, selected="2022-02-01"),
      selectInput(input = "end_date", label = "End Date", choices = CDC_DATES, selected="2022-08-01"),
      selectInput(input = "corr_method", label = "Correlation Method", choices = c("spearman", "pearson", "kendall"), selected="spearman"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        tabPanel("State SVI",
                 fluidRow(
                          column(10, plotOutput("svi_state_tract_plot")),
                          column(10, plotOutput("svi_state_county_tract_plot"))
                 ), value="svi"),
        tabPanel("State Covid Cases vs. State Un-Vaccinated Rates",
                 fluidRow(
                       column(10, plotOutput("covid_state_agg_cases_plot")),
                       column(10, plotOutput("covid_state_vax_rates_plot"))
                  ), value="covid_cases_v_vax"),
        tabPanel("State SVI vs Covid Correlations",
                 plotOutput("svi_covid_state_county_corr_matrix"),
                 value="svi_v_covid_corr"),
      )
    )
  )
))

