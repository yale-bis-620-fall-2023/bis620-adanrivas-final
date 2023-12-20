library(shiny)
library(ggplot2)

# setwd("~/git/github.com/yale-bis-620-fall-2023/bis620-adanrivas-final/bis620f23final")
# source("R/covid_svi_util.R")


shinyServer(function(input, output, session) {

  get_summ_covid_st_cases_data = reactive({
    if (input$cdc_covid_st_cases_data_url == "") {
      cdc_covid_st_county_cases_fpath <- file.path(COVID_DIR, "United_States_COVID-19_Community_Levels_by_County.csv")
      cat("Defaulting to CDC data.")
    } else {
      cdc_covid_st_county_cases_fpath <- input$cdc_st_covid_cases_data_url
    }

    uppercase_state_name <- toupper(input$state)

    state_svi_tract_df <- get_state_svi_tract(
      svi_tract_df=svi_tract_df,
      state_name=uppercase_state_name)

    state_county_svi_df <- summarize_state_county_svi_tracts(st_svi_tract_df=state_svi_tract_df)

    covid_start_date <- lubridate::ymd(input$start_date)
    covid_end_date <- lubridate::ymd(input$end_date)

    summ_covid_st_cases_df <- read.csv(cdc_covid_st_county_cases_fpath) |>
      rename_with(tolower) |>
      process_covid_df(
        county_colname="county",
        start_date=covid_start_date,
        end_date=covid_end_date,
        date_col="date_updated",
        format="%Y-%m-%d") |>
      summarize_state_covid_cases(
        st_county_svi_df = state_county_svi_df,
        state_name = input$state)

    return (summ_covid_st_cases_df)
  })

  get_summ_covid_vax_rates_data = reactive({
    if (input$cdc_covid_st_vax_data_url == "") {
      cdc_covid_st_county_vax_fpath <- file.path(COVID_DIR, "COVID-19_Vaccinations_in_the_United_States_County.csv")
    } else {
      cdc_covid_st_county_vax_fpath <- input$cdc_covid_vax_data_url
    }

    uppercase_state_name <- toupper(input$state)

    state_svi_tract_df <- get_state_svi_tract(
      svi_tract_df=svi_tract_df,
      state_name=uppercase_state_name)

    state_county_svi_df <- summarize_state_county_svi_tracts(st_svi_tract_df=state_svi_tract_df)

    covid_start_date <- lubridate::ymd(input$start_date)
    covid_end_date <- lubridate::ymd(input$end_date)

    summ_covid_st_vax_rates_df <- read.csv(cdc_covid_st_county_vax_fpath) |>
      rename_with(tolower) |>
      rename("county" = "recip_county", "st_abbr" = "recip_state") |>
      # mutate(date = as.Date(date, format="%m/%d/%Y")) |>
      process_covid_df(
        county_colname="county",
        start_date=covid_start_date,
        end_date=covid_end_date,
        date_col="date",
        format="%m/%d/%Y") |>
      summarize_state_covid_vax_rates(
        st_county_svi_df=state_county_svi_df,
        state_name=input$state)

    return (summ_covid_st_vax_rates_df)

  })


  output$svi_state_tract_plot = renderPlot({
    uppercase_state_name <- toupper(input$state)

    state_svi_tract_df <- get_state_svi_tract(
      svi_tract_df=svi_tract_df,
      state_name=uppercase_state_name)

    state_svi_plt <- plot_state_svi(
      st_svi_tract_df=state_svi_tract_df,
      svi_theme=input$svi_theme
    )

    state_svi_plt

  })

  output$svi_state_county_tract_plot = renderPlot({
    uppercase_state_name <- toupper(input$state)

    state_svi_tract_df <- get_state_svi_tract(
      svi_tract_df=svi_tract_df,
      state_name=uppercase_state_name)

    state_county_svi_df <- summarize_state_county_svi_tracts(st_svi_tract_df=state_svi_tract_df)

    state_county_svi_plt <- plot_state_svi(
      st_svi_tract_df=state_county_svi_df,
      svi_theme=input$svi_theme,
      agg_col=input$svi_agg_measure
    )

    state_county_svi_plt

  })

  output$covid_state_agg_cases_plot = renderPlot({
    summ_covid_state_county_cases_df <- get_summ_covid_st_cases_data()

    state_county_covid_cases_agg_plt <- plot_state_county_covid_cases(
      st_county_svi_covid_cases_df=summ_covid_state_county_cases_df,
      # TODO: add input to change covid cases column
      covid_cases_colname="covid_cases_per_100k",
      agg_col=input$covid_agg_measure)

    state_county_covid_cases_agg_plt

  })

  output$covid_state_vax_rates_plot = renderPlot({
    summ_covid_st_vax_rates_df <- get_summ_covid_vax_rates_data()

    state_county_covid_vax_rates_mean_plt <- plot_state_county_covid_vax_rates(
      covid_st_county_vax_rates_df=summ_covid_st_vax_rates_df,
      # TODO: add input to change vax rates column
      covid_vax_rate_colname="administered_dose1_pop_pct",
      # TODO: add input to change aggregation for vaccination rates
      agg_col="mean")

    state_county_covid_vax_rates_mean_plt
  })

  output$svi_covid_state_county_corr_matrix = renderPlot({
    summ_covid_state_county_cases_df <- get_summ_covid_st_cases_data()
    summ_covid_st_vax_rates_df <- get_summ_covid_vax_rates_data()

    corr_mat_plt <- build_correlation_matrix(
      st_county_svi_covid_cases_df=summ_covid_state_county_cases_df,
      st_county_svi_covid_vax_rates_df=summ_covid_st_vax_rates_df,
      corr_method=input$corr_method
    )

    corr_mat_plt
  })

})
