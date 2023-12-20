library(bis620f23final)
library(testthat)
library(vdiffr)

# preparing test data for unit tests
ma_state_name = 'Massachusetts'
uppercase_ma_state_name = toupper(ma_state_name)
covid_start_date <- lubridate::ymd("2022-02-01")
covid_end_date <- lubridate::ymd("2022-05-01")

covid_st_county_cases_test_df <- cdc_covid_st_county_cases_df |>
  process_covid_df(
    county_colname = "county",
    start_date = covid_start_date,
    end_date = covid_end_date,
    date_col = "date_updated")

ma_abbrv <- STATE_ABBR_DICT[[uppercase_ma_state_name]]

covid_st_county_vax_rates_test_df <- cdc_covid_st_county_vax_rates_df |>
  process_covid_df(
    county_colname = "county",
    start_date = covid_start_date,
    end_date = covid_end_date,
    date_col = "date",
    format = "%m/%d/%Y")

ma_summ_svi_tract_test_df <- svi_tract_df |>
  get_state_svi_tract(state_name = uppercase_ma_state_name) |>
  summarize_state_county_svi_tracts()


testthat::test_that("process_covid_df correctly removes 'County' from county field values and filters on date arguments", {
  # Define sample data
  cdc_covid_df <- data.frame(
    county = c("Fairfield County", "New Haven County", "Hartford County"),
    date = c("2022-01-01", "2022-01-02", "2022-01-03"),
    cases = c(100, 200, 300)
  )

  # Define expected output
  expected_output <- dplyr::tibble(
    county = c("New Haven", "Hartford"),
    date = c(as.Date("2022-01-02"), as.Date("2022-01-03")),
    cases = c(200, 300)
  )

  result <- process_covid_df(
    cdc_covid_df=cdc_covid_df,
    county_colname="county",
    start_date="2022-01-02",
    end_date="2022-01-03")

  # Test process_covid_df function
  testthat::expect_identical(result, expected_output)

})


testthat::test_that("get_state_svi_tract correctly filters CDC SVI data for a particular state", {

  # Test that the function correctly filters for "California"
  ca_result <- get_state_svi_tract(svi_tract_df, "CALIFORNIA")
  expect_equal(nrow(ca_result), 8041)
  expect_equal(unique(ca_result$state), "CALIFORNIA")

  # Test that the function correctly filters for "Texas"
  ct_result <- get_state_svi_tract(svi_tract_df, "CONNECTICUT")
  expect_equal(nrow(ct_result), 830)
  expect_equal(unique(ct_result$state), "CONNECTICUT")

  # Test that the function returns an empty data frame for states not in the data
  # note that state name should be all caps
  fl_result <- get_state_svi_tract(svi_tract_df, "Florida")
  expect_equal(nrow(fl_result), 0)

})


testthat::test_that("summarize_state_county_svi_tracts correctly aggregates SVI theme measures by county", {
  ma_svi_tract_test_df <- get_state_svi_tract(
    svi_tract_df = svi_tract_df,
    state_name = toupper(ma_state_name)
  )

  # Test that the function correctly aggregates SVI theme measures
  ma_summ_result <- summarize_state_county_svi_tracts(ma_svi_tract_test_df) |>
    # round to 3 decimal places
    mutate(
      rpl_theme1_mean = round(rpl_theme1_mean, 3),
      rpl_theme1_med = round(rpl_theme1_med, 3),
      rpl_theme2_mean = round(rpl_theme2_mean, 3),
      rpl_theme2_med = round(rpl_theme2_med, 3),
      rpl_theme3_mean = round(rpl_theme3_mean, 3),
      rpl_theme3_med = round(rpl_theme3_med, 3),
      rpl_theme4_mean = round(rpl_theme4_mean, 3),
      rpl_theme4_med = round(rpl_theme4_med, 3)
      )
  expect_equal(nrow(ma_summ_result), 14) # one row per county
  expect_equal(ma_summ_result$rpl_theme1_mean[1:4], c(0.236, 0.407, 0.526, 0.180))
  expect_equal(ma_summ_result$rpl_theme1_med[1:4], c(0.188, 0.351, 0.513, 0.122))
  expect_equal(ma_summ_result$rpl_theme2_mean[1:4], c(0.384, 0.546, 0.554, 0.313))
  expect_equal(ma_summ_result$rpl_theme2_med[1:4], c(0.321, 0.499, 0.573, 0.312))
})



testthat::test_that("plot_state_svi works", {
  # Call the function with the mock dataframe
  state_town_svi_plot <- svi_tract_df |>
    get_state_svi_tract(state_name = uppercase_ma_state_name) |>
    plot_state_svi(svi_theme="rpl_theme1")

  # Check that the output matches the known correct output
  vdiffr::expect_doppelganger("known_st_town_svi_plot", state_town_svi_plot)

  # do the same county-level aggregated data
  state_county_svi_plot <- plot_state_svi(
    st_svi_tract_df=ma_summ_svi_tract_test_df,
    svi_theme="rpl_theme1",
    agg_col="med")

  # Check that the output matches the known correct output
  vdiffr::expect_doppelganger("known_st_county_svi_plot", state_county_svi_plot)

})


testthat::test_that("summarize_state_covid_cases works", {
  # Call the function with the mock data frames
  result <- summarize_state_covid_cases(
    covid_st_county_cases_df=covid_st_county_cases_test_df,
    st_county_svi_df=ma_summ_svi_tract_test_df,
    state_name=ma_state_name,
    covid_cases_colname="covid_inpatient_bed_utilization"
  )

  # Check that the output has the correct number of rows
  expect_equal(nrow(result), 14)

  # Check that the output has the correct columns
  expected_cols <- c(
    "county",
    "state",
    "covid_inpatient_bed_utilization_mean",
    "covid_inpatient_bed_utilization_med",
    "covid_inpatient_bed_utilization_sd",
    "covid_inpatient_bed_utilization_q1",
    "covid_inpatient_bed_utilization_q3",
    "n_samples",
    "covid_inpatient_bed_utilization_se",
    "covid_inpatient_bed_utilization_ub",
    "covid_inpatient_bed_utilization_lb",
    "covid_inpatient_bed_utilization_iqr")
  expect_equal(colnames(result[,1:12]), expected_cols)
})


testthat::test_that("plot_state_county_covid_cases correctly plots aggregated COVID-19 cases per 100k", {
  ma_covid_cases_summ <- summarize_state_covid_cases(
    covid_st_county_cases_df=covid_st_county_cases_test_df,
    st_county_svi_df=ma_summ_svi_tract_test_df,
    state_name=ma_state_name,
    covid_cases_colname="covid_inpatient_bed_utilization"
  )

  # Test that the function correctly plots mean COVID-19 cases per 100k
  mean_cases_plot <- plot_state_county_covid_cases(
    st_county_svi_covid_cases_df=ma_covid_cases_summ,
    covid_cases_colname="covid_inpatient_bed_utilization",
    agg_col="mean",
    plot_title=NULL)
  vdiffr::expect_doppelganger("known_ma_mean_county_covid_cases_plot", mean_cases_plot)

  # Test that the function correctly plots custom title
  med_cases_plot <- plot_state_county_covid_cases(
    st_county_svi_covid_cases_df=ma_covid_cases_summ,
    covid_cases_colname="covid_inpatient_bed_utilization",
    agg_col="med",
    plot_title="Median Covid Inpatient Bed Utilization")
  vdiffr::expect_doppelganger("known_ma_med_county_covid_cases_plot", med_cases_plot)

})



testthat::test_that("summarize_state_covid_vax_rates correctly aggregates COVID-19 vaccination rates by county", {
  # Create a mock data frame
  covid_st_county_vax_rates_df <- data.frame(
    st_abbr = c("CA", "CA", "TX", "TX", "NY"),
    county = c("County1", "County1", "County2", "County2", "County3"),
    administered_dose1_pop_pct = c(1, 2, 3, 4, 5)
  )
  st_county_svi_df <- data.frame(
    county = c("County1", "County2", "County3")
  )

  # Test that the function correctly aggregates vaccination rates for "California"
  result <- summarize_state_covid_vax_rates(
    covid_st_county_vax_rates_df=covid_st_county_vax_rates_df,
    st_county_svi_df=st_county_svi_df,
    state_name="California",
    covid_vax_rate_colname="administered_dose1_pop_pct")
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$st_abbr), "CA")
  expect_equal(result$administered_dose1_pop_pct_mean, 1.5)
  expect_equal(result$administered_dose1_pop_pct_med, 1.5)

  # Test that the function correctly aggregates vaccination rates for "Texas"
  result <- summarize_state_covid_vax_rates(
    covid_st_county_vax_rates_df=covid_st_county_vax_rates_df,
    st_county_svi_df=st_county_svi_df,
    state_name="Texas",
    covid_vax_rate_colname="administered_dose1_pop_pct")
  expect_equal(nrow(result), 1)
  expect_equal(unique(result$st_abbr), "TX")
  expect_equal(result$administered_dose1_pop_pct_mean, 3.5)
  expect_equal(result$administered_dose1_pop_pct_med, 3.5)
})


testthat::test_that("plot_state_county_covid_vax_rates correctly plots aggregated COVID-19 vaccination rates", {
  ma_covid_vax_rates_summ <- summarize_state_covid_vax_rates(
    covid_st_county_vax_rates_df=covid_st_county_vax_rates_test_df,
    st_county_svi_df=ma_summ_svi_tract_test_df,
    state_name=ma_state_name,
    covid_vax_rate_colname="administered_dose1_pop_pct"
  )

  # Test that the function correctly plots mean COVID-19 vaccination rates
  ma_mean_covid_vax_rates_plot <- plot_state_county_covid_vax_rates(
    covid_st_county_vax_rates_df=ma_covid_vax_rates_summ,
    covid_vax_rate_colname="administered_dose1_pop_pct",
    agg_col="mean",
    plot_title=NULL)
  vdiffr::expect_doppelganger("known_ma_mean_county_vax_rates_plot", ma_mean_covid_vax_rates_plot)

  # Test that the function correctly plots median COVID-19 vaccination rates
  ma_med_covid_vax_rates_plot <- plot_state_county_covid_vax_rates(
    covid_st_county_vax_rates_df=ma_covid_vax_rates_summ,
    covid_vax_rate_colname="administered_dose1_pop_pct",
    agg_col="med",
    plot_title=NULL)
  vdiffr::expect_doppelganger("known_ma_med_county_vax_rates_plot", ma_med_covid_vax_rates_plot)
})


testthat::test_that("build_correlation_matrix correctly computes a correlation matrix of SVI and COVID-19 data", {
  # Use test data to get county-level summary
  ma_county_svi_covid_cases_df <- summarize_state_covid_cases(
      covid_st_county_cases_df=covid_st_county_cases_test_df,
      st_county_svi_df=ma_summ_svi_tract_test_df,
      state_name=ma_state_name,
      covid_cases_colname = "covid_cases_per_100k")

  ma_county_svi_covid_vax_rates_df <- summarize_state_covid_vax_rates(
    covid_st_county_vax_rates_df=covid_st_county_vax_rates_test_df,
    st_county_svi_df=ma_summ_svi_tract_test_df,
    state_name=ma_state_name,
    covid_vax_rate_colname="administered_dose1_pop_pct")

  # Test that the function correctly computes the correlation matrix
  corr_result <- build_correlation_matrix(
    ma_county_svi_covid_cases_df,
    ma_county_svi_covid_vax_rates_df,
    "pearson")

  expect_equal(dim(corr_result$corr), c(18, 18))

})


