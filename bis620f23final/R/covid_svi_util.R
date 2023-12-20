library(readr)
library(DT)
library(ggplot2)
library(dplyr)
library(corrplot)
library(lubridate)
library(RColorBrewer)

library(sf)
sf_use_s2(FALSE)


#####################################
#### Local and Global Variables #####
#####################################

project_dir <- "~/git/github.com/yale-bis-620-fall-2023/bis620-adanrivas-final/bis620f23final"
SVI_DIR <- file.path(project_dir, "inst/data/svi")
COVID_DIR <- file.path(project_dir, "inst/data/covid")

svi_tract_fname <- file.path(SVI_DIR, "SVI2018_US_tract.shp")
svi_tract_df <- read_sf(svi_tract_fname) |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate_if(is.numeric, ~ ifelse(.x == -999, NA, .x))

SVI_STATES <- unique(svi_tract_df$state)
SVI_MEASURES <- svi_tract_df |>
  dplyr::select(starts_with("rpl_")) |>
  colnames()

state_names_and_abbr <- svi_tract_df |>
  tibble() |>
  dplyr::select(state, st_abbr) |>
  dplyr::distinct()

STATE_ABBR_DICT <- split(state_names_and_abbr$st_abbr, state_names_and_abbr$state)

# remove geometry from list of svi measures
SVI_MEASURES <- SVI_MEASURES[SVI_MEASURES!= 'geometry']
SVI_THEMES_DICT <- list(
  "rpl_theme1" = "Socioeconomic Status",
  "rpl_theme2" = "Household Characteristics",
  "rpl_theme3" = "Racial & Ethnic Minority Status",
  "rpl_theme4" = "Housing Type & Transport",
  "rpl_themes" = "Overall Vulnerability"
)

# obtained from CDC query tool
sample_cdc_covid_st_county_cases_url <- "https://data.cdc.gov/resource/3nnm-4jni.csv?$query=SELECT%0A%20%20%60county%60%2C%0A%20%20%60county_fips%60%2C%0A%20%20%60state%60%2C%0A%20%20%60county_population%60%2C%0A%20%20%60health_service_area_number%60%2C%0A%20%20%60health_service_area%60%2C%0A%20%20%60health_service_area_population%60%2C%0A%20%20%60covid_inpatient_bed_utilization%60%2C%0A%20%20%60covid_hospital_admissions_per_100k%60%2C%0A%20%20%60covid_cases_per_100k%60%2C%0A%20%20%60covid_19_community_level%60%2C%0A%20%20%60date_updated%60%0AWHERE%0A%20%20caseless_one_of(%0A%20%20%20%20%60state%60%2C%0A%20%20%20%20%22Massachusetts%22%2C%0A%20%20%20%20%22Connecticut%22%2C%0A%20%20%20%20%22California%22%2C%0A%20%20%20%20%22New%20York%22%0A%20%20)%0A%20%20AND%20(%60date_updated%60%0A%20%20%20%20%20%20%20%20%20BETWEEN%20%222021-01-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0A%20%20%20%20%20%20%20%20%20AND%20%222022-12-31T23%3A59%3A59%22%20%3A%3A%20floating_timestamp)%0AORDER%20BY%20%60date_updated%60%20ASC%20NULL%20LAST"
sample_cdc_covid_st_county_vax_rates_url <- "https://data.cdc.gov/resource/8xkx-amqh.csv?$query=SELECT%0A%20%20%60date%60%2C%0A%20%20%60fips%60%2C%0A%20%20%60mmwr_week%60%2C%0A%20%20%60recip_county%60%2C%0A%20%20%60recip_state%60%2C%0A%20%20%60completeness_pct%60%2C%0A%20%20%60administered_dose1_recip%60%2C%0A%20%20%60administered_dose1_pop_pct%60%2C%0A%20%20%60administered_dose1_recip_5plus%60%2C%0A%20%20%60administered_dose1_recip_5pluspop_pct%60%2C%0A%20%20%60administered_dose1_recip_12plus%60%2C%0A%20%20%60administered_dose1_recip_12pluspop_pct%60%2C%0A%20%20%60administered_dose1_recip_18plus%60%2C%0A%20%20%60administered_dose1_recip_18pluspop_pct%60%2C%0A%20%20%60administered_dose1_recip_65plus%60%2C%0A%20%20%60administered_dose1_recip_65pluspop_pct%60%2C%0A%20%20%60series_complete_yes%60%2C%0A%20%20%60series_complete_pop_pct%60%2C%0A%20%20%60series_complete_5plus%60%2C%0A%20%20%60series_complete_5pluspop_pct%60%2C%0A%20%20%60series_complete_5to17%60%2C%0A%20%20%60series_complete_5to17pop_pct%60%2C%0A%20%20%60series_complete_12plus%60%2C%0A%20%20%60series_complete_12pluspop_pct%60%2C%0A%20%20%60series_complete_18plus%60%2C%0A%20%20%60series_complete_18pluspop_pct%60%2C%0A%20%20%60series_complete_65plus%60%2C%0A%20%20%60series_complete_65pluspop_pct%60%2C%0A%20%20%60booster_doses%60%2C%0A%20%20%60booster_doses_vax_pct%60%2C%0A%20%20%60booster_doses_5plus%60%2C%0A%20%20%60booster_doses_5plus_vax_pct%60%2C%0A%20%20%60booster_doses_12plus%60%2C%0A%20%20%60booster_doses_12plus_vax_pct%60%2C%0A%20%20%60booster_doses_18plus%60%2C%0A%20%20%60booster_doses_18plus_vax_pct%60%2C%0A%20%20%60booster_doses_50plus%60%2C%0A%20%20%60booster_doses_50plus_vax_pct%60%2C%0A%20%20%60booster_doses_65plus%60%2C%0A%20%20%60booster_doses_65plus_vax_pct%60%2C%0A%20%20%60second_booster_50plus%60%2C%0A%20%20%60second_booster_50plus_vax_pct%60%2C%0A%20%20%60second_booster_65plus%60%2C%0A%20%20%60second_booster_65plus_vax_pct%60%2C%0A%20%20%60svi_ctgy%60%2C%0A%20%20%60series_complete_pop_pct_svi%60%2C%0A%20%20%60series_complete_5pluspop_pct_svi%60%2C%0A%20%20%60series_complete_5to17pop_pct_svi%60%2C%0A%20%20%60series_complete_12pluspop_pct_svi%60%2C%0A%20%20%60series_complete_18pluspop_pct_svi%60%2C%0A%20%20%60series_complete_65pluspop_pct_svi%60%2C%0A%20%20%60metro_status%60%2C%0A%20%20%60series_complete_pop_pct_ur_equity%60%2C%0A%20%20%60series_complete_5pluspop_pct_ur_equity%60%2C%0A%20%20%60series_complete_5to17pop_pct_ur_equity%60%2C%0A%20%20%60series_complete_12pluspop_pct_ur_equity%60%2C%0A%20%20%60series_complete_18pluspop_pct_ur_equity%60%2C%0A%20%20%60series_complete_65pluspop_pct_ur_equity%60%2C%0A%20%20%60booster_doses_vax_pct_svi%60%2C%0A%20%20%60booster_doses_12plusvax_pct_svi%60%2C%0A%20%20%60booster_doses_18plusvax_pct_svi%60%2C%0A%20%20%60booster_doses_65plusvax_pct_svi%60%2C%0A%20%20%60booster_doses_vax_pct_ur_equity%60%2C%0A%20%20%60booster_doses_12plusvax_pct_ur_equity%60%2C%0A%20%20%60booster_doses_18plusvax_pct_ur_equity%60%2C%0A%20%20%60booster_doses_65plusvax_pct_ur_equity%60%2C%0A%20%20%60census2019%60%2C%0A%20%20%60census2019_5pluspop%60%2C%0A%20%20%60census2019_5to17pop%60%2C%0A%20%20%60census2019_12pluspop%60%2C%0A%20%20%60census2019_18pluspop%60%2C%0A%20%20%60census2019_65pluspop%60%2C%0A%20%20%60bivalent_booster_5plus%60%2C%0A%20%20%60bivalent_booster_5plus_pop_pct%60%2C%0A%20%20%60bivalent_booster_12plus%60%2C%0A%20%20%60bivalent_booster_12plus_pop_pct%60%2C%0A%20%20%60bivalent_booster_18plus%60%2C%0A%20%20%60bivalent_booster_18plus_pop_pct%60%2C%0A%20%20%60bivalent_booster_65plus%60%2C%0A%20%20%60bivalent_booster_65plus_pop_pct%60%0AWHERE%0A%20%20caseless_one_of(%60recip_state%60%2C%20%22CT%22%2C%20%22MA%22%2C%20%22CA%22%2C%20%22NY%22)%0A%20%20AND%20(%60date%60%0A%20%20%20%20%20%20%20%20%20BETWEEN%20%222021-01-01T00%3A00%3A00%22%20%3A%3A%20floating_timestamp%0A%20%20%20%20%20%20%20%20%20AND%20%222022-12-31T23%3A59%3A59%22%20%3A%3A%20floating_timestamp)"

covid_cases_fname <- file.path(COVID_DIR, "United_States_COVID-19_Community_Levels_by_County.csv")
covid_vax_rates_fname <- file.path(COVID_DIR, "COVID-19_Vaccinations_in_the_United_States_County.csv")

cdc_covid_st_county_cases_df <- read.csv(covid_cases_fname)
cdc_covid_st_county_vax_rates_df <- read.csv(covid_vax_rates_fname) |>
  rename_with(tolower) |>
  rename("county" = "recip_county", "st_abbr" = "recip_state")

CDC_STATES <- unique(cdc_covid_st_county_cases_df$state)
CDC_DATES <- cdc_covid_st_county_cases_df |>
  dplyr::mutate(
    date_updated = lubridate::ymd(date_updated),
    year_month_date_updated = lubridate::floor_date(date_updated, "month")
    ) |>
  dplyr::select(year_month_date_updated) |>
  dplyr::distinct() |>
  pull()


##############################
##### Helper Functions #######
##############################

#' @description: Cleans CDC covid data by simplifying county names and filter on specified data range.
#' @param cdc_covid_df A data frame containing CDC COVID-19 data.
#' @param county_colname The name of the column containing county names.
#' @param start_date The start date of the data range to filter.
#' @param end_date The end date of the data range to filter.
#' @param date_col The name of the column containing dates. Default is 'date'.
#' @param format The format of the dates in date_col. Default is '%Y-%m-%d'.
#' @return A tibble containing cleaned CDC COVID-19 data.
#'
#' @importFrom dplyr mutate filter sym
#' @export
#'
process_covid_df <- function(
  cdc_covid_df,
  county_colname,
  start_date,
  end_date,
  date_col='date',
  format='%Y-%m-%d'
  ){

  cdc_covid_df <- cdc_covid_df |>
    dplyr::mutate(
      county = gsub(" County", "", !!dplyr::sym(county_colname)),
      date = as.Date(!!dplyr::sym(date_col), format=format)
      ) |>
    dplyr::filter(date >= start_date & date <= end_date) |>
    dplyr::tibble()

  return(cdc_covid_df)
}


#' @description: Filters CDC SVI data for a particular state.
#'
#' @param svi_tract_df A data frame containing CDC SVI data.
#' @param state_name The name of the state to filter.
#' @return A tibble containing CDC SVI data for the specified state.
#'
#' @importFrom dplyr filter
#' @export
#'
get_state_svi_tract <- function(svi_tract_df, state_name) {

  # convert to upper case if not already
  state_name <- toupper(state_name)
  st_svi_tract_df <- svi_tract_df |>
    dplyr::filter(state == state_name)

  return (st_svi_tract_df)
}


#' @description: Groups by state and county to aggregate SVI theme measures.
#'
#' @param st_svi_tract_df A data frame containing CDC SVI data for a specific state.
#' @return A tibble containing aggregated SVI theme measures by county.
#'
#' @importFrom dplyr group_by summarize
#' @export
#'
summarize_state_county_svi_tracts <- function(st_svi_tract_df) {

  st_county_svi_df <- st_svi_tract_df |>
    dplyr::group_by(county) |>
    dplyr::summarize(
      # theme 1: socioeconomic status
      rpl_theme1_mean = mean(rpl_theme1, na.rm=TRUE),
      rpl_theme1_med = median(rpl_theme1, na.rm=TRUE),
      # theme 2: household composition
      rpl_theme2_mean = mean(rpl_theme2, na.rm=TRUE),
      rpl_theme2_med = median(rpl_theme2, na.rm=TRUE),
      # theme 3: minority status and language
      rpl_theme3_mean = mean(rpl_theme3, na.rm=TRUE),
      rpl_theme3_med = median(rpl_theme3, na.rm=TRUE),
      # theme 4: housing type and transportation
      rpl_theme4_mean = mean(rpl_theme4, na.rm=TRUE),
      rpl_theme4_med = median(rpl_theme4, na.rm=TRUE),
      # aggregate of themes
      rpl_themes_mean = mean(rpl_themes, na.rm=TRUE),
      rpl_themes_med = median(rpl_themes, na.rm=TRUE)
    ) |>
    suppressMessages()

  return(st_county_svi_df)

}


#' @description: Plot a specific SVI measure geographically.
#'
#' @param st_svi_tract_df A data frame containing CDC SVI data for a specific state.
#' @param svi_theme The name of the SVI theme to plot.
#' @param agg_col The name of the column to aggregate by (optional). Current aggregations supported are median and mean. More agregeations to come.
#' @return A ggplot2 object containing a geographic plot of the specified SVI measure.
#'
#' @importFrom ggplot2 aes geom_sf scale_fill_viridis_b theme_bw labs
#' @export
#'
plot_state_svi <- function(st_svi_tract_df, svi_theme, agg_col = NULL) {

  theme_title = SVI_THEMES_DICT[[svi_theme]]

  if (!is.null(agg_col)) {
    # standardize aggregate column name
    agg_col <- dplyr::case_when(
      agg_col == "median" ~ "med",
      agg_col == "upper_bound" ~ "ub",
      agg_col == "upper bound" ~ "ub",
      .default = agg_col
    )

    agg_col_name <- dplyr::case_when(
      agg_col == "mean" ~ "Mean",
      agg_col == "med" ~ "Median",
      agg_col == "ub" ~ "Upper Bound",
      agg_col == "iqr" ~ "Inter-quantile Range",
    )

     agg_col <- ifelse(agg_col == "median", "med", agg_col)
    svi_theme <- paste0(svi_theme, "_", agg_col)
    theme_title = paste0(agg_col_name, "(",theme_title, ")")
  }

  st_town_svi_sf_plt <- ggplot2::ggplot(data=st_svi_tract_df, aes(geometry = geometry, fill = !!dplyr::sym(svi_theme))) +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_viridis_b(direction = -1, name="") +
    ggplot2::theme_bw() +
    ggplot2::labs(title=theme_title)

  return(st_town_svi_sf_plt)

}


#' @description: Groups by state county and aggregates covid cases per 100k statistics.
#'
#' @param covid_st_county_cases_df A data frame containing COVID-19 cases data by state and county.
#' @param st_county_svi_df A data frame containing CDC SVI data by state and county.
#' @param state_name The name of the state to filter.
#' @param covid_cases_colname Covid cases statistic to aggregate (optional).
#' @return A tibble containing aggregated COVID-19 cases per 100k statistics by county.
#'
#' @importFrom dplyr filter group_by summarize mutate inner_join
#' @export
#'
summarize_state_covid_cases <- function(
  covid_st_county_cases_df,
  st_county_svi_df,
  state_name,
  covid_cases_colname = "covid_cases_per_100k"
  ){

  # convert to lowercase if not already (except for letter)
  state_name <- paste0(toupper(substr(state_name, 1, 1)), tolower(substr(state_name, 2, nchar(state_name))))

  st_county_svi_covid_cases_df <- covid_st_county_cases_df |>
    dplyr::filter(state == state_name) |>
    dplyr::group_by(county, state) |>
    dplyr::summarize(
      !!(paste0(covid_cases_colname, "_mean")) := mean(!!dplyr::sym(covid_cases_colname), na.rm=TRUE),
      !!(paste0(covid_cases_colname, "_med")) := median(!!dplyr::sym(covid_cases_colname), na.rm=TRUE),
      !!(paste0(covid_cases_colname, "_sd")) := sd(!!dplyr::sym(covid_cases_colname), na.rm=TRUE),
      !!(paste0(covid_cases_colname, "_q1")) := quantile(!!dplyr::sym(covid_cases_colname), probs=0.25, na.rm=TRUE),
      !!(paste0(covid_cases_colname, "_q3")) := quantile(!!dplyr::sym(covid_cases_colname), probs=0.75, na.rm=TRUE),
      n_samples = n()
    ) |>
    dplyr::mutate(
      !!(paste0(covid_cases_colname, "_se")) := !!dplyr::sym(paste0(covid_cases_colname, "_sd")) / sqrt(n_samples),
      !!(paste0(covid_cases_colname, "_ub")) := !!dplyr::sym(paste0(covid_cases_colname, "_mean")) +
        qt(1 - (0.05 / 2), n_samples - 1) * !!dplyr::sym(paste0(covid_cases_colname, "_se")),
      !!(paste0(covid_cases_colname, "_lb")) := !!dplyr::sym(paste0(covid_cases_colname, "_mean")) -
        qt(1 - (0.05 / 2), n_samples - 1) * !!dplyr::sym(paste0(covid_cases_colname, "_se")),
      !!(paste0(covid_cases_colname, "_iqr")) := !!dplyr::sym(paste0(covid_cases_colname, "_q3")) - !!sym(paste0(covid_cases_colname, "_q1"))
      # covid_cases_per_100k_se = covid_cases_per_100k_sd / sqrt(n_samples),
      # covid_cases_per_100k_ub = covid_cases_per_100k_mean + qt(1 - (0.05 / 2), n_samples - 1) * covid_cases_per_100k_se,
      # covid_cases_per_100k_lb = covid_cases_per_100k_mean - qt(1 - (0.05 / 2), n_samples - 1) * covid_cases_per_100k_se,
      # covid_cases_per_100k_iqr = covid_cases_per_100k_q3 - covid_cases_per_100k_q1
    ) |>
    dplyr::inner_join(y=st_county_svi_df, by=c("county"))
}


#' @description This function plots county-level, aggregated COVID-19 cases per 100k.
#' @param st_county_svi_covid_cases_df A dataframe containing county-level COVID-19 case data.
#' @param covid_cases_colname The column name in the dataframe for COVID-19 cases per 100k population. Default is "covid_cases_per_100k".
#' @param agg_col The aggregation method to use for the plot. Can be "mean", "med" (median), "ub" (upper bound), or "iqr" (inter-quantile range). Default is "mean".
#' @param plot_title The title of the plot. Default is "County-level Covid Cases per 100k". Default is NULL.
#' @return A ggplot object of the county-level aggregated COVID-19 cases per 100k population.
#'
#' @importFrom dplyr case_when
#' @importFrom ggplot2 ggplot aes geom_sf labs theme_bw
#' @importFrom scico scale_fill_scico
#' @export
#'
plot_state_county_covid_cases <- function(
  st_county_svi_covid_cases_df,
  covid_cases_colname="covid_cases_per_100k",
  agg_col="mean",
  plot_title=NULL
  ){

  # standardize aggregate column name
  agg_col <- dplyr::case_when(
    agg_col == "median" ~ "med",
    agg_col == "upper_bound" ~ "ub",
    agg_col == "upper bound" ~ "ub",
    .default = agg_col
  )

  agg_col_name <- dplyr::case_when(
    agg_col == "mean" ~ "Mean",
    agg_col == "med" ~ "Median",
    agg_col == "ub" ~ "Upper Bound",
    agg_col == "iqr" ~ "Inter-quantile Range",
  )

  covid_casese_per_100k_col <- paste0(covid_cases_colname, "_", agg_col)
  if (is.null(plot_title)) {
    plot_title <- paste0(agg_col_name, "(", covid_cases_colname, ")")
  }

  st_county_covid_cases_agg_plt <- ggplot2::ggplot(
      data=st_county_svi_covid_cases_df,
      mapping=aes(geometry = geometry, fill = !!dplyr::sym(covid_casese_per_100k_col))) +
    ggplot2::geom_sf() +
    scico::scale_fill_scico(palette = "lajolla", direction=-1, name="") +
    ggplot2::labs(title=plot_title) +
    ggplot2::theme_bw()

  return(st_county_covid_cases_agg_plt)

}


#' @description This function groups by state and county to aggregate COVID-19 vaccination rates.
#' @param covid_st_county_vax_rates_df A dataframe containing state county-level COVID-19 vaccination rates.
#' @param st_county_svi_df A dataframe containing county-level social vulnerability index (SVI) data.
#' @param state_name The name of the state.
#' @param covid_vax_rate_colname The column name in the dataframe for COVID-19 vaccination rates. Default is "administered_dose1_pop_pct".
#' @return A dataframe of the aggregated COVID-19 vaccination rates at the county level for the specified state.
#'
#' @importFrom dplyr filter group_by summarize inner_join
#' @export
#'
summarize_state_covid_vax_rates <- function(
    covid_st_county_vax_rates_df,
    st_county_svi_df,
    state_name,
    covid_vax_rate_colname="administered_dose1_pop_pct"
  ){
  # conver to uppercase if not already
  uppercase_state_name <- toupper(state_name)
  st_name_abbr <- STATE_ABBR_DICT[[uppercase_state_name]]

  st_county_svi_covid_vax_rates_df <- covid_st_county_vax_rates_df |>
    dplyr::filter(st_abbr == st_name_abbr) |>
    dplyr::group_by(county, st_abbr) |>
    dplyr::summarize(
      !!(paste0(covid_vax_rate_colname, "_mean")) := mean(!!sym(covid_vax_rate_colname), na.rm=TRUE),
      !!(paste0(covid_vax_rate_colname, "_med")) := median(!!sym(covid_vax_rate_colname), na.rm=TRUE),
      !!(paste0(covid_vax_rate_colname, "_sd")) := sd(!!sym(covid_vax_rate_colname), na.rm=TRUE),
      !!(paste0(covid_vax_rate_colname, "_q1")) := quantile(!!sym(covid_vax_rate_colname), probs=0.25, na.rm=TRUE),
      !!(paste0(covid_vax_rate_colname, "_q3")) := quantile(!!sym(covid_vax_rate_colname), probs=0.75, na.rm=TRUE),
      n_samples = n()
    ) |>
    dplyr::inner_join(y=st_county_svi_df, by=c("county"))

  return(st_county_svi_covid_vax_rates_df)

}


#' @description This function plots county-level, aggregated COVID-19 vaccination rates.
#' @param covid_st_county_vax_rates_df A dataframe containing state county-level COVID-19 vaccination rates.
#' @param covid_vax_rate_colname The column name in the dataframe for COVID-19 vaccination rates. Default is "administered_dose1_pop_pct".
#' @param agg_col The aggregation method to use for the plot. Can be "mean", "med" (median), "ub" (upper bound), or "iqr" (inter-quantile range). Default is "mean".
#' @param plot_title The title of the plot. Default is "County-level Covid Unvaccinated Rates".
#' @return A ggplot object of the county-level aggregated COVID-19 vaccination rates. Default value is NULL.
#'
#' @importFrom ggplot2 ggplot aes geom_sf labs theme_bw
#' @importFrom scico scale_fill_scico
#' @export
#'
plot_state_county_covid_vax_rates <- function(
    covid_st_county_vax_rates_df,
    covid_vax_rate_colname="administered_dose1_pop_pct",
    agg_col="mean",
    plot_title=NULL
  ){

  # standardize aggregate column name
  agg_col <- dplyr::case_when(
    agg_col == "median" ~ "med",
    agg_col == "upper_bound" ~ "ub",
    agg_col == "upper bound" ~ "ub",
    .default = agg_col
  )

  agg_col_name <- dplyr::case_when(
    agg_col == "mean" ~ "Mean",
    agg_col == "med" ~ "Median",
    agg_col == "ub" ~ "Upper Bound",
    agg_col == "iqr" ~ "Inter-quantile Range",
  )

  if (is.null(plot_title)) {
    plot_title <- paste0(agg_col_name, "(", covid_vax_rate_colname, ")")
  }

  agg_vax_colname <- paste0(covid_vax_rate_colname, "_", agg_col)

  st_county_covid_vax_rates_mean_plt <- ggplot2::ggplot(
      data=covid_st_county_vax_rates_df,
      mapping=aes(geometry = geometry, fill = !!sym(agg_vax_colname))) +
    ggplot2::geom_sf() +
    scico::scale_fill_scico(palette = "lajolla", direction=-1, name="") +
    ggplot2::labs(title=plot_title) +
    ggplot2::theme_bw()


  return(st_county_covid_vax_rates_mean_plt)

}


#' @description This function computes and displays a correlation matrix of SVI and COVID-19 data.
#' @param st_county_svi_covid_cases_df A dataframe containing county-level COVID-19 case data.
#' @param st_county_svi_covid_vax_rates_df A dataframe containing state county-level COVID-19 vaccination rates.
#' @param corr_method The correlation method to use for the correlation matrix. Acceptable methods are pearson, spearman, and kendall.
#' @return A correlation matrix of SVI and COVID-19 data.
#'
#' @importFrom dplyr select inner_join
#' @importFrom corrplot corrplot
#' @importFrom RColorBrewer brewer.pal
#' @export
#'
build_correlation_matrix <- function(
  st_county_svi_covid_cases_df,
  st_county_svi_covid_vax_rates_df,
  corr_method
  ){
  cleaned_st_county_svi_covid_vax_rates_df <- st_county_svi_covid_vax_rates_df |>
    na.omit() |>
    # sf::st_drop_geometry() |>
    dplyr::select(-c(geometry, st_abbr, n_samples)) |>
    # drop median statistics
    dplyr::select(!ends_with("_med"))

  st_county_level_measures <- st_county_svi_covid_cases_df |>
    # sf::st_drop_geometry() |>
    # drop duplicate svi measures before joining with vaccination data
    dplyr::select(!starts_with("rpl_theme")) |>
    dplyr::select(-c(geometry, n_samples)) |>
    dplyr::inner_join(
      y=cleaned_st_county_svi_covid_vax_rates_df,
      by="county")

  corr_matrix <- st_county_level_measures |>
    dplyr::ungroup() |>
    dplyr::select(-c(county, state)) |>
    as.matrix() |>
    cor(method=corr_method) |>
    corrplot::corrplot(
      method="color",
      type="upper",
      col=RColorBrewer::brewer.pal(n=8, name="PuRd")
    )

  return(corr_matrix)
}

