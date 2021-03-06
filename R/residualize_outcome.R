#' Function to residualize out covariates from a given dataset.
#'
#' @description
#'  This function regresses an outcome on a given set of covariates and
#'  obtains and stores the residual for each observation.
#'
#' @param input_data The data containing the outcome and covariates (data.table).
#' @param outcome_var The name of the outcome variable (character).
#' @param discrete_vars A list of discrete covariates (character).
#' @param continuous_vars A list of continuous variables (character).
#' @param weight_var (optional) The name of the weight variable, if the
#'     regression is to be weighted (character).

residualize_outcome <- function(input_data,
                                outcome_var,
                                discrete_vars,
                                continuous_vars,
                                weight_var = NULL,
                                use_one_hot = TRUE) {

  # extractions
  covariates <- c(discrete_vars, continuous_vars)
  outcome_vector <- input_data[, get(outcome_var)]
  covariate_matrix <- input_data[, .SD, .SDcols = covariates]
  invisible(gc())

  # prepare covariate matrix
  if (use_one_hot){
    for (discrete_var in discrete_vars) {
      covariate_matrix[, (discrete_var) := as.factor(get(discrete_var))]
      covariate_matrix <- one_hot(covariate_matrix, cols = discrete_var)
    }
  } else {
    for (discrete_var in discrete_vars) {
      vals <- unique(covariate_matrix[, .(discrete_var)])
      for (val in vals){
        covariate_matrix[, paste0(discrete_var, "_", val) := as.integer(get(discrete_var) == val)]
      }
    }
  }

  covariate_matrix[, ones := 1]
  covariate_matrix <- as.matrix(covariate_matrix)
  invisible(gc())

  # run fast regression
  if (!is.null(weight_var)){
    weight_vector <- input_data[, get(weight_var)]
    lm_fit_output <- lm.wfit(covariate_matrix, outcome_vector, weight_vector)
  } else {
    lm_fit_output <- .lm.fit(covariate_matrix, outcome_vector)
  }

  input_data[, (paste0(outcome_var, "_residual")) :=
               lm_fit_output$residuals]
  input_data[, (paste0(outcome_var, "_predict")) :=
               get(outcome_var) - get(paste0(outcome_var, "_residual"))]

  return(input_data)
}

#' Remove time trends
#'
#' @description \code{remove_time_trends} removes seasonality from the data and
#'     detrends the data based on preferences provided by the user.
#' @param input_data The data containing the outcome and covariates (data.table).
#' @param outcome_var The name of the outcome variable (character).
#' @param month_or_quarter Is time on month or quarter level? Should be either
#'     \code{"month"} or \code{"quarter"} (character).
#' @param year_var The name of the year variable (character).
#' @param month_dummies Residualize out month-of-year (or quarter-of-year) effects? (logical)
#' @param calendar_time Residualize out calendar-time effects? (logical)
#' @param product_group_trend Residualize out product-group-specific trends? (logical)
#' @param census_region_trends Residualize out trends with census region cells? (logical)
#' @param census_division_trends Residualize out trends with census division cells? (logical)
#' @param weight_var Optional. The name of the variable to weight the
#'     regressions when residualizing (character).
#' @details \code{remove_time_trends} is meant to residualize data in a variety
#'     of ways, depending on what the user wants. Some of these options are
#'     mutually exclusive. These include the options \code{month_dummies} and
#'     \code{calendar_time} - both cannot be \code{TRUE}, as they would be
#'     collinear. Additionally, the code does not allow \code{product_group_trend}
#'     and \code{calendar_time} to both be \code{TRUE}.
#'
#' For the pictures in calendar time, we should try the following: de-trend and
#' adjust for seasonality.
#'
#' The following are taken from an email from Lancelot on 11/22/2018:
#'
#'   A) Take the sample that you create (in your R code) with theProductXCounty
#'     level data and regress the outcome that we want to plot (sales or prices)
#'     on a variable indicating the number of months since january 2008 (so that
#'     we estimate a linear time trend).  Weigh the regression by January 2008
#'     sales.  Then take the residuals and take the average for each "group"
#'     (increase, decrease, no change) - weigh the observations by january 2008
#'     sales when you compute the averages. (args: month_dummies = F)
#'
#'   B) Another version would fit a different time trend by "product group"
#'     (interact number of months since january 2008 with a dummy for each
#'     product group).  Then take residuals, average over counties within each
#'     group and plot. (args: month_dummies = F, product_group_trend = T)
#'
#'   C) Do A) again but include a dummy for which month of the year in the
#'     regression (so a dummy for january, a dummy for february,...).  Then take
#'     residuals, average over counties within each group and plot. (code is set up)
#'     (args: month_dummies = T)
#'
#'   D) Do B) again but include a dummy for which month of the year interacted
#'     with a product group specific dummy.  Then take residuals, average over
#'     counties within each group and plot. (args: month_dummies = T, product_group_trend = T)
#'
#'   E) Instead of a time trend and month dummy, just take out calendar time
#'     effects (so regress on a dummy for each calendar time, take residuals,
#'     average over counties within each group and plot). (code is set up)
#'     (args: month_dummies = F, calendar_time = T)
#'


remove_time_trends <- function(input_data, outcome_var, month_or_quarter, year_var,
                               month_dummies = FALSE, calendar_time = FALSE,
                               product_group_trend = FALSE, census_region_trends = FALSE,
                               census_division_trends = FALSE, weight_var = NULL,
                               use_one_hot = TRUE){
  assertDataTable(input_data)
  assertCharacter(outcome_var)
  assertCharacter(month_or_quarter)
  assertCharacter(year_var)
  assertLogical(month_dummies)
  assertLogical(calendar_time)
  assertLogical(product_group_trend)
  assertCharacter(weight_var, null.ok = T)
  assertSubset(outcome_var, names(input_data))
  assertSubset(month_or_quarter, c("month", "quarter"))
  assertSubset(month_or_quarter, names(input_data))
  assertSubset(year_var, names(input_data))
  assertSubset(weight_var, names(input_data))

  #### NEED TO TEST ####
  if (census_region_trends & !census_division_trends){
    assertSubset("fips_state", names(input_data))
    regions <- list(
      northeast = c(9, 23, 25, 33, 44, 50, 34, 36, 42),
      midwest = c(17, 18, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46),
      south = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 1, 21, 28, 47, 5, 22, 40, 48),
      west = c(4, 8, 16, 30, 32, 35, 49, 56, 2, 6, 15, 41, 53)
    )

    return_data <- data.table(NULL)
    for (region in regions){
      return_data <- rbind(return_data,
                           remove_time_trends(input_data = input_data[fips_state %in% region],
                                              outcome_var = outcome_var,
                                              month_or_quarter = month_or_quarter,
                                              year_var = year_var,
                                              month_dummies = month_dummies,
                                              calendar_time = calendar_time,
                                              product_group_trend = product_group_trend,
                                              census_region_trends = FALSE,
                                              census_division_trends = FALSE,
                                              weight_var = weight_var,
                                              use_one_hot = use_one_hot))
    }
    return(return_data)
  } else if (census_division_trends){
    assertSubset("fips_state", names(input_data))
    divisions <- list(
      new_england <- c(9, 23, 25, 33, 44, 50),
      middle_atlantic <- c(34, 36, 42),
      east_north_central <- c(17, 18, 26, 39, 55),
      west_north_central <- c(19, 20, 27, 29, 31, 38, 46),
      south_atlantic <- c(10, 11, 12, 13, 24, 37, 45, 51, 54),
      east_south_central <- c(1, 21, 28, 47),
      west_south_central <- c(5, 22, 40, 48),
      mountain <- c(4, 8, 16, 30, 32, 35, 49, 56),
      pacific <- c(2, 6, 15, 41, 53)
    )
    if (census_region_trends){
      warning("Both `census_region_trends` and `census_division_trends` were specified TRUE. Trends will only be residualized on the finer, division level.")
    }
    return_data <- data.table(NULL)
    for (division in divisions){
      return_data <- rbind(return_data,
                           remove_time_trends(input_data = input_data[fips_state %in% division],
                                              outcome_var = outcome_var,
                                              month_or_quarter = month_or_quarter,
                                              year_var = year_var,
                                              month_dummies = month_dummies,
                                              calendar_time = calendar_time,
                                              product_group_trend = product_group_trend,
                                              census_region_trends = FALSE,
                                              census_division_trends = FALSE,
                                              weight_var = weight_var,
                                              use_one_hot = use_one_hot))
    }
    return(return_data)
  }

  ############################

  if (calendar_time & month_dummies){
    stop("`calendar_time' and `month_dummies' cannot both be true")
  } else if (calendar_time & product_group_trend){
    stop("`calendar_time' and `product_group_trend' cannot both be true")
  }
  if (month_or_quarter == "quarter"){
    input_data[, linear_trend := (get(month_or_quarter) + get(year_var) * 4) -
                 (1 + 2008 * 4)]
  } else if (month_or_quarter == "month"){
    input_data[, linear_trend := (get(month_or_quarter) + get(year_var) * 12) -
                 (1 + 2008 * 12)]
  }

  if (!calendar_time & !product_group_trend){
    if (!month_dummies){
      return(
        residualize_outcome(input_data,
                            outcome_var = outcome_var,
                            discrete_vars = NULL,
                            continuous_vars = "linear_trend",
                            weight_var = weight_var,
                            use_one_hot = use_one_hot)
             )
    } else if (month_dummies){
      return(
        residualize_outcome(input_data,
                            outcome_var = outcome_var,
                            discrete_vars = month_or_quarter,
                            continuous_vars = "linear_trend",
                            weight_var = weight_var,
                            use_one_hot = use_one_hot)
      )
    }
  } else if (calendar_time & !product_group_trend){
    # we remove calendar_time effects by demeaning
    if (!is.null(weight_var)){
      input_data[, ct_mean := weighted.mean(x = get(outcome_var),
                                            w = get(weight_var)), by = linear_trend]
    } else {
      input_data[, ct_mean := mean(get(outcome_var)), by = linear_trend]
    }

    input_data[, (paste0(outcome_var, "_residual")) :=
                 get(outcome_var) - ct_mean]
    input_data[, (paste0(outcome_var, "_predicted")) := ct_mean]
    input_data[, ct_mean := NULL]
    return(input_data)
  } else if (product_group_trend & !calendar_time){
    assertSubset("product_group_code", names(input_data))

    ## The goal here is to residualize on a product-group level. The reason for
    ## doing it this way is because the current method with one_hot doesn't
    ## work with product-group specific trends.

    group_codes <- unique(input_data$product_group_code)
    grouped_input_data <- NULL
    for (code in group_codes){
      group_data <- input_data[product_group_code == code]
      group_data <- remove_time_trends(input_data = group_data,
                                       outcome_var = outcome_var,
                                       month_or_quarter = month_or_quarter,
                                       year_var = year_var,
                                       month_dummies = month_dummies,
                                       calendar_time = FALSE,
                                       product_group_trend = FALSE,
                                       weight_var = weight_var,
                                       use_one_hot = use_one_hot)
      grouped_input_data <- rbind(grouped_input_data, group_data)
    }
    return(grouped_input_data)

  }

}

