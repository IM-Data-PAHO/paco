#' Summarize Coverage
#'
#' This function calculates the weighted coverage for a set of countries, using the population as the weights.
#'
#' @param df A data frame containing the columns \code{ISO_CODE}, \code{YEAR}, \code{COVERAGE_CODE}, \code{COVERAGE}, and \code{VALUE}.
#' @param ... Additional grouping variables (unquoted) to group by.
#' @param show_N A logical value indicating whether to include the number of countries in each group in the result. Defaults to \code{FALSE}.
#'
#' @return A data frame with the calculated coverage for each group, including the total population for the group, and optionally the number of countries.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Groups the data by \code{ISO_CODE}, \code{YEAR}, and \code{COVERAGE_CODE}.
#'   \item Filters out rows with missing \code{COVERAGE} or \code{VALUE}.
#'   \item Limits the \code{COVERAGE} to 100.
#'   \item Rounds the \code{COVERAGE} and \code{VALUE} columns.
#'   \item Optionally groups the data by additional columns provided in \code{...}.
#'   \item Calculates the total population for each group.
#'   \item Calculates the weighted coverage for each group.
#'   \item Summarizes the data to get the final coverage and population for each group.
#'   \item Optionally includes the number of countries in each group if \code{show_N} is \code{TRUE}.
#' }
#'
#' @examples
#' df <- data.frame(
#'   ISO_CODE = c("USA", "CAN", "MEX"),
#'   REGION = c("NOA", "NOA", "NOA"),
#'   YEAR = c(2020, 2020, 2020),
#'   COVERAGE_CODE = c("VAC1", "VAC1", "VAC1"),
#'   COVERAGE = c(95, 85, 90),
#'   VALUE = c(1000, 800, 1200)
#' )
#' summarize_coverage(df, show_N = FALSE)
#' # or with additional grouping
#' summarize_coverage(df, REGION, show_N = FALSE)
#'
#' @import dplyr
#' @import janitor
#' @export
summarize_coverage <- function(df, ..., show_N = FALSE) {
  # prepare the data frame
  result <- df %>% 
    # group for each country, year and vaccine
    dplyr::group_by(ISO_CODE, YEAR, COVERAGE_CODE) %>% 
    # filter out rows with no coverage or population data
    dplyr::filter(!(is.na(COVERAGE) | is.na(VALUE))) %>% 
    # limit the coverage to 100
    dplyr::mutate(LIMIT_COVERAGE = min(COVERAGE, 100)) %>% 
    # round both coverage and population
    dplyr::mutate(dplyr::across(c(LIMIT_COVERAGE, VALUE), ~ janitor::round_half_up(.)))
  
  # group accordingly
  result <- result %>% group_by(YEAR, COVERAGE_CODE, ...)
  
  # calculate population for the group
  result <- result %>% 
    # population for each group
    dplyr::mutate(TOTAL_VALUE = sum(VALUE)) %>% 
    # calculate weight of each country w.r.t the group
    dplyr::mutate(WEIGHTED_VALUE = VALUE / TOTAL_VALUE) %>% 
    # calculate weighted coverage for each country
    dplyr::mutate(WEIGHTED_COVERAGE = WEIGHTED_VALUE * LIMIT_COVERAGE) %>% 
    # get coverage for the group
    dplyr::summarise(
      COVERAGE = round(sum(WEIGHTED_COVERAGE), 2),
      VALUE = sum(VALUE),
      # show N conditionally (for backwards compatibility)
      # N is the number of countries in the group
      N = if(show_N) { n() } else {}
    )
  
  # result
  return(result)
}