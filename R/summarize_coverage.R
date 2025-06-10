#' Summarize Coverage
#'
#' This function calculates the weighted coverage for a set of countries, using the population as the weights.
#'
#' @param df A data frame containing the columns \code{ISO_CODE}, \code{YEAR}, \code{COVERAGE_CODE}, \code{COVERAGE} and \code{VALUE} (the population).
#' @param ... Additional grouping variables (unquoted) to group by.
#' @param show_N Logical (optional). A logical value indicating whether to include the number of countries in each group in the result. Defaults to \code{FALSE}.
#' @param round_to Numeric (optional). Number of digits for rounding of the final \code{COVERAGE} and \code{VALUE} columns.
#'
#' @return A data frame with the calculated \code{COVERAGE} for each group, including the total population (\code{VALUE}) for the group, and optionally the number of countries (\code{N}).
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Filters out rows with missing \code{COVERAGE} or \code{VALUE}.
#'   \item Limits the \code{COVERAGE} to 100 for each row.
#'   \item Optionally groups the data by additional columns provided in \code{...}.
#'   \item Calculates the total population for each group.
#'   \item Calculates the weighted coverage for each group.
#'   \item Summarizes the data to get the final coverage and population for each group.
#'   \item Rounds to \code{round_to} digits using \code{janitor::round_half_up}.
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
summarize_coverage <- function(df, ..., show_N = FALSE, round_to = 0) {
  # prepare the data frame
  result <- df %>% 
    rowwise() %>% 
    # filter out rows with no coverage or population data
    dplyr::filter(!(is.na(COVERAGE) | is.na(VALUE))) %>% 
    # limit the coverage to 100
    dplyr::mutate(LIMIT_COVERAGE = min(COVERAGE, 100))
  
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
      COVERAGE = janitor::round_half_up( sum(WEIGHTED_COVERAGE), round_to ),
      VALUE = janitor::round_half_up( sum(VALUE), round_to ),
      # show N conditionally (for backwards compatibility)
      # N is the number of countries in the group
      N = if(show_N) { n() } else {}
    )
  
  # result
  return(result)
}