#' Filter Indicators by Indicator Code and Year
#'
#' This function filters a data table for a specific set of indicators and years, creates a composite indicator code, 
#' and selects relevant columns for output.
#'
#' @param table A data frame containing the data to filter. 
#' It must include columns \code{INDCODE}, \code{YEAR}, \code{DIMENSION2}, \code{COUNTRY}, and \code{VALUE}.
#' @param indicators A character vector specifying the indicator codes (\code{INDCODE}) to filter by.
#' @param years Optional. A numeric vector specifying the years (\code{YEAR}) to filter by.
#' 
#' @import dplyr
#'
#' @export
filter_indicators <- function(table, indicators, years = NULL) {
  
  # filter table
  filtered_table <- table %>% 
    filter(INDCODE %in% indicators)
  
  if(!is.null(years)) {
    filtered_table <- filtered_table %>% filter(YEAR %in% years)
  }
  
  # produce table
  result <- filtered_table %>% 
    # produce COMPOSITE_INDCODE
    mutate(COMPOSITE_INDCODE = if_else(
      is.na(DIMENSION2) | DIMENSION2 == "", 
      INDCODE, 
      paste0(INDCODE, "_", DIMENSION2)
    )) %>% 
    # select columns of interest
    select(COUNTRY, YEAR, INDCODE, COMPOSITE_INDCODE, VALUE)
  
  return(result)
}