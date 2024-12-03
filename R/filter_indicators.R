#' Filter Indicators by Indicator Code and Year
#'
#' This function filters a data table for a specific set of indicators and years, creates a composite indicator code, 
#' and selects relevant columns for output.
#'
#' @param table A data frame containing the data to filter. It must include columns `INDCODE`, `YEAR`, `DIMENSION2`, 
#' `COUNTRY`, and `VALUE`.
#' @param indicators A character vector specifying the indicator codes (`INDCODE`) to filter by.
#' @param years A numeric vector specifying the years (`YEAR`) to filter by.
#' 
#' @import dplyr
#'
#' @export
filter_indicators <- function(table, indicators, years) {
  
  # produce table
  result <- table %>% 
    # filter according to parameters
    filter(INDCODE %in% indicators, YEAR %in% years) %>% 
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