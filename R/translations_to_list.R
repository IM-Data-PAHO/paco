#' Convert a translation data frame to a nested list
#'
#' This function receives a data frame with PAHO-style translations and produces a nested list using specified ID columns and a language column.
#'
#' @param data A data frame containing translation data.
#' @param language A string specifying the name of the column containing translated values in a specific language.
#' @param id_module An optional string specifying the column to be used as the first level key of the list.
#' @param id_section An optional string specifying the column to be used as the second level key.
#' @param id_element An optional string specifying the column to be used as the third level key.
#'
#' @return A nested list with the structure module > section > element.
#'
#' @note Originally written by Alejandro Vasquez.
#'
#' @import dplyr
#' @export
translations_to_list <- function(data, language, id_module = "id_module", id_section = "id_section", id_element = "id_element") {
  
  # Select the IDs and the language column.
  cols_to_select <- c(id_module, id_section, id_element, language)
  df_to_translate <- data %>% select(all_of(cols_to_select))
  
  # Create an empty list that will contain all other sublists.
  list_module <- list()
  # Initiate a counter to track the index for every module.
  count_module <- 1
  
  # Repeat code for each unique module in the id_module column.
  for(module in unique(df_to_translate$id_module)){
    # Create an empty list that will contain all id_section lists.
    list_section <- list()
    # Initiate a counter to track the index for every section within the module.
    count_section <- 1
    # Repeat code for each unique section in the id_section column.
    for(section in unique(df_to_translate[df_to_translate$id_module == module, id_section])){
      # Create an empty list that will contain all elements from id_element as 
      # a key, and the language as the value.
      list_element <- list()
      # Initiate a counter to track the index for every element within the 
      # section.
      count_element <- 1
      # Repeat code for each unique element in the id_element column.
      for(element in unique(df_to_translate[df_to_translate$id_module == module &
                                            df_to_translate$id_section == section, id_element])){
        # Add to the element list all the pairs of the id_element (as a key), 
        # and the language (as the value).
        list_element <- append(list_element, list("temp_element" = df_to_translate[df_to_translate$id_module == module &
                                                                                     df_to_translate$id_section == section &
                                                                                     df_to_translate$id_element == element, language]))
        # Modify the name of every object of the list to match the id_element
        # value.
        names(list_element)[count_element] <- element
        # Increase by 1 the count_element counter.
        count_element <- count_element + 1
      }
      # Add to the section list all the pairs of the id_section (as a key), and 
      # the list_element (as the value).
      list_section <- append(list_section, list("temp_section" = list_element))
      # Modify the name of every object of the list to match the id_section
      # value.
      names(list_section)[count_section] <- section
      # Increase by 1 the count_section counter.
      count_section <- count_section + 1
    }
    # Add to the module list all the pairs of the id_module (as a key), and the 
    # list_section (as the value).
    list_module <- append(list_module, list("temp_module" = list_section))
    # Modify the name of every object of the list to match the id_module
    # value.
    names(list_module)[count_module] <- module
    # Increase by 1 the count_module counter.
    count_module <- count_module + 1
  }
  
  # Return the list_module object, which contains the nested list.
  return(list_module)
}
