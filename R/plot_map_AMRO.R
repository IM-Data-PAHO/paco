#' Create a Map of the Americas with Caribbean Inset
#'
#' This function generates a map of the Americas (using provided ADM0 files) and paints it according
#' to the provide data. It includes a zoomed inset for the Caribbean.
#'
#' @param shp An \code{sf} object with country-level geometries and ISO3 codes.
#' @param data A data frame with a column matching \code{iso_col}, and a fill column (default: \code{DATA}).
#' @param iso_col Character. Name of the column with ISO3 codes in both \code{shp} and \code{data}.
#' @param fill_col Character. Name of the column with the data to use as fill.
#' @param region_col Character. Name of the column with the subregion codes for AMRO, must be present in \code{shp} or \code{data}. Default is \code{REGION_CODE}.
#' @param fill_levels Character vector. Optional levels for the factor of \code{fill_col}.
#' @param fill_labels Character vector. Optional labels for factor levels of \code{fill_col}.
#' @param legend_title Character. Title for the legend. Default is "Value".
#' @param palette Character. Color palette to use in \code{scale_fill_brewer()}. Default is "PuBuGn".
#' @param add_zoom Logical. Whether to add the zoomed inset for the Caribbean. Default is \code{TRUE}.
#'
#' @return A \code{ggplot} object.
#' @export
#' @import sf ggplot2 dplyr grid stringr
plot_map_AMRO <- function(shp,
                          data,
                          iso_col,
                          fill_col,
                          region_col = "REGION_CODE",
                          fill_levels = NULL,
                          fill_labels = NULL,
                          legend_title = "Value",
                          palette = "PuBuGn",
                          add_zoom = TRUE) {
  
  # TODO: Add verification of inputs
  # TODO: Make CAR plot optional
  
  # Join data with shapefile
  shp_data <- shp %>%
    right_join(data, by = iso_col)
  
  # Ensure fill column is a factor and ordered if labels provided
  if (!is.null(fill_labels) & !is.null(fill_levels)) {
    shp_data[[fill_col]] <- factor(
      shp_data[[fill_col]],
      levels = fill_levels,
      labels = fill_labels,
      ordered = TRUE
    )
  }
  
  # Caribbean subset for inset
  CAR_shp <- shp_data %>%
    filter(!!sym(region_col) %in% c("LAC", "CAR")) %>%
    filter(!(!!sym(iso_col) %in% c("GUF", "GUY", "SUR", "BLZ")))
  
  # Inset plot (Caribbean)
  if(add_zoom) {
    CAR_subplot <- ggplot(CAR_shp) +
      geom_sf(aes_string(fill = fill_col), linewidth = 0.06) +
      scale_fill_brewer(palette = palette, drop = FALSE) +
      theme_void() +
      coord_sf(crs = st_crs(3857), expand = TRUE) +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "white", linewidth = 0),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.35)
      )
  }
  
  # Define coordinate limits only if zoom is enabled
  my_coord_sf <- if (add_zoom) {
    coord_sf(xlim = c(-19000000, 10000000), crs = st_crs(3857), expand = FALSE)
  } else {
    coord_sf(crs = st_crs(3857), expand = FALSE)
  }
  
  # Main map
  basemap <- ggplot(shp_data) +
    geom_sf(aes_string(fill = fill_col), linewidth = 0.1) +
    my_coord_sf +
    scale_fill_brewer(palette = palette, drop = FALSE) +
    labs(fill = str_wrap(legend_title, width = 20)) +
    theme_void() +
    theme(legend.position = c(0.1, 0.2))
  
  # Add annotation
  if(add_zoom) {
    basemap <- basemap + 
      annotation_custom(
        grob = ggplotGrob(CAR_subplot),
        xmin = -6200000 + 400000, xmax = 11900000 + 400000,
        ymin = -500000, ymax = 19000000
      )
  }
  
  return(basemap)
}
