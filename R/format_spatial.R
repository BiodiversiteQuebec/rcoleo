

#' Format les donnees avec leurs cordonnées spatiaux
#'
#' the location of data needs to be correctly formatted for injection into coleo
#'
#' @param df_to_inject data.frame to be injected
#'
#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @export
format_spatial <- function(df_to_inject) {

  ## dataframe must have lat and lon columns
  assertthat::assert_that(assertthat::has_name(df_to_inject, "lon"))
  assertthat::assert_that(assertthat::has_name(df_to_inject, "lat"))

  formatted_input_data <- df_to_inject %>%
    dplyr::mutate(geoj = purrr::map2(.x = .data$lon,
                                     .y = .data$lat,
                                     ~ geojsonio::geojson_list(c(lon = .x, lat =  .y),
                                                               lat = "lat", lon = "lon")),
                  feat = purrr::map(.data$geoj, "features"),
                  feat = purrr::map(.data$feat, purrr::flatten),
                  geom = purrr::map(.data$feat, "geometry"))

  keep_these <- which(! names(formatted_input_data) %in% c("geoj", "feat", "lat", "lon") )

  formatted_input_data <- formatted_input_data[, keep_these]

  return(formatted_input_data)

}
