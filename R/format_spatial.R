

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
  assertthat::assert_that(inherits(df_to_inject, "rowwise_df"))


  formatted_input_data <- df_to_inject %>%
    dplyr::mutate(geoj = list(geojsonio::geojson_list(c(lon = lon,
                                                        lat =  lat),
                                                      lat = "lat", lon = "lon")),
                  feat = list(geoj[["features"]]),
                  feat = list(purrr::flatten(feat)),
                  geom = list(feat[["geometry"]]))

  keep_these <- which(! names(formatted_input_data) %in% c("geoj", "feat", "lat", "lon") )

  formatted_input_data <- formatted_input_data[, keep_these]

  return(formatted_input_data)

}
