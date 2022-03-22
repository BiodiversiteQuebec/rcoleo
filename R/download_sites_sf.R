#' Get rcoleo sites as sf dataframe
#'
#' Download the site dataset and its metadata, and make it an sf dataframe.
#'
#' The response from [get_sites()] comes back as a list of simple data.frames.
#' However, for plotting, it is very convenient to work with `sf` data.frames.
#' This function facilitates the process by doing three things:
#' * download all the sites
#' * combine multiple pages in response, if any
#' * pass the response through [sf::st_point()] and [sf::st_as_sf()] to return a sf data.frame
#'
#' @param ... Arguments to [get_sites()]
#'
#' @export
download_sites_sf <- function(...) {
  rcoleo_sites <- get_sites(...)

  # extract the body of the response from the nested list. this might be a list
  body <- purrr::flatten(purrr::map(rcoleo_sites, "body"))

  if (!is.list(body)) stop("Response from get_sites is not a list. Did you change the API or rcoleo functions?")

  # ombine pages if any
  site_info_df <- do.call(rbind, body)

  # convert to sf
  site_info_df$geom.coordinates <- lapply(site_info_df$geom.coordinates,
                                          sf::st_point)

  site_info_sf <-  sf::st_as_sf(site_info_df)
  return(site_info_sf)
}
