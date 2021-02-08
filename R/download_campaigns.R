

#' Generate an API call from a data frame
#'
#' Take a data.frame that stores query information and send a GET request with
#' that info
#'
#' The coleo API returns a data.frame for most reponses. This data.frame can in
#' turn become the input for another request. This function facilitates that
#' process
#'
#' @param query_df the data.frame containing the query information. Usually from
#'   a call to [get_gen()]
#' @param query_info a character vector of column names in `query_df` which will
#'   be passed to the API
#'
#' @return a data.frame containing the body of the responses.
#'
#' @export
query_df_gen <- function(query_df, query_info, ...) {
  if (!is.data.frame(query_df)) stop("download_one_campaign expects a data frame")

  assertthat::assert_that(assertthat::has_name(query_df,
                                               query_info))

  # select the variables required for a request and ask the API
  queries <- purrr::transpose(query_df[c("type",
                                                 "opened_at",
                                                 "closed_at",
                                                 "site_id")])

  responses <- purrr::map(queries,
                          rcoleo::get_gen,
                          endpoint = rcoleo:::endpoints()$campaigns, ...)



  if (!is.list(responses)) stop("Response from get_sites is not a list.
                                Did you change the API or rcoleo functions?")

  # extract body and combine pages

  resp_df <- purrr::map_dfr(responses, "body")

  return(resp_df)

}


# TODO perhaps generalize this function to take an endpoint and a vector of names
download_one_campaign_set <- function(campaign_df, ...) {
  if (!is.data.frame(campaign_df)) stop("download_one_campaign expects a data frame")

  assertthat::assert_that(assertthat::has_name(campaign_df,
                                          c("type", "opened_at",
                                            "closed_at", "site_id")))

  # select the variables required for a request and ask the API
  camp_queries <- purrr::transpose(campaign_df[c("type",
                                                 "opened_at",
                                                 "closed_at",
                                                 "site_id")])

  camp_responses <- purrr::map(camp_queries,
                               rcoleo::get_gen,
                               endpoint = rcoleo:::endpoints()$campaigns, ...)



  if (!is.list(camp_responses)) stop("Response from get_sites is not a list. Did you change the API or rcoleo functions?")

  # extract body and combine pages

  camp_df <- purrr::map_dfr(camp_responses, "body")

  return(camp_df)

}


#' Download all the campaigns
#'
#' Download every campaign described in a data.frame of sites
#'
#' This function is designed to work with the same kind of object returned by
#' [download_sites_sf()]. However it does not need to be an sf data.frame. Any
#' data.frame with a column containing campaign information _should_ work.
#'
#' @param campaign_df A data frame containing campaign information
#' @param camp_col The name of the column holding the campaigns. Defaults to
#'   "campaigns" which is what the API returns
#'
#' @return A data.frame just like the `campaign_df`, but with a list-column
#'   which contains the response. This column is called `campaign_responses`
#'
#' @export
download_all_campaigns <- function(campaign_df, camp_col = "campaigns"){
  # check that the campaign column is a list
  assertthat::assert_that(
    assertthat::has_name(campaign_df,camp_col))
  assertthat::assert_that(
    is.list(campaign_df[[camp_col]]))

  # Go down the list and request campaigns. Don't do anything if its a 0 row df

  all_campaigns <- purrr::map_if(campaign_df[[camp_col]],
                                 function(d) nrow(d) > 0,
                                 download_one_campaign_set)

  campaign_df$campaign_responses <- all_campaigns

  return(campaign_df)

}
