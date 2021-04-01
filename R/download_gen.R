

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
#' @param endpoint the API endpoint to send all this to
#' @param ... extra arguments to [get_gen()]
#'
#' @return a data.frame containing the body of the responses.
#'
#' @export
query_df_gen <- function(query_df, query_info, endpoint, ...) {

  assertthat::assert_that(
    endpoint %in% names(endpoints())
  )

  if (!is.data.frame(query_df)) stop("download_one_campaign expects a data frame")

  assertthat::assert_that(assertthat::has_name(query_df,
                                               query_info))

  ## assert that sensible things are being requested
  if (endpoint == "observations") assertthat::assert_that(query_info == "campaign_id")

  # select the variables required for a request and ask the API
  queries <- purrr::transpose(query_df[query_info])

  # send a GET request to the specified endpoint.
  responses <- purrr::map(queries,
                          rcoleo::get_gen,
                          endpoint = endpoints()[[endpoint]], ...)



  if (!is.list(responses)) stop("Response from get_sites is not a list.
                                Did you change the API or rcoleo functions?")

  # extract body and combine pages

  resp_df <- purrr::map_dfr(responses, "body")

  return(resp_df)

}


#' Download all queries stored in a list
#'
#' Download the result of every query described in list-column
#'
#' This function is designed to work with the same kind of object returned by
#' [download_sites_sf()]. However it does not need to be an sf data.frame. Any
#' data.frame with a column containing campaign information _should_ work. Very
#' often a table like this has a list-column of other, smaller dataframes. These
#' describe the info you need to send to the API to get a table related to the
#' first.
#'
#' @param starting_df A data frame containing info about a table in the API. You
#'   start with this, and you want to add the new information to it
#' @param request_info_col The name of the column holding the campaigns.
#'   Defaults to "campaigns" which is what the API returns. It's expected to be
#'   a list-column of data.frames
#' @param query_info The names of the columns that hold the query information. passed to [query_df_gen()]
#' @param endpoint the name of the endpoint to query. passed to [query_df_gen()]
#' @param request_col_name name of column where the responses will go after the request! ie, the output.
#' @param token the authentification token
#'
#' @return A data.frame just like the `starting_df`, but with a list-column
#'   which contains the response.
#'
#' @export
download_all_requests <- function(starting_df,
                                  request_info_col = "campaigns",
                                  query_info,
                                  endpoint,
                                  request_col_name,
                                  token = bearer()){
  # check that the campaign column is a list
  assertthat::assert_that(
    assertthat::has_name(starting_df, request_info_col))
  assertthat::assert_that(
    is.list(starting_df[[request_info_col]]))

  # Go down the list and request campaigns. Don't do anything if its a 0 row df

  all_responses <- purrr::map_if(starting_df[[request_info_col]],
                                 function(d) nrow(d) > 0,
                                 ~ query_df_gen(query_df = .,
                                                query_info = query_info,
                                                endpoint = endpoint,
                                                token = token))

  starting_df[[request_col_name]] <- all_responses

  return(starting_df)

}


download_one_campaign_set <- function(campaign_df, ...) {
  resp <- query_df_gen(query_df = campaign_df,
                       query_info = c("type",
                                      "opened_at",
                                      "closed_at",
                                      "site_id"),
                       endpoint = "campaigns")

  return(resp)

}



fix_id_name <- function(df, replace_id){
  nn <- names(df)
  is_id <- which(nn == "id")
  names(df)[is_id] <- replace_id
  return(df)
}


