

#' Download all the observations of a specific site
#'
#' Starting from a site data.frame, request all the observations
#'
#' this function assumes you have a site dataframe of the kind returned by
#' [get_sites()] or [download_sites_sf()]. It is a simple manual wrapper around one workflow.
#'
#' @param site_df a dataframe containing site information
#'
#' @export
get_all_observations_from_a_site <- function(site_df, token = bearer()){

  test_camp <- download_all_requests(starting_df = site_df,
                                     token = token,
                                     request_info_col = "campaigns",
                                     query_info = c("type",
                                                    "opened_at",
                                                    "closed_at",
                                                    "site_id"),
                                     endpoint = "campaigns",
                                     request_col_name = "camp_resp")

  # rename this column
  test_camp$camp_resp <- purrr::map(test_camp$camp_resp,
                                    fix_id_name,
                                    replace_id = "campaign_id")

  #
  test_obs <- download_all_requests(starting_df = test_camp,
                                    request_info_col = "camp_resp",
                                    query_info = "campaign_id",
                                    endpoint = "observations",
                                    request_col_name = "obs_resp",
                                    token = token)
  return(test_obs)
}
