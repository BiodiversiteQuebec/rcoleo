
download_one_observation_set <- function(campaign_response_df, ...) {
  resp <- query_df_gen(query_df = campaign_response_df,
                       query_info = c("id"),
                       endpoint = "observations", ...)

  return(resp)

}

#
# rc <- mapselector::get_rcoleo_sites_sf()
# camp_resp_df <- download_all_campaigns(head(rc))
# #
#
# camp_resp_df <- download_all_campaigns(rc[8,])
#
# test_resp <- download_one_observation_set(camp_resp_df$campaign_responses[[1]])
# #
# #
# # camp_resp_df
# #
# dplyr::glimpse(test_resp)
#
#
# test_gen_dl <- download_all_campaigns(campaign_df = camp_resp_df, camp_col = "campaign_responses")
#
#
# dplyr::glimpse(test_gen_dl)
