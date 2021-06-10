
#' get the campaign summary for a site
#'
#' @param campaign_type campaign type. Can be any valid campaign name, see [validate_campaign_type()]
#' @param site_code any site code. Must be a single value
#'
#' @details This function gives a list of all sampling campaigns ever collected at a site.
#' You must supply either the type of campaign, the code of the site, or both.
#'
#' @return data.frame of campaigns and years (for a site) or sites and years (for a campaign)
#' @export
get_campaign_summary <- function(campaign_type = NULL, site_code = NULL){

  # need to supply a site_code
  assertthat::assert_that(is.character(site_code) | is.character(campaign_type))

  camp_type <- if ( is.null(campaign_type) ) NULL else validate_campaign_type(campaign_type)

  params <- list(campaign_type = camp_type, site_code = site_code)

  # drop any arguments that are NULL
  params <- purrr::discard(params, is.null)

  query_resp <- query_gen("summary", params)
  return(query_resp)
}

