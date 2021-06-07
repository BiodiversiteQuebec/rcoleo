#' Download
#'
#' get a datafram of unique site and species occurences for any type of sampling campaign.
#'
#' @param campaign_type any campaign type. see [validate_campaign_type()]. Can be a partial match.
#' @param site_type any site type.
#' @param site_code a site_code.
#' @param by_site whether to calculate richness by site
#' @param by_site_type whether to calculate richness by site type
#' @param by_campaign_type whether to calculature richness by campaign type
#' @param by_date whether to calculature richness by date
#'
#' @return data.frame with richness values. Richness is simply calculated as unique species names that are not in category "autres".
#' @export
get_richness <- function(campaign_type=NULL, site_type=NULL, site_code=NULL, by_site=NULL, by_site_type=NULL, by_campaign_type=NULL, by_date=NULL) {

  camp_type <- if ( is.null(campaign_type) ) NULL else validate_campaign_type(campaign_type)

  site_type <- if ( is.null(site_type) ) NULL else validate_site_type(site_type)

  params <-  list(campaign_type = camp_type,
	          	site_type = site_type,
	          	site_code = site_code,
              by_site = by_site,
	          	by_site_type = by_site_type,
	          	by_campaign_type = by_campaign_type,
              by_date = by_date,
          	)
  params <- params[lapply(params,function(t){!is.null(t)}) == TRUE]

  # function to query desired endpoing and campaign type
  query_resp <- query_gen("richness",
                          params)

  return(query_resp)

}
