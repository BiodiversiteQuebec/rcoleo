#' Download
#'
#' get a datafram of unique site and species occurences for any type of sampling campaign.
#'
#' @param campaign_type any campaign type. see [validate_campaign_type()]. Can be a partial match.
#' @param site_type any site type.
#' @param site_code a site_code.
#' @param species_category a species category, for ex. "poissons".
#' @param by_site whether to calculate richness by site
#' @param by_site_type whether to calculate richness by site type
#' @param by_campaign_type whether to calculature richness by campaign type
#' @param by_species_category whether to calculature richness by species_category
#' @param by_date whether to calculature richness by date
#' @param mean whether to return the mean richness for campaigns across a given campaign type or site type
#'
#' @return data.frame with richness values. Richness is simply calculated as unique species names that are not in category "autres".
#' @export
get_richness <- function(campaign_type=NULL, site_type=NULL, site_code=NULL, species_category=NULL, by_site=NULL, by_site_type=NULL, by_campaign_type=NULL, by_date=NULL, by_species_category=NULL, mean=NULL) {

  if(is.null(mean)){
    camp_type <- if ( is.null(campaign_type) ) NULL else validate_campaign_type(campaign_type)

    site_type <- if ( is.null(site_type) ) NULL else validate_site_type(site_type)

    params <-  list(campaign_type = camp_type,
  	          	site_type = site_type,
  	          	site_code = site_code,
                species_category = species_category,
                by_site = by_site,
                by_site_type = by_site_type,
  	          	by_campaign_type = by_campaign_type,
                by_date = by_date,
                by_species_category = by_species_category
            	)
    params <- params[lapply(params,function(t){!is.null(t)}) == TRUE]

    # function to query desired endpoing and campaign type
    query_resp <- query_gen("richness",
                            params)
  }else{
    params <-  list(by_site_type = by_site_type,
                    by_campaign_type = by_campaign_type)
    params <- params[lapply(params,function(t){!is.null(t)}) == TRUE]
    query_resp <- query_gen("richness/mean", params)
  }
  return(query_resp)

}
