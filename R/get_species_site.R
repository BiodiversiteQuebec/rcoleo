#' Download one species by site matrix
#'
#' get a datafram of unique site and species occurences for any type of sampling campaign.
#'
#' @param campaign_type any campaign type. see [validate_campaign_type()]. Can be a partial match.
#'
#' @return data. frame of species occurences in sites according to the database.
#' @export
get_species_site <- function(campaign_type) {

  camp_type <- validate_campaign_type(campaign_type)

  # function to query desired endpoing and campaign type
  query_resp <- query_gen("sites_species",
                          list(campaign_type = camp_type))

  return(query_resp)

}
