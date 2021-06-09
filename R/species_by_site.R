# functions for getting species lists and species-by-site matrices for specific sites


#' get the species list for a site
#'
#' @param campaign_type campaign type. Can be any valid campaign name, see [validate_campaign_type()]
#' @param site_code any site code. can also be a vector
#'
#' @return the species list for a site, as a list
#' @export
get_species_list <- function(campaign_type = NULL, site_code = NULL){

  camp_type <- validate_campaign_type(campaign_type)

  params <- list(campaign_type = camp_type, site_code = site_code)

  params <- params[lapply(params,function(t){!is.null(t)}) == TRUE]

  query_resp <- query_gen("species_list",params)
  return(query_resp)
}


species_site_matrix <- function(species_list){

  # annoying -- pull out the names of the species from the data.frame
  just_species <- purrr::map(species_list, "taxa_name")

  # combine all the names together into a total list
  total_spp <- purrr::reduce(just_species, union)

  # identify which of these total species are in each site - in a 0-1 matrix
  matches <- purrr::map(just_species, ~ match(x = total_spp, table = ., nomatch = 0L))

  m <- (do.call(what = rbind, args = purrr::map(matches, ~ . > 0L))) * 1

  # rename appropriately
  rownames(m) <- names(species_list)
  colnames(m) <- total_spp

  return(m)
}

## convenience function to combine these together
