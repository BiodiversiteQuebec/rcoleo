species_list_sites <- function(campaign_type = "acoustique", site_code){

  assertthat::assert_that(is.character(site_code))

  query_fn <- function(s_c) query_gen("species_list",
                                          list(campaign_type = campaign_type,
                                               site_code = s_c))

  purrr::map(purrr::set_names(site_code), query_fn)
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
