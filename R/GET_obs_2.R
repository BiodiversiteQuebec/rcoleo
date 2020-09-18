get_obs_2 <- function (site_code = NULL, opened_at = NULL, closed_at = NULL,
          type = NULL, ...)
{
  endpoint <- endpoints()$observations
  token <- ifelse(is.na(bearer()), list(...)$token, bearer())
  responses <- list()
  class(responses) <- "coleoGetResp"
  if (all(is.null(site_code), is.null(opened_at), is.null(closed_at),
          is.null(type))) {
    responses[[1]] <- get_gen(endpoint, ...)
    responses[[1]] <- lapply(responses[[1]], function(page) {
      campaign_ids <- unique(page$body$campaign_id)
      campaigns_info <- list()
      for (i in 1:length(campaign_ids)) {
        campaign <- httr::content(httr::GET(url = paste0(server(),
                                                         "/api/v1/campaigns/", campaign_ids[i]),
                                            config = httr::add_headers(`Content-type` = "application/json",
                                                                       Authorization = paste("Bearer", token)),
                                            ua), simplify = TRUE)
        if (is.null(campaign$closed_at))
          campaign$closed_at <- NA
        campaigns_info[[i]] <- data.frame(campaign_id = campaign$id,
                                          site_id = campaign$site_id, opened_at = campaign$opened_at,
                                          closed_at = campaign$closed_at, type = campaign$type)
      }
      campaigns_info <- do.call(plyr::rbind.fill, campaigns_info)
      page$body <- merge(page$body, campaigns_info, by = "campaign_id")
      site_ids <- unique(page$body$site_id)
      sites_info <- list()
      for (i in 1:length(site_ids)) {
        site <- httr::content(httr::GET(url = paste0(server(),
                                                     "/api/v1/sites/", site_ids[i]), config = httr::add_headers(`Content-type` = "application/json",
                                                                                                                Authorization = paste("Bearer", token)),
                                        ua), simplify = TRUE)
        sites_info[[i]] <- data.frame(site_id = site$id,
                                      site_code = site$site_code, cell_code = site$cell$cell_code)
      }
      sites_info <- do.call(plyr::rbind.fill, sites_info)
      page$body <- merge(page$body, sites_info, by = "site_id")
      page$body <- dplyr::select(page$body, "id",
                                 "cell_code", "site_code", "opened_at",
                                 "closed_at", "type", "axis",
                                 "distance", "distance_unit", "sample_id",
                                 "date_obs", "obs_species.taxa_name",
                                 "obs_species.variable", "obs_species.value",
                                 "is_valid", "media", "notes",
                                 "created_at", "updated_at")
      return(page)
    })
  }
  else {
    len <- max(c(length(site_code), length(opened_at), length(closed_at),
                 length(type)))
    for (r in 1:len) {
      campaigns_ids <- as.data.frame(get_campaigns(site_code = site_code[r],
                                                   opened_at = opened_at[r], closed_at = closed_at[r],
                                                   type = type[r]))$id
    }
    for (i in 1:length(campaigns_ids)) responses[[i]] <- get_gen(endpoint,
                                                                 query = list(campaign_id = campaigns_ids[i]), ...)
    responses <- lapply(responses, function(response) {
      lapply(response, function(page) {
        campaign_id <- unique(page$body$campaign_id)
        stopifnot(length(campaign_id) == 1)
        campaign_info <- httr::content(httr::GET(url = paste0(server(),
                                                              "/api/v1/campaigns/", campaign_id), config = httr::add_headers(`Content-type` = "application/json",
                                                                                                                             Authorization = paste("Bearer", ifelse(is.na(bearer()),
                                                                                                                                                                    token, bearer()))), ua), simplify = TRUE)
        site_info <- httr::content(httr::GET(url = paste0(server(),
                                                          "/api/v1/sites/", campaign_info$site_id),
                                             config = httr::add_headers(`Content-type` = "application/json",
                                                                        Authorization = paste("Bearer", ifelse(is.na(bearer()),
                                                                                                               token, bearer()))), ua), simplify = TRUE)
        page$body$site_code <- site_info$site_code
        page$body$cell_code <- site_info$cell$cell_code
        page$body$opened_at <- campaign_info$opened_at
        page$body$closed_at <- campaign_info$closed_at
        page$body$type <- campaign_info$type
        page$body <- dplyr::select(page$body, "id",
                                   "cell_code", "site_code", "opened_at",
                                   "closed_at", "type", "axis",
                                   "distance", "distance_unit", "sample_id",
                                   "date_obs", "obs_species.taxa_name",
                                   "obs_species.variable", "obs_species.value",
                                   "is_valid", "media", "notes",
                                   "created_at", "updated_at")
        return(page)
      })
    })
  }
  return(responses)
}
# <bytecode: 0x000000001eddc510>
#   <environment: namespace:rcoleo>
