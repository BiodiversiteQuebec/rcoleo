#' Retourne les observations présentes dans la base de données
#'
#' @return
#' returns a dataframe with all the observations, and the type of campaign that yielded them
#' @examples
#' \dontrun{
#' get_all_obs()
#' }
#' @export
get_all_obs <- function(){
  obs_list <- get_gen(endpoints()$observations)
  obs_df <- do.call(rbind, obs_list$body)

  camps <- get_gen(endpoints()$campaigns)
  camps_df <- do.call(rbind, camps$body)

  merge(obs_df, camps_df, by.x = "campaign_id", by.y = "id")
}


