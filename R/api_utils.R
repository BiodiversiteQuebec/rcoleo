#' Transformer la liste de réponses en data.frame
#'
#' @param x liste de réponse des fonctions de GET/POST
#' @param row.names inherits from data.frame. unused
#' @param optional inherits from data.frame. unused
#' @param ... arguments to as.data.frame
#'
#' @export
#'
as.data.frame.coleoGetResp <- function(x, row.names, optional, ...){

  # responses = x

  stopifnot(is.coleoGetResp(x))

  # Loop over query
  # all_body <- lapply(responses, function(query){
  #   # if query contains multipages
  #   if(length(query) > 1){
  #     return(do.call(
  #       plyr::rbind.fill,
  #       lapply(query, function(page){
  #         return(page$body)
  #       })
  #     ))
  #   } else {
  #     if(length(query[[1]]$body) == 0){
  #       return(NULL)
  #     } else {
  #       return(query[[1]]$body)
  #     }
  #   }
  # })

  # return(tibble::as.tibble(do.call(plyr::rbind.fill, all_body), ...))

  just_body <- purrr::map(x, "body")

  combined <- do.call(rbind, just_body)

  as.data.frame(combined, ...)
}

#'  Test sur la classe `coleoGetResp` (Objet S3)

#' @param x Objet à tester
#' @export
is.coleoGetResp <- function(x) inherits(x,'coleoGetResp')

#' Test sur la classe `coleoPostResp` (Objet S3)
#' @param x Objet à tester
#' @export
is.coleoPostResp <- function(x) inherits(x,'coleoPostResp')
