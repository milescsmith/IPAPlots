#' removeSeedInfo
#' @description This gets rid of the "(and other miRNAs w/seed GGAAUGU)"-like info after all the miRNAs
#' @param urlists
#'
#' @importFrom stringr str_remove
#' @importFrom purrr map
#' @return list
#' @export
#'
#' @examples
removeSeedInfo <- function(urlists){
  map(urlists, function(j){
    j[['Upstream Regulator']] %<>% str_remove(pattern = " \\([0-9a-zA-Z\\/ ]+\\)")
    j
  })
}
