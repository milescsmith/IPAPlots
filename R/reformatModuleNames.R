#' @title reformatModuleNames
#'
#' @description Reformats shorthand used for file names to text for titles
#'
#' @param urlist_names List of character vectors to rename.
#'
#' @importFrom stringr str_replace str_match str_glue
#' @importFrom purrr map_chr
#' @return List of character vectors.
#' @export
#'
#' @examples
reformatModuleNames <- function(urlist_names){
  map_chr(urlist_names, function(i){
    i %>%
      str_replace(pattern = " POS ",
                  replacement = " ANA+ ") %>%
      str_replace(pattern = " NEG ",
                  replacement = " ANA- ") %>%
      str_replace(pattern = " B ",
                  replacement = " B cells ") %>%
      str_replace(pattern = " T ",
                  replacement = " T cells ") %>%
      str_replace(pattern = " M ",
                  replacement = " Monocytes ") %>%
      str_replace(pattern = "DEG module",
                  replacement = "DE genes") %>%
      str_replace(pattern = '[a-z0-9]+$',
                  replacement = str_glue("{str_match(i, pattern = '[a-z0-9]+$')} module"))
    })
}
