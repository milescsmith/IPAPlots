#' @title prepUpstreamRegulatorLists
#'
#' @description Read in a directory populated with upstream regulator tables from IPA
#' and return a named list ready for making plots
#' @param path The path to a directory containing output files from IPA
#' @param filename_extension File extension used for the IPA output files.  Default: "txt"
#'
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @importFrom data.table fread
#' @importFrom purrr map
#' @return list of data.frames, each of which can be plotted using \code[IPAPlots]{plotRegulatorZscores}
#' @export
#'
#' @examples reg_lists <- prepUpstreamRegulatorList(path = "~/path_to_ipa_output")
prepUpstreamRegulatorLists <- function(path, filename_extension = "txt"){
  urfiles <- dir(path = path,
                 pattern = glue("{filename_extension}$"),
                 full.names = TRUE)
  urfiles <- map(urfiles, fread, data.table = FALSE)
  urnames <- dir(path = path,
                 pattern = glue("{filename_extension}$"),
                 full.names = FALSE) %>%
    str_remove(pattern = glue("\\.+{filename_extension}$")) %>%
    reformatModuleNames()
  names(urfiles) <- urnames
  urfiles %<>% removeSeedInfo()
  urfiles <- map(urfiles, function(m){
    colnames(m) %<>% str_replace_all(pattern = " ",
                                 replacement = "_") %>%
      str_replace_all(pattern = "-",
                      replacement = "_") %>%
      tolower()
    m
  })
  return(urfiles)
}
