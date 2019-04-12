#' @title prepCanonicalPathways
#'
#' @description Read in data for Canonical pathways as exported by IPA and prepare for plotting
#'
#' @param path Path to files exported from IPA
#' @param filename_extension Extension on output files.  Used to filter files in the directory.  Default: txt
#'
#' @importFrom data.table fread
#' @importFrom purrr map
#' @importFrom stringr str_remove
#'
#' @return
#' @export
#'
#' @examples
prepCanonicalPathways <- function(path, filename_extension = "txt"){
  plot_files <- dir(path = path,
                    full.names = TRUE,
                    pattern = "txt$")

  plot_data <- map(plot_files, fread, data.table = FALSE)

  names(plot_data) <- dir(path = info_text_files_path,
                          full.names = FALSE,
                          pattern = "txt$") %>%
    str_remove(".txt$")
  return(plot_data)
}
