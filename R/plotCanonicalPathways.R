#' @title plotCanonicalPathways
#'
#' @param plot_data Either one data.frame/tibble or a list of data.frames/tibbles to plot.
#' Accepts data from \link[IPAPlots]{prepCanonicalPathways}
#' @param number_pathways Number of pathways to plot.  Default: 5
#' @param ... Additional parameters to pass
#'
#' @importFrom dplyr arrange select top_n mutate
#' @importFrom ggplot2 ggplot aes geom_col coord_flip scale_fill_gradient2 labs scale_x_discrete theme element_line element_text
#' @importFrom stringr str_remove str_replace str_wrap
#' @importFrom forcats fct_reorder
#' @importFrom purrr map
#'
#' @return
#' @export
#'
#' @examples
plotCanonicalPathways <- function(plot_data, ...){
  UseMethod('plotCanonicalPathways')
}


#' @rdname plotCanonicalPathways
#' @method plotCanonicalPathways data.frame
#' @return
#' @export
plotCanonicalPathways.data.frame <- function(plot_data, number_pathways = 5){
  # IPA cannot always assign a z-scores and instead of going with 0, it uses NaN
  # This kills the coloration of the graph
  plot_data %<>% rename(z_score = `z-score`) %>% mutate(z_score = replace_na(z_score, replace = 0))
  # This makes sure we have an equivalent min and max range
  scale_val <- max(abs(plot_data[["z_score"]]))

  pl <- plot_data %>%
    arrange(`-log(p-value)`) %>%
    select(`Ingenuity Canonical Pathways`,
           `-log(p-value)`,
           z_score,
           Ratio) %>%
    top_n(number_pathways, `-log(p-value)`) %>%
    mutate(pathway = fct_reorder(`Ingenuity Canonical Pathways`,
                                 `-log(p-value)`)) %>%
    ggplot(aes(x = pathway,
               y = `-log(p-value)`,
               fill = z_score)) +
    geom_col(color = 'black') +
    coord_flip() +
    scale_fill_gradient2(low = 'blue',
                         mid = "white",
                         high = 'orange',
                         midpoint = 0,
                         limits = c(-scale_val,scale_val)) +
    labs(title = names(plot_data) %>%
           str_remove(pattern = "^Canonical pathways for ") %>%
           str_replace(pattern = "DEG", replacement = "DE genes"),
         fill = "Activation\nz-score") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    theme(panel.background = element_blank(),
          axis.line.x = element_line(color="black", size = 0.25),
          axis.line.y = element_line(color="black", size = 0.25),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.title.x = element_text(size = 18),
          axis.title.y = element_text(size = 18),
          legend.title = element_text(size = 18),
          plot.title = element_text(size = 20))
  return(pl)
}

#' @rdname plotCanonicalPathways
#' @method plotCanonicalPathways list
#' @return
#' @export
plotCanonicalPathways.list <- function(plot_data, number_pathways = 5, ...){
  pls <- map(plot_data, plotCanonicalPathways, number_pathways, ...)
  return(pls)
}
