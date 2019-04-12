#' @title plotRegulatorZscores
#'
#' @param module_table A data.frame or tibble with the columns "Upstream Regulator", "Activation z-score",
#' and "p-value of overlap".  Use \link[IPAPlots]{prepUpstreamRegulatorLists}
#' @param number_genes_show The number of genes with the top negative and positive z-scores to label
#' @param pos_label_force The replusion force for the positive z-scoring gene names, passed to geom_text_repel
#' @param neg_label_force The replusion force for the negative z-scoring gene names, passed to geom_text_repel
#' @param ... Additional parameters to pass
#'
#' @importFrom ggplot2 ggplot aes geom_point theme geom_hline labs scale_color_gradient scale_fill_gradient
#' @importFrom ggplot2 guides element_rect element_blank
#' @importFrom dplyr filter top_n
#' @importFrom ggrepel geom_label_repel
#'
#'
#' @return a ggplot object
#' @export
#'
#' @examples
plotRegulatorZscores <- function(...){
  UseMethod("plotRegulatorZscores")
}

#' @rdname plotRegulatorZscores
#' @method plotRegulatorZscores data.frame
#' @return
#' @export
plotRegulatorZscores.data.frame <- function(module_table,
                                   number_genes_show = 10,
                                   pos_label_force = 10,
                                   neg_label_force = 10){
  module_table[is.na(module_table)] <- 0
  modplot <- module_table %>%
    ggplot(aes(x = reorder(upstream_regulator, activation_z_score),
               y = activation_z_score,
               color = log(p_value_of_overlap))
    ) +
    geom_point() +
    geom_label_repel(data = subset(module_table %>%
                                    filter(activation_z_score > 0) %>%
                                    top_n(number_genes_show, wt = activation_z_score)),
                    force = pos_label_force,
                    hjust = 0,
                    vjust = 0,
                    color = "black", aes(
                      label = upstream_regulator,
                      fill = log(p_value_of_overlap))) +
    geom_label_repel(data = subset(module_table %>%
                                    filter(activation_z_score < 0) %>%
                                    top_n(-number_genes_show, wt = activation_z_score)),
                    force = neg_label_force,
                    color = 'black', aes(
                      label = upstream_regulator,
                      fill = log(p_value_of_overlap))) +
    theme(panel.background = element_rect(fill = NA,
                                          color = 'grey50'),
          axis.text.x = element_blank(),
          legend.position = "bottom") +
    geom_hline(yintercept = 0, linetype = 3) +
    labs(x = "Upstream Regulator",
         y = "Activation z-score",
         color = "Overlap\np-value") +
    scale_color_gradient(low = "deepskyblue2", high = "tomato2") +
    scale_fill_gradient(low = "deepskyblue2", high = "tomato2") +
    guides(fill = "none",
           alpha = "none")

  return(modplot)
}

#' @rdname plotRegulatorZscores
#' @method plotRegulatorZscores list
#' @return
#' @export
plotRegulatorZscores.list <- function(module_tables, ...){
  modplots <- map(module_tables, plotRegulatorZscores, ...)
  return(modplots)
}
