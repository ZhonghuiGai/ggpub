#' add significance
#'
#' @param comparisons a list
#' @param test the type of test
#' @param y_position numeric
#' @param map_signif_level logical
#' @param step_increase numeric
#' @param tip_length numeric
#' @param ... other parameters
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' library(ggplot2)
#' data <- iris
#' colnames(data)[5] <- "group"
#' bar_plot(data = data, variable = "Petal.Width") + add_signif(list(c("setosa", "versicolor")))
add_signif <- function(comparisons = NULL,
                       test = "wilcox.test",
                       y_position = NULL,
                       map_signif_level = TRUE,
                       step_increase = 0,
                       tip_length = 0,
                       ...){
  ggsignif::geom_signif(comparisons = comparisons,
                        test = test,
                        y_position = y_position,
                        size = 0.5,
                        textsize = ifelse(map_signif_level, 6, 4),
                        map_signif_level = ifelse(map_signif_level, TRUE,
                                                  function(p) sprintf("p = %.3f", p)),
                        tip_length = tip_length,
                        margin_top = 0.05,
                        vjust = ifelse(map_signif_level, 0.5, 0),
                        fontface = ifelse(map_signif_level, "bold", "bold.italic"),
                        step_increase = step_increase,
                        ...)
}
