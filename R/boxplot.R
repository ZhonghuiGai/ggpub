#' boxplot using grouping information
#'
#' @param data `data.frame` the import data
#' @param variable `character`, the variable to be ploted
#' @param border `logical`, whether to show the plot border
#' @param angle `numeric`, the x axis test angle
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' library(ggplot2)
#' data <- iris
#' colnames(data)[5] <- "group"
#' box_plot(data = data, variable = "Petal.Width", angle = 0) + add_dunn_letters(data = data, variable = "Petal.Width")
#' box_plot(data = data, variable = "Petal.Width", angle = 0) + add_aov_letters(data = data, variable = "Petal.Width")
#' box_plot(data = data, variable = "Petal.Width", angle = 0) + add_signif(list(c("setosa", "versicolor")))
box_plot <- function(data,
                     variable = NULL,
                     border = TRUE,
                     angle = 45){
  if(!"group" %in% colnames(data)){
    stop("The grouping information must be the 'group' collum.")
  }
  stopifnot(variable %in% colnames(data))
  dt <- data[, c("group", variable)]
  dt <- melt(dt, id.vars = "group")

  p <- ggplot(data = dt, aes(x = group, y = value)) +
    stat_boxplot(geom = "errorbar", width = 0.25, position = position_dodge(width = 1)) +
    geom_boxplot(aes(fill = group), position = position_dodge(width = 1), width = 0.5,
                 outlier.size = 0.25) +
    scale_y_continuous(limits = c(NA, 1.2*max(dt[, "value"])))+
    xlab(NULL) +
    ylab(variable) +
    ggtheme::theme_prism(angle = angle, border = border) +
    ggsci::scale_fill_aaas() +
    theme(legend.position = 0)
  return(p)
}



