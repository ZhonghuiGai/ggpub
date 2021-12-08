#' barplot using grouping information
#'
#' @param data `data.frame` the import data
#' @param variable `character`, the variable to be ploted
#' @param jitter `logical`, point jitter or not
#' @param border `logical`, whether to show the plot border
#' @param fun.data `character`, the type of error bar, one of "mean_sdl" or "mean_se",
#'  the default value is "mean_sdl"
#' @param angle `numeric`, the x axis test angle
#'
#' @return a ggplots object
#' @export
#'
#' @examples
#' library(ggplot2)
#' data <- iris
#' colnames(data)[5] <- "group"
#' bar_plot(data = data, variable = "Petal.Width")
bar_plot <- function(data,
                    variable = NULL,
                    jitter = FALSE,
                    border = TRUE,
                    fun.data = "mean_sdl",
                    angle = 45){
  if(!"group" %in% colnames(data)){
    stop("The grouping information must be the 'group' collum.")
  }
  stopifnot(variable %in% colnames(data))
  dt <- data[, c("group", variable)]
  dt <- melt(dt, id.vars = "group")

  p <- ggplot(data = dt, aes(x = group, y = value)) +
    stat_summary(geom = "errorbar", fun.data = get(fun.data), width = 0.2, size = 0.7) +
    stat_summary(geom = "bar", fun = mean, aes(fill = group)) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(NA, 1.25*max(dt[, "value"]))) +
    xlab(NULL) +
    ylab(variable) +
    ggtheme::theme_prism(angle = angle, border = border) +
    ggsci::scale_fill_aaas() +
    theme(legend.position = 0)

  if (jitter) p <- p + geom_jitter(width = 0.25, fill = "white", shape = 21, size = 1.8)
  return(p)
}


