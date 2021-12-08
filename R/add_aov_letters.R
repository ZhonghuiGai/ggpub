#' add significance
#'
#' @param data data frame
#' @param variable variable
#'
#' @return
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' #' library(ggplot2)
#' data <- iris
#' colnames(data)[5] <- "group"
#' bar_plot(data = data, variable = "Petal.Width") + add_aov_letters(data = data, variable = "Petal.Width")
add_aov_letters <- function(data, variable){
  geom_text(data = anno_aov(data, variable),
            aes(x = group, y = pos, label = lab), inherit.aes = FALSE,
            size = 5, color = "#ea4f93", fontface = "bold")

}

anno_aov <- function(data, variable){
  data <- data[, c("group", variable)]
  res <- data.frame(
    group = rownames(aov_letters(data)),
    lab = aov_letters(data)[, variable],
    pos = 1.1*gmax(data)[, variable]
  )
  return(res)
}


