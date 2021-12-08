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
#' bar_plot(data = data, variable = "Petal.Width") + add_dunn_letters(data = data, variable = "Petal.Width")
add_dunn_letters <- function(data, variable){
  geom_text(data = anno_dunn(data, variable),
            aes(x = group, y = pos, label = lab), inherit.aes = FALSE,
            size = 5, color = "#ea4f93", fontface = "bold")
}

anno_dunn <- function(data, variable){
  names <- rownames(dunn_letters(data))
  m = gmax(data)
  rownames(m) <- m$group
  res <- data.frame(
    group = names,
    lab = dunn_letters(data)[names, variable],
    pos = m[names, variable] * 1.1
  )
  return(res)
}
