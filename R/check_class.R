#' Check if a data frame having grouping information, the the class of the rest vvariables are numeric!
#'
#' @param data a data frame
#'
#' @return a logical value
#'
#' @author Zhonghui Gai
#' @examples
#' check_class(iris)
#' data <- iris
#' colnames(data)[5] <- "group"
#' check_class(data)
check_class <- function(data){
  stopifnot(is.data.frame(data))
  if (!"group" %in% colnames(data)) {
    warning("'group' isnot a variable of data, sugestting that there may be no grouping information!")
  } else {
    message("group is one of variables!")
  }
  res <- vapply(X = data, FUN = class, FUN.VALUE = character(1))
  if ("group" %in% colnames(data)) {
    res <- res[!(names(res) %in% "group")]
  }
  ck <- all(res == "numeric")
  if (all(res == "numeric")) {
    cat("Congratulations! All the variables are numeric!\n")
  } else {
    warning("Not all variables are numeric, Please check it using str function.")
  }
  if ("group" %in% colnames(data) & all(res == "numeric")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
