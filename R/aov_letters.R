#' ANOVA analysis and labele significance with letters
#'
#' @param data a data frame
#'
#' @return a data frame
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' aov_letters(data)
#' data1 <- data[1:100, , drop = TRUE]  |> droplevels()
#' aov_letters(data1)
aov_letters <- function(data){
  ck <- check_class(data)
  if (ck) {
    variable <- subset(colnames(data), colnames(data) != "group")
    test <- matrix(NA,
                   nrow = nlevels(data$group),
                   ncol = (ncol(data) -1))
    colnames(test) <- variable
    rownames(test) <- levels(data$group)
    for (i in variable) {
      fit1 <- aov(as.formula(sprintf("%s ~ group",i)), data = data)
      tuk1 <- multcomp::glht(fit1, linfct = multcomp::mcp(group = "Tukey"))
      res1 <- multcomp::cld(tuk1, alpah = 0.05)
      test[, i] <- res1$mcletters$Letters
    }
  }
  return(test)
}

#' Calculate the max values among different groups
#'
#' @param data a data frame
#'
#' @return

gmax <- function(data){
  ck <- check_class(data)
  if (ck) {
    return(aggregate(. ~ group, data, max, na.rm = TRUE))
  }
}
