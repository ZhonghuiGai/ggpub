#' A function for non-parametric multi-group test using dunn.test package
#'
#' @param letter show p value using letters, default value is TRUE
#' @param adjust adjust p value using "bh", default value is FALSE
#' @param data a data frame with grouping information
#'
#' @return a data frame
#'
#' @author Zhonghui Gai
#' @examples
#' data <- iris
#' colnames(data)[5] <- "group"
#' dunn_letters(data = data, adjust = TRUE)
#' dunn_letters(data = data, letter = FALSE)
dunn_letters <- function(data, letter = TRUE, adjust = FALSE){
  name <- colnames(data)
  stopifnot("group" %in% name)
  results <- NULL
  data <- as.data.frame(data)
  name <- name[!name %in% "group"]
  for(i in name){
    dunn <- with(data,
                 dunn.test::dunn.test(data[, i], group,
                                      method = "bh",
                                      kw = TRUE, table = FALSE,
                                      list = TRUE, altp = TRUE))
    dunn <- data.frame(dunn[-which(names(dunn)=="chi2")])[, c(4,2,3)]
    rownames(dunn) <- dunn$comparisons
    dunn <- dunn[, -1]
    num <- length(rownames(dunn))
    dunn <- t(dunn)
    result <- matrix(NA, nrow = 1, ncol = 2*num, byrow = TRUE)
    colnames(result) <- c(paste(rownames(dunn)[1], colnames(dunn), sep = "."),
                          paste(rownames(dunn)[2], colnames(dunn), sep = "."))
    rownames(result) <- i
    result[1, 1:num] <- dunn[1,]
    result[1, (num+1):(2*num)] <- dunn[2, ]
    results <- rbind(results, result)
  }
  if (letter) {
    dunn <- results
    ncol <- ncol(dunn)
    if(adjust){
      dunn <- dunn[, ((ncol/2)+1):ncol, drop = FALSE]
      colnames(dunn) <- gsub("altP.adjusted.", "", colnames(dunn))
      colnames(dunn) <- gsub(" ", "", colnames(dunn))
    }else{
      dunn <- dunn[, 1:(ncol/2), drop = FALSE]
      colnames(dunn) <- gsub("altP.", "", colnames(dunn))
      colnames(dunn) <- gsub(" ", "", colnames(dunn))
    }
    # turn pairwise p.value to LETTER
    stopifnot(all(rownames(dunn) == name))
    results <- c()
    for(i in 1:length(name)){
      temp <- multcompView::multcompLetters(dunn[i, ])
      results <- cbind(results, temp$Letters)
    }
    colnames(results) <- name
  }
  return(results)
}



