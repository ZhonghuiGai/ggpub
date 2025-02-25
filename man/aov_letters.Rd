% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aov_letters.R
\name{aov_letters}
\alias{aov_letters}
\title{ANOVA analysis and labele significance with letters}
\usage{
aov_letters(data)
}
\arguments{
\item{data}{a data frame}
}
\value{
a data frame
}
\description{
ANOVA analysis and labele significance with letters
}
\examples{
data <- iris
colnames(data)[5] <- "group"
aov_letters(data)
data1 <- data[1:100, , drop = TRUE]  |> droplevels()
aov_letters(data1)
}
\author{
Zhonghui Gai
}
