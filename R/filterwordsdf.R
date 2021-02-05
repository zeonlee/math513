#' @title
#' Filtering dataframe to display words that are entered in a vector form
#'
#' @name filterwordsdf
#'
#' @description
#' This function will filter the data frame's words according to those that are in vec
#'
#' @param data data frame
#' @param vec vector of strings
#'
#' @return data frame
#'
#' @examples
#'  \dontrun{
#'  filterwordsdf(dataframe, vector)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL


filterwordsdf <- function(data, vec) {
  return(filter(data, words %in% vec))
}
