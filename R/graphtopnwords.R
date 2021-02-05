#' @title
#' graphtopnwords, a function which combines mostwords & finalfunc
#' which return result in graph
#'
#' @name graphtopnwords
#'
#' @description
#' Creating graph of the top "n" words
#'
#' @param data graphtopnwords(data, n)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  graphtopnwords(df, 4)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
graphtopnwords <- function(data, n, vector = ""){
  topnwordsinfo <- mostwords(data, n, vector)
  topwords <- topnwordsinfo$words # this is stored as an array
  graph <- finalfunc(data, topwords)
  return (graph)
}
