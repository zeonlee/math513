#' @title
#' Zipf's Law, Final Graph
#'
#' @name zipffinal
#'
#' @description
#' To produce Zipf's Law graph with tokenwstop(),
#' sortnorder() and zipfgraph()
#'
#' @param data (df)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  zipffinal(df)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
zipffinal <- function(data)
{
  tokenized <- tokenwstop(data)
  sorted <- sortnorder(tokenized)
  graph <- zipfgraph(sorted)
  return(graph)
}
