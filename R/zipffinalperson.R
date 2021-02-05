#' @title
#' Zipf's Law, Final Graph by Speaker
#'
#' @name zipffinalperson
#'
#' @description
#' To produce Zipf's Law graph with tokenwstop(),
#' sortnorder() and zipfgraphperson()
#'
#' @param data (df)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  zipffinalperson(df)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
zipffinalperson <- function(data)
{
  tokenized <- tokenwstop(data)
  sorted <- sortnorder(tokenized)
  graph <- zipfgraphperson(sorted)
  return(graph)
}
