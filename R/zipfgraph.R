#' @title
#' Zipf's Law, to produce a Zipf Graph
#'
#' @name zipfgraph
#'
#' @description
#' To produce Zipf's Law graph
#'
#' @param data (df)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  zipfgraph(df)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
zipfgraph <- function(data)
{
  graph <- ggplot(data, mapping = aes (x = rank, y = tf)) +
    geom_line(mapping = aes(colour = person), size = 1) +
    geom_smooth(method = 'lm', colour = 'black', size = 0.5, se = FALSE) +
    scale_y_log10() + scale_x_log10() +
    labs(title = "Zipf's Law for the 2012 US Presidential Debate dataset", y = "Term frequency(tf)", x = "Word rank")
  return(graph)
}
