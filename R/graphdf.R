#' @title
#' Graphing frequency of words over time of the presidential debate
#'
#' @name graphdf
#'
#' @description
#' This functions create a graph with points to illustrate the frequency that each word is mentioned per turn with points
#' and also adds a smooth curve to show an area of error.
#'
#' @param data data frame
#'
#' @return ggplot point graph
#'
#' @examples
#' \dontrun{
#'  graphdf(dataframe)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
graphdf <- function(data) {
  graph <- data %>%
    ggplot(mapping = aes(x = turn, y = p, colour = words)) +
    geom_point() +
    facet_wrap(~ words, scales = "free") +
    geom_vline(xintercept = c(541, 1798), linetype = "dashed") +
    geom_smooth() +
    geom_smooth(method = "lm", se = FALSE, colour = "black") +
    scale_y_continuous(label = percent) +
    theme(legend.position = "none") +
    labs(title = "Change of Word Frequency in the 2012 Presidential Debate Over Time", x = "Debate turn number", y = "Percentage of words in 2012 presidental debate")
  return (graph)
}
