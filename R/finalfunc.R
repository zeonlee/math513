#' @title
#' This function put together cleandf, filterwordsdf and graphdf.
#'
#' @name finalfunc
#'
#' @description
#' Assist to clean, filter stopwords and graph data and indicated vector.
#'
#' @param data data frame
#'
#' @return ggplot point graph
#'
#' @examples
#' \dontrun{
#'  finalfunc(df,vec)
#'  for our case we will use vector:
#'  "china", "dollar", "energy", "jobs", "military", "people"
#'  to showcase our sample data df.
#'  finalfunc(df, c("china", "dollar", "energy", "jobs", "military", "people"))
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL

finalfunc <- function(data, vec){
  clean <- cleandf(data)
  filter <- filterwordsdf(clean, vec)
  graph <- graphdf(filter)
  return (graph)
}
