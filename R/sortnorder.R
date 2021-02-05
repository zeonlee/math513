#' @title
#' Zipf's Law, Sort and arrange data
#'
#' @name sortnorder
#'
#' @description
#' Sort and arrange the tokenized words by person and their tf value
#'
#' @param data (df)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  sortnorder(df)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
sortnorder <- function(data){
  sorted<- data %>%
    group_by(person) %>%
    slice_max(order_by = tf, n = 500, with_ties = TRUE) %>%
    mutate(rank = row_number())
  return(sorted)
}
