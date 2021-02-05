#' @title
#' Grouping tokenised dialogue by turn
#'
#' @name cleandf
#'
#' @description
#' This function will take in a data frame and return a new data frame that has been:
#' 1. Tokenised without stop words
#' 2. Group the results by turn
#' 3. Return a data frame with count, n, of each word in each turn and also the proportion of the word
#'
#' @param data data frame
#'
#' @return data frame
#'
#' @examples
#'  \dontrun{
#'  cleandf(pedf)
#'}
#'
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
cleandf <- function(data) {
  token <- data %>%
    unnest_tokens(words, dialogue)
  clean <- token %>%
    anti_join(stop_words, by = c("words" = "word"))
  final <- clean %>%
    count(turn, words) %>%
    group_by(turn) %>%
    mutate(p = n/sum(n))
  return (final)
}
