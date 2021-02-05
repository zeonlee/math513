#' @title
#' Finding the most words in data
#'
#' @name mostwords
#'
#' @description
#' Find top m words and
#' Sort these words according to number of times appeared
#' and only return top m
#'
#' @param data mostwords(data, n)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  mostwords(data, n)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL

mostwords <- function(data, m = 10, vector = ""){
  token <- data %>%
    unnest_tokens(words, dialogue)
  clean <- token %>%
    filter(!(words %in% c(stop_words$word, vector))) %>%
    #     anti_join(stop_words, by = c("words" = "word")) %>%
    count(words, sort = TRUE) %>%
    mutate(proportion = n/sum(n)) %>%
    head(m)
}
