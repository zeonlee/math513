#' @title
#' tokenize the dialogue, by person and words
#'
#' @name tokenwperson
#'
#' @description
#' This function will token the dialogue and remove stopwords by person
#'
#' @param data ()
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  tokenwperson()
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
tokenwperson <- function(data, vector = ""){
  token <- data %>%
    unnest_tokens(words, dialogue)
  # remove the stop words
  clean <- token %>%
    filter(!(words %in% c(stop_words$word, vector))) %>%
    count(person, words) %>%
    bind_tf_idf(words, person, n)
  return (clean)
}
