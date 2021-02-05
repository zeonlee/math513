#' @title
#' Zipf's Law, tokenize with stopwords
#'
#' @name tokenwstop
#'
#' @description
#' Tokenize our dialogue and removing the stop words
#' and calculate the tf value
#'
#' @param data (df)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  tokenwstop(df)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
tokenwstop <- function(data){
  token <- data %>%
    unnest_tokens(words, dialogue) %>%
    count(person, words) %>%
    bind_tf_idf(words, person, n)
  return (token)
}
