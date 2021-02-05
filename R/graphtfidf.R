#' @title
#' Function to produce the tokenize dialogue by person
#'
#' @name graphtfidf
#'
#' @description
#' This function will graph out the tokenize dialogue by person
#' Option to include additional stopword with a customized vector
#'
#' @param data (df)
#'
#' @return data frame
#'
#' @examples
#' \dontrun{
#'  graphtfidf(pedf, vector = add_stopw)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL
graphtfidf <- function(data, n = 10, vector = ""){
  graph <- data %>%
    persontop(n = n, vector) %>%
    ggplot(mapping = aes(x = reorder(words,tf_idf), y = tf_idf, fill = person)) +
    geom_col() +
    facet_wrap(~person, scales = "free", ncol = 2) +
    coord_flip() +
    theme(legend.position = "none", axis.title.y = element_blank()) +
    labs(title = "Highest tf-idf Words in the 2012 US Presidential Debate", x = "tf-idf index")
  return(graph)
}
