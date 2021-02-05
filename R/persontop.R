#' @title
#' Group the result by person and find their top n most used words
#'
#' @name persontop
#'
#' @description
#' Group the result by person and find their top n most used words
#'
#' @param data (n)
#'
#' @return persontop(n = n)
#'
#' @examples
#' \dontrun{
#'  persontop(n)
#'}
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL


persontop <- function(data, n, vector = "")
{
  result <- data %>%
    tokenwperson(vector) %>%
    group_by(person) %>%
    slice_max(order_by = tf_idf, n = n, with_ties = TRUE)
  return(result)
}
