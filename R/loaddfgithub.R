#' @title
#' Sample Data Loading from github
#'
#' @name loaddfgithub
#'
#' @description
#' Sample Data from provide for assessment will be loaded from github
#' In this case MATH513 BD&SNV_Report_presidential_debates_2012.csv will be loaded as df
#'
#'
#' @param data ()
#'
#' @return Data Frame, df will be loaded, in this case file is loaded from https://github.com/zeonlee/math513/raw/main/df.RData
#'
#' @examples
#' \dontrun{
#' loaddfgithub()
#' }
#'
#' @import ggplot2
#' @import gridExtra
#' @import scales
#' @import tidytext
#' @import tidyverse
#' @import dplyr
#' @export
NULL



loaddfgithub<- function(){
  githubURL <- "https://github.com/zeonlee/math513/raw/main/pedf.RData"
  load(url(githubURL), .GlobalEnv)
}
