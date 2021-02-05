## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup,message=FALSE,warning=FALSE----------------------------------------
library(UOPMDISFEB2021)
library(ggplot2)
library(gridExtra)
library(tidytext)
library(tidyverse)
library(scales)
library(dplyr)

## ----message=FALSE,warning=FALSE----------------------------------------------
loaddfgithub<- function(){
  githubURL <- "https://github.com/zeonlee/math513/raw/main/pedf.RData"
  load(url(githubURL), .GlobalEnv)
} 

loaddfgithub() 

## ----message=FALSE, warning=FALSE---------------------------------------------
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

## ----message=FALSE,warning=FALSE----------------------------------------------
filterwordsdf <- function(data, vec) { 
  return(filter(data, words %in% vec)) 
} 

## ----message=FALSE,warning=FALSE----------------------------------------------
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

## ----message=FALSE,warning=FALSE----------------------------------------------
finalfunc <- function(data, vec){ 
  clean <- cleandf(data) 
  filter <- filterwordsdf(clean, vec) 
  graph <- graphdf(filter) 
  return (graph) 
} 

## ----message=FALSE, fig.width=7,fig.height=4----------------------------------
finalfunc(pedf, c("china", "dollar", "energy", "jobs", "military", "people")) 

## ----message=FALSE,warning=FALSE----------------------------------------------
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

## ----message=FALSE, warning=FALSE---------------------------------------------
persontop <- function(data, n, vector = "") 
{ 
  result <- data %>%  
    tokenwperson(vector) %>%  
    group_by(person) %>%  
    slice_max(order_by = tf_idf, n = n, with_ties = TRUE) 
  return(result) 
} 

## ----message=FALSE,warning=FALSE----------------------------------------------
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

## ----message=FALSE, fig.width=7,fig.height=4----------------------------------

add_stopw <- c("mister", "governor", "president", "romney","romney's", "president's") 

graphtfidf(pedf, vector = add_stopw) 


## ----message=FALSE,warning=FALSE----------------------------------------------

left_join(persontop(pedf, 1) %>% select(c("person", "tf_idf")), pedf %>% group_by(person) %>% count()) 


## ----message=FALSE,warning=FALSE----------------------------------------------
tokenwstop <- function(data){ 
  token <- data %>%  
    unnest_tokens(words, dialogue) %>%  
    count(person, words) %>%  
    bind_tf_idf(words, person, n)  
  return (token) 
}

## ----message=FALSE,message=FALSE----------------------------------------------
sortnorder <- function(data){ 
  sorted<- data %>%  
    group_by(person) %>% 
    slice_max(order_by = tf, n = 500, with_ties = TRUE) %>% 
    mutate(rank = row_number())  
  return(sorted) 
}

## ----message=FALSE,warning=FALSE----------------------------------------------
zipfgraph <- function(data) 
{ 
  graph <- ggplot(data, mapping = aes (x = rank, y = tf)) +  
    geom_line(mapping = aes(colour = person), size = 1) +  
    geom_smooth(method = 'lm', colour = 'black', size = 0.5, se = FALSE) + 
    scale_y_log10() + scale_x_log10() + 
    labs(title = "Zipf's Law for the 2012 US Presidential Debate dataset", y = "Term frequency(tf)", x = "Word rank") 
  return(graph) 
}

## ----message=FALSE,warning=FALSE----------------------------------------------
zipffinal <- function(data) 
{ 
  tokenized <- tokenwstop(data) 
  sorted <- sortnorder(tokenized) 
  graph <- zipfgraph(sorted) 
  return(graph) 
}

## ----message=FALSE, fig.width=7,fig.height=4----------------------------------

zipffinal(pedf) 


## ----message=FALSE,warning=FALSE----------------------------------------------
zipfgraphperson <- function(data)  
{  
  graph <- ggplot(data, mapping = aes (x = rank, y = tf)) +   
    geom_line(mapping = aes(colour = person), size = 1) +   
    geom_smooth(method = 'lm', colour = 'black', size = 0.5, se = FALSE) +  
    facet_wrap(~person)+  
    scale_y_log10() + scale_x_log10() +  
    labs(title = "Zipf's Law for the 2012 US Presidential Debate dataset",  
         y = "Term frequency(tf)", x = "Word rank") +  
    theme(legend.position = "none") 
  return(graph)  
}

## ----message=FALSE, fig.width=7,fig.height=4----------------------------------
zipffinalperson <- function(data) 
{ 
  tokenized <- tokenwstop(data) 
  sorted <- sortnorder(tokenized) 
  graph <- zipfgraphperson(sorted) 
  return(graph) 
} 

## ----message=FALSE, fig.width=7,fig.height=4----------------------------------

zipffinalperson(pedf) 


## ----message=FALSE,warning=FALSE----------------------------------------------
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

## ----message=FALSE, warning=FALSE---------------------------------------------
graphtopnwords <- function(data, n, vector = ""){ 
  topnwordsinfo <- mostwords(data, n, vector) 
  topwords <- topnwordsinfo$words # this is stored as an array 
  graph <- finalfunc(data, topwords) 
  return (graph) 
}

## ----message=FALSE,fig.width=7,fig.height=4-----------------------------------

graphtopnwords(pedf, 10, add_stopw) 


