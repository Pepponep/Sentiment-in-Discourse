# Sentiment-in-Discourse
Analysis of sentiments in public political discourse
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
# pomiędzy pt1 a pt2 wpada numer strony indeksu
base_url_pt1 <- "http://wiadomosci.gazeta.pl/wiadomosci/0,114871.html?str="
base_url_pt2 <- "_19834953"
 
# ile stron indeksu pobieramy?
n_index_pages <- 50
get_article_list_from_page <- function(page_no) {
  page_url <- paste0(base_url_pt1, page_no, base_url_pt2)
  
  page <- read_html(page_url)
  
  links <- page %>%
     html_node("article") %>%
     html_nodes("li") %>%
     html_nodes("h2")
  
  articles_tmp <- data_frame(link = links %>% html_node("a") %>% html_attr("href"),
                             title =  links %>% html_node("a") %>% html_text())
  
  articles_tmp <- articles_tmp %>%
     filter(str_sub(link, 1, 41) == "http://wiadomosci.gazeta.pl/wiadomosci/7,")
  
  return(articles_tmp)
}

