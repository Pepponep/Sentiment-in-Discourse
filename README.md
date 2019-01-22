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
article_links <- tibble()
 # zamieniłem przestarzałą data_frame() na tibble()
for(i in 1:n_index_pages) {
  article_links_tmp <- get_article_list_from_page(i)
 
  article_links <- bind_rows(article_links, article_links_tmp)
 
  # czekamy, żeby być grzecznym dla serwerów
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
}
 
rm(article_links_tmp, i)
get_article <- function(article_url) {
  page <- read_html(article_url, encoding = "ISO_8859-2")
  
  # autor tekstu
  author <- page %>% html_node("div#gazeta_article_author") %>% html_text() %>% trimws()
  
  # data publikacji
  date <- page %>% html_node("div#gazeta_article_date") %>% html_text() %>% trimws() %>%
     str_replace_all("[\t\n ]", "") %>% dmy_hm()
  
  # tytuł tekstu
  title <- page %>%html_node("h1") %>% html_text() %>% trimws()
  
  # lead
  lead <- page %>% html_node("div#gazeta_article_lead") %>% html_text() %>% trimws()
 
  # pełna treść artykułu
  body <- page %>% html_node("div#gazeta_article_body") %>% html_text() %>% trimws()
  
  # wszystkie dane pakujemy razem
  article <- data_frame(title = title,
                        lead = lead,
                        body = body,
                        author = author,
                        date = date,
                        url = article_url)
  
  # czekamy, żeby być grzecznym dla serwera
  Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
 
  return(article)
}
articles <- article_links %>%
   # działaj wierszami
   rowwise() %>%
   # dla każdego wiersza wywołaj funkcję get_article() z parametrem wziętym z kolumny "link"
   do(get_article(.$link)) %>%
   # złącz wszystkie otrzymane rezultaty
   bind_rows() %>% 
   ungroup()
   saveRDS(articles, file = "articles.RDS")
