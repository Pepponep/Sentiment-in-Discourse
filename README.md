# Sentiment-in-Discourse
# Analysis of sentiments in public political discourse, part 1. (pobieranie artykułów z gazeta.pl
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

# pomiędzy pt1 a pt2 wpada numer strony indeksu
base_url_pt1 <- "http://wiadomosci.gazeta.pl/wiadomosci/0,114871.html?str="
base_url_pt2 <- "_19834953"
 
# ile stron indeksu pobieramy?
n_index_pages <- 50

#próbuje poprawić XXXXXXXX
safe_read_html <- safely(read_html)
get_article_details <- function(art_url) {
   # próbujemy pobrać stronę
   page <- safe_read_html(art_url, encoding = "iso-8859-2")
 
   # jeśli się nie udało - wychodzimy z pustą tabelką
   if(is.null(page$result)) {
      return(tibble())
   }
 
   # udało się - wynik mamy w $result
   page <- page$result
 # tytuł - to H1 na stronie
   tytul <- page %>%
      html_node(xpath = "//h1") %>%
      html_text() %>%
      trimws()
 
   # autor
   autor <- page %>%
      html_node(xpath = "//div[@id='gazeta_article_author']/span") %>%
      html_text() %>%
     trimws()
 
   # data publikacji
   data <- page %>%
      html_node(xpath = "//div[@id='gazeta_article_date']/time") %>%
      html_attr("datetime") %>%
      ymd_hm()
 
   # lead
   lead <- page %>%
      html_node(xpath = "//div[@id='gazeta_article_lead']") %>%
      html_text() %>%
      trimws()
 
   # pakujemy to w 1-wierszowa tabele
   article <- tibble(url = art_url, title = tytul, author = autor, date = data, lead = lead)
 
   return(article)
}

# XXXXXXXXXXXXX

get_article_list_from_page <- function(page_no) 
{
  page_url <- paste0(base_url_pt1, page_no, base_url_pt2)
  
  page <- read_html(page_url)
  
  links <- page %>%
     html_node("article") %>%
     html_nodes("li") %>%
     html_nodes("h2")
  
  articles_tmp <- tibble(link = links %>% html_node("a") %>% html_attr("href"),
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
  article <- tibble(title = title,
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
