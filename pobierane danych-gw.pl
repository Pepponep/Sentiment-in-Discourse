library(tidyverse)
library(rvest)
library(glue)
library(stringr)
library(lubridate)
library(knitr)
library(kableExtra)
	
# jaką stronę wczytujemy?


# potrzebujemy miejsca, gdzie zgromadzimy linki
linki <- vector()
 
# dla pierwszych pięciu stron powtarzamy to samo:
for(str in 1:5) {
 
   # budujemy urla kolejnej podstrony
   # ?? czy nie wyrzucić _19834953
   page_url <- glue("http://wiadomosci.gazeta.pl/wiadomosci/0,114871.html?str={str}_19834953")
 
   # pobieramy tą podstronę
   page <- read_html(page_url)
 
   # szukamy wszystkich linków do artykułów
   
   linki_tmp <- page %>%
      # layout strony:
      html_node("div#columns_wrap") %>%
      html_node("div#col_left") %>%
      html_node("div#row_1") %>%
      html_node("div#holder_201") %>%
      # właściwy indeks artykułów:
      html_node("article") %>%
      html_node("section.body") %>%
      # lista artykułów:
      html_node("ul") %>%
      # pojedynczy artykuł
      html_nodes("li") %>%
      # linki do artykułów
      html_node("a") %>%
      # wartość parametru href w znaczniku a
      html_attr("href")
      
      # dodajemy zgromadzone linki z jednej strony do wszystkich zebranych wcześniej linków
   linki <- c(linki, linki_tmp)
}
linki <- linki[grepl("http://wiadomosci.gazeta.pl/wiadomosci/7,", linki)]

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
      
      # body - pełna treść artykułu
  body <- page %>% html_node("div#gazeta_article_body") %>% html_text() %>% trimws()
  
  # wszystkie dane pakujemy razem
 
   # pakujemy to w 1-wierszowa tabele
   articles <- tibble(url = art_url, title = tytul, author = autor, date = data, lead = lead, body = body)
   
   # czekamy, żeby być grzecznym dla serwera
 Sys.sleep(sample(seq(0.25, 1, 0.25), 1))
 
   return(articles)
}
# articles <- linki %>%
#   map_df(get_article_details)
   
#   articles %>% count(author) %>% arrange(n) %>% mutate(author = fct_inorder(author)) %>% ggplot(aes(author, n)) + geom_col()
# KOMUNIKAT Error in grouped_df_impl(data, unname(vars), drop) : 
 # Column `author` is unknown

articles <- article_links %>%
   # działaj wierszami
   rowwise() %>%
   # dla każdego wiersza wywołaj funkcję get_article() z parametrem wziętym z kolumny "link"
   do(get_article(.$link)) %>%
   # złącz wszystkie otrzymane rezultaty
   bind_rows() %>% 
   ungroup()
   
saveRDS(articles, file = "articles.RDS")
