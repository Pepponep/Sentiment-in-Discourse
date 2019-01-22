# Sentiment-in-Discourse
Alalysis of sentiments in public political discourse
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)
# pomiędzy pt1 a pt2 wpada numer strony indeksu
base_url_pt1 <- "http://wiadomosci.gazeta.pl/wiadomosci/0,114871.html?str="
base_url_pt2 <- "_19834953"
 
# ile stron indeksu pobieramy?
n_index_pages <- 50
