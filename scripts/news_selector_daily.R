# ###################################################################
# Biblioteki
# ###################################################################
library(tibble)
library(rjson)
library(xml2) #
library(tidytext)
library(tokenizers)
library(tidyr)
library(dplyr)
library(widyr) #
# library(ggplot2)
library(chron) #
library(scales)
library(openxlsx) #
library(stargazer) #

library(lubridate)

library(wordcloud) #
library(RColorBrewer)

library(glmnet) #

library(stringr)

library(ggpage) #

library(lexRankr) #
library(arrangements) #

# Ladowanie funkcji
working_dir <- "D:/Osobiste/GitHub/"

source(paste0(working_dir, "News_Selector/scripts/dunning_functions.R"), encoding = "UTF8")
source(paste0(working_dir, "News_Selector/scripts/my_lex_rank_functions.R"), encoding = "UTF8")
source(paste0(working_dir, "News_Selector/scripts/text_cleaning_functions.R"), encoding = "UTF8")
source(paste0(working_dir, "News_Selector/scripts/topic_selection_functions.R"), encoding = "UTF8")
# Stop Words
source(paste0(working_dir, "News_Selector/scripts/PL_stop_words.R"), encoding = "UTF8")

# Minimalna liczba wystapien w artykulach
v_min_lambda_daily <- 10
# ###################################################################
# Load files
# ###################################################################
DF_1 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_RMF_daily.json")) %>% clean_RMF(.)
DF_2 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Gazeta_daily.json")) %>% clean_Gazeta(.)
DF_3 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Interia_daily.json")) %>% clean_Interia(.)
DF_4 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Dziennik_daily.json")) %>% clean_Dziennik(.)
DF_5 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_RadioZET_daily.json")) %>% clean_RadioZET(.)
DF_6 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_PAP_daily.json")) %>% clean_PAP(.)
DF_7 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_TVN24_daily.json")) %>% clean_TVN24(.)
DF_8 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_TVN24bis_daily.json")) %>% clean_TVN24bis(.)

# Laczenie wszystkich stron
DF <- DF_1 %>%
    dplyr::select(id, date, time, site, url, text) %>%
    union_all(DF_2 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_3 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_4 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_5 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_6 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_7 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_8 %>%
                  dplyr::select(id, date, time, site, url, text))
rm(DF_1, DF_2, DF_3, DF_4, DF_5, DF_6, DF_7, DF_8)

# Data do analizy
v_date <- Sys.time() %>% ymd_hms() %>% as.Date() - 1
v_date <- v_date %>% as.character()

DF <- DF %>%
    filter(date == v_date) %>%
    mutate(date = date %>% as.character())

# Zapisywanie artykulow i ustawienie sciezki
save(DF, file = paste0(working_dir, "News_Selector/data/daily_articles/archiv/articles_", v_date, ".RData"))


# ###################################################################
# Load Dictionaries
# ###################################################################
# Slownik gramatyczny
load(paste0(working_dir, "News_Selector/data/grammar_data.RData"))

gc(reset = T)
# ###################################################################
# Obrobka tekstu
# ###################################################################
# TO DO:
# Poprawic rozbicie RMF24 - to chyba wynika z twardej spacji

## Glowna czesc rozbicia plikow
articles_sentences <- DF %>%
    # Podzial na paragrafy
    unnest_tokens(text, text, token = "regex", pattern = " \\|\\| ", to_lower = F) %>%
    # Wyznaczanie id paragrafow
    group_by(id) %>%
    mutate(paragraph_id = paste0(id, "_", seq(1, n()))) %>%
    ungroup() %>%
    # Podzial na zdania
    unnest_tokens(text, text, token = "sentences", to_lower = F) %>%
    # Usuniecie bardzo krotkich zdan
    filter(nchar(text) > 80) %>%
    # Wyznaczenie id zdan
    group_by(id) %>%
    mutate(sentence_id = paste0(id, "_", seq(1, n()))) %>%
    ungroup() %>%
    # Usuniecie zdan gdzie ponad 35 porcent stanowia wielkie litery
    mutate(characters = nchar(text),
           capital_letters = stringr::str_count(text,"[A-Z]")) %>%
    filter((capital_letters / characters) < 0.35)
    

articles_unnested <- articles_sentences %>%
    unnest_tokens(word, text, to_lower = F) %>%
    group_by(sentence_id) %>%
    mutate(position = seq(1, n())) %>%
    ungroup() %>%
    strip_numeric(., word) %>%
    filter(!word %in% stop_words_pl) %>%
    left_join(grammar_data, by = c("word" = "tekst")) %>%
    mutate(word = ifelse(is.na(slowo), word, slowo)) %>%
    dplyr::select(-slowo) %>%
    filter(!word %in% stop_words_pl) 

# Check distribution of data
data_grouped <-  articles_unnested %>%
    group_by(word) %>%
    summarise(counts = n())

v_min_counts <- quantile(data_grouped$counts, probs = 0.90) # lub 0.91

# Filter out rare words
filtered_sentences_unnested <- articles_unnested %>%
    group_by(word) %>%
    filter(n() > 3) %>%
    ungroup()

gc(reset = T)
# ###################################################################
# Statystyki ogolne
# ###################################################################
# Data Set of dates used in general stats
used_dates <- read.csv2(paste0(working_dir, "News_Selector/data/Used_dates_in_stats.csv")) %>%
    dplyr::select(date)
# Statistics
load(paste0(working_dir, "News_Selector/data/General_stats_updated.RData"))

if (! v_date %in% used_dates$date){

    general_stats <- general_stats %>%
        dplyr::select(word, counts_general) %>%
        union_all(articles_unnested %>%
                      group_by(word) %>%
                      summarise(counts_general = n()) %>%
                      ungroup()) %>%
        group_by(word) %>%
        summarise(counts_general = sum(counts_general)) %>%
        ungroup() %>%
        mutate(perc_general = counts_general / sum(counts_general)) %>%
        dplyr::select(word, perc_general, counts_general)
    
    # Aktualizacja dat
    used_dates <- bind_rows(used_dates, tibble(date = v_date))

    save(general_stats, file = paste0(working_dir, "News_Selector/data/General_stats_updated.RData"))
    write.csv2(used_dates, file = paste0(working_dir, "News_Selector/data/Used_dates_in_stats.csv"), row.names = F)
}


#####################################################################
# Grupowanie slow do tematow
# ###################################################################
# Wyznaczenie podobienstwa na poziomie paragrafow
res <- TopicClustering(articles_unnested, general_stats, paragraph_id, word, id, min_counts = v_min_counts, 
                       min_lambda_daily = v_min_lambda_daily, min_association = 0.5, association_type = "Cosinus",
                       min_words_topic = 2) # 4
dlPairs <- res[[1]]
mat <- res[[2]]

dlPairs <- DescTools::as.matrix.xtabs(dlPairs)

## TO DO:
## Add this to the TopicClustering

# Grupowanie slow do tematow
cos_res <- CosineClustering(dlPairs, mat, 0.25, silhouette_flag = F) # 0.25 - paragrafy lub 0.01 dla silhouette_flag = T
list_topics <- cos_res[[1]]
# mat_topics <- cos_res[[2]]
# max_val_history <- cos_res[[3]]
# silhouette_history <- cos_res[[4]]
# 
# plot(seq(1, length(max_val_history)), max_val_history)
# plot(seq(1, length(silhouette_history)), silhouette_history)
# max_silhouette <- which(silhouette_history == max(silhouette_history))
# max_val_history[max_silhouette]

# Tutaj slowa sa juz przypisane do tematow
# Dodawanie do listy informacji o statystyce Dunninga (Lambda) oraz sortowanie listy wg najwyzszej wartosci statystyki w danym temacie
list_topics <- Lambd_Extractor(list_topics, articles_unnested, general_stats, min_counts = v_min_counts, 
                               min_lambda_daily = v_min_lambda_daily)
list_topics <- Arrange_Topics(list_topics, "max_lambda")

#####################################################################
# Wyznaczanie zdan ktore podsumowuja poszczegolne tematy
# ###################################################################
# Wybrane zdania stanowia ok 5% wszystkich ktore opisuja dany temat - wybieranych jest nie mniej niz 3 zdania oraz nie wiecej niz 10 zdan

# Ta funcja znajduje i przypisuje zdania do poszczegolnych tematow
# Zdania sa posortowane wg wartosci zwroconej przez LexRank
list_topics <- Topics_Summariser(list_topics, filtered_sentences_unnested, articles_sentences, paragraph_id, word, 
                                 sentence_id, general_stats, filtered_similarity = F, min_key_freq = 0.8, 
                                 value_log_log, min_association = 0.5, sites)


list_topics <- Clear_Sentences(list_topics)

# # Ten fragment kodu sluzy do sprawdzenia jakie slowa i zdania zostaly przypisane do poszczegolnych tematow
# for(name in names(list_topics)){
#     # print(list_topics[[name]][["word"]])
#     print(list_topics[[name]][["max_lambda"]])
#     print(list_topics[[name]][["words_DF"]])
#     print("")
#     print(paste0(list_topics[[name]][["site_name"]], ": ", list_topics[[name]][["sentences"]]))
#     print("----------------------------------")
# }

# Data Frame ze statystyka Dunninga dla wybranych slow
lambda_daily_DF <- calculate_lambda_statistics(articles_unnested, general_stats, min_counts = v_min_counts, min_lambda_daily = v_min_lambda_daily) %>% 
    mutate(lambda_log = log(lambda + 1) ) %>%
    rename(name = word) %>%
    dplyr::select(name, lambda, lambda_log)

# Zapisywanie listy tematow z wybranymi slowami oraz zdaniami podsumowujacymi dany temat, macierzy podobienstwa miedzy slowami oraz tablicy ze statystyka Dunninga dla wybranych slow
save(list_topics, dlPairs, lambda_daily_DF, file = paste0(working_dir, "News_Selector/data/topics/daily_topics_list.RData"))


