# ###################################################################
# Libraries
# ###################################################################
library(tibble)
library(rjson)
library(xml2)
library(tidytext)
library(tokenizers)
library(tidyr)
library(dplyr)
library(widyr)
library(chron) 
library(openxlsx) 
library(reticulate)
library(lubridate)
library(stringr)

# Shoudl topics be printed at the end
print_topics <- FALSE

# Minimal lambda value
v_min_lambda_daily <- 10

# Loading functions
# Setting main directory
working_dir <- "D:/Osobiste/GitHub/"

# Sourcing R code
source(paste0(working_dir, "News_Selector/scripts/dunning_functions.R"), encoding = "UTF8")
source(paste0(working_dir, "News_Selector/scripts/text_cleaning_functions.R"), encoding = "UTF8")
source(paste0(working_dir, "News_Selector/scripts/topic_selection_functions.R"), encoding = "UTF8")

# Sourcing Python code
source_python(paste0(working_dir, "News_Selector/scripts/python_functions.py"))

# Stop Words
source(paste0(working_dir, "News_Selector/scripts/PL_stop_words.R"), encoding = "UTF8")
# ###################################################################
# Load files
# ###################################################################
DF_1 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_RMF_daily.json")) %>% clean_RMF(.)
DF_2 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Gazeta_daily.json")) %>% clean_Gazeta(.)
DF_3 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Interia_daily.json")) %>% clean_Interia(.)
# DF_4 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Dziennik_daily.json")) %>% clean_Dziennik(.)
DF_5 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_RadioZET_daily.json")) %>% clean_RadioZET(.)
DF_6 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_PAP_daily.json")) %>% clean_PAP(.)
DF_7 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_TVN24_daily.json")) %>% clean_TVN24(.)
DF_8 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_TVN24bis_daily.json")) %>% clean_TVN24bis(.)
DF_9 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_TVP_INFO_daily.json")) %>% clean_TVP_INFO(.)
DF_10 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Polskie_Radio_daily.json")) %>% clean_Polskie_Radio(.)
DF_11 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Polsat_News_daily.json")) %>% clean_Polsat_News(.)
DF_12 <- fromJSON(file = paste0(working_dir, "News_Selector/data/daily_articles/page_Wprost_daily.json")) %>% clean_Wprost(.)

# Meeging all sites
DF <- DF_1 %>%
    dplyr::select(id, date, time, site, url, text) %>%
    union_all(DF_2 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_3 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    # union_all(DF_4 %>%
    #               dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_5 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_6 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_7 %>%
                  dplyr::select(id, date, time, site, url, text)) %>%
    union_all(DF_8 %>%
                  dplyr::select(id, date, time, site, url, text))%>%
    union_all(DF_9 %>%
                  dplyr::select(id, date, time, site, url, text))%>%
    union_all(DF_10 %>%
                  dplyr::select(id, date, time, site, url, text))%>%
    union_all(DF_11 %>%
                  dplyr::select(id, date, time, site, url, text))%>%
    union_all(DF_12 %>%
                  dplyr::select(id, date, time, site, url, text))
rm(DF_1, DF_2, DF_3, DF_5, DF_6, DF_7, DF_8, DF_9, DF_10, DF_11, DF_12)

# Setting as date to analyse yesterday
v_date <- Sys.time() %>% ymd_hms() %>% as.Date() - 1
v_date <- v_date %>% as.character()

DF <- DF %>%
    filter(date == v_date) %>%
    mutate(date = date %>% as.character())

# Saving articles 
save(DF, file = paste0(working_dir, "News_Selector/data/daily_articles/archiv/articles_", v_date, ".RData"))


# ###################################################################
# Load grammar dictionary
# ###################################################################
load(paste0(working_dir, "News_Selector/data/grammar_data.RData"))

gc(reset = T)
# ###################################################################
# Modification of dataset
# ###################################################################
# TO DO:
# Correct sentence splitting (RMF24 - it's probably hard space)

# Selecting sentences that are to be included in analysis
articles_sentences <- DF %>%
    # Split into paragraphs
    unnest_tokens(text, text, token = "regex", pattern = " \\|\\| ", to_lower = F) %>%
    # Set paragraphs ids
    group_by(id) %>%
    mutate(paragraph_id = paste0(id, "_", seq(1, n()))) %>%
    ungroup() %>%
    # Split into sentences
    unnest_tokens(text, text, token = "sentences", to_lower = F) %>%
    # Delete too short sentences
    filter(nchar(text) > 80) %>%
    # Set sentences ids
    group_by(id) %>%
    mutate(sentence_id = paste0(id, "_", seq(1, n()))) %>%
    ungroup() %>%
    # Delete sentences with too many capital letters
    mutate(characters = nchar(text),
           capital_letters = stringr::str_count(text,"[A-Z]")) %>%
    filter((capital_letters / characters) < 0.35)
    
# Unnest sentences
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

# Select minimum number of tokens occurence basing on distribution of data
data_grouped <-  articles_unnested %>%
    group_by(word) %>%
    summarise(counts = n())
v_min_counts <- quantile(data_grouped$counts, probs = 0.95) # or 0.91

gc(reset = T)

# ###################################################################
# General statistics needed for Dunning statistic
# ###################################################################
# Data Set of dates used in general stats
used_dates <- read.csv2(paste0(working_dir, "News_Selector/data/Used_dates_in_stats.csv")) %>%
    dplyr::select(date)
# Statistics
load(paste0(working_dir, "News_Selector/data/General_stats_updated.RData"))

# Update statistics if date is new
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
    
    # Update dates included in general statistics
    used_dates <- bind_rows(used_dates, tibble(date = v_date))

    save(general_stats, file = paste0(working_dir, "News_Selector/data/General_stats_updated.RData"))
    write.csv2(used_dates, file = paste0(working_dir, "News_Selector/data/Used_dates_in_stats.csv"), row.names = F)
}

#####################################################################
# Cluster and summarise with embeddings in Python
# ###################################################################
# Prepare inputs for Python
inputs <- prepare_python_inputs(articles_unnested,
                                articles_sentences,
                                general_stats,
                                id, paragraph_id, sentence_id, word, 
                                v_min_counts, 
                                v_min_lambda_daily)

sections_and_articles <- inputs[[1]]
filtered_lambda_statistics <- inputs[[2]]
log_lambda_statistics_df <- inputs[[3]]
lemmatized_sentences <- inputs[[4]]
lemmatized_articles <- inputs[[5]]
sentences_text <- inputs[[6]]


# Sending data to Python for clustering and summarisation with the
# use of dimensions reduction (LSA)
# The algorithm is to select approximately 5% of the sentences that include 
# topic tokens (words), but no less than 3 and no more than 10. 
topics <- cluster_and_summarise(sections_and_articles, filtered_lambda_statistics,
                               # Clustering
                               min_association=0.25, do_silhouette=TRUE, 
                               singularity_penalty=-0.1,
                               # Summarization
                               lemmatized_sentences=lemmatized_sentences, 
                               lemmatized_articles=lemmatized_articles,
                               sentences_text=sentences_text,
                               log_lambda_statistics_df=log_lambda_statistics_df,
                               min_key_freq=0.25, max_sentence_simil=0.5,
                               section_id="section_id", word_col="word",
                               use_sparse=TRUE)

list_topics <- topics[[1]]
words_similarity_matrix <- topics[[2]]
selected_tokens <- topics[[3]]
silhouette_history <- topics[[4]]
max_simil_history <- topics[[5]]
plot(silhouette_history)
colnames(words_similarity_matrix) <- selected_tokens
rownames(words_similarity_matrix) <- selected_tokens

# Adding names to topics
names(list_topics) <- paste0("Gr_", seq(length(list_topics)))

# Add info about Dunning statistics, sort topics and clean sentences
list_topics <- lambd_extractor(list_topics, articles_unnested, 
                               general_stats, min_counts = v_min_counts, 
                               min_lambda_daily = v_min_lambda_daily)
list_topics <- arrange_topics(list_topics, "max_lambda")
list_topics <- clear_sentences(list_topics)

#####################################################################
# Topics listing
# ###################################################################
# This enables to print out and check all topics
if(print_topics){
    for(name in names(list_topics)){
        print(list_topics[[name]][["max_lambda"]])
        print(list_topics[[name]][["words_DF"]])
        print("")
        print(paste0(list_topics[[name]][["site_name"]], ": ", list_topics[[name]][["sentences"]]))
        print("----------------------------------")
    }
}

#####################################################################
# Saving output for report
# ###################################################################
# Data frame with Dunning statistics for plotting
lambda_daily_DF <- calculate_lambda_statistics(articles_unnested, general_stats, 
                                               min_counts = v_min_counts, 
                                               min_lambda_daily = v_min_lambda_daily) %>% 
    mutate(lambda_log = log(lambda + 1) ) %>%
    rename(name = word) %>%
    dplyr::select(name, lambda, lambda_log)

# Saving topics list, similarity matrix and df with Dunning statistics
save(list_topics, 
     words_similarity_matrix, 
     lambda_daily_DF, 
     file = paste0(working_dir, "News_Selector/data/topics/daily_topics_list.RData"))


