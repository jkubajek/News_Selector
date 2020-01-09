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
v_min_lambda_daily <- 100

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
# Vectors for loops
# ###################################################################
start_dates <- as.Date("2019-01-01") + seq(0, 358)
end_dates <- as.Date("2019-01-07") + seq(0, 358)

# ###################################################################
# Load grammar dictionary
# ###################################################################
load(paste0(working_dir, "News_Selector/data/grammar_data.RData"))

gc(reset = T)

# ###################################################################
# General statistics needed for Dunning statistic
# ###################################################################
# Statistics
load(paste0(working_dir, "News_Selector/data/general_stats_2018-2019.RData"))
# ###################################################################
# Annual data
# ###################################################################
load(paste0(working_dir, "News_Selector/data/annual_articles/articles_2019.RData"))
DF_all <- DF
rm(DF)
gc(reset=T)
# ###################################################################
# Load files
# ###################################################################
for(i in seq(48, 52)){
    
    v_date_0 <- as.Date(start_dates[i], format="%Y-%m-%d")
    v_date_1 <- as.Date(end_dates[i], format="%Y-%m-%d")
    selected_dates <- seq(from=v_date_0, to=v_date_1, by="day")
    selected_dates <- selected_dates %>% as.character()
    
    DF <- DF_all %>%
        mutate(date = date %>% as.character()) %>%
        filter(date %in% selected_dates)
    gc(reset=T)
    # ###################################################################
    # Modification of dataset
    # ###################################################################
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
        filter((capital_letters / characters) < 0.35) %>%
        mutate(text = gsub("\\s+", " ", str_trim(text)))
        
    # Unnest sentences
    articles_unnested <- articles_sentences %>%
        dplyr::select(id, paragraph_id, sentence_id, text) %>%
        unnest_tokens(word, text, to_lower = F) %>%
        group_by(sentence_id) %>%
        mutate(position = seq(1, n())) %>%
        ungroup() %>%
        strip_numeric(., word) %>%
        filter(!word %in% stop_words_pl) %>%
        left_join(grammar_data, by = c("word" = "tekst")) %>%
        mutate(word = ifelse(is.na(slowo), word, slowo)) %>%
        dplyr::select(-slowo) %>%
        mutate(word = gsub(" ", "", word)) %>%
        filter(!word %in% stop_words_pl) 
    
    # Select minimum number of tokens occurence basing on distribution of data
    data_grouped <-  articles_unnested %>%
        group_by(word) %>%
        summarise(counts = n())
    v_min_counts <- quantile(data_grouped$counts, probs = 0.95) 
    gc(reset=T)
    
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
                                   singularity_penalty=0.0,
                                   # Summarization
                                   lemmatized_sentences=lemmatized_sentences, 
                                   lemmatized_articles=lemmatized_articles,
                                   sentences_text=sentences_text,
                                   log_lambda_statistics_df=log_lambda_statistics_df,
                                   min_key_freq=0.1, max_sentence_simil=0.4, #0.5
                                   section_id="section_id", word_col="word",
                                   use_sparse=TRUE, freq_to_lex_rank=0.05,
                                   max_sentences_num=20, min_sentences_num=5,
                                       freq_to_show=0.01, embedding_size=256,
                                   only_groups=TRUE)
    
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
    # list_topics <- lambd_extractor(list_topics, articles_unnested, 
    #                                general_stats, min_counts = v_min_counts, 
    #                                min_lambda_daily = v_min_lambda_daily)
    # list_topics <- date_extractor(list_topics, articles_sentences)
    # list_topics <- arrange_topics(list_topics, "max_lambda")
    # list_topics <- clear_sentences(list_topics)
    # list_topics <- delete_unrelevant_topics(list_topics, 150, 100)
    # 
    # # Select filtered topic words
    # selected_words <- filter_selected_words(list_topics)
    selected_words <- filtered_lambda_statistics$word[1:30]
    words_similarity_matrix <- words_similarity_matrix[selected_words, selected_words]
    
    #####################################################################
    # Saving output for report
    # ###################################################################
    # Data frame with Dunning statistics for plotting
    lambda_daily_DF <- calculate_lambda_statistics(articles_unnested, general_stats, 
                                                   min_counts = v_min_counts, 
                                                   min_lambda_daily = v_min_lambda_daily) %>% 
        mutate(lambda_log = log(lambda + 1) ) %>%
        rename(name = word) %>%
        dplyr::select(name, lambda, lambda_log) %>%
        filter(name %in% selected_words)
    
    # Saving topics list, similarity matrix and df with Dunning statistics
    file_name <- paste0(working_dir, 
                        "News_Selector/data/topics/weeks/weekly_topics_", 
                        start_dates[i], "-", end_dates[i], ".RData")
    save(list_topics, 
         words_similarity_matrix, 
         lambda_daily_DF, 
         file = file_name)
}
    
