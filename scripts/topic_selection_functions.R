strip_numeric <- function(data, word_col){
  #' This function finds and delets numeric tokens
  #' @param data Data frame of unnested tokens
  #' @param word_col Name of column with tokens
  word_col <- enquo(word_col)
  data <- data %>%
    mutate(!! word_col := gsub(!! word_col, pattern = ",", replacement = ".", fixed = T)) %>%
    filter(is.na(as.numeric(!! word_col)) == T)
  
  return(data)
}


prepare_python_inputs <- function(articles_unnested,
                                  sentences_text,
                                  general_stats,
                                  article_id, paragraph_id, sentence_id, word_col, 
                                  min_counts = 10, 
                                  min_lambda_daily = 10){
    #' This function prepares Python inputs
    #' @param articles_unnested data frame of unnested sentences
    #' @param sentences_text data frame of raw sentences
    #' @param general_stats data frame with general statistics - benchamrk for Dunning 
    #' statistics
    #' @param article_id name of column with article id
    #' @param paragraph_id name of column with paragraph id
    #' @param sentence_id name of column with sentence id
    #' @param word_col name of column with tokens
    #' @param min_counts integer with minimal number of occurence of tokens
    #' that are to be included in clustering
    #' @param min_lambda_daily integer with minimal value of Dunning statistics that 
    #' let tokens to be included in clustering
    
    # Word column name
    word_col <- enquo(word_col)
    # Word column name string
    word_col_str <- quo_name(word_col)
    # Article id column
    article_id <- enquo(article_id)
    # Paragraph id column
    paragraph_id <- enquo(paragraph_id)
    # Paragraph id column
    sentence_id <- enquo(sentence_id)
    
    # Prepare inputs for summarisation
    sections <- articles_unnested %>%
        group_by(!!article_id, !!paragraph_id) %>%
        dplyr::summarise(text = paste(word, collapse = " ")) %>%
        ungroup() %>%
        dplyr::select(-!!paragraph_id)
    
    sections_and_articles <- sections %>%
        group_by(!!article_id) %>%
        dplyr::summarise(text = paste(text, collapse = " ")) %>%
        ungroup() %>%
        union_all(sections)
    
    ids_to_sites <- sentences_text %>%
      dplyr::select(id, site) %>%
      distinct(id, .keep_all = T)
    
    sections_and_articles <- sections_and_articles %>%
      left_join(ids_to_sites, by = "id")
    
    lemmatized_sentences <- articles_unnested %>%
        group_by(!!article_id, !!sentence_id) %>%
        summarise(text = paste(word, collapse = " ")) %>%
        ungroup() %>%
        arrange(!!article_id, !!sentence_id)
    
    filtered_sentences_ids <- lemmatized_sentences %>%
        distinct(!!sentence_id)
    
    lemmatized_articles <- articles_unnested %>%
        group_by(!!article_id) %>%
        summarise(text = paste(word, collapse = " ")) %>%
        ungroup() %>%
        arrange(!!article_id)
    
    sentences_text <- sentences_text %>%
        arrange(!!article_id, !!sentence_id) %>%
        left_join(sites, by = "site") %>%
        inner_join(filtered_sentences_ids, by = "sentence_id")
    
    log_lambda_statistics_df <- calculate_lambda_statistics(articles_unnested, general_stats, 
                                                            min_counts = 1, min_lambda_daily = -10000) %>% 
        mutate(lambda = ifelse(lambda < -2, 0, lambda),
               # lambda_log = log(lambda + 2) + 1) %>% # Added one after logarithm to smooth this function
                # lambda_log = log(lambda + 3) + 1) %>% # Added one after logarithm to smooth this function
                lambda_log = sqrt(lambda + 3)) %>% # Added one after logarithm to smooth this function
        dplyr::select(word, lambda_log) %>%
        rename(lambda = lambda_log)
    
    filtered_lambda_statistics <- calculate_lambda_statistics(articles_unnested, general_stats,  
                                                              min_counts = min_counts, 
                                                              min_lambda_daily = min_lambda_daily)
    
    rm(sections, filtered_sentences_ids)
    return(list(sections_and_articles, filtered_lambda_statistics, log_lambda_statistics_df,
                lemmatized_sentences, lemmatized_articles, sentences_text))
}

lambd_extractor <- function(list_topics, unnested_articles, general_stats, min_counts = 10, min_lambda_daily = 10){
    #' This is a function which add to topics' list data frame of words with their lambda value
    #' @param list_topics List of topics
    #' @param unnested_articles Data frame of unnested articles from particular period to calculate lambda statistics
    #' @param general_stats General statistics for Dunning statistics
    #' @param min_counts Minimum number of occurences to include word
    #' @param min_lambda_daily Minimum value of Dunning statistics
    #' @return List of topics
    #' @export
    
    # Compute daily Dunning statistics
    daily_lambda <- calculate_lambda_statistics(unnested_articles, general_stats, min_counts = min_counts, min_lambda_daily = min_lambda_daily) %>%
        dplyr::select(word, lambda, counts_1) %>%
        rename(counts = counts_1)
    
    for(name in names(list_topics)){
        words <- list_topics[[name]][["word"]]
        words <- data.frame(word = words, stringsAsFactors = F) %>%
            left_join(daily_lambda, by = "word")
        list_topics[[name]][["words_DF"]] <- words
        list_topics[[name]][["max_lambda"]] <- max(words[["lambda"]])
    }
    
    rm(words, daily_lambda)
    return(list_topics)
}

date_extractor <- function(list_topics, sentences){
    #' This is a function which add to topics' list data frame of words with their lambda value
    #' @param list_topics List of topics
    #' @param sentences Data frame with sentences that contains sentence_id and date
    #' @return List of topics
    #' @export
    
    
    for(name in names(list_topics)){
        sentences_ids <- list_topics[[name]][["sentences_ids"]]
        sentences_ids <- data.frame(sentence_id = sentences_ids, stringsAsFactors = F) %>%
            left_join(sentences, by = "sentence_id")
        list_topics[[name]][["dates"]] <- sentences_ids$date
    }
    
    return(list_topics)
}

arrange_topics <- function(list_topics, v_value = "max_lambda"){
    #' This is a function which sorts topics list, by v_value - it should be greates lambda value in the list
    #' @param list_topics List of topics
    #' @param v_value String of list's element with value to sort decreasingly
    #' @return List of topics
    #' @export
    
    values <- c()
    for(name in names(list_topics)){
        values <- c(values, list_topics[[name]][[v_value]])
    }
    
    topics_seq <- seq(1, length(list_topics))
    topics_seq <- topics_seq[order(values, decreasing = T)]
    
    list_topics <- list_topics[topics_seq]
    
    rm(values, v_value, topics_seq)
    return(list_topics)
}

delete_unrelevant_topics <- function(list_topics, min_singular_lambda=150,
                                     min_topic_lambda=100){
  
  for(name in names(list_topics)){
    if( (list_topics[[name]][["max_lambda"]] < min_topic_lambda)){
      list_topics[[name]] <- NULL
    } else if((length(list_topics[[name]][["word"]]) == 1) & (list_topics[[name]][["max_lambda"]] < min_singular_lambda)) {
      
    }
  }
  return(list_topics)
}

filter_selected_words <- function(list_topics){
  # Correct TABS
  selected_words <- c()
  for(topic in list_topics){
    selected_words <- c(selected_words, topic[["word"]])
  }
  return(selected_words)
}

plot_all_words_correlation <- function(words_similarity_matrix, scale_font = c(8, 4), 
                                       class_num = 6, min_association = 0.4, lambda_DF,
                                       maximum_words_num = 200){
    #' Function plotting all words that are included in a particular report
    
    set.seed(1234)
    require(classInt)
    
    # Limit number of words to be displayed
    if(nrow(lambda_DF) > maximum_words_num){
        lambda_DF <- lambda_DF %>%
            top_n(maximum_words_num, lambda)
        words <- lambda_DF[["name"]]
        words_similarity_matrix <- words_similarity_matrix[words, words]
    }
    
    simil_df <- as.data.frame.matrix(words_similarity_matrix) %>%
        mutate(col_1 = rownames(.)) %>%
        tidyr::gather(key = "col_2", value = "simil", -col_1) %>%
        mutate(name = col_1)
    
    
    # values <- lambda_DF[["lambda"]] %>% sqrt()
    values <- lambda_DF[["lambda_log"]]
    
    # Znajdz breaki
    brks <- classIntervals(values, n = class_num, style = "fisher") %>% .$brks #jenks
    # Znajdz kolejnosc w skali
    intervals <- findInterval(values, brks, all.inside = T)
    # Przypsiz kolory
    # colors <- brewer.pal(9, "YlGnBu")[(10-class_num):9]
    colors <- rev(pals::ocean.haline(class_num+1))[2:(class_num+1)]
    # colors <- brewer.pal(class_num, "YlGnBu")
    # colors <- rev(viridis::viridis(7))[(8-class_num):7]
    ordered.colors <- colors[intervals]
    lambda_DF[["color"]] <- ordered.colors
    lambda_DF[["color_num"]] <- as.character(intervals)
    
    # Rozmiar na wykresie
    # freq <- lambda_DF[["lambda"]]
    freq <- lambda_DF[["lambda_log"]]
    normedFreq <-  (freq - min(freq)) / (max(freq) - min(freq))
    size <- (scale_font[1] - scale_font[2]) * normedFreq + scale_font[2]
    lambda_DF[["size_plot"]] <- size
    scale_font[2] <- scale_font[2]*0.5
    
    # Plot
    plot <- simil_df %>%
        filter(simil > min_association) %>%
        group_by(name) %>%
        top_n(2, simil) %>%
        ungroup() %>%
        mutate(simil = simil - 0.001) %>%
        igraph::graph_from_data_frame(vertices = lambda_DF %>%
                                          inner_join(simil_df %>%
                                                         filter(simil > min_association) %>%
                                                         distinct(name), by = "name")) %>%
        ggraph::ggraph(layout = "fr", niter = 5000) +
        ggraph::geom_edge_link(aes(edge_width = simil*0.5), edge_colour = "#005353", # edge_colour = "cyan4", 
                               edge_alpha = 0.2,
                               check_overlap = T, show.legend = F)+
        ggraph::geom_node_point(aes(size = size_plot*0.5), color = "lightblue", #color = "lightblue", 
                                show.legend = F, alpha=1.0)+
        ggraph::geom_node_text(aes(label = name, size = size_plot, color = color_num), repel = TRUE,
                               point.padding = 1e-06, check_overlap = T, show.legend = F,
                               segment.alpha=0.5, segment.size=0.2, min.segment.length=0.25,
                               box.padding=0.175, force=2, max.iter=4000) +
        # scale_size(range = c(2, 5))+
        scale_size(range = rev(scale_font))+
        scale_color_manual(values = colors)+
        theme_void()
    
    rm(simil_df, words_similarity_matrix, lambda_DF, values, brks, intervals, colors, ordered.colors, freq, normedFreq, size)
    return(plot)
}

count_quantile <- function(number_of_words){
    #' Function setting quantile from setting minimal association that 
    #' is plotted as connection between tokens
    quant = 0.25^(1/number_of_words)
    return(quant)
}

plot_topic_correlation <- function(topic_words, words_similarity_matrix, 
                                   scale_font = c(8, 4), class_num = 6, 
                                   min_association = 0.4, lambda_DF,
                                   maximum_words_num = 40){
    #' Ploting correlation and importance of words in a particular topic
    set.seed(1234)
    library(classInt)
    
    
    
    similarity_df <- as.data.frame.matrix(words_similarity_matrix) %>%
        mutate(col_1 = rownames(.)) %>%
        tidyr::gather(key = "col_2", value = "simil", -col_1) %>%
        mutate(name = col_1)
    
    # values <- lambda_DF[["lambda"]] %>% sqrt()
    values <- lambda_DF[["lambda_log"]]
    
    # Znajdz breaki
    brks <- classIntervals(values, n = class_num, style = "fisher") %>% .$brks #jenks
    # Znajdz kolejnosc w skali
    intervals <- findInterval(values, brks, all.inside = T)
    # Przypsiz kolory
    # colors <- brewer.pal(9, "YlGnBu")[(10-class_num):9]
    colors <- rev(pals::ocean.haline(class_num+1))[2:(class_num+1)]
    # colors <- brewer.pal(class_num, "YlGnBu")
    # colors <- rev(viridis::viridis(7))[(8-class_num):7]
    ordered_colors <- colors[intervals]
    lambda_DF[["color"]] <- ordered_colors
    lambda_DF[["color_num"]] <- as.character(intervals)
    
    # Limit number of words to be displayed
    lambda_topic_DF <- lambda_DF %>%
        filter(name %in% topic_words)
    if(length(topic_words) > maximum_words_num){
        lambda_topic_DF <- lambda_topic_DF %>%
            top_n(maximum_words_num, lambda)
        topic_words <- lambda_topic_DF[["name"]]
        # lambda_DF <- lambda_topic_DF
    }
    colors_num <- lambda_topic_DF[["color_num"]] %>% unique() %>% 
        as.numeric() %>%  base::sort()
    colors <- colors[colors_num]
    
    
    # Rozmiar na wykresie
    freq <- lambda_DF[["lambda_log"]]
    normedFreq <-  (freq - min(freq)) / (max(freq) - min(freq))
    size <- (scale_font[1] - scale_font[2]) * normedFreq + scale_font[2]
    lambda_DF[["size_plot"]] <- size
    scale_font[2] <- scale_font[2]*0.5
    
    # Plot
    plot <- similarity_df %>%
        filter(col_1 %in% topic_words & col_2 %in% topic_words) 
    
    # quant <- count_quantile(length(topic_words))
    # min_association <- quantile(plot$simil, quant)
    plot <- plot %>%
        filter(simil > min_association) %>%
        group_by(name) %>%
        top_n(2, simil) %>%
        ungroup()
    
    if(nrow(plot) == 0){
        plot <- similarity_df %>%
            filter(col_1 %in% topic_words & col_2 %in% topic_words) %>%
            mutate(simil = NA) %>%
            igraph::graph_from_data_frame(vertices = lambda_DF %>%
                                              filter(name %in% topic_words))
    } else {
        plot <- plot %>%
            mutate(simil = simil - 0.001) %>%
            igraph::graph_from_data_frame(vertices = lambda_DF %>%
                                          filter(name %in% topic_words))
    }
    
    plot <- plot %>%
        ggraph::ggraph(layout = "fr", niter = 3000)+
        ggraph::geom_edge_link(aes(edge_width = simil*0.5), edge_colour = "#005353", # edge_colour = "cyan4", 
                               edge_alpha = 0.2,
                               check_overlap = T, show.legend = F)+
        ggraph::geom_node_point(aes(size = size_plot*0.5), color = "lightblue", #color = "lightblue", 
                                show.legend = F, alpha=1.0)+
        ggraph::geom_node_text(aes(label = name, size = size_plot, color = color_num), repel = TRUE,
                               point.padding = 1e-06, check_overlap = T, show.legend = F,
                               segment.alpha=0.5, segment.size=0.2, min.segment.length=0.25,
                               box.padding=0.175, force=3, max.iter=4000)+
        scale_size(range = rev(scale_font))+
        scale_color_manual(values = colors)+
        theme_void()
    
    rm(similarity_df, words_similarity_matrix, lambda_DF, values, brks, intervals, colors, ordered.colors, freq, normedFreq, size)
    return(plot)
}

clear_sentences <- function(list_topics){
    for(name in names(list_topics)){
        list_topics[[name]][["sentences"]] <- stringr::str_replace_all(list_topics[[name]][["sentences"]], pattern = "\\n", replacement = " ")
        list_topics[[name]][["sentences"]] <- stringr::str_replace_all(list_topics[[name]][["sentences"]], pattern = "(\\s{2,20})", replacement = " ")
        list_topics[[name]][["sentences"]] <- stringr::str_replace_all(list_topics[[name]][["sentences"]], pattern = "  ", replacement = " ")
        list_topics[[name]][["sentences"]] <- stringr::str_replace_all(list_topics[[name]][["sentences"]], pattern = "^\\s", replacement = "")
        list_topics[[name]][["sentences"]] <- stringr::str_replace_all(list_topics[[name]][["sentences"]], pattern = "^-", replacement = "")
    }
    
    return(list_topics)
}

calculate_lambda_statistics <- function(unnested_articles, general_stats, grammar_type_data = NULL, selected_type = NULL, min_counts = NULL, min_lambda_daily = 10){
    daily_stats <- unnested_articles %>%
        group_by(word) %>%
        dplyr::summarise(counts_1 = n()) %>%
        ungroup() %>%
        left_join(general_stats, by = "word")
    
    n_1 <- sum(daily_stats$counts_1)
    n_2 <- sum(general_stats$counts_general) - n_1
    
    daily_stats <- daily_stats %>%
        mutate(perc_1 = counts_1 / n_1,
               counts_2 = counts_general - counts_1,
               perc_2 = counts_2 / n_2) %>%
        mutate(lambda = log_Formulae_Dunning(perc_general, perc_1, n_1, counts_1, perc_2, n_2, counts_2)) %>% # UWAGA ta wersja jest poprawna, do poprawy sa wczesniejsze obliczenia, nie powinny sie one bardzo istotnie zmienic
        arrange(desc(lambda))
    
    if(!is.null(selected_type)){
        daily_stats <- daily_stats %>%
            inner_join(grammar_type_data %>%
                           filter(main_type %in% selected_type), by = "word")
    }
    
    if(!is.null(min_counts)){
        daily_stats <- daily_stats %>%
            filter(counts_1 >= min_counts)
    }
    
    if(!is.null(min_lambda_daily)){
        daily_stats <- daily_stats %>%
            filter(lambda > min_lambda_daily) # Wybor tylko istotnych slow
    }
    
    return(daily_stats)
}

