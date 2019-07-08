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

words_association <- function(vec_1, vec_2, n){
    AB <- sum(vec_1 * vec_2) 
    A_not_B <- sum(vec_1 * (!vec_2))
    not_A_B <- sum((!vec_1) * vec_2)
    not_A_not_B <- n - AB - A_not_B - not_A_B
    
    # First group - prob. of A when B
    k_1 <- AB
    n_1 <- AB + not_A_B
    perc_1 <- k_1 / n_1
    
    # Second group - prob. of A when not B
    k_2 <- A_not_B
    n_2 <- A_not_B + not_A_not_B
    perc_2 <- k_2 / n_2
    
    # 
    perc_general <- (k_1 + k_2) / (n_1 + n_2)
    
    lambda_stat <- log_Formulae_Dunning(perc_general, perc_1, n_1, k_1, perc_2, n_2, k_2) # This function already returns negative statistics
    return(lambda_stat)
}

words_association_weighted <- function(vec_1, vec_2, vec_raw_1, vec_raw_2, n){
    AB <- sum(vec_1 * vec_2) * lexRankr:::idfCosineSimil(rbind(vec_raw_1, vec_raw_2))
    A_not_B <- sum(vec_1 * (!vec_2)) #* mean(vec_raw_1[vec_1 & (!vec_2)]) / mean(vec_raw_1[vec_1])
    not_A_B <- sum((!vec_1) * vec_2) #* mean(vec_raw_2[(!vec_1) & (vec_2)]) / mean(vec_raw_2[vec_2])
    not_A_not_B <- n - AB - A_not_B - not_A_B
    
    # First group - prob. of A when B
    k_1 <- AB
    n_1 <- AB + not_A_B
    perc_1 <- k_1 / n_1
    
    # Second group - prob. of A when not B
    k_2 <- A_not_B
    n_2 <- A_not_B + not_A_not_B
    perc_2 <- k_2 / n_2
    
    # 
    perc_general <- (k_1 + k_2) / (n_1 + n_2)
    
    lambda_stat <- log_Formulae_Dunning(perc_general, perc_1, n_1, k_1, perc_2, n_2, k_2) # This function already returns negative statistics
    
    return(lambda_stat)
}


Dunning_association <- function(mat, n_words, n){
    resInd = 1;
    nChoose2 = n_words*(n_words)/2;
    results <- vector("numeric", nChoose2)
    for(i in 1:n_words){
        for(j in (i):n_words){
            results[resInd] = words_association(mat[i, ], mat[j, ], n);
            resInd = resInd + 1;
        }
    }
    # https://stackoverflow.com/questions/22569176/r-permutations-and-combinations-with-without-replacement-and-for-distinct-non-d
    dlPairsDf <- as.data.frame(combinations(sort(rownames(mat)), 2, replace = TRUE), stringsAsFactors = F) 
    dlPairsDf[["lambda_ass"]] <- results
    names(dlPairsDf) <- c("word1", "word2", "lambda_ass")
    return(dlPairsDf)
}

Dunning_weighted_association <- function(mat, mat_raw, n_words, n){
    resInd = 1;
    nChoose2 = n_words*(n_words)/2;
    results <- vector("numeric", nChoose2)
    for(i in 1:n_words){
        for(j in (i):n_words){
            results[resInd] = words_association_weighted(mat[i, ], mat[j, ], mat_raw[i, ], mat_raw[j, ], n);
            resInd = resInd + 1;
        }
    }
    # https://stackoverflow.com/questions/22569176/r-permutations-and-combinations-with-without-replacement-and-for-distinct-non-d
    dlPairsDf <- as.data.frame(combinations(sort(rownames(mat)), 2, replace = TRUE), stringsAsFactors = F) 
    dlPairsDf[["lambda_ass"]] <- results
    names(dlPairsDf) = c("word1", "word2", "lambda_ass")
    return(dlPairsDf)
}

Cosinus_association <- function(mat){
    dlPairsDf <- as.data.frame(combinations(sort(rownames(mat)), 2, replace = FALSE), stringsAsFactors = F)
    dlPairsDf[["lambda_ass"]] <- lexRankr:::idfCosineSimil(mat)
    
    names(dlPairsDf) = c("word1", "word2", "lambda_ass")
    
    return(dlPairsDf)
}

PairsDf_to_mat <- function(dlPairsDf){
    dlPairsDf <- dlPairsDf %>%
        union_all(dlPairsDf %>%
                      filter(word2 != word1) %>%
                      rename(word22 = word1,
                             word1 = word2) %>%
                      rename(word2 = word22))
    
    dlPairs <- xtabs(lambda_ass ~ word1 + word2, dlPairsDf, drop.unused.levels = F)
    return(dlPairs)
}

TopicSelection <- function(articles, general_stats, section, word_col, id_col, min_counts = 10, min_lambda_daily = 10, 
                           min_association = NULL, association_type = c("Dunning", "Cosinus", "W_Dunning", "Dunning_Cosinus"),
                           min_words_topic = 2){
    
    #' This is a function which select words and indicates their position in sections
    #' @param articles this is data frame with unnested text
    #' @param general_stats General statistics of frequencies
    #' @param word_col Name of word column
    #' @param id_col Name of articles' id column
    #' @param min_counts Minimum counts of word in a selected date
    #' @param min_lambda_daily Minimum value of lambda statistics to include word into topics' list
    #' @param min_association Minimum association with main word in topic - 4 for Dunning, 0.5 for Cosinus, 4 for W_Dunning
    #' @param min_words_topic Minimum number of words in topic
    #' @return List of topics
    #' @export
    
    
    if(length(association_type) > 1) association_type = association_type[1]
    if(!association_type %in%  c("Dunning", "Cosinus", "W_Dunning", "Dunning_Cosinus")) stop("You need to input one of the following association types: Dunning, Cosinus, W_Dunning!")
    if(is.null(min_association)){
        if(association_type == "Cosinus"){
            min_association <- 0.5
        } else {
            min_association <- 4.0
        }
    } 
    
    
    # Word column name
    word_col <- enquo(word_col)
    # Word column name string
    word_col_str <- quo_name(word_col)
    # Section column
    section <- enquo(section)
    # ID column
    id_col <- enquo(id_col)
    
    # Compute daily Dunning statistics
    daily_lambda <- calculate_lambda_statistics(articles, general_stats,  min_counts = min_counts, min_lambda_daily = min_lambda_daily)
    daily_lambda_ <- articles %>%
        inner_join(daily_lambda %>%
                       dplyr::select(!! word_col), by = word_col_str) %>%
        dplyr::select(!! section, !! word_col) %>%
        rename(SectionId := !! section,
               word := !! word_col)
    
    dlList = split(daily_lambda_, paste0(daily_lambda_$SectionId, daily_lambda_$word))
    dlList = lapply(dlList, function(dfi) {
        dfi[["tf"]] = nrow(dfi) > 0 
        unique(dfi)
    })
    dl = do.call("rbind", dlList)
    dlList = split(dl, dl$word)
    dl = do.call("rbind", dlList)
    rownames(dl) = NULL
    dl = dl[order(dl$SectionId, dl$word), c("SectionId", "word", "tf")]
    
    mat = xtabs(tf ~ word + SectionId, dl)
    
    # Number of 
    n <- articles %>%
        distinct(!! section) %>%
        nrow()
    n_words <- nrow(mat)
    
    # Dodanie artykulow, w ktorych nie wystepuje zadne z waznych/wybranych slow
    mat <- cbind(mat, matrix(0, ncol = (n - ncol(mat)), nrow = n_words))
    
    # Calculate association
    if(association_type == "Dunning"){
        ### Dunning
        # Zamiana wartosci numerycznych na logiczne
        mat = apply(mat, c(1,2), function(x){
            x = x > 0
        })
        
        dlPairsDf <- Dunning_association(mat, n_words, n)
        dlPairs <- PairsDf_to_mat(dlPairsDf)
        rm(mat, dlPairsDf)
        
    } else if(association_type == "Cosinus"){
        ### Cosinus
        dlPairsDf <- as.data.frame(combinations(sort(rownames(mat)), 2, replace = FALSE), stringsAsFactors = F)
        dlPairsDf[["lambda_ass"]] <- lexRankr:::idfCosineSimil(mat)
        
        names(dlPairsDf) = c("word1", "word2", "lambda_ass")
        
        dlPairs <- PairsDf_to_mat(dlPairsDf)
        diag(dlPairs) <- 1
        
        rm(dlPairsDf, mat)
        
    } else if(association_type == "W_Dunning"){
        ### Weighted Dunning
        # Copy the raw matrix
        mat_raw <- mat
        
        # Zamiana wartosci numerycznych na logiczne
        mat = apply(mat, c(1,2), function(x){
            x = x > 0
        })
        
        dlPairsDf <- Dunning_weighted_association(mat, mat_raw, n_words, n)
        dlPairs <- PairsDf_to_mat(dlPairsDf)
        
        rm(mat, mat_raw, dlPairsDf)
    } else {
        ### Dunning - Cosinus
        # Copy the raw matrix
        mat_raw <- mat
        # Zamiana wartosci numerycznych na logiczne
        mat = apply(mat, c(1,2), function(x){
            x = x > 0
        })
        
        
        # Dunning
        dlPairsDf <- Dunning_association(mat, n_words, n) # Tu jest blad
        D_mat <- PairsDf_to_mat(dlPairsDf)
        
        # Cosinus
        dlPairsDf_cos <- Cosinus_association(mat)
        Cos_mat <- PairsDf_to_mat(dlPairsDf_cos)
        diag(Cos_mat) <- 1
        
        # Multipication
        dlPairs <- D_mat * Cos_mat
        
        rm(D_mat, Cos_mat, dlPairsDf_cos, dlPairsDf, mat, mat_raw)
    }
    
    # Reorder data
    daily_lambda <- daily_lambda[order(daily_lambda[["word"]]), ]
    dlPairs <- dlPairs[order(daily_lambda[["lambda"]], decreasing = T), order(daily_lambda[["lambda"]], decreasing = T)]
    
    # Copy raw matrix
    dlPairs_raw <- dlPairs
    dlPairs = apply(dlPairs, c(1,2), function(x){
        x = x > min_association
    })
    
    
    results_topics <- vector("logical", n_words)
    results_topics[1] <- T
    
    for(i in 2:n_words){
        vec <- dlPairs[i, results_topics]
        results_topics[i] <- !any(vec)
    }

    
    topics_pos <- which(results_topics)
    list_topics <- list()
    
    for(i in topics_pos){
        associated_words <- names(dlPairs[i, i:n_words])[which(dlPairs[i, i:n_words])]
        lambda <- dlPairs_raw[i, i:n_words][which(dlPairs[i, i:n_words])]
        list_topics[[colnames(dlPairs)[i]]] <- list(word = associated_words, lambda_main_word = lambda)
    }
    
    # Eliminate topics with too few words
    for(name in names(list_topics)){
        if(length(list_topics[[name]]$word) < min_words_topic){
            list_topics <- list_topics[names(list_topics) != name]
        }
    }
    return(list_topics)
}

find_list_number <- function(object, grouped_objects){
    #' Finds number of list entity in which is object
    #' @param object Object name
    #' @param grouped_objects List of groups with names of objects
    
    for(i in seq(1, length(grouped_objects))){
        if(object %in% grouped_objects[[i]]) return(i)
    }
}

calculate_inner_mean_distance <- function(object, list_number, grouped_objects, distance_matrix){
    #' Calculate mean distance to other objects in a group
    #' @param object Name of object
    #' @param list_number Number of list entity - group of an object
    #' @param grouped_objects List of groups with names of objects
    #' @param distance_matrix distance between object
    
    # Select names of other object in a group
    neighbours <- grouped_objects[[list_number]]
    neighbours <- neighbours[-which(neighbours == object)]
    
    distances <- distance_matrix[object, neighbours]
    return(mean(distances))
}

calculate_outer_min_distance <- function(object, list_number, grouped_objects, ungrouped_objects, distance_matrix){
    #' Calculate minimum distance to other groups
    #' @param object Name of object
    #' @param list_number Number of list entity - group of an object
    #' @param grouped_objects List of groups with names of objects
    #' @param ungrouped_objects Vector of ungrouped objects' names
    #' @param distance_matrix distance between object
    
    # Prealocate min distance
    min_distance <- 1
    
    # Select names of other object in a group
    grouped_objects <- grouped_objects[-list_number]
    
    for (group in grouped_objects) {
        group_objects <- unlist(group)
        distance <- distance_matrix[object, group_objects] %>%
            mean()
        if(distance < min_distance) min_distance = distance
    }
    # Find minimal distance to ungrouped objects
    min_ugrouped_distance <- distance_matrix[object, ungrouped_objects] %>%
        min()
    if(min_ugrouped_distance < min_distance) min_distance = min_ugrouped_distance
    
    return(min_distance)
}

object_silhouette <- function(object, grouped_objects, ungrouped_objects, distance_matrix){
    #' Function calculating silhouette value for particular object
    #' @param object Name of object
    #' @param grouped_objects List of groups with names of objects
    #' @param ungrouped_objects Vector of ungrouped objects' names
    #' @param distance_matrix distance between object
    
    # If object is not grouped then its silhouette value is zero
    if(object %in% ungrouped_objects){
        silhouette_value <- 0
    } else {
        list_number <- find_list_number(object, grouped_objects)
        inner_distance <- calculate_inner_mean_distance(object, list_number, grouped_objects, distance_matrix)
        outer_min_distance <- calculate_outer_min_distance(object, list_number, grouped_objects, ungrouped_objects, distance_matrix)
        silhouette_value <- (outer_min_distance - inner_distance) / max(outer_min_distance, inner_distance)
    }
    return(silhouette_value)
}

silhouette <- function(distance_matrix, list_topics, objects_field = "word"){
    #' This function calculate silhouette value
    #' @param distance_matrix Matrix of distances between objects
    #' @param list_topics List of groups with names of objects belonging to each group
    #' @param object_field Name of objects vector in list_topics

    objects <- colnames(distance_matrix)
    grouped_objects <- list()
    iter <- 1
    for (topic in list_topics){
        grouped_objects[[iter]] <- topic[[objects_field]]
        iter <- iter + 1
    }
    ungrouped_objects <- objects[!objects %in% unlist(grouped_objects)]
    silhouette_vector <- c()
    
    for (object in objects){
        silhouette_value <- object_silhouette(object, grouped_objects, ungrouped_objects, distance_matrix)
        silhouette_vector <- c(silhouette_vector, silhouette_value)
    }
    return(mean(silhouette_vector))
}

CosineClustering <- function(similarity_matrix, mat, min_association, silhouette_flag = TRUE){
    
    
    #' This is a function which cluster words to topics according to cosine simillarity
    #' @param similarity_matrix Matrix of cosinus simillarity between words
    #' @param mat Matrix with number of words in particular paraghraphs - name it as distribution_matrix
    #' @param min_association Minimal association between topic and new word
    #' @param silhouette_flag Should silhouette algorithm be used
    #' @return List of topics
    #' @export
    
    
    # Prealocate list
    list_topics <- list()
    max_val <- max(similarity_matrix)
    
    # Prealocate vectors for assesing grouping quality
    max_val_history <- c()
    max_silhouette <- 0
    silhouette_history <- c(max_silhouette)
    
    # Copy matrices
    distance_matrix <-  1 - similarity_matrix
    mat_raw <- mat
    
    topic_num <- 1
    while(max_val >= min_association){
        max_list <- which(similarity_matrix == max_val, arr.ind = TRUE)
        
        
        sampled_row <- sample(seq(1, nrow(max_list)), 1)
        positions <- max_list[sampled_row,]
        
        name_1 <- colnames(similarity_matrix)[max_list[sampled_row, 1]]
        name_2 <- colnames(similarity_matrix)[max_list[sampled_row, 2]]
        
        # Check if selected entities are a topic
        logic_1 <- (name_1 %in% names(list_topics))
        logic_2 <- (name_2 %in% names(list_topics))
        
        # Depending on group create topic or add to topic that already exists
        # If both rows are not groups then create a new group
        if(!logic_1 & !logic_2){ 
            topic_name <- paste0("Gr_", topic_num)
            list_topics[[topic_name]] <- list(word = c(name_1, name_2))
            topic_num <- topic_num + 1 # Index to enable creation of next group
        # If both rows are groups then leave only first one, and add words of second to the first
        } else if (logic_1 & logic_2){
            topic_name <- name_1
            list_topics[[topic_name]][["word"]] <- c(list_topics[[topic_name]][["word"]], list_topics[[name_2]][["word"]])
            list_topics <- list_topics[names(list_topics) != name_2]
        # If first row is a group then add word to it
        } else if(logic_1){
            topic_name <- name_1
            list_topics[[topic_name]][["word"]] <- c(list_topics[[topic_name]][["word"]], name_2)
        # If second row is a group then add word to it
        } else {
            topic_name <- name_2
            list_topics[[topic_name]][["word"]] <- c(list_topics[[topic_name]][["word"]], name_1)
        }
        
        # Modify similarity matrix na distribution matrix
        similarity_matrix <- similarity_matrix[-positions, -positions]
        mat <- rbind((mat[positions[1], ] + mat[positions[2], ]), mat[-positions,])
        
        # Calculate similarity of new group with other entites
        cos_simil <- vector("numeric", nrow(mat)-1)
        for(i in 2:nrow(mat)){
            cos_simil[i-1] <- lexRankr:::idfCosineSimil(mat[c(1, i), ])
        }
        
        similarity_matrix <- cbind(cos_simil, similarity_matrix)
        similarity_matrix <- rbind(c(0, cos_simil), similarity_matrix)
        
        new_names <- c(topic_name, colnames(similarity_matrix)[-1])
        colnames(similarity_matrix) <- new_names
        rownames(similarity_matrix) <- new_names
        rownames(mat) <- new_names
        
        max_val <- max(similarity_matrix)
        
        # Evaluate clustering with silhouette
        if(silhouette_flag){
            max_val_history <- c(max_val_history, max_val) 
            silhouette_value <- silhouette(distance_matrix, list_topics, objects_field = "word")
            if(silhouette_value > max_silhouette){
                best_list_topics <- list_topics
                best_distribution_matrix <- mat
                max_silhouette <- silhouette_value
            }
            silhouette_history <- c(silhouette_history, silhouette_value)
        }else{
            best_list_topics <- list_topics
            best_distribution_matrix <- mat
        }
    }
    
    return(list(best_list_topics, best_distribution_matrix, max_val_history, silhouette_history))
}

Lambd_Extractor <- function(list_topics, unnested_articles, general_stats, min_counts = 10, min_lambda_daily = 10){
    #' This is a function which add to topics' list data frame of words with their lambda value
    #' @param list_topics List of topics
    #' @param unnested_articles Data frame of unnested articles from particular period to calculate lambda statistics
    #' @param general_stats General statistics for Dunning statistics
    #' @param min_counts Minimum number of occurences to include word
    #' @param min_lambda_daily Minimum value of Dunning statistics
    #' @return List of topics
    #' @export
    #' 
    
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

Arrange_Topics <- function(list_topics, v_value = "max_lambda"){
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

CosineClustering_Update <- function(list_topics, mat_raw, mat_topics, min_association){
    
    topic_names <- names(list_topics)
    for(name in topic_names){
        
        topic_tf <- mat_topics[name, ]
        cos_dist <- vector("numeric", nrow(mat_raw))
        for(i in 1:nrow(mat)){
            mat_bind <- rbind(topic_tf, mat_raw[i, ])
            cos_dist[i] <- lexRankr:::idfCosineSimil(mat_bind)
        }
        selected_words <- rownames(mat_raw)[cos_dist >= min_association]
        list_topics[[name]] <- list(word = selected_words)
        
        mat_topics[name, ] <- apply(mat_raw[selected_words,], 2, function(x){
            x = sum(x)
        })
    }
    
    return(list(list_topics, mat_topics))
}

#https://medium.com/@JLMC/understanding-three-simple-statistics-for-data-visualizations-2619dbb3677a
Inner_Cosinus_Similarity <- function(list_topics, mat_raw){
    
    # Iterate through groups
    for(name in names(list_topics)){
        words <- list_topics[[name]][["word"]]
        n_words <- length(words)
        cos_vec <- vector("numeric", n_words*(n_words-1)/2)
        words_mat <- mat_raw[words, ]
        cos_vec <- lexRankr:::idfCosineSimil(words_mat)
        # Save vector
        list_topics[[name]][["Cosinus_vec"]] <- cos_vec
        # Calculate mean cosinus
        list_topics[[name]][["Cosinus_mean"]] <- mean(cos_vec)
    }
    
    return(list_topics)
}

Inner_Cosinus_Similarity_vec <- function(list_topics, mat_raw){
    
    # Prealocate vector for inner similarity
    v_s <- c()
    
    # Iterate through groups
    for(name in names(list_topics)){
        words <- list_topics[[name]][["word"]]
        n_words <- length(words)
        cos_vec <- vector("numeric", n_words*(n_words-1)/2)
        words_mat <- mat_raw[words, ]
        cos_vec <- lexRankr:::idfCosineSimil(words_mat)
        # Save mean of vector
        v_s <- c(v_s, mean(cos_vec))
    }
    
    return(mean(v_s))
}

Outer_Cosinus_Dissimilarity <- function(list_topics, mat_topics){
    
    topics_names <- names(list_topics)
    mat_topics <- mat_topics[topics_names, ]
    cos_vec <- lexRankr:::idfCosineSimil(mat_topics)
    # Save mean
    list_topics[[1]][["Dissimilarity_mean"]] <- mean((1 - cos_vec))
    
    return(list_topics)
}

Outer_Cosinus_Dissimilarity_vec <- function(list_topics, mat_topics){
    
    # Prealocate vector for outer dissimilarity
    v_d <- c()
    
    topics_names <- names(list_topics)
    mat_topics <- mat_topics[topics_names, ]
    cos_vec <- lexRankr:::idfCosineSimil(mat_topics)
    # Save mean
    v_d <- mean((1 - cos_vec))
    
    return(v_d)
}

# Liczenie liczby slow w temacie
count_words <- function(list_topics){
    lapply(list_topics, function(x){
        x <- length(x[["word"]])
    })}


extract_words <- function(list_topics){
    lapply(list_topics, function(x){
        x <- x[["word"]]
    })}

# Wyciaganie Cosinusa z listy
extract_cosinus_mean <- function(list_topics){
    lapply(list_topics, function(x){
        x <- x[["Cosinus_mean"]]
    })}

extract_cosinus_vec <- function(list_topics){
    lapply(list_topics, function(x){
        x <- x[["Cosinus_vec"]]
    })}


MaxDistClustering <- function(dlPairs, min_association){
    # Prealocate list
    list_topics <- list()
    max_val <- max(dlPairs)
    
    topic_num <- 1
    while(max_val >= min_association){
        max_list <- which(dlPairs == max_val, arr.ind = TRUE)
        
        
        sampled_row <- sample(seq(1, nrow(max_list)), 1)
        positions <- max_list[sampled_row,]
        
        name_1 <- colnames(dlPairs)[max_list[sampled_row,1]]
        name_2 <- colnames(dlPairs)[max_list[sampled_row,2]]
        
        logic_1 <- (name_1 %in% names(list_topics))
        logic_2 <- (name_2 %in% names(list_topics))
        
        # Depending on group create topic or add to topic that already exists
        # If both rows are not groups then create a new group
        if(!logic_1 & !logic_2){ 
            topic_name <- paste0("Gr_", topic_num)
            list_topics[[topic_name]] <- list(word = c(name_1, name_2))
            topic_num <- topic_num + 1 # Index to enable creation of next group
            # If both rows are groups then leave only first one, and add words of second to the first
        } else if (logic_1 & logic_2){
            topic_name <- name_1
            list_topics[[topic_name]][["word"]] <- c(list_topics[[topic_name]][["word"]], list_topics[[name_2]][["word"]])
            list_topics <- list_topics[names(list_topics) != name_2]
            # If first row is a group then add word to it
        } else if(logic_1){
            topic_name <- name_1
            list_topics[[topic_name]][["word"]] <- c(list_topics[[topic_name]][["word"]], name_2)
            # If second row is a group then add word to it
        } else {
            topic_name <- name_2
            list_topics[[topic_name]][["word"]] <- c(list_topics[[topic_name]][["word"]], name_1)
        }
        
        
        dist_selected <- rbind(dlPairs[positions,])[, -positions]
        dist_selected <- apply(dist_selected, 2, function(x){
            x <- min(x) # Maximum Distance is here the minimum Cosinus similarity
        })
        # Delete similarities for old rows/cols
        dlPairs <- dlPairs[-positions, -positions]
        
        dlPairs <- cbind(dist_selected, dlPairs)
        dlPairs <- rbind(c(0, dist_selected), dlPairs)
        
        new_names <- c(topic_name, colnames(dlPairs)[-1])
        colnames(dlPairs) <- new_names
        rownames(dlPairs) <- new_names
        
        max_val <- max(dlPairs)
    }
    
    return(list_topics)
}

TopicClustering <- function(articles, general_stats, section, word_col, id_col, min_counts = 10, min_lambda_daily = 10, 
                            min_association = NULL, association_type = "Cosinus",
                            min_words_topic = 2){
    #' This is a function which select words and indicates their position in sections
    #' @param articles this is data frame with unnested text
    #' @param general_stats General statistics of frequencies
    #' @param word_col Name of word column
    #' @param id_col Name of articles' id column
    #' @param min_counts Minimum counts of word in a selected date
    #' @param min_lambda_daily Minimum value of lambda statistics to include word into topics' list
    #' @param min_association Minimum association with main word in topic - 4 for Dunning, 0.5 for Cosinus, 4 for W_Dunning
    #' @param min_words_topic Minimum number of words in topic
    #' @return List of topics
    #' @export
    
    ### TO DO
    # This function should be modified and cleared
    
    if(length(association_type) > 1) association_type = association_type[1]
    if(!association_type %in%  c("Dunning", "Cosinus", "W_Dunning", "Dunning_Cosinus")) stop("You need to input one of the following association types: Dunning, Cosinus, W_Dunning!")
    if(is.null(min_association)){
        if(association_type == "Cosinus"){
            min_association <- 0.5
        } else {
            min_association <- 4.0
        }
    } 
    
    
    # Word column name
    word_col <- enquo(word_col)
    # Word column name string
    word_col_str <- quo_name(word_col)
    # Section column
    section <- enquo(section)
    # ID column
    id_col <- enquo(id_col)
    
    # Compute daily Dunning statistics
    daily_lambda <- calculate_lambda_statistics(articles, general_stats,  min_counts = min_counts, min_lambda_daily = min_lambda_daily)
    daily_lambda_ <- articles %>%
        inner_join(daily_lambda %>%
                       dplyr::select(!! word_col), by = word_col_str) %>%
        dplyr::select(!! section, !! word_col) %>%
        rename(SectionId := !! section,
               word := !! word_col)
    
    dlList = split(daily_lambda_, paste0(daily_lambda_$SectionId, daily_lambda_$word))
    dlList = lapply(dlList, function(dfi) {
        dfi[["tf"]] = nrow(dfi) > 0 
        unique(dfi)
    })
    dl = do.call("rbind", dlList)
    dlList = split(dl, dl$word)
    dl = do.call("rbind", dlList)
    rownames(dl) = NULL
    dl = dl[order(dl$SectionId, dl$word), c("SectionId", "word", "tf")]
    
    mat = xtabs(tf ~ word + SectionId, dl)
    
    # Number of 
    n <- articles %>%
        distinct(!! section) %>%
        nrow()
    n_words <- nrow(mat)
    
    # Dodanie artykulow, w ktorych nie wystepuje zadne z waznych/wybranych slow
    mat <- cbind(mat, matrix(0, ncol = (n - ncol(mat)), nrow = n_words))
    
    # Calculate association
    ### Cosinus
    dlPairsDf <- as.data.frame(combinations(sort(rownames(mat)), 2, replace = FALSE), stringsAsFactors = F)
    dlPairsDf[["lambda_ass"]] <- lexRankr:::idfCosineSimil(mat)
        
    names(dlPairsDf) = c("word1", "word2", "lambda_ass")
        
    dlPairs <- PairsDf_to_mat(dlPairsDf)
    
    rm(articles, general_stats, dlList, dl, dlPairsDf, daily_lambda_, daily_lambda)
    
    # Tworzenie clustrow
    return(list(dlPairs, mat))
}

Similarity_with_topic <- function(topic_words, unnested_articles, section_id, word_col, filtered_similarity = T){
    #' This is a function that calculate cosine similarity between words and topic
    #' @param topic_words Vector of words that are in topic
    #' @param unnested_articles DF of unnested articles for selected date
    #' @param section_id Name of grouping column
    #' @param filtered_similarity Boolean value, for TRUE we also those articles which included topic words
    #' @return DF with cosine similarity with topic
    #' @export
    
    # NSE
    section_id <- enquo(section_id)
    section_id_str <- quo_name(section_id)
    word_col <- enquo(word_col)
    
    # Compute daily Dunning statistics
    DF_TF <- unnested_articles
    if(filtered_similarity){
        DF_TF <- DF_TF %>%
            filter(!! word_col %in% topic_words) %>%
            distinct(id) %>%
            inner_join(unnested_articles, by = "id")
    }
    DF_TF <- DF_TF %>%
        dplyr::select(!! section_id, !! word_col) %>%
        rename(SectionId := !! section_id,
               word := !! word_col) %>%
        group_by(SectionId, word) %>%
        summarise(tf = n()) %>%
        ungroup() %>%
        arrange(SectionId, word)
    
    mat = xtabs(tf ~ word + SectionId, DF_TF)
    mat = DescTools::as.matrix.xtabs(mat)
    
    # Sum TF of words in topic
    topic_vec <- mat[topic_words, ]
    topic_vec <- apply(topic_vec, 2, function(x){
        x = sum(x)
    })
    
    n_words <- nrow(mat)
    result <- vector("numeric", n_words)
    for(i in 1:n_words){
        result[i] <- lexRankr:::idfCosineSimil(rbind(topic_vec, mat[i, ]))
    }
    Cos_DF <- data.frame(word = rownames(mat), cos_simil = result, stringsAsFactors = F)
    
    return(Cos_DF)
}

Sentences_Extractor <- function(topic_words, unnested_articles, articles_text, section_id, 
                                word_col, sentence_id, general_stats, filtered_similarity = T, 
                                min_key_freq = 0.1){
    #' This function extracts phrases that best summarise topics
    #' @param topic_words Vector of words that are in topic
    #' @param unnested_articles DF of unnested sentences
    #' @param section_id Name of grouping column in similarity with topic
    #' @param word_col Name of word column
    #' @param sentence_id Name of sentence id
    #' @param general_stats General Statistics
    #' @param min_key_freq Minimum relative frequency of key words - this eliminates irrelevant articles
    #' @return DF of extracted sentences
    #' @export
    sentence_id <- enquo(sentence_id)
    sentence_id_str <- quo_name(sentence_id)
    word_col <- enquo(word_col)
    section_id <- enquo(section_id)
    
    # Select articles in which topic's words appear
    selected_ids <- unnested_articles %>%
        filter(word %in% topic_words) %>%
        distinct(id) 
    
    tokenDf <- articles_text %>%
        inner_join(selected_ids, by = "id") %>%
        distinct(id, !! sentence_id) %>%
        arrange(id, !! sentence_id) %>%
        mutate(sentenceId = !! sentence_id) %>%
        inner_join(unnested_articles, by = c("id", sentence_id_str)) %>%
        arrange(id, !! sentence_id, position) %>%
        rename(token = word,
               docId = id) %>%
        dplyr::select(docId, sentenceId, token, !! sentence_id) %>%
        as.data.frame()
    
    # Delete articles that have too few key words
    key_words_num <- unnested_articles %>%
        inner_join(selected_ids, by = "id") %>%
        filter(word %in% topic_words) %>%
        group_by(id) %>%
        summarise(key_counts = n()) %>%
        ungroup()
    
    selected_articles <- unnested_articles %>%
        inner_join(selected_ids, by = "id") %>%
        group_by(id) %>%
        summarise(all_counts = n()) %>%
        ungroup()
    
    all_mean <- sum(key_words_num$key_counts) / sum(selected_articles$all_counts)
    
    selected_articles <- selected_articles %>%
        left_join(key_words_num, by = "id") %>%
        mutate(freq = key_counts / all_counts) %>%
        filter(freq > min_key_freq * all_mean) %>%
        distinct(id) %>%
        rename(docId = id)
    
    tokenDf <- tokenDf %>%
        inner_join(selected_articles, by = "docId")
    
    #' TO DO
    #' Change naming of variables
    
    # Token statistics
    stm <- tokenDf %>%
        group_by(token, docId, sentenceId) %>%
        summarise(tf = n()) %>%
        ungroup() %>%
        arrange(sentenceId, token)
    
    TF_DF <- stm
    
    # Use lambda statistics instead of idf
    lambda_all <- calculate_lambda_statistics(unnested_articles, general_stats, min_counts = 1, min_lambda_daily = -10000)
    lambda_daily_DF <- lambda_all %>% 
        mutate(lambda = ifelse(lambda < 0, 0, lambda),
               lambda_log = log(lambda + 1) ) %>%
        rename(token := !! word_col) %>%
        dplyr::select(token, lambda_log, lambda)
    
    # Calculate Cosinus similarity with topic
    topic_simil <- Similarity_with_topic(topic_words, unnested_articles, !! section_id, !! word_col, filtered_similarity) %>%
        rename(token := !! word_col) %>%
        dplyr::select(token, cos_simil)
    
    stm <- stm %>%
        left_join(lambda_daily_DF %>%
                      dplyr::select(token, lambda_log) %>%
                      rename(lambda = lambda_log), by = "token") %>%
        left_join(topic_simil, by = "token") %>%
        mutate(tfidf = tf * lambda * cos_simil) %>% 
        dplyr::select(-lambda, -cos_simil) %>%
        filter(!is.na(tfidf)) %>%
        filter(tfidf > 0) %>%
        arrange(sentenceId, token)
    
    
    rownames(stm) = NULL
    
    if (nrow(stm) == 0){
        stop("All values in sentence term tfidf matrix are 0.  Similarities would return as NaN")}
    if (length(unique((stm$sentenceId))) == 1){
        stop("Only one sentence had nonzero tfidf scores.  Similarities would return as NaN")}
    
    stm = xtabs(tfidf ~ sentenceId + token, stm)
    n_sent <- nrow(stm)
    sentencePairsDf = as.data.frame(t(combn(sort(rownames(stm)), 
                                            2)), stringsAsFactors = FALSE)
    # http://gallery.rcpp.org/articles/subsetting/
    # https://stackoverflow.com/questions/6098024/is-there-an-equivalent-function-in-c-c-to-gnu-r-which
    # http://dirk.eddelbuettel.com/code/rcpp/Rcpp-sugar.pdf
    sentencePairsDf[["similVal"]] = lexRankr:::idfCosineSimil(stm)
    names(sentencePairsDf) = c("sent1", "sent2", "similVal")    
    
    similDf <- sentencePairsDf
    
    # Wyznaczanie glownych zdan
    topNSents <- my_lexRankFromSimil(s1 = similDf$sent1, s2 = similDf$sent2, 
                                     simil = similDf$similVal, threshold = 0.3, n = n_sent, 
                                     damping = 0.85, usePageRank = T, continuous = T) # UWAGA kod oryginalnej funkcji jest bledny
    
    topNSents <- topNSents %>%
        left_join(tokenDf %>%
                      distinct(docId, sentenceId, !! sentence_id), by = "sentenceId") %>%
        left_join(articles_text %>%
                      dplyr::select(!! sentence_id, text), by = sentence_id_str) %>%
        arrange(desc(value)) %>%
        left_join(tokenDf %>%
                      filter(token %in% topic_words) %>%
                      group_by(docId, sentenceId, !! sentence_id) %>%
                      summarise(counts = n()) %>%
                      ungroup() %>%
                      dplyr::select(counts, sentenceId), by = "sentenceId") %>%
        left_join(tokenDf %>%
                      group_by(docId, sentenceId) %>%
                      summarise(counts_all = n()) %>%
                      ungroup() %>%
                      dplyr::select(counts_all, sentenceId), by = "sentenceId") %>%
        filter(! is.na(counts)) %>%
        mutate(value_log = value * log(counts),
               value_log_log = value_log / log(counts_all),
               value_sqrt = value * sqrt(counts))
    
    rm(tokenDf, stm, similDf, sentencePairsDf,  topic_simil, lambda_daily_DF, lambda_all, selected_ids, key_words_num, selected_articles)
    
    return(list(topNSents, TF_DF))
}

Sentence_Selector <- function(topNSents, value_col, TF_DF, min_association = 0.5){
    
    #' This is a function select phrases that summarise topics uniquely
    #' @param topNSents Extracted sentences by Sentence_Extractor
    #' @param value_col Name of column of topNSents Data Frame that should be used to arrange sentences
    #' @param TF_DF Data frame of terms' frequency grouped by sentences
    #' @param min_association Minimum value of cosin similiarities which dfines that sentences are similar
    #' @return DF of selected sentences
    #' @export
    
    value_col <- enquo(value_col)
    value_col_str <- quo_name(value_col)
    
    topNSents <- topNSents %>%
        arrange(sentenceId)
    
    # Calculate similarites between sentences
    TF_DF <- TF_DF %>%
        inner_join(topNSents %>%
                       dplyr::select(sentenceId), by = "sentenceId")
    
    stm = xtabs(tf ~ sentenceId + token, TF_DF)
    n_sent <- nrow(stm)
    sentencePairsDf <- as.data.frame(combinations(sort(rownames(stm)), 2, replace = FALSE), stringsAsFactors = F)
    sentencePairsDf[["similVal"]] = lexRankr:::idfCosineSimil(stm)
    names(sentencePairsDf) = c("sent1", "sent2", "similVal")    
    
    
    sentencePairsDf <- sentencePairsDf %>%
        union_all(sentencePairsDf %>%
                      rename(sent22 = sent1,
                             sent1 = sent2) %>%
                      rename(sent2 = sent22))
    
    sentencePairs <- xtabs(similVal ~ sent1 + sent2, sentencePairsDf)
    
    # Reorder data
    sentencePairs <- sentencePairs[order(topNSents[[value_col_str]], decreasing = T), order(topNSents[[value_col_str]], decreasing = T)]
    
    # Check if the sentence is simillar to previous
    sentencePairs = apply(sentencePairs, c(1,2), function(x){
        x = x > min_association
    })
    
    n_sent <- nrow(sentencePairs)
    selected_sentences <- vector("logical", n_sent)
    selected_sentences[1] <- T
    
    for(i in 2:n_sent){
        similar_sentences <- sentencePairs[i, selected_sentences]
        selected_sentences[i] <- !any(similar_sentences)
    }
    
    selected_sentences <- rownames(sentencePairs)[selected_sentences]
    
    num_selected_sentences <- min(max(ceiling(nrow(topNSents) * 0.05), 3), 10) # Minimum 3 sentences and maximum 10
    
    topNSents <- topNSents %>%
        filter(sentenceId %in% selected_sentences) %>%
        arrange(desc(!! value_col)) %>%
        .[1:num_selected_sentences, ]
    
    rm(sentencePairs, sentencePairsDf, selected_sentences, TF_DF, n_sent, num_selected_sentences, value_col, value_col_str)
    return(sentences = topNSents)
}

Topics_Summariser <- function(list_topics, unnested_articles, articles_sentences_text, section_id, word_col, sentence_id, general_stats, filtered_similarity = F, min_key_freq = 0.8,
                              value_col, min_association = 0.5, sites){
    
    #' This is a function selecs that summarise topics
    #' @param list_topics List of topics with words
    #' @param unnested_articles Data Frame of unnested articles, with id of paragraph and sentences
    #' @param articles_sentences_text Data Frame of original text splited into sentences
    #' @param section_id Name of column with section id
    #' @param word_col Name of column with words in unnested DF
    #' @param sentence_id Name of columne with sentences id
    #' @param general_stats Data Frame with general statistics
    #' @param filtered_similarity Boolean value, for TRUE we also ??? those articles which included topic words
    #' @param min_key_freq Minimum relative frequency of key words - this eliminates irrelevant articles
    #' @param value_col Name of column which we use to choose the best sentences to summarise - after LexRank
    #' @param min_association Minimum association ???
    #' @param sites Data Frame with codes and names of sites
    #' @return DF of selected sentences
    #' @export
    
    # NSE
    sentence_id <- enquo(sentence_id)
    word_col <- enquo(word_col)
    section_id <- enquo(section_id)
    value_col <- enquo(value_col) # Should be value_log_log
    
    
    for(name in names(list_topics)){
        # Select topic's words
        topic_words <- list_topics[[name]][["word"]]
        
        # Return sentences
        res_extr <- Sentences_Extractor(topic_words, unnested_articles, articles_sentences_text, 
                                        !! section_id, !! word_col, !! sentence_id, general_stats, 
                                        filtered_similarity, min_key_freq)
        extracted_sentences <- res_extr[[1]]
        TF_DF <- res_extr[[2]]
        
        # Summarise topic
        sentences <- Sentence_Selector(extracted_sentences, !! value_col, TF_DF, min_association)
        
        # Add name of site
        sentences <- sentences %>%
            left_join(articles_sentences_text %>%
                          dplyr::select(site, sentence_id, url), by = c("sentenceId" = "sentence_id")) %>%
            left_join(sites, by = "site") %>%
            dplyr::select(-site)
        
        # Add vector of sentences
        list_topics[[name]][["sentences"]] <- sentences[["text"]]
        list_topics[[name]][["sentences_ids"]] <- sentences[["sentenceId"]]
        list_topics[[name]][["site_name"]] <- sentences[["site_name"]]
        list_topics[[name]][["url"]] <- sentences[["url"]]
        
       gc(reset = T)
    }
    
    rm(res_extr, extracted_sentences, TF_DF, sentences)
    return(list_topics)
}


Plot_all_words_correlation <- function(dlPairs, scale_font = c(8, 4), class_num = 6, min_association = 0.4, lambda_daily_DF){
    set.seed(1234)
    require(classInt)
    
    dlPairs_DF <- as.data.frame.matrix(dlPairs) %>%
        mutate(col_1 = rownames(.)) %>%
        tidyr::gather(key = "col_2", value = "simil", -col_1) %>%
        mutate(name = col_1)
    
    values <- lambda_daily_DF[["lambda"]]
    
    # Znajdz breaki
    brks <- classIntervals(values, n = class_num, style = "jenks") %>% .$brks
    # Znajdz kolejnosc w skali
    intervals <- findInterval(values, brks, all.inside = T)
    # Przypsiz kolory
    colors <- brewer.pal(9, "YlGnBu")[(10-class_num):9]
    ordered.colors <- colors[intervals]
    lambda_daily_DF[["color"]] <- ordered.colors
    lambda_daily_DF[["color_num"]] <- as.character(intervals)
    
    # Rozmiar na wykresie
    # freq <- lambda_daily_DF[["lambda"]]
    freq <- lambda_daily_DF[["lambda_log"]]
    normedFreq <-  (freq - min(freq)) / (max(freq) - min(freq))
    size <- (scale_font[1] - scale_font[2]) * normedFreq + scale_font[2]
    lambda_daily_DF[["size_plot"]] <- size
    
    # Plot
    plot <- dlPairs_DF %>%
        filter(simil > min_association) %>%
        mutate(simil = simil - 0.001) %>%
        igraph::graph_from_data_frame(vertices = lambda_daily_DF %>%
                                          inner_join(dlPairs_DF %>%
                                                         filter(simil > min_association) %>%
                                                         distinct(name), by = "name")) %>%
        ggraph::ggraph(layout = "fr") +
        ggraph::geom_edge_link(aes(edge_width = simil*1.5), edge_colour = "cyan4", edge_alpha = 0.2,
                               check_overlap = T, show.legend = F) +
        ggraph::geom_node_point(aes(size = size_plot), color = "lightblue", show.legend = F) +
        ggraph::geom_node_text(aes(label = name, size = size_plot, color = color_num), repel = TRUE,
                               point.padding = unit(0.2, "lines"), check_overlap = T, show.legend = F) +
        scale_size(range = c(2, 5))+
        scale_color_manual(values = colors)+
        theme_void()
    
    rm(dlPairs_DF, dlPairs, lambda_daily_DF, values, brks, intervals, colors, ordered.colors, freq, normedFreq, size)
    return(plot)
}

Plot_topic_correlation <- function(topic_words, dlPairs, scale_font = c(8, 4), class_num = 6, min_association = 0.4, lambda_daily_DF){
    set.seed(1234)
    library(classInt)
    
    dlPairs_DF <- as.data.frame.matrix(dlPairs) %>%
        mutate(col_1 = rownames(.)) %>%
        tidyr::gather(key = "col_2", value = "simil", -col_1) %>%
        mutate(name = col_1)
    
    values <- lambda_daily_DF[["lambda"]]
    
    # Znajdz breaki
    brks <- classIntervals(values, n = class_num, style = "jenks") %>% .$brks
    # Znajdz kolejnosc w skali
    intervals <- findInterval(values, brks, all.inside = T)
    # Przypsiz kolory
    colors <- brewer.pal(9, "YlGnBu")[(10-class_num):9]
    ordered.colors <- colors[intervals]
    lambda_daily_DF[["color"]] <- ordered.colors
    lambda_daily_DF[["color_num"]] <- as.character(intervals)
    
    # Rozmiar na wykresie
    freq <- lambda_daily_DF[["lambda_log"]]
    normedFreq <-  (freq - min(freq)) / (max(freq) - min(freq))
    size <- (scale_font[1] - scale_font[2]) * normedFreq + scale_font[2]
    lambda_daily_DF[["size_plot"]] <- size
    
    # Plot
    plot <- dlPairs_DF %>%
        filter(col_1 %in% topic_words & col_2 %in% topic_words) %>%
        filter(simil > min_association) 
    
    if(nrow(plot) == 0){
        plot <- dlPairs_DF %>%
            filter(col_1 %in% topic_words & col_2 %in% topic_words) %>%
            mutate(simil = NA) %>%
            igraph::graph_from_data_frame(vertices = lambda_daily_DF %>%
                                              filter(name %in% topic_words))
    } else {
        plot <- plot %>%
            mutate(simil = simil - 0.001) %>%
            igraph::graph_from_data_frame(vertices = lambda_daily_DF %>%
                                          filter(name %in% topic_words))
    }
    
    plot <- plot %>%
        ggraph::ggraph(layout = "kk")+
        ggraph::geom_edge_link(aes(edge_width = simil*1.5), edge_colour = "cyan4", edge_alpha = 0.2,
                               check_overlap = T, show.legend = F)+
        ggraph::geom_node_point(aes(size = size_plot), color = "lightblue", show.legend = F)+
        ggraph::geom_node_text(aes(label = name, size = size_plot, color = color_num), repel = TRUE,
                               point.padding = unit(0.2, "lines"), check_overlap = T, show.legend = F)+
        scale_size(range = rev(scale_font))+
        scale_color_manual(values = colors)+
        theme_void()
    
    rm(dlPairs_DF, dlPairs, lambda_daily_DF, values, brks, intervals, colors, ordered.colors, freq, normedFreq, size)
    return(plot)
}

Clear_Sentences <- function(list_topics){
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
        summarise(counts_1 = n()) %>%
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

