import numpy as np
import scipy.sparse as sp
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity as cos
from itertools import chain
import math


def space_tokenizer(text):
    return text.split(" ")


def create_embeddings(articles, lambda_statistics, pipeline=False):
    """
    This function creates embeddings for each word in data frame.
    
    :param articles data frame with pasted lematizated text
    :param lambda_statistics data frame with lambda statistics and selected words that are 
    important in a particular 'day'
    :param pipeline bool for True function outputs only files needed in next steps of pipeline
    """

    selected_words = list(lambda_statistics["word"])
    corpus = list(articles["text"])

    vectorizer = CountVectorizer(lowercase=False, tokenizer=space_tokenizer,
                                 min_df=1, encoding="UTF-8")
    tfidf_matrix = vectorizer.fit_transform(corpus)
    # tfidf_matrix_normalized = tfidf_matrix
    tfidf_matrix = tfidf_matrix.transpose()
    # tfidf_matrix_normalized = tfidf_matrix_normalized.transpose()

    svd = TruncatedSVD(n_components=256, n_iter=15, random_state=42)
    embeddings = svd.fit_transform(tfidf_matrix)
    selected_words.sort()
    selected_words_indices = []
    for word in selected_words:
        try:
            selected_words_indices.append(vectorizer.vocabulary_[word])
        except KeyError:
            print(word)

    selected_embeddings = embeddings[selected_words_indices, :]
    similarity_matrix = cos(selected_embeddings)
    np.fill_diagonal(similarity_matrix, 0)

    distribution_matrix = tfidf_matrix[selected_words_indices, :].todense()

    if pipeline:
        return (similarity_matrix, distribution_matrix,
                selected_words, selected_embeddings,
                embeddings, vectorizer.vocabulary_)
    else:
        return (similarity_matrix, np.transpose(svd.components_),
                distribution_matrix, selected_words, selected_embeddings)


def delete_indices(_list, indices):
    return [i for j, i in enumerate(_list) if j not in indices]


class Clustering:
    def __init__(self, tokens, min_association=0.25, do_silhouette=False, singularity_penalty=-0.1):
        self.tokens = tokens
        self.tokens_len = len(tokens)
        self.topics = [[i] for i in range(len(tokens))]
        self.best_topics = [[i] for i in range(len(tokens))]
        self.do_silhouette = do_silhouette
        self.singularity_penalty = singularity_penalty
        self.max_silhouette = singularity_penalty
        self.silhouette_history = [singularity_penalty]
        self.max_simil_history = []
        self.inner_distance = np.zeros((len(tokens),))
        self.outer_distance = np.zeros((len(tokens),))
        if do_silhouette:
            self.min_association = 0.01
        else:
            self.min_association = min_association

    def find_tokens(self):
        self.topics = [[self.tokens[token_index] for token_index in topic] for topic in self.topics]

    def silhouette(self, tokens_to_topics_distance, tokens_distribution, distribution_matrix):
        """
        Calculation of silhouette value in a particular iteration.
        Function updates inner similarity only for the new topic.

        :param tokens_to_topics_distance numpy array of distance between tokens and topics
        :param tokens_distribution numpy array of tokens embeddings
        :param distribution_matrix numpy array of topics embeddings
        """
        # Outer similarity
        for topic_index, topic in enumerate(self.topics):
            tokens_to_topics = np.copy(tokens_to_topics_distance)
            tokens_to_topics = np.delete(tokens_to_topics, topic_index, axis=1)
            for token_index in topic:
                self.outer_distance[token_index] = np.amin(tokens_to_topics[token_index, :], keepdims=False)

        # Inner similarity - updated is only topic created in last iteration
        for token_index in self.topics[0]:
            token_distribution = np.copy(tokens_distribution[token_index, :])
            reference_distribution = np.subtract(np.copy(distribution_matrix[0, :]), token_distribution)
            # reference_distribution = np.copy(distribution_matrix[0, :])
            token_distribution = np.reshape(token_distribution, (-1, token_distribution.shape[0]))
            reference_distribution = np.reshape(reference_distribution, (-1, reference_distribution.shape[0]))
            self.inner_distance[token_index] = np.subtract(1.0, cos(reference_distribution, token_distribution))

        # Calculate silhouette values and their mean
        silhouette_values = np.zeros((len(self.outer_distance),))
        selected_indices = np.where((self.inner_distance != 0.0) & (self.outer_distance != 0.0))
        selected_inner = self.inner_distance[selected_indices]
        selected_outer = self.outer_distance[selected_indices]
        silhouette_values[selected_indices] = np.subtract(selected_outer, selected_inner)
        maximum_values = np.maximum(selected_outer, selected_inner)
        silhouette_values[selected_indices] = np.divide(silhouette_values[selected_indices], maximum_values)

        # Calculate penalty - number of single tokens * penalty_value (0.1)
        single_tokens = self.tokens_len - (self.tokens_len - len(self.topics))
        penalty = (single_tokens * self.singularity_penalty) / self.tokens_len
        mean_silhouette = np.mean(silhouette_values) + penalty

        # Update best topic and history
        if mean_silhouette > self.max_silhouette:
            self.max_silhouette = mean_silhouette
            self.best_topics = self.topics

        self.silhouette_history.append(mean_silhouette)

    def find_topics(self, similarity_matrix, distribution_matrix):
        """Main function that hierarchically finds topics"""
        # Prealocate vectors for assessing grouping quality
        if self.do_silhouette:
            # simil_shape = similarity_matrix.shape
            word_to_topics_distance = np.subtract(1.0, np.copy(similarity_matrix))
            tokens_distribution = np.copy(distribution_matrix)

        number_of_topics = len(self.topics)

        max_index = np.unravel_index(np.argmax(similarity_matrix, axis=None), similarity_matrix.shape)
        max_simil = similarity_matrix[max_index]

        while (len(self.topics) > 2) and (max_simil >= self.min_association):
            self.max_simil_history.append(max_simil)

            # Update information about topics
            new_topics = self.topics[max_index[0]] + self.topics[max_index[1]]
            self.topics = delete_indices(self.topics, max_index)
            self.topics = [new_topics] + self.topics
            number_of_topics -= 1

            # Update distribution of new topic
            new_dist = np.copy(distribution_matrix[max_index, :])
            new_dist = np.sum(new_dist, axis=0, keepdims=True)

            # Delete rows or cols associated with merged topics
            distribution_matrix = np.delete(distribution_matrix, max_index, axis=0)
            similarity_matrix = np.delete(similarity_matrix, max_index, axis=0)
            similarity_matrix = np.delete(similarity_matrix, max_index, axis=1)

            # New similarity
            new_simil = cos(new_dist, distribution_matrix)

            # Update matrices
            distribution_matrix = np.concatenate([new_dist, distribution_matrix])
            new_similarity_matrix = np.zeros((number_of_topics, number_of_topics))
            new_similarity_matrix[1:, 1:] = similarity_matrix
            new_similarity_matrix[0, 1:] = new_simil
            new_similarity_matrix[1:, 0] = new_simil
            similarity_matrix = new_similarity_matrix

            # Silhouette algorithm
            if self.do_silhouette and (len(self.topics[0]) > 1):
                word_to_topics_distance = np.delete(word_to_topics_distance, max_index, axis=1)
                new_word_to_topic_distance = np.subtract(1.0, cos(tokens_distribution, new_dist))
                word_to_topics_distance = np.concatenate([new_word_to_topic_distance,
                                                          word_to_topics_distance], axis=1)
                self.silhouette(word_to_topics_distance, tokens_distribution, distribution_matrix)

            # Find new max
            max_index = np.unravel_index(np.argmax(similarity_matrix, axis=None), similarity_matrix.shape)
            max_simil = similarity_matrix[max_index]

        if self.do_silhouette:
            self.topics = self.best_topics


def page_rank(matrix, damping_factor=0.85):
    """ Function calculates PageRank for given markov matrix

    :param matrix numpy 2D square array of a markov chain
    :param damping_factor double defining percentage of original matrix in 
    PageRank graph
    """

    n_rows, n_cols = matrix.shape
    assert n_rows == n_cols

    # Row-normalise input matrix
    row_sums = np.sum(matrix, axis=1)
    non_zero = row_sums != 0
    matrix[non_zero, :] = np.divide(matrix[non_zero, :], row_sums[non_zero, None])
    matrix = np.multiply(matrix, damping_factor)
    # Subtract from identity transposition of normalised matrix
    identity = np.eye(n_rows)
    matrix = np.subtract(identity, matrix.T)

    # Create damping vector that asserts convergence of markov chain
    damping_vector = np.ones((n_rows, 1))
    scaling_factor = (1.0 - damping_factor) / n_rows
    damping_vector = np.multiply(damping_vector, scaling_factor)

    x = np.linalg.solve(matrix, damping_vector)

    return x  # / x.sum()


class TopicsSummariser:
    def __init__(self, topics, lemmatized_sentences, lemmatized_articles,
                 sentences_text, log_lambda_statistics_df, embeddings,
                 embeddings_vocab, min_key_freq=0.8, max_sentence_simil=0.5,
                 section_id="section_id", word_col="word", use_sparse=True):
        self.topics = topics
        self.lemmatized_sentences = lemmatized_sentences
        self.lemmatized_articles = lemmatized_articles
        self.tf_matrix_sentences = sp.csr_matrix((0, 0))
        self.tf_matrix_articles = sp.csr_matrix((0, 0))
        self.sentences_text = sentences_text
        self.min_key_freq = min_key_freq
        self.max_sentence_simil = max_sentence_simil
        self.section_id = section_id
        self.word_col = word_col
        self.log_lambda_statistics_df = log_lambda_statistics_df
        self.log_lambda_statistics = np.array((0,))
        self.embeddings = embeddings
        self.embeddings_vocab = embeddings_vocab
        self.selected_sentences = []
        self.sentences_in_articles = []
        self.use_sparse = use_sparse
        self.vectorizer_sentences = CountVectorizer(lowercase=False,
                                                    tokenizer=space_tokenizer,
                                                    min_df=1)
        self.vectorizer_articles = CountVectorizer(lowercase=False,
                                                   tokenizer=space_tokenizer,
                                                   min_df=1)

    def vectorize_sentences(self):
        """Vectorization of sentences"""
        corpus = list(self.lemmatized_sentences["text"])
        self.tf_matrix_sentences = self.vectorizer_sentences.fit_transform(corpus)

    def vectorize_articles(self):
        """Vectorization of articles"""
        corpus = list(self.lemmatized_articles["text"])
        self.tf_matrix_articles = self.vectorizer_articles.fit_transform(corpus)

    def group_sentences_into_articles(self):
        """Creates list of lists with ids of sentences in particular article"""
        self.sentences_in_articles = []
        articles = list(self.lemmatized_sentences["id"])
        sentences_list = [0]
        previous_id = articles[0]
        for index, article_id in enumerate(articles[1:]):
            if previous_id == article_id:
                sentences_list.append(index + 1)
            else:
                self.sentences_in_articles.append(sentences_list)
                sentences_list = [index + 1]
            previous_id = article_id
        self.sentences_in_articles.append(sentences_list)

    def set_lambda_order(self):
        """Selects only statistics for tokens present in vocabulary and adjusts thier order"""
        tokens = list(self.log_lambda_statistics_df[self.word_col])
        values = list(self.log_lambda_statistics_df["lambda"])
        statistics = {token: values[index] for index, token in enumerate(tokens)}
        vocab = self.vectorizer_sentences.vocabulary_
        inv_vocab = {v: k for k, v in vocab.items()}
        ordered_vocabulary = [inv_vocab[i] for i in range(len(inv_vocab))]
        ordered_statistics = [statistics[token] for token in ordered_vocabulary]
        self.log_lambda_statistics = np.array(ordered_statistics)

    def set_embeddings_order(self):
        """Selects only embeddings for tokens present in vocabulary and adjusts thier order"""
        vocab = self.vectorizer_sentences.vocabulary_
        inv_vocab = {v: k for k, v in vocab.items()}
        ordered_embeddings = [self.embeddings_vocab[inv_vocab[i]] for i in range(len(inv_vocab))]
        self.embeddings = self.embeddings[ordered_embeddings, :].copy()

    @staticmethod
    def scale_ranking(ranking, topic_words_sums, all_words_sums):
        """
        Scale PageRank ranking by log(topic_words)/log(all_words).
        This modification upscales sentences with more topic words and
        downscales long sentences.

        :param ranking numpy array of ranking values
        :param topic_words_sums numpy array with sums of topic words' occurences in sentences
        :param all_words_sums numpy array with number of tokens in sentences
        """
        topic_words_sums = np.array(topic_words_sums)
        all_words_sums = np.array(all_words_sums)

        pos = topic_words_sums > 0
        topic_tokens_count_log = np.zeros(topic_words_sums.shape)
        topic_tokens_count_log[pos] = np.log(topic_words_sums[pos])
        all_tokens_count_log = np.log(all_words_sums)
        all_pos = all_tokens_count_log > 0
        scaling_factors = np.zeros(all_tokens_count_log.shape)
        scaling_factors[all_pos] = np.divide(topic_tokens_count_log[all_pos],
                                             all_tokens_count_log[all_pos])

        ranking = np.multiply(ranking, scaling_factors).flatten()
        return ranking

    def select_non_duplicated_sentences(self, simil_matrix, ranking, topic_sentences, order):
        """
        This method selects sentences that are not too much similar to each other.
        This is done recursively.

        :param simil_matrix numpy array of similarity between sentences, calculated with the use TF matrix
        :param ranking numpy array with value of the ranking for particular sentences
        :param topic_sentences list of ids of sentences that are from accepted articles
        :param order numpy array with the ordered topic sentences ids - ids are not from the whole TF matrix
        as in topic_sentences. This are ids in a matrix that contains only topic sentences.
        """
        np.fill_diagonal(simil_matrix, 0.0)
        simil_matrix = simil_matrix > self.max_sentence_simil

        sentence_number = len(ranking)
        selected_sentences = np.zeros((sentence_number,), dtype=bool)
        selected_sentences[0] = True

        for i in range(1, sentence_number):
            simil_to_i = simil_matrix[i, :]
            similar_sentences = simil_to_i[selected_sentences]
            selected_sentences[i] = ~np.any(similar_sentences)

        selected_sentences_ids = list(order[selected_sentences])
        zero_rankings = np.sum(ranking == 0.0)
        non_zero_sentences = sentence_number - zero_rankings
        num_selected = min(max(math.ceil(non_zero_sentences * 0.05), 3), 10)
        selected_sentences_ids = selected_sentences_ids[:num_selected]
        selected_sentences_ids = [sent_id for index, sent_id in enumerate(topic_sentences)
                                  if index in selected_sentences_ids]
        return selected_sentences_ids

    def sentences_selection(self, topic_words):
        """
        Returns sentences ids that summarise the topic.
        Ranking is done with the use of PageRank

        :topic_words list of words in a topic
        """

        # Check which articles contains topic words
        topic_words_indices = [self.vectorizer_articles.vocabulary_[word] for word in topic_words]
        is_topic_article = self.tf_matrix_articles[:, topic_words_indices].sum(axis=1) > 0
        topic_articles = np.where(is_topic_article)[0]

        # Delete articles that have too few key words
        ix_grid = np.ix_(topic_articles, topic_words_indices)
        topic_words_sums = self.tf_matrix_articles[ix_grid].sum(axis=1)
        all_words_sums = self.tf_matrix_articles[topic_articles, :].sum(axis=1)
        topic_words_freq = np.divide(topic_words_sums, all_words_sums)
        all_articles_mean = np.divide(np.sum(topic_words_sums),
                                      np.sum(all_words_sums))
        selected_articles = np.where(topic_words_freq > all_articles_mean)[0]
        topic_articles = [article_index for index, article_index in
                          enumerate(topic_articles)
                          if index in selected_articles]

        # Select topic sentences
        topic_sentences = [article_sentence for article_index, article_sentence in
                           enumerate(self.sentences_in_articles)
                           if article_index in topic_articles]
        topic_sentences = list(chain.from_iterable(topic_sentences))
        topic_sentences_tf_matrix = self.tf_matrix_sentences[topic_sentences, :].copy()

        # Calculate cosine simillarity between words and topic
        topic_embedding = np.sum(self.embeddings[topic_words_indices, :], axis=0, keepdims=True)
        words_topic_simil = cos(self.embeddings, topic_embedding).flatten()

        # Tokens importance
        importance = np.multiply(words_topic_simil, self.log_lambda_statistics)

        # Multiplication of sentence TF matrix by log_lambda_statistic and cosine 
        # simillarity between words and topic
        if self.use_sparse:
            importance = sp.csr_matrix(importance)
            topic_sentences_tf_matrix = topic_sentences_tf_matrix.multiply(importance)
        else:
            topic_sentences_tf_matrix = topic_sentences_tf_matrix.toarray()
            topic_sentences_tf_matrix = np.multiply(topic_sentences_tf_matrix,
                                                    importance)

        # PageRank
        simil_matrix = cos(topic_sentences_tf_matrix)
        simil_matrix = np.array(simil_matrix)
        np.fill_diagonal(simil_matrix, 0.0)
        ranking = page_rank(simil_matrix)

        # Scale ranking
        topic_sentences_tf_matrix = self.tf_matrix_sentences[topic_sentences, :].copy()
        topic_words_sums = topic_sentences_tf_matrix[:, topic_words_indices].sum(axis=1)
        all_words_sums = topic_sentences_tf_matrix.sum(axis=1)
        ranking = self.scale_ranking(ranking, topic_words_sums, all_words_sums)

        # Order TF matrix by scaled ranking
        order = np.argsort(ranking)[::-1]  # .flatten()
        topic_sentences_tf_matrix = topic_sentences_tf_matrix[order, :].toarray()

        # Select sentences with non duplicated meaning
        simil_matrix = cos(topic_sentences_tf_matrix)
        selected_sentences_ids = self.select_non_duplicated_sentences(simil_matrix, ranking,
                                                                      topic_sentences, order)

        return selected_sentences_ids


def cosine_clustering(similarity_matrix, distribution_matrix,
                      selected_tokens, min_association=0.25, do_silhouette=False,
                      singularity_penalty=-0.1):
    """
    Function that clusters tokens to topics
    
    :param similarity_matrix numpy 2D array with cosine similarity between tokens
    :param distribution_matrix numpy 2D array with embeddings of selected tokens
    :param selected_tokens list of strings - tokens selected for clustering in the same order
    as embeddings
    :param min_association float used to manually set end condiction in clustering.
    It stands for minimal simillarity between topics to merge them.
    When do_silhouette is True then this parameter is set to 0.01.
    :param do_silhouette bool for True Silhouette algorithm automatically optimise clustering.
    :param singularity_penalty float penalty value in silhouette algorithm for every topic that has only one token.
    """

    clustering = Clustering(tokens=selected_tokens, min_association=min_association,
                            do_silhouette=do_silhouette, singularity_penalty=singularity_penalty)
    clustering.find_topics(similarity_matrix=similarity_matrix, distribution_matrix=distribution_matrix)
    clustering.find_tokens()
    for i, words in enumerate(clustering.topics):
        clustering.topics[i] = {"word": words}

    return (clustering.topics,
            np.asarray(clustering.max_simil_history),
            np.asarray(clustering.silhouette_history))


def summarise_topics(topics, lemmatized_sentences, lemmatized_articles,
                     sentences_text, log_lambda_statistics_df, embeddings,
                     embeddings_vocab, min_key_freq=0.8, max_sentence_simil=0.5,
                     section_id="section_id", word_col="word",
                     use_sparse=True):
    """
    Function that uses TopicsSummariser class to summarise documents.

    :param topics list of topics identified by cosine_clustering
    :param lemmatized_sentences data frame with lemmatized sentences
    :param lemmatized_articles data frame with lemmatized articles
    :param sentences_text data frame with original sentences
    :param log_lambda_statistics_df data frame with modified Dunning measure - 0 when p1 < p0 and
    log(d + 1) where d is Dunning measure
    :param embeddings numpy array of words embeddings - output of create_embeddings
    :param embeddings_vocab dict with tokens and their indices in embeddings
    :param min_key_freq float with minimal relative frequency of topic words that let article to be
    included in the analysis
    :param max_sentence_simil float maximal similarity between sentences returned as summary of the topic
    :param section_id string name of data frame column with section id
    :param word_col string name of data frame column with tokens
    :param use_sparse bool value - for True cosine similarity between sentences is calculated with the use
    of scipy sparse matrix
    """

    summariser = TopicsSummariser(topics, lemmatized_sentences, lemmatized_articles,
                                  sentences_text, log_lambda_statistics_df, embeddings,
                                  embeddings_vocab, min_key_freq, max_sentence_simil,
                                  section_id, word_col, use_sparse)
    summariser.vectorize_sentences()
    summariser.vectorize_articles()
    summariser.group_sentences_into_articles()
    summariser.set_lambda_order()
    summariser.set_embeddings_order()
    for index, topic in enumerate(topics):
        selected_ids = summariser.sentences_selection(topic_words=topic["word"])
        selected_sentences = summariser.sentences_text.iloc[selected_ids, :]
        topics[index]["sentences"] = list(selected_sentences["text"])
        topics[index]["sentences_ids"] = list(selected_sentences["sentence_id"])
        topics[index]["url"] = list(selected_sentences["url"])
        topics[index]["site_name"] = list(selected_sentences["site_name"])

    return topics


def cluster_and_summarise(sections_and_articles, filtered_lambda_statistics,
                          # Clustering
                          min_association, do_silhouette, singularity_penalty,
                          # Summarization
                          lemmatized_sentences, lemmatized_articles,
                          sentences_text, log_lambda_statistics_df,
                          min_key_freq=0.8, max_sentence_simil=0.5,
                          section_id="section_id", word_col="word",
                          use_sparse=True):
    """
    Function merging full pipeline. At first it creates tokens' embeddings. Then it cluster them to topics.
    And at the end, it summarise topics.

    :param sections_and_articles data frame with pasted lematizated text of both paragraphs and articles
    :param filtered_lambda_statistics data frame with lambda statistics for selected words that are
    important in a particular 'day'
    :param min_association float used to manually set end condiction in clustering.
    It stands for minimal simillarity between topics to merge them.
    When do_silhouette is True then this parameter is set to 0.01.
    :param do_silhouette bool for True Silhouette algorithm automatically optimise clustering.
    :param singularity_penalty float penalty value in silhouette algorithm for every topic that has only one token.
    :param lemmatized_sentences data frame with lemmatized sentences
    :param lemmatized_articles data frame with lemmatized articles
    :param sentences_text data frame with original sentences
    :param log_lambda_statistics_df data frame with modified Dunning measure - 0 when p1 < p0 and
    log(d + 1) where d is Dunning measure
    :param min_key_freq float with minimal relative frequency of topic words that let article to be
    included in the analysis
    :param max_sentence_simil float maximal similarity between sentences returned as summary of the topic
    :param section_id string name of data frame column with section id
    :param word_col string name of data frame column with tokens
    :param use_sparse bool value - for True cosine similarity between sentences is calculated with the use
    of scipy sparse matrix
    """

    # Create embeddings
    outputs = create_embeddings(sections_and_articles,
                                filtered_lambda_statistics,
                                pipeline=True)

    similarity_matrix = outputs[0]
    # distribution_matrix = outputs[1] 
    selected_tokens = outputs[2]
    selected_embeddings = outputs[3]
    embeddings = outputs[4]
    embeddings_vocab = outputs[5]

    # Cluster tokens
    outputs = cosine_clustering(similarity_matrix, selected_embeddings,
                                selected_tokens, min_association, do_silhouette, singularity_penalty)
    topics = outputs[0]
    max_simil_history = outputs[1]
    silhouette_history = outputs[2]

    # Summarise topics
    topics = summarise_topics(topics, lemmatized_sentences, lemmatized_articles,
                              sentences_text, log_lambda_statistics_df, embeddings,
                              embeddings_vocab, min_key_freq, max_sentence_simil,
                              section_id, word_col, use_sparse)

    return topics, similarity_matrix, selected_tokens, silhouette_history, max_simil_history
