# Scripts
The main script that is responsible for the whole pipeline (from scraping to
report generation) is a bash file **daily_runner.sh** - it evokes scraping,
then clustering and summarization and at the end report creation.

**news_selector_daily.R** is a script responsible for dataset modification,
sending data to Python and saving data that is used by **knitting_button.R**.
The **news_selector_daily.R** helps to understand how the model and dataset 
preprocessing work. It also calculates importance of tokens 
(Dunning statistics).

The mathematical core of the model is done in Python, with the use of functions
and classes from **python_functions.py**. At first, tokens embeddings are
created. It is done with the use of *LSA* (Latent Semantic Analysis).
For this purpuse *TruncatedSVD* from scipy is used. Embeddings are multiplied by
the square root of the Dunning Statistics. This is done to give higher
influence in clustering to the most important words.

Afterwards, tokens are hierarchically clustered with the use of 
*silhouette* algorithm that automatically find optimal number of clusters. 
*Silhouette* maximize inner-cluster similarity and outer-cluster dissimilarity.
This algorithm may be provided with *singularity_penalty* argument which 
specifies penalty for topics consisting of only one word. Model usually works
best when it is provided with penalty equal to *-0.1*.

After clustering, determined topics are summarized. In the first step sentences'
TF matrix is converted into sentences embeddings - dot product between TF matrix 
and words' embeddings. Next, the model selects subset of the sentences the most 
similar to the topic. Then it ranks this subset with the use of modified **LexRank**,
which is an adaptation of **PageRank** algorithm to text data. PageRank takes as 
an input the matrix of cosine similarity between sentences' embeddings. 
The output of the LexRank is multiplied by the sentences' similarity to the topic.

Ranking, returned for given sentence, is upscaled when there are more important topic 
tokens. At the end, algorithm selects non-duplicated sentences, 
by recursively checking similarity to the sentences already included in summary.

The output from Python returns to R, where topics are arranged according
to the highest token importance in a topic.

At the end, **knitting_button.R** render Rmarkdown reports in *html* and *docx*.
Which are send to the recipients through gmail. Rmarkdown templets are placed
in **reports** folder.