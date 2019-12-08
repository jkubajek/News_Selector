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
For this purpuse *TruncatedSVD* from scipy is used.
Afterwards, tokens are hierarchically clustered with the use of 
*silhouette* algorithm that automatically find optimal number of clusters. 
*Silhouette* maximize inner-cluster similarity and outer-cluster dissimilarity.
After clustering, determined topics are summarized with the use of LexRank,
which is an adaptation of PageRank to text data. Ranking, returned for given
sentence, is upscaled when there are more topic tokens and downscaled when
sentence is long. At the end, algorithm selects non-duplicated sentences,
by recursively checking similarity to the sentences already included
in summary.

The output from Python returns to R, where topics are arranged according
to the highest token importance in a topic.

At the end, **knitting_button.R** render Rmarkdown reports in *html* and *docx*.
Which are send to the recipients through gmail. Rmarkdown templets are placed
in **reports** folder.