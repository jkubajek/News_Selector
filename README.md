# News Selector
This repo contains model for clustering and summarization of topics present
in a particular day on selected Polish news sites.

To run the pipeline, you need to change directiories in some files: **news_selector_daily.R**, **daily_report_html.Rmd**, **daily_report_word.Rmd**, **knitting_button.R** and **daily_runner.sh**. It's also necessary to install requirements for Python and packages that are loaded in R scripts. The easiest way to install R libraries is to open project in R Studio - it will automatically check what need to be installed (manually need to be installed only **igraph** library). The detailed description of the model can be found in *scripts* folder. 

Presentation describing the whole pipeline can be found in [docs](https://jkubajek.github.io/News_Selector/News_Selector.pdf).

On [GitHub Pages](https://jkubajek.github.io/News_Selector/index.html) there is the page with the summary of the 2019 in Polish media, created with the use of this model.

An an example of daily report for 9th December 2019 is present on [GitHub Pages](https://jkubajek.github.io/News_Selector/daily_report_2019_12_09).

In addition, there is the [report](https://jkubajek.github.io/News_Selector/BBC_2019_12_01-09) for BBC articles published between 1st and 9th December 2019.
