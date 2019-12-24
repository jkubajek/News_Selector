#!/bin/bash

# Path should be changed
#PATH='/home/jkubajek/anaconda3/bin:/home/jkubajek/anaconda3/condabin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin'; export PATH

# When running on Linux change paths
cd D:\\Osobiste\\GitHub\\News_Selector

scrapy runspider scrapy\\scraper.py -a domain="gazeta" -a ranges_start="1,1,1,1" -a ranges_end="12,8,9,5" -t json --nolog -o - > data\\daily_articles\\page_Gazeta_daily.json
scrapy runspider scrapy\\scraper.py -a domain="rmf" -a ranges_start="1" -a ranges_end="35" -t json --nolog -o - > data\\daily_articles\\page_RMF_daily.json
scrapy runspider scrapy\\scraper.py -a domain="tvn24" -a ranges_start="1" -a ranges_end="42" -t json --nolog -o - > data\\daily_articles\\page_TVN24_daily.json
scrapy runspider scrapy\\scraper.py -a domain="interia" -a ranges_start="1,1,1" -a ranges_end="7,22,16" -t json --nolog -o - > data\\daily_articles\\page_Interia_daily.json
scrapy runspider scrapy\\scraper.py -a domain="dziennik" -a ranges_start="1,1,1,1,1" -a ranges_end="13,10,10,2,6" -t json --nolog -o - > data\\daily_articles\\page_Dziennik_daily.json
scrapy runspider scrapy\\scraper.py -a domain="radio_zet" -a ranges_start="1,1,1" -a ranges_end="9,3,2" -t json --nolog -o - > data\\daily_articles\\page_RadioZET_daily.json
scrapy runspider scrapy\\scraper.py -a domain="pap" -a ranges_start="0,0,0" -a ranges_end="28,14,7" -t json --nolog -o - > data\\daily_articles\\page_PAP_daily.json
scrapy runspider scrapy\\scraper.py -a domain="tvn24bis" -a ranges_start="1" -a ranges_end="12" -t json --nolog -o - > data\\daily_articles\\page_TVN24bis_daily.json

#Rscript scripts\\news_selector_daily.R
#Rscript scripts\\knitting_button.R