#!/bin/bash

# Path should be changed
#PATH='/home/jkubajek/anaconda3/bin:/home/jkubajek/anaconda3/condabin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin'; export PATH

# When running on Linux path
# cd D:/Osobiste/GitHub/News_Selector

scrapy runspider scrapy/scraper.py -a domain="gazeta" -a ranges_start="1,1,1,1" -a ranges_end="6,4,6,3" -t json --nolog -o - > data/daily_articles/page_Gazeta_daily.json
scrapy runspider scrapy/scraper.py -a domain="rmf" -a ranges_start="1" -a ranges_end="16" -t json --nolog -o - > data/daily_articles/page_RMF_daily.json
scrapy runspider scrapy/scraper.py -a domain="tvn24" -a ranges_start="1" -a ranges_end="20" -t json --nolog -o - > data/daily_articles/page_TVN24_daily.json
scrapy runspider scrapy/scraper.py -a domain="interia" -a ranges_start="1,1,1" -a ranges_end="5,11,11" -t json --nolog -o - > data/daily_articles/page_Interia_daily.json
scrapy runspider scrapy/scraper.py -a domain="dziennik" -a ranges_start="1,1,1,1,1" -a ranges_end="7,5,5,3,5" -t json --nolog -o - > data/daily_articles/page_Dziennik_daily.json
scrapy runspider scrapy/scraper.py -a domain="radio_zet" -a ranges_start="1,1,1" -a ranges_end="11,6,6" -t json --nolog -o - > data/daily_articles/page_RadioZET_daily.json
scrapy runspider scrapy/scraper.py -a domain="pap" -a ranges_start="0,0,0" -a ranges_end="16,11,6" -t json --nolog -o - > data/daily_articles/page_PAP_daily.json
scrapy runspider scrapy/scraper.py -a domain="tvn24bis" -a ranges_start="1" -a ranges_end="13" -t json --nolog -o - > data/daily_articles/page_TVN24bis_daily.json
scrapy runspider scrapy/scraper.py -a domain="tvp_info" -a ranges_start="1, 1" -a ranges_end="6,6" -t json --nolog -o - > data/daily_articles/page_TVP_INFO_daily.json
scrapy runspider scrapy/scraper.py -a domain="polsat_news" -a ranges_start="1, 1, 1, 1" -a ranges_end="8,6,5,5" -t json --nolog -o - > data/daily_articles/page_Polsat_News_daily.json
scrapy runspider scrapy/scraper.py -a domain="polskie_radio" -a ranges_start="1, 1" -a ranges_end="7,6" -t json --nolog -o - > data/daily_articles/page_Polskie_Radio_daily.json
scrapy runspider scrapy/scraper.py -a domain="wprost" -a ranges_start="1, 1, 1" -a ranges_end="4,5,5" -t json --nolog -o - > data/daily_articles/page_Wprost_daily.json



#Rscript scripts/news_selector_daily.R
#Rscript scripts/knitting_button.R