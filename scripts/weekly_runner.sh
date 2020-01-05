#!/bin/bash

# Path should be changed
#PATH='/home/jkubajek/anaconda3/bin:/home/jkubajek/anaconda3/condabin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin'; export PATH

# When running on Linux change paths
#cd D:/Osobiste/GitHub/News_Selector

scrapy runspider scrapy/scraper.py -a domain="gazeta" -a ranges_start="1,1,1,1" -a ranges_end="12,8,9,5" -t json --nolog -o - > data/weekly_articles/page_Gazeta_weekly.json
scrapy runspider scrapy/scraper.py -a domain="rmf" -a ranges_start="1" -a ranges_end="35" -t json --nolog -o - > data/weekly_articles/page_RMF_weekly.json
scrapy runspider scrapy/scraper.py -a domain="tvn24" -a ranges_start="1" -a ranges_end="42" -t json --nolog -o - > data/weekly_articles/page_TVN24_weekly.json
scrapy runspider scrapy/scraper.py -a domain="interia" -a ranges_start="1,1,1" -a ranges_end="7,22,16" -t json --nolog -o - > data/weekly_articles/page_Interia_weekly.json
#scrapy runspider scrapy/scraper.py -a domain="dziennik" -a ranges_start="1,1,1,1,1" -a ranges_end="13,10,10,2,6" -t json --nolog -o - > data/weekly_articles/page_Dziennik_weekly.json
scrapy runspider scrapy/scraper.py -a domain="radio_zet" -a ranges_start="1,1,1" -a ranges_end="9,3,2" -t json --nolog -o - > data/weekly_articles/page_RadioZET_weekly.json
scrapy runspider scrapy/scraper.py -a domain="pap" -a ranges_start="0,0,0" -a ranges_end="28,14,7" -t json --nolog -o - > data/weekly_articles/page_PAP_weekly.json
scrapy runspider scrapy/scraper.py -a domain="tvn24bis" -a ranges_start="1" -a ranges_end="12" -t json --nolog -o - > data/weekly_articles/page_TVN24bis_weekly.json
scrapy runspider scrapy/scraper.py -a domain="tvp_info" -a ranges_start="1, 1" -a ranges_end="22,22" -t json --nolog -o - > data/weekly_articles/page_TVP_INFO_weekly.json
scrapy runspider scrapy/scraper.py -a domain="polsat_news" -a ranges_start="1, 1,1,1" -a ranges_end="24,18,12,12" -t json --nolog -o - > data/weekly_articles/page_Polsat_News_weekly.json
scrapy runspider scrapy/scraper.py -a domain="polskie_radio" -a ranges_start="1, 1" -a ranges_end="30,30" -t json --nolog -o - > data/weekly_articles/page_Polskie_Radio_weekly.json
scrapy runspider scrapy/scraper.py -a domain="wprost" -a ranges_start="1, 1, 1" -a ranges_end="18,25,25" -t json --nolog -o - > data/weekly_articles/page_Wprost_weekly.json



#Rscript scripts/news_selector_weekly.R
#Rscript scripts/knitting_button.R