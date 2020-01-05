#!/bin/bash

# Path should be changed
#PATH='/home/jkubajek/anaconda3/bin:/home/jkubajek/anaconda3/condabin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin'; export PATH

# When running on Linux change paths
#cd D:/Osobiste/GitHub/News_Selector

scrapy runspider scrapy/scraper.py -a domain="gazeta" -a ranges_start="1,1,1,1" -a ranges_end="550,290,380,165" -t json --nolog -o - > data/annual_articles/page_Gazeta_2019.json
scrapy runspider scrapy/scraper.py -a domain="rmf" -a ranges_start="1" -a ranges_end="1550" -t json --nolog -o - > data/annual_articles/page_RMF_2019.json
scrapy runspider scrapy/scraper.py -a domain="tvn24" -a ranges_start="1" -a ranges_end="2100" -t json --nolog -o - > data/annual_articles/page_TVN24_2019.json
scrapy runspider scrapy/scraper.py -a domain="interia" -a ranges_start="1,1,1" -a ranges_end="200,900,550" -t json --nolog -o - > data/annual_articles/page_Interia_2019.json
#scrapy runspider scrapy/scraper.py -a domain="dziennik" -a ranges_start="1,1,1,1,1" -a ranges_end="40,35,35,8,22" -t json --nolog -o - > data/annual_articles/page_Dziennik_annual.json
scrapy runspider scrapy/scraper.py -a domain="radio_zet" -a ranges_start="1,1,1" -a ranges_end="300,90,80" -t json --nolog -o - > data/annual_articles/page_RadioZET_2019.json
scrapy runspider scrapy/scraper.py -a domain="pap" -a ranges_start="0,0,0" -a ranges_end="1000,430,250" -t json --nolog -o - > data/annual_articles/page_PAP_2019.json
scrapy runspider scrapy/scraper.py -a domain="tvn24bis" -a ranges_start="1" -a ranges_end="450" -t json --nolog -o - > data/annual_articles/page_TVN24bis_2019.json
scrapy runspider scrapy/scraper.py -a domain="tvp_info" -a ranges_start="1, 1" -a ranges_end="551,320" -t json --nolog -o - > data/annual_articles/page_TVP_INFO_2019.json
scrapy runspider scrapy/scraper.py -a domain="polsat_news" -a ranges_start="1, 1, 1, 1" -a ranges_end="556,556,180,190" -t json --nolog -o - > data/annual_articles/page_Polsat_News_2019.json
scrapy runspider scrapy/scraper.py -a domain="polskie_radio" -a ranges_start="1, 1" -a ranges_end="1000,420" -t json --nolog -o - > data/annual_articles/page_Polskie_Radio_2019.json
scrapy runspider scrapy/scraper.py -a domain="wprost" -a ranges_start="1, 1, 1" -a ranges_end="120,477,360" -t json --nolog -o - > data/annual_articles/page_Wprost_2019.json



#Rscript scripts/news_selector_annual.R
#Rscript scripts/knitting_button.R