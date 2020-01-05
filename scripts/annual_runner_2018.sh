#!/bin/bash

# Path should be changed
#PATH='/home/jkubajek/anaconda3/bin:/home/jkubajek/anaconda3/condabin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin'; export PATH

# When running on Linux change paths
#cd D:/Osobiste/GitHub/News_Selector

scrapy runspider scrapy/scraper.py -a domain="gazeta" -a ranges_start="540,285,370,160" -a ranges_end="550,530,700,320" -t json --nolog -o - > data/annual_articles/page_Gazeta_2018.json
scrapy runspider scrapy/scraper.py -a domain="rmf" -a ranges_start="1530" -a ranges_end="3150" -t json --nolog -o - > data/annual_articles/page_RMF_2018.json
scrapy runspider scrapy/scraper.py -a domain="tvn24" -a ranges_start="2040" -a ranges_end="4170" -t json --nolog -o - > data/annual_articles/page_TVN24_2018.json
scrapy runspider scrapy/scraper.py -a domain="interia" -a ranges_start="180,850,530" -a ranges_end="340,1680,1190" -t json --nolog -o - > data/annual_articles/page_Interia_2018.json
#scrapy runspider scrapy/scraper.py -a domain="dziennik" -a ranges_start="1,1,1,1,1" -a ranges_end="40,35,35,8,22" -t json --nolog -o - > data/annual_articles/page_Dziennik_annual.json
scrapy runspider scrapy/scraper.py -a domain="radio_zet" -a ranges_start="290,80,70" -a ranges_end="555,160,150" -t json --nolog -o - > data/annual_articles/page_RadioZET_2018.json
scrapy runspider scrapy/scraper.py -a domain="pap" -a ranges_start="980,405,240" -a ranges_end="1700,800,490" -t json --nolog -o - > data/annual_articles/page_PAP_2018.json
scrapy runspider scrapy/scraper.py -a domain="tvn24bis" -a ranges_start="430" -a ranges_end="1030" -t json --nolog -o - > data/annual_articles/page_TVN24bis_2018.json
scrapy runspider scrapy/scraper.py -a domain="tvp_info" -a ranges_start="530, 305" -a ranges_end="1010,610" -t json --nolog -o - > data/annual_articles/page_TVP_INFO_2018.json
#scrapy runspider scrapy/scraper.py -a domain="polsat_news" -a ranges_start="1, 1" -a ranges_end="90,75" -t json --nolog -o - > data/annual_articles/page_Polsat_News_2018.json
scrapy runspider scrapy/scraper.py -a domain="polskie_radio" -a ranges_start="980, 400" -a ranges_end="1580,810" -t json --nolog -o - > data/annual_articles/page_Polskie_Radio_2018.json
#scrapy runspider scrapy/scraper.py -a domain="wprost" -a ranges_start="1, 1, 1" -a ranges_end="120,477,360" -t json --nolog -o - > data/annual_articles/page_Wprost_2018.json



#Rscript scripts/news_selector_annual.R
#Rscript scripts/knitting_button.R