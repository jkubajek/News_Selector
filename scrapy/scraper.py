import scrapy
from w3lib.html import remove_tags
from w3lib.html import replace_escape_chars
import lxml
import re
import json

def clear_text(text: str):
    text = text.replace('\n', '')
    text = re.sub('\s+', ' ', text)
    return text

class DOMAINS:
    DZIENNIK = 'dziennik'
    GAZETA = 'gazeta'
    INTERIA = 'interia'
    PAP = 'pap'
    RADIO_ZET = 'radio_zet'
    RMF = 'rmf'
    TVN24 = 'tvn24'
    TVN24bis = 'tvn24bis'
    TVP_INFO = 'tvp_info'
    POLSKIE_RADIO = 'polskie_radio'
    WPROST = 'wprost'
    POLSAT_NEWS = 'polsat_news'


DOMAIN_URLS = {
    DOMAINS.DZIENNIK: ['https://wiadomosci.dziennik.pl/polityka,', # Polityka
                        'https://wiadomosci.dziennik.pl/wydarzenia,', # Polska
                        'https://wiadomosci.dziennik.pl/swiat,', # Swiat
                        'https://wiadomosci.dziennik.pl/media,', # Media
                        'https://gospodarka.dziennik.pl/news,'], # Gospodarka
    DOMAINS.GAZETA: ['http://wiadomosci.gazeta.pl/wiadomosci/0,114871.html?str=', # Najnowsze
                        'http://wiadomosci.gazeta.pl/wiadomosci/0,114884.html?str=', # Polityka
                        'http://wiadomosci.gazeta.pl/wiadomosci/0,114883.html?str=', # Polska
                        'http://wiadomosci.gazeta.pl/wiadomosci/0,114881.html?str='], # Swiat
    DOMAINS.INTERIA: ['https://fakty.interia.pl/wiadomosci-lokalne,nPack,', # Wiadomosci lokalne
                        'https://fakty.interia.pl/polska,nPack,', # Polska
                        'https://fakty.interia.pl/swiat,nPack,'], # Swiat
    DOMAINS.PAP: ['https://www.pap.pl/kraj?page=', # Polska
                        'https://www.pap.pl/swiat?page=', # Swiat
                        'https://www.pap.pl/gospodarka?page='], # Ekonomia
    DOMAINS.RADIO_ZET: ['https://wiadomosci.radiozet.pl/Polska/(offset)/', # Polska
                        'https://wiadomosci.radiozet.pl/Swiat/(offset)/', # Swiat
                        'https://biznes.radiozet.pl/Newsy/(offset)/'], # Swiat
    DOMAINS.RMF: ['https://www.rmf24.pl/fakty,nPack,'],
    DOMAINS.TVN24: ['https://www.tvn24.pl/najnowsze,49/'],
    DOMAINS.TVN24bis: ['https://www.tvn24bis.pl/najnowsze,72/'],
    DOMAINS.TVP_INFO: ['https://www.tvp.info/191866/polska?page=',
                       'https://www.tvp.info/191867/swiat?page='],
    DOMAINS.POLSKIE_RADIO: ['https://www.polskieradio24.pl/POLSKA/Tag295/Strona',
                            'https://www.polskieradio24.pl/SWIAT/Tag6638/Strona'],
    DOMAINS.WPROST: ['https://biznes.wprost.pl/gospodarka/',
                     'https://www.wprost.pl/kraj/',
                     'https://www.wprost.pl/swiat/'],
    DOMAINS.POLSAT_NEWS: ['https://www.polsatnews.pl/wyszukiwarka/?text=polska&type=event&page=',
                          'https://www.polsatnews.pl/wyszukiwarka/?text=swiat&type=event&page=']
}

ALLOWED_DOMAINS = {
    DOMAINS.DZIENNIK: None,
    DOMAINS.GAZETA: None,
    DOMAINS.INTERIA: None,
    DOMAINS.PAP: None,
    DOMAINS.RADIO_ZET: None,
    DOMAINS.RMF: None,
    DOMAINS.TVN24: ['www.tvn24.pl'],
    DOMAINS.TVN24bis: ['tvn24bis.pl'],
    DOMAINS.TVP_INFO: None,
    DOMAINS.POLSKIE_RADIO: None,
    DOMAINS.WPROST: None,
    DOMAINS.POLSAT_NEWS: None
}

ARTICLES_LINKS = {
    # DOMAINS.DZIENNIK: '.widget-box .widget-list-content-box div.widget-list-content h4 a::attr("href")',
    DOMAINS.DZIENNIK: '.itarticle a::attr("href")',
    DOMAINS.GAZETA: '.entry .article a ::attr("href")',
    DOMAINS.INTERIA: '.brief-list-item .tile-magazine-title-url ::attr("href")',
    DOMAINS.PAP: 'div.newsList div.imageWrapper a::attr("href")',
    DOMAINS.RADIO_ZET: 'div.list-element__image a::attr("href")',
    DOMAINS.RMF: '.article .thumbnail:not(.thumbnail.sponsored) .image ::attr("href")',
    DOMAINS.TVN24: 'article h1 a ::attr("href")',
    DOMAINS.TVN24bis: 'article div.photo-container a ::attr("href")',
    DOMAINS.POLSKIE_RADIO: '.article a.main-link ::attr("href")',
    DOMAINS.WPROST: '.main-list .box-list-item .news-data a.news-open ::attr("href")',
    DOMAINS.POLSAT_NEWS: '#searchwrap article.news a::attr("href")'
}


class PageSpider(scrapy.Spider):
    name = "my_spider"

    def __init__(self, domain: str, ranges_start: str, ranges_end: str):
        '''Initialize spider for given domain'''
        self.domain = domain
        ranges_start = ranges_start.split(',')
        ranges_start = [int(i) for i in ranges_start]
        ranges_end = ranges_end.split(',')
        ranges_end = [int(i) for i in ranges_end]
        self.start_urls = []
        domain_urls = DOMAIN_URLS[domain]
        for url_num in range(0, len(domain_urls)):
            self.start_urls += [domain_urls[url_num] + str(i) for i in range(ranges_start[url_num], ranges_end[url_num])]
        
        allowed_domains = ALLOWED_DOMAINS[domain]

    def parse(self, response):
        if self.domain == DOMAINS.DZIENNIK:
            domain_parser = self.parse_dziennik
        elif self.domain == DOMAINS.GAZETA:
            domain_parser = self.parse_gazeta
        elif self.domain == DOMAINS.INTERIA:
            domain_parser = self.parse_interia
        elif self.domain == DOMAINS.PAP:
            domain_parser = self.parse_pap
        elif self.domain == DOMAINS.RADIO_ZET:
            domain_parser = self.parse_radiozet
        elif self.domain == DOMAINS.RMF:
            domain_parser = self.parse_rmf
        elif self.domain == DOMAINS.TVN24:
            domain_parser = self.parse_tvn24
        elif self.domain == DOMAINS.TVN24bis:
            domain_parser = self.parse_tvn24bis
        elif self.domain == DOMAINS.TVP_INFO:
            domain_parser = self.parse_tvp_info
        elif self.domain == DOMAINS.POLSKIE_RADIO:
            domain_parser = self.parse_polskie_radio
        elif self.domain == DOMAINS.WPROST:
            domain_parser = self.parse_wprost
        elif self.domain == DOMAINS.POLSAT_NEWS:
            domain_parser = self.parse_polsat_news
        else:
            print("Wrong domain: " + self.domain)

        if self.domain == DOMAINS.TVP_INFO:
            pattern = re.compile(r"window.__directoryData = ({.*?});", re.MULTILINE | re.DOTALL)
            data = response.xpath('//script[contains(., "window")]/text()')
            data = data.re(pattern)[0]
            data = json.loads(data)
            items = data['items']
            links = [item['url'] for item in items]
        else:
            links = response.css(ARTICLES_LINKS[self.domain]).extract()

        for article_url in links:
            article_url = re.sub('\s+', '', article_url)
            yield response.follow(article_url, callback=domain_parser)
        # https://docs.scrapy.org/en/latest/topics/dynamic-content.html

    def parse_dziennik(self, response):
        '''Parser for dziennik.pl'''
        url = response.url
        art_id = url.split('artykuly/')[1]
        art_id = art_id.split(',')[0]
        
        date = response.xpath("//meta[@property='article:published_time']/@content").extract()[0]
        print(date)
        date = date.split(' ')
        time = date[1]
        date = date[0]
        
        title = response.xpath("//meta[@property='og:title']/@content").extract()
        
        lead = response.css("article .lead::text").extract()
        lead = ' '.join(lead)                  
        lead = remove_tags(lead)
        
        text = response.css('article .detail p').extract()
        
        # W R usunac akapity ze zdjeciami oraz wpisami z twittera - https://t.co/ lub pic.twitter.com/               
        text = ' || '.join(text)
        text = remove_tags(text)
        
        # Joining lead with text
        text = ' || '.join([lead, text])
        source = response.css(".articleFooter span[itemprop='name']::text").extract()
        tags = response.css(".relatedTopics .relatedTopic a::attr('title')").extract()
        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'source': ', '.join(source),
               'tags': ', '.join(tags)
               }
    
    def parse_gazeta(self, response):
        '''Parser for gazeta.pl'''
        url = response.url
        art_id = url.split(',')[2]
        
        date = response.css('.article_date time::attr("datetime")').extract_first()
        date = date.split(' ')
        time = date[1]
        date = date[0]
        
        title = response.css("h1#article_title::text").extract()
        title = ' '.join(title)
        title = replace_escape_chars(title)
        
        lead = response.css("#gazeta_article_lead").extract()
        lead = ' '.join(lead)                  
        lead = remove_tags(lead)
        
        text = response.css('p.art_paragraph').extract()
        text = ' || '.join(text)
        text = remove_tags(text)
        
        # Joining lead with text
        text = ' || '.join([lead, text])
        
        autor = response.css(".article_author::text").extract()
        tags = response.css(".tags_list  .tags_item a::text").extract()
        
        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'autor': ', '.join(autor),
               'tags': ', '.join(tags)}
    
    def parse_interia(self, response):
        '''Parser for Interia'''
        url = response.url
        art_id = url.split('nId,')[1]
        
        date = response.css('.article-date ::attr("content")').extract_first()
        date = date.split('T')
        time = date[1]
        date = date[0]
        
        title = response.css("h1.article-title::text").extract()
        title = ' '.join(title)
        title = replace_escape_chars(title)
        
        lead = response.css(".article-body .article-lead::text").extract()
        lead = ' '.join(lead)                  
        lead = remove_tags(lead)
        
        art_path = '//div[@class = "article-container"]/div[not(*/@class = "embed")]/p[not(/aside[@class = "embed embed-photo embed-center"])]'
        text = response.xpath(art_path)
        text = text.extract()             
        text = ' || '.join(text)
        text = remove_tags(text)
        
        source = response.css(".article-footer .article-source ::attr('content')").extract()
        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'source': ', '.join(source) 
               }
    
    def parse_pap(self, response):
        url = response.url
        art_id = url.split('%2C')[1]
        
        date = response.css('article div.moreInfo').extract_first()
        date = date.split('</svg>')[1]
        date = date.split(', ')
        time = date[1][0:5]
        date = date[0]
        
        title = response.css("h1.title ::text").extract()
        title = ' '.join(title)
        title = replace_escape_chars(title)
        
        lead = response.css("article div.field.field--name-field-lead ::text").extract()
        lead = ' '.join(lead)                  
        
        text = response.css('article div.field.field--name-body p ::text').extract()
        
        # Czyszczenie tekstu
        text.pop()
        
        for i in range(0, len(text)):
            text[i] = remove_tags(text[i]).strip()
        
        if len(text[-1]) < 100:
            if 'arch.' in text[-1]:
                text.pop()
                if len(text[-1]) < 100:
                   autor = text[-1]
                   text.pop() 
            else:
                autor = text[-1]
                text.pop()
        else:
            autor = ''
        
        if re.search('^(A|a)utor.*:', text[-1]) != None or len(text[-1]) < 100:
            text.pop()
            if len(text[-1]) < 100:
                text.pop()
                
        text[-1] = re.sub('(\(PAP\)|\(PAP Biznes\))', '', text[-1])
        
        text = ' || '.join(text)
        
        # Joining lead with text
        text = ' || '.join([lead, text])
        tags = response.css("article div.field.field--name-field-tags  .tagsList .field--item a::text").extract()
        
        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'autor': autor,
               'tags': ', '.join(tags)}
               
    def parse_radiozet(self, response):
        url = response.url
        
        date = response.css('article .info-header__date--published__date::text').extract_first()
        #date = date.split(' ')
        #date = date[0]
        date = date.replace('.', '-')
        time = response.css('article .info-header__date--published__time::text').extract_first()
        
        title = response.css("article header .full__title.full__article__title::text").extract()
        title = ' '.join(title)
        title = replace_escape_chars(title)
        
        lead = response.css(".full__article__lead ::text").extract()
        lead = ' '.join(lead)                  
        lead = remove_tags(lead)
        lead = re.sub('\s+', ' ', lead)
        lead = re.sub(' \n', '', lead)
        
        exclude_selectors = (
        'not(ancestor::*[contains(@class, "advert")])'
        ' and not(ancestor::*[contains(@class, "embed__article")])'
        ' and not(ancestor::*[contains(@class, "SandboxRoot")])'
        ' and not(ancestor::*[contains(@class, "twitter-tweet")])'
        ' and not(ancestor::div[contains(@class, "cnnStoryElementBox")])'
        ' and not(descendant::*[starts-with(text(), "ZOBACZ TAKŻE:")])')
        
        #text = response.css('div.full__article__body p:not([class^="embed__article"])').extract()
        selector = '//div[contains(@class, "full__article__body")]//p[%s]' % exclude_selectors
        text = response.xpath(selector)
        text = text.extract()
         
        # W R usunac akapity ze zdjeciami oraz wpisami z twittera - https://t.co/ lub pic.twitter.com/ 
        source = text[-1]
        text.pop(-1)
        text.pop(0)             
        text = ' || '.join(text)
        text = remove_tags(text)
        source = remove_tags(source)
        
        # Joining lead with text
        text = ' || '.join([lead, text])
        
        tags = response.css('div.full__article__tags__list a::attr("title")').extract()
        yield {'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'source': source,
               'tags': ', '.join(tags)
               }
               
    def parse_rmf(self, response):
        url = response.url
        art_id = url.split('nId,')[1]
        
        date = response.css('.article-date ::attr("content")').extract_first()
        date = date.split('T')
        time = date[1]
        date = date[0]
        
        title = response.css(".article-header .article-title::text").extract()
        title = ' '.join(title)
        title = replace_escape_chars(title)
        lead = response.css(".article-body .article-lead::text").extract()
        
        art_path = '//div[@class = "article-container"]/div[@class = "article-body"]/div[@class = "articleContent"][not(*/@class = "embed")]/p[not(contains(descendant-or-self, "u") or contains(descendant-or-self, "sub") or contains(descendant-or-self, "b") or contains(ancestor-or-self, "aside")  or contains(descendant-or-self, "aside") or contains(ancestor-or-self, "twitter-widget") or contains(@class, "Tweet-text"))]'
        text = response.xpath(art_path)
        text = text.extract()
        
        twitter = response.css(".article-container .article-body .articleContent .embed-blockquote").extract()
        
        # Usuniecie wpisow twitterowych
        # TO DO - usunac lepiej twitty
        if len(twitter) > 0:
            for a in range(0, len(twitter)):
                for t in range(0, len(text)):
                    if text[t]  == twitter[a]:
                        text[t] = ""
        
        text = ' || '.join(text)
        text = remove_tags(text)
        
        # Joining lead with text
        lead = ' '.join(lead)
        text = ' || '.join([lead, text])
        text = re.sub('\s+', ' ', text)
        autor = response.css(".article-author-name::text").extract()
        source = response.css(".article-footer .article-source ::attr('content')").extract()
        tags = response.css(".elementTagsList a::text").extract()
        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'autor': ', '.join(autor),
               'source': ', '.join(source),
               'tags': ', '.join(tags)}
               
    def parse_tvn24(self, response):
        url = response.url
        art_id = url.split(',')[-1].split('.')[0]
        
        date = response.css('div.articleDateContainer time::attr("datetime")').extract_first()
        date = date.split(' ')
        time = date[1]
        date = date[0]
        
        title = response.css("div.mainContainer h1 ::text").extract_first()
        title = replace_escape_chars(title).strip()
        
        lead = response.css("article h2 span.black ::text").extract_first()
        lead = replace_escape_chars(lead).strip()
        
        text = response.xpath('//*[not(contains(self, "em") or contains(self, "figure") or contains(self, "aside") or contains(@class, "innerArticleModule.onRight.cols.externalContent.innerText") or contains(@class, "lead"))]/div[@class="textArticleDefault"]//article/p[not(contains(self, "em") or contains(self, "figure") or contains(self, "aside") or contains(@class, "innerArticleModule.onRight.cols.externalContent.innerText") or contains(@class, "innerText") or contains(@class, "lead") or contains(@class, "textHolder") or contains(self, "div") or contains(text(), "czytaj"))]/text()').extract()
        
        text = ' || '.join(text)
        text = remove_tags(text)
        text = replace_escape_chars(text)
        
        # Joining lead with text
        text = ' || '.join([lead, text])
        autor = response.css("div.articleAuthors ::text").extract()
        source = autor[2].strip().replace('Źródło: ', '')
        autor = autor[0].strip().replace('Autor: ', '')
        
        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'autor': autor,
               'source': source
               }
               
    def parse_tvn24bis(self, response):
        url = response.url
        art_id = url.split(',')[-1].split('.')[0]
        
        date = response.css('article.detail header time::attr("datetime")').extract_first()
        date = date.split(' ')
        time = date[1][0:4]
        date = date[0]
        
        title = response.css("article.detail header h1 ::text").extract_first()
        title = replace_escape_chars(title).strip()
        
        lead = response.css("div.content p.lead ::text").extract_first()
        lead = replace_escape_chars(lead).strip()
        
        text = response.xpath('//div[@class="content"]/p[not(contains(@clas, "rules") or contains(@clas, "footer"))]/text()').extract()
        
        text = ' || '.join(text)
        text = remove_tags(text)
        text = replace_escape_chars(text)
        
        autor = response.css("div.content div.footer ::text").extract()[1].split('/')
        if len(autor) > 1:
            source = autor[1]
            source = source.strip().replace('Źródło: ', '')
            autor = autor[0].strip().replace('Autor: ', '')
        else:
            source = ''
            autor = autor[0].strip().replace('Autor: ', '')
        
        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'autor': autor,
               'source': source
               }

    def parse_tvp_info(self, response):
        '''Parser for TVP INFO'''
        url = response.url
        art_id = url.split('/')[3]

        date = response.css('.info-article .layout-article .date ::text').extract_first()
        date = date.split(',')
        time = date[1]
        date = date[0]
        date = date.replace('.', '-')

        title = response.xpath("//meta[@property='og:title']/@content").extract_first()

        lead = response.css(".article-layout p.am-article__heading ::text").extract()
        lead = ' '.join(lead)
        lead = remove_tags(lead)

        exclude_selectors = (
            ''
            'not(self::*[contains(@class, "advert")])'
            ' and not(self::*[contains(@class, "embed__article")])'
            ' and not(self::*[contains(@class, "SandboxRoot")])'
            ' and not(self::*[contains(@class, "twitter-tweet")])'
            ' and not(self::*[contains(@class, "am-article__image")])'
            ' and not(self::*[contains(@class, "facebook-paragraph")])'
            ' and not(self::*[contains(@class, "am-article__source")])'
            ' and not(self::*[contains(@class, "article-tags")])'
            ' and not(self::*[contains(@class, "Tweet")])'
            ' and not(self::*[contains(@class, "embed")])'
            ' and not(self::*[contains(@class, "social-article")])'
            ' and not(self::*[contains(@class, "video-module")])'
            ' and not(self::a)'
        )

        selector_text = '//div[contains(@class, "article-layout")]/*[%s]//text()' % exclude_selectors
        text = response.xpath(selector_text).extract()
        text = ' || '.join(text)
        text = clear_text(text)

        autor = response.css(".info-article__header .info-article__date .name ::text").extract()
        tags = response.css(".article-tags .article-tags__tag::text").extract()
        tags = ', '.join(tags)
        tags = clear_text(tags)

        source = response.css(".am-article__source .am-article__tag ::text").extract()

        yield {'id': art_id,
               'url': url,
               'date': date,
               'time': time,
               'title': ''.join(title),
               'lead': lead,
               'text': text,
               'autor': ', '.join(autor),
               'tags': tags,
               'source': ', '.join(source)}