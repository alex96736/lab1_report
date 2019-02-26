library('rvest')

get_tags <- function(node){
  raw_data <- html_nodes(node, selector) %>% html_text
  data_NAs <- ifelse(length(raw_data) == 0, NA, raw_data)
}
### Загрузка адреса страницы
url <- 'http://market.yandex.ru/catalog--mobilnye-telefony/54726/list?hid=91491&suggest_text=Смартфоны&suggest=1&suggest_type=recipe&was_redir=1&rs=eJwzYgpgBAABcwCG&rt=12&glfilter=4940921%3A13475069&onstock=1&local-offers-first=0&viewtype=list'
webpage <- read_html(url)

### Парсинг
#Название модели смартфона
title_data <- html_nodes(webpage,'.n-snippet-card2__title') %>% html_text 
length(title_data)
head(title_data)

#Цена
price <- html_nodes(webpage, 'div.n-snippet-card2__price > div > div > a > div') %>% html_text() 
price <- gsub( '\\W','',price) 
price <- as.numeric(price)
length(price) 
head(price) 

#Оценка
selector <- 'div.rating__value'
doc <- html_nodes(webpage, 'div.n-snippet-card2__header-stickers')
score_data <- sapply(doc, get_tags)
score_data <- as.numeric(score_data)
length(score_data)
head(score_data)

#Количество отзывов
selector <- 'span'
doc <- html_nodes(webpage, 'div.n-snippet-card2__header-stickers')
comment_data <- sapply(doc, get_tags)
comment_data <- gsub('\\W', '', comment_data) 
comment_data <- as.numeric(comment_data) 
length(comment_data)
head(comment_data)

#Описание
selector <- 'ul' 
doc <- html_nodes(webpage, 'div.n-snippet-card2__content') 
text_data <- sapply(doc, get_tags) 
length(text_data) 
head(text_data)

#Колличество проданных товаров за 2 месяца
selector <- 'div:nth-child(3)>div' 
doc <- html_nodes(webpage, 'div.n-snippet-card2__content') 
byers <- sapply(doc, get_tags)
byers <- gsub( '\\W','',byers) 
byers <- gsub( '2$','',byers) 
byers <- as.numeric(byers)
length(byers)
head(byers) 

#Покупателям нравится
selector <- 'div:nth-child(1) > div > div > span' 
doc <- html_nodes(webpage, 'div.n-snippet-card2__content') 
reasons_data <- sapply(doc, get_tags) 
reasons_data <- gsub('Покупателям нравится', '', reasons_data) 
length(reasons_data) 
head(reasons_data)

#Количество предложений
cnt <- html_nodes(webpage, 'div.n-snippet-card2__more-prices-link > a') %>% html_text() 
cnt <- gsub( '\\W','',cnt) 
cnt <- as.numeric(cnt)
length(cnt)
head(cnt)


DF_Smartphone_yandexmarket = data.frame(Name = title_data, Price = price, Score = score_data,
                                        Count_comment = comment_data, Text = text_data, 
                                        Count_byers = byers, Reason = reasons_data,
                                        Count_suggest = cnt)
str(DF_Smartphone_yandexmarket)
dim(DF_Smartphone_yandexmarket)
head(DF_Smartphone_yandexmarket)

write.csv(DF_Smartphone_yandexmarket, file = 'C:\\Users\\ALeX96736\\Documents\\Smartphone_yandexmarket.csv', row.names = F)