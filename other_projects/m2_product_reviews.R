#Dataset: Amazon pet reviews (train)
#Source: https://www.kaggle.com/c/amazon-pet-product-reviews-classification/data?select=test.csv


library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(purr)

library(tidyverse)
library(tidytext)
library(textdata)
library(viridis)
library(wesanderson)

#Import data
reviews = read.csv("reviews_d2.csv", header=TRUE)
dim(reviews)

#remove unicode dec manually
unicode_dec = c("&#34")
reviews$text = gsub(paste0(unicode_dec,collapse="|"),"", reviews$text)
reviews$text = gsub("[[:digit:]]", "", reviews$text)

#convert to lowercase, remove punctuation
reviews_stem = reviews %>% select(text) %>% unnest_tokens(word, text) %>% mutate(word = SnowballC::wordStem(word, "english"))
head(reviews_stem)
fix(reviews_stem)

#remove stop words
cleaned_reviews = reviews_stem %>% anti_join(stop_words)
head(cleaned_reviews)

#find 10 most common words 
common = cleaned_reviews %>% count(word) %>% arrange(desc(n))
head(common)

cleaned_reviews %>% count(word, sort=TRUE) %>% top_n(10) %>% mutate(word= reorder(word,n)) %>% ggplot (aes(x=word, y=n, fill=n)) + geom_col() + xlab(NULL) + scale_fill_viridis() + coord_flip() + theme_classic() + labs(x= "Count", y="Unique Words", title = "Unique word counts found in dog reviews")

#word cloud
cleaned_reviews %>% count(word) %>% arrange(desc(n)) %>% with(wordcloud(word,n,max.words=100,random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2")))

#sentiment analysis bing 
bing_reviews = cleaned_reviews %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort=TRUE) %>% ungroup()
head(bing_reviews)

#visualise neg and pos sentiments
p = bing_reviews %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word=reorder(word,n)) %>% ggplot(aes(word,n, fill=sentiment)) + geom_col(show.legend=FALSE) + facet_wrap(~sentiment, scales="free_y") + labs(title = "Reviews on dog products", y = "Contribution to sentiment", x= NULL) + coord_flip() + theme_minimal() + scale_fill_manual(values=wes_palette(n=2, name="Moonrise2"))

#create function to get score for each review
sentiment_bing = function(text) {
	rev_tbl = tibble(text = text) %>% 
	mutate(
	stripped_text= gsub("&#34","", text)
	) %>%
	unnest_tokens(word,stripped_text) %>%
	anti_join(stop_words) %>%
	inner_join(get_sentiments("bing")) %>%
	count(word, sentiment, sort=TRUE) %>%
	ungroup() %>%
	mutate(
	score= case_when(
	sentiment == 'negative'~n*(-1), 
	sentiment == 'positive'~n*1)
	)
	sent.score=case_when(
	nrow(rev_tbl)==0~0, 
	nrow(rev_tbl)>0~sum(rev_tbl$score)
	)
	zero.type= case_when(
	nrow(rev_tbl)==0~"Type 1", 
	nrow(rev_tbl)>0~"Type 2",
	)
	list(score= sent.score, type= zero.type, rev_tbl= rev_tbl)
}

#apply function
review_sent = lapply(reviews$text, function(x){sentiment_bing(x)})

#create tibble 
review_sentiment = bind_rows(
	tibble(
	score = unlist(map(review_sent,'score')),
	type = unlist(map(review_sent, 'type'))
	))
review_sentiment

#viz distribution of sentiment scores 
freq = review_sentiment %>% group_by(score) %>% tally()
fill_col= c("#e36414","#e36414","#e36414","#e36414","#e36414","#e36414", "#495057","#1282a2","#1282a2","#1282a2","#1282a2","#1282a2","#1282a2","#1282a2")

ggplot(freq, aes(x=score, y= n, color=)) + geom_bar(stat='identity', fill= fill_col) + theme_classic() + labs(title= "Distribution of sentiment scores", x= "score", y = NULL)

#Proportion
freq2 = freq %>% mutate(cg = case_when(score<0 ~ "negative", score==0~"neutral", score>0~"positive")) %>% group_by(cg) %>% tally(n) %>% mutate(prop = n/sum(n))

ggplot(freq2, aes(x=as.factor(cg), y=prop, fill=cg)) + geom_bar(stat="identity") + scale_fill_manual(values= c("#e36414","#495057","#1282a2")) + geom_text(aes(label= scales::percent(prop), y=prop), stat="identity", vjust=-.5) + theme_minimal() + theme(legend.position="none", axis.text.y= element_blank(), axis.ticks.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + labs(title= "Sentiments of dog product reviews", x= NULL, y = NULL) + theme(plot.title= element_text(size=12))



