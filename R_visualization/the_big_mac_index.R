# Title: TidyTuesday Week 52 submission code 
# Data set: The Big Mac index from TheEconomist

library(tidyverse)
library(lubridate)
library(Hmisc)
library(wesanderson)
library(ggdark)

bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')
dim(bigmac)

#data preparation
bigmac <- bigmac %>%
  mutate(qtr = paste0(substring(year(date),1,4),"/0",quarter(date))) %>% 
  filter(!is.na(adj_price)) %>% 
  mutate(difference= round((adj_price - dollar_price),2))
summary(bigmac$difference)

bigmac_203 = bigmac %>% filter(qtr=="2020/03") 
bigmac_203$positive= ifelse(bigmac_203$difference>0,"1","0")
Hmisc::describe(bigmac_203$positive) 

#diverging lollipop chart
bigmac_203 %>% ggplot(aes(x=name, y=difference)) + geom_segment(aes(x=reorder(name,difference), xend=name, y=0, yend=difference,color=positive)) + geom_point(aes(x=name, y=difference,color=positive)) + coord_flip() + dark_theme_minimal() + theme(panel.grid.major.y=element_blank(), panel.border=element_blank(),legend.position="none", plot.title=element_text(size=14), axis.title=element_text(size=10)) + scale_color_manual(values=wes_palette("Moonrise2")) + labs(x="", y="Adjusted price minus local price in dollars", title = "Big Mac Index", subtitle = "Difference between adjusted price (in dollars) and local price (in dollars)\non 01 July 2020", caption= "Data from The Economist (economist.com)") + scale_y_continuous(limits=c(-1.5,2), breaks=c(-1.5,-1,-0.5,0,0.5,1,1.5,2))

#diverging dot plot
theme_set(theme_bw())
ggplot(bigmac_203, aes(x=reorder(name,difference), y=difference, label=difference)) + geom_point(stat='identity', aes(col=positive), size=5) + scale_color_manual(values=wes_palette("Moonrise2")) + geom_text(color="black", size=1.7) + labs(x="", y="Adjusted price minus local price in dollars", title="Big Mac Index", subtitle="Difference between adjusted price (in dollars) and local price (in dollars)\non 01 July 2020") + scale_y_continuous(limits=c(-2,2)) + coord_flip() + dark_theme_minimal() + theme(legend.position="none")


