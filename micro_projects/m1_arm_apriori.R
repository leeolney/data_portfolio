# Groceries dataset (https://www.kaggle.com/heeraldedhia/groceries-dataset)
# Market basket analysis exercise with Apriori 

# Objective: Preparing transaction data and apply association rule mining with Apriori in R
# Reference notebooks: 

# Load libraries
library(plyr)
library(arules)
library(arulesViz)
library(Hmisc)
library(lubridate)

# Import data
groc = read.csv("Groceries_dataset.csv", header=TRUE) 

# Summary
dim(groc)
str(groc)

# Missing value analysis
sum(is.na(groc))

# Sort by member number and change variable types
groc2 = groc[order(groc$Member_number),]
groc2$Member_number = as.numeric(groc2$Member_number)

# Group items by customer (same day)
groc3 = ddply(groc2, c("Member_number","Date"), function(df1)paste(df1$itemDescription, collapse = ","))

# Format date
groc3$newDate = as.Date(parse_date_time(groc3$Date,"dmy"))
groc3 = groc3 %>% mutate(year= as.factor(year(newDate)), month= as.factor(month(newDate)), day= as.factor(day(newDate)))
head(groc3)

# viz year 
v1= ggplot(groc3, aes(x=factor(year))) + geom_bar(stat="count") + theme_classic()

# viz year and month 
v2 = groc3 %>% group_by(year, month) %>% tally() %>% ggplot(aes(x= month, y=n, fill= n)) + geom_col(position="dodge") + facet_wrap(~year, nrow=2) + theme_classic() + labs(x= "Month", y= "Transaction count", fill = "Transaction count", title= "Transactions throughout the year")

# viz customer 
groc2$Member_number = as.factor(groc2$Member_number)
v3 = table(groc2$Member_number) %>% as.data.frame() %>% arrange(desc(Freq)) %>% group_by(Freq) %>% tally() %>% ggplot(aes(x=Freq, y=n, fill=n)) + geom_col(position="dodge") + theme_classic() + labs(x= "Transactions per customer", y= "Frequency", fill = "Frequency", title = "Frequency of transactions per customer")


# Df without Member_number and date
groc4 = groc3[c(-1,-2)]
head(groc4)

# Convert csv to basket format
write.csv(groc4, "groc4.csv", quote = FALSE, row.names=TRUE)
g5 = read.transactions(file="groc4.csv", rm.duplicates=TRUE, format="basket", sep=",", cols=1);
print(g5) 

# Remove quotes
g5@itemInfo$labels <- gsub("\"","",g5@itemInfo$labels)

# Summary of transaction
inspect(g5[1:10])
length(g5)
# find out how many items in a transaction
size(g5[1:10])
# show count of every transaction
count = size(g5[1:length(g5)])

# Most frequent item count per transaction
Hmisc::describe(count) #66.9% of the transactions have item count of 2

# top 20 most frequent items brought 
itemFrequencyPlot(g5, topN=20, type='absolute', col= heat.colors(20))

# top 20 most frequent items brought using ggplot
itemFrequencyGGPlot <- function(x, topN) {
  library(tidyverse)
  x %>%
    itemFrequency %>%
    sort %>%
    tail(topN) %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    ggplot(aes(reorder(rowname, `.`),`.`)) + 
    geom_col() + 
    coord_flip() + theme_classic() + labs(x= "Items", y= "Frequency", title = "20 Most frequent items brought")
}  
v20a = itemFrequencyGGPlot(g5, 20)

# top 20 most frequent items brought using ggplot
itemFrequencyGGPlot <- function(x, topN) {
  library(tidyverse)
  x %>%
    itemFrequency %>%
    sort %>%
    head(topN) %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    ggplot(aes(reorder(rowname, `.`),`.`)) + 
    geom_col() + 
    coord_flip() + theme_classic() + labs(x= "Items", y= "Frequency", title = "20 Least frequent items brought")
}  
v20b = itemFrequencyGGPlot(g5,20) 


# Run Apriori rule with min len 2
R5 = apriori(g5, list(minlen=2, supp=.005, conf=.1))
inspect(R5) #19 rules
inspect(R5[is.redundant(R5)]) 

## Sort R5 rules based on parameters
R5_conf= sort(R5, by="confidence", decreasing=TRUE)
inspect(R5_conf[1:10])
R5_lift= sort(R5, by="lift", decreasing=TRUE)
inspect(R5_lift[1:10])

# Scatter plot 
plot(R5, jitter = 0)
# Matrix  
plot(R5, method="grouped") 
# Rule graph 
plot(R5, method="graph") 
# Parallel coordinates plot
plot(R5, method="paracoord")

# Run Apriori rule with min len 3
R6 = apriori(g5, list(minlen=3, supp=.001, conf=.1))
inspect(R6) #17 rules 
inspect(R6[is.redundant(R6)]) 

## Sort rules based on parameters
R6_conf= sort(R6, by="confidence", decreasing=TRUE)
inspect(R6_conf[1:10])
R6_lift= sort(R6, by="lift", decreasing=TRUE)
inspect(R6_lift[1:10])

# Scatter plot
plot(R6, jitter = 0)
# Matrix viz 
plot(R6, method="grouped")
# Rule graph
plot(R6, method="graph")
# Parallel coordinates plot
plot(R6, method="paracoord")




#Interactive charts
plotly_arules(R6, method="matrix")
plotly_arules(R6, measure= c("support","lift"), shading= "confidence")






