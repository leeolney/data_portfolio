#Title: Mobile Strategy Games on Apple App Store
#Dataset: 17k Mobile Strategy Games (https://www.kaggle.com/tristan581/17k-apple-app-store-strategy-games)
#Research question 1: What are the variables that can predict if a game has a user rating score(URS) of 4.0 and above and what are their relationships?
#Research question 2: What is the relationship between user rating count(URC), price and size? 

#load packages 
library("tidyverse")
library("outliers")
library("Hmisc")
library("ggcorrplot") 
library("pscl")
library("caret")
library("tree")
library("cluster")
library("factoextra")
library("NbClust")
library("dendextend")
library("ggdendro")

#import dataset 
df=read.csv("games_v1.csv",header=TRUE)
dim(df)
str(df)

#change variable types
df$IAP=as.factor(df$IAP)
df$size_group=as.factor(df$size_group)
df$good_URS=as.factor(df$good_URS)
df$lang_multi=as.factor(df$lang_multi)
summary(df)

#check
#identifying outliers in size mb
library("ggplot2")
ggplot(df,aes(x="size_mb",y= size_mb))+ geom_boxplot()
#get z-score 
library("outliers")
zscore= scores(df$size_mb)
#view outliers
is_outlier = zscore>3 | zscore < -3 
df$is_outlier = is_outlier
df_outliers = df[df$is_outlier ==T,]
fix(df_outliers)
dim(df_outliers) #43 outliers 
summary(df_outliers$size_mb) #outliers are games above 1225mb and above
summary(df_outliers)

#check correlation 
df2=select_if(df,is.numeric)
corr=rcorr(as.matrix(df2[,-2]))
corr
corr2= round(cor(df2),1)
head(corr2)
ggcorrplot(corr2,hc.order=TRUE, type="lower",lab = TRUE, outline.col="white",ggtheme=ggplot2::theme_grey,colors=c("#6D9EC1", "white", "#E46726")) 

#split data
set.seed(123)
y1=sample(1:2272,1818) #80:20 
xtrain=df[y1,]
xtest=df[-y1,]
#check representation
Hmisc:: describe(df2$good_URS)  
Hmisc:: describe(xtrain$good_URS)
Hmisc:: describe(xtest$good_URS)

#logistic regression
#model with lowest AIC and highest pseudo r2 out of 6 models tested
model2=glm(good_URS~model + size_mb + genre_new + age_rating + lang_multi, data=xtrain, family="binomial")
summary(model2) #AIC 1398
pR2(model2) #0.117
anova(model2,test="Chisq")

#prediction using model 2
prob=predict(model2,xtest,type="response")
prob3=rep(0,454)
prob3[prob>0.5]=1
table(prob3,xtest$good_URS) #387 correct, 85%
confusionMatrix(as.factor(prob3),xtest$good_URS)
#check linearity of num var to logit of outcome
library(dplyr)
mydata = xtest %>%
dplyr::select(size_mb)
predictors = colnames(mydata)
#bind logit
mydata = mydata %>%
mutate(logit = log(prob/(1-prob))) %>%
gather(key="predictors", value="predictor.value", -logit) 
#scatterplot
library('ggplot2')
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#decision tree 
#treemodel1 using LRmodel 2 features
treemodel1 = tree(good_URS~model+age_rating+size_mb+genre_new+lang_multi,xtrain)
plot(treemodel1)
text(treemodel1,pretty=0)
#prediction
Treepred=predict(treemodel1,xtest,type="class")
table(Treepred,xtest$good_URS) #382 correct
Treepred=predict(treemodel1,xtest,type="class")
confusionMatrix(Treepred, xtest$good_URS)
#pruning 
cv=cv.tree(treemodel1,FUN=prune.misclass)
names(cv)
plot(cv$size,cv$dev)
newmodel=prune.misclass(treemodel1, best=4) #4,2,3 give the same results
#prediction after pruning 
Treepred2=predict(newmodel, xtest, type="class")
table(Treepred2, xtest$good_URS)
Treepred2=predict(newmodel, xtest, type="class")
confusionMatrix(Treepred2, xtest$good_URS)


#clustering 
#data preparation for clustering 
#outliers in URC
df3 = df
zscore= scores(df3$URC)
URC_outlier = zscore>3 | zscore < -3
df3$URC_outlier = URC_outlier
URC_outlier = df3[df3$URC_outlier ==T,]
dim(URC_outlier)
#outliers in size_mb
zscore2= scores(df3$size_mb)
size_outlier = zscore2 >3 | zscore2 < -3
df3$size_outlier = size_outlier
size_outlier = df3[df3$size_outlier ==T,]
dim(size_outlier) 
#outliers in price
zscore3= scores(df3$price)
price_outlier = zscore3 >3 | zscore3 < -3
df3$price_outlier = price_outlier
price_outlier = df3[df3$price_outlier ==T,]
dim(price_outlier) 
#subset without outliers in URC, size_mb and price
x = df3[df3$URC_outlier==F & df3$size_outlier==F & df3$price_outlier==F,]
dim(x) 
#scaling before clustering 
x1 = x %>% select(URC, price, size_mb)
x1scaled= scale(x1)
head (x1scaled)

#hierarchical clustering
x1h1 = hclust(dist(x1scaled))
plot(x1h1) 
dend_obj = as.dendrogram(x1h1)
dend4 = color_branches(dend_obj,h=8)
plot(dend4) 
dend3 = color_branches(dend_obj,k=3)
plot(dend3)

#k-means clustering
#find optimal clusters using elbow method 
set.seed(124)
fviz_nbclust(x1scaled,kmeans,method="wss") #elbow plot
#k4
set.seed(131)
x1k4 = kmeans(x1scaled,centers=4,nstart=50)
x1k4
fviz_cluster(x1k4, data=x1scaled, labelsize=0) #cluster plot
with(df3,pairs(x1scaled,col=(1:4)[x1k4$cluster])) #pair plot
#k3
set.seed(142)
x1k3 = kmeans(x1scaled,centers=3,nstart=50)
x1k3
fviz_cluster(x1k3, data=x1scaled, labelsize=0) #cluster plot
with(df3,pairs(x1scaled,col=(1:3)[x1k3$cluster]))#pair plot

#previous model testing
#model 1
model1=glm(good_URS~model + size_mb + genre_new + age_rating + lang_num, data=xtrain, family="binomial")
summary(model1) #AIC 1406.9
pR2(model1) #0.112
anova(model1,test="Chisq") 
#model2 change lang_num to lang_multi 
model2=glm(good_URS~model + size_mb + genre_new + age_rating + lang_multi, data=xtrain, family="binomial")
summary(model2) #AIC 1398
pR2(model2) #0.117
anova(model2,test="Chisq")
#model3 remove genre
model3=glm(good_URS~model + size_mb + age_rating + lang_multi, data=xtrain, family="binomial")
summary(model3) #AIC 1402
pR2(model3) #0.109
anova(model3,test="Chisq")
#model4 use numeric vars whenever possible except lang
model4=glm(good_URS~ price + IAP + size_mb + age_rating + genre_new + lang_multi, data=xtrain,  family="binomial")
summary(model4) #AIC 1391
pR2(model4) #0.12
anova(model4,test="Chisq")
#model 5 use size_group instead of mb
model5=glm(good_URS~model + size_group + genre_new + age_rating + lang_multi, data=xtrain, family="binomial")
summary(model5) #AIC 1449
pR2(model5) #0.085
anova(model5,test="Chisq")
#model 6 remove age rating
model6=glm(good_URS~model + size_mb + genre_new + lang_multi, data=xtrain, family="binomial")
summary(model6) #AIC 1396
pR2(model6) #0.114
anova(model6,test="Chisq")

#previous probablity threshold testing 
#threshold at 0.5 have the highest accuracy
prob=predict(model2,xtest,type="response")
prob1=rep(0,454)
prob1[prob>0.6]=1
table(prob1,xtest$good_URS) #383 correct
prob2=rep(0,454)
prob2[prob>0.7]=1
table(prob2,xtest$good_URS) #372 correct
prob3=rep(0,454)
prob3[prob>0.5]=1
table(prob3,xtest$good_URS) #387 correct, 85%
prob4=rep(0.454)
prob4[prob>0.4]=1
table(prob4,xtest$good_URS) #382 correct
#prediction using model 6
probb=predict(model6,xtest,type="response")
probb1=rep(0,454)
probb1[probb>0.5]=1
table(probb1,xtest$good_URS)  #383 correct
