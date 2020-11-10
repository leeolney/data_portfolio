#Title: Onshore pipeline accidents in the US (2010-2017)  
#Dataset: 17k Mobile Strategy Games (https://www.kaggle.com/usdot/pipeline-accidents)

#Research question 1: What are the variables that can predict if an onshore pipeline accident in the US incur all cost of above $100,000?
#Research question 2: What is the relationship between Net Loss Barrels (NLB), All Cost (AC) and Liquid Recovery Barrels (LRB)? 

#load packages
library(tidyverse)
library(Hmisc)
library(ggpubr)
library(outliers)
library(cluster)
library(factoextra)
library(NbClust)
library(dendextend)
library(ggdendro)
library(pscl)
library(tree)
library(corrplot)
library(caret)
library(networkD3)
library(htmlwidgets)
library(skimr)
library(viridis)
library(ggsci)

#import dataset
df=read.csv("pipeline.csv",header=TRUE)
dim(df)
str(df) 

#change variable type
df$Accident.Year = as.factor(df$Accident.Year)
df$Operator.ID = as.factor(df$Operator.ID)
df$Report.Number = as.factor(df$Report.Number)
df$Supplemental.Number = as.factor(df$Supplemental.Number)

#summary
skim(df)
Hmisc:: describe (df$Pipeline.Location) #only 18 obs are offshore
df %>% group_by(Pipeline.Type) %>% tally() %>% mutate(prop= n/sum(n))
df %>% group_by(Liquid.Type) %>% tally() %>% mutate(prop= n/sum(n))

#drop transition area, offshore and biofuel
dfonshore= df[which(df$Pipeline.Location=='ONSHORE'),]
dftr=dfonshore[-which(dfonshore$Pipeline.Type=="TRANSITION AREA"),]
dflt=dftr[-which(dftr$Liquid.Type=="BIOFUEL / ALTERNATIVE FUEL(INCLUDING ETHANOL BLENDS)"),]
x=dflt

#viz
#net loss barrels and all costs 
ggscatter(x,x="Net.Loss_barrels",y="All.Costs", add= "reg.line", conf.int=TRUE, cor.coef = TRUE, cor.method ="pearson",color="#457b9d")
#unintentional release barrels and all cost
ggplot(x,aes(x=Unintentional.Release_barrels,y=All.Costs)) + geom_point(color='#f77f00',alpha=0.7) + theme_classic()

#year 
df %>% group_by(Accident.Year) %>% tally() %>% mutate(prop=n/sum(n)) 
df %>% filter(Accident.Year== 2017) %>% select(Accident.Date.Time) #both 2017 obs are in january 
#accident count by year (2010 to 2016)
y1 = df %>% filter(!Accident.Year ==2017) %>% group_by(Accident.Year) %>% tally() %>% rename(cy=n) %>% as.data.frame() 
ggplot(y1, aes(x=Accident.Year, y=cy, group=1)) + geom_line() + geom_point() + theme_classic() + labs(title="Onshore oil pipeline accidents in the US", y= "Count", x = "Year ")
#avg all cost by year 
df %>% filter(!Accident.Year ==2017) %>% group_by(Accident.Year) %>% summarise(med = median(All.Costs)) %>% ggplot() + geom_col(aes(x=Accident.Year, y=med, fill=med)) + scale_fill_viridis() + theme_classic() + theme(legend.position="none") + labs(x="Year", y= "Median All.Costs", title= "Onshore oil pipeline accidents in the US")

#accidents by accident state
x %>% group_by(Accident.State) %>% tally() %>% rename (c1=n) %>% ggplot() + geom_col(aes(x = Accident.State, y=c1, fill=c1)) + theme_classic() + coord_flip() + scale_fill_viridis() + theme(legend.position="none") + xlab("Accident.State") + ylab("Accident count")
#avg all costs by accident state
x %>% group_by(Accident.State) %>% summarise(mean=mean(All.Costs)) %>% ggplot() + geom_col(aes(x = Accident.State, y=mean, fill=mean)) + theme_classic() + coord_flip() + scale_fill_viridis() + theme(legend.position="none") + xlab("Accident.State") + ylab("Avg. All.Cost")

#median all costs by liquid type
x %>% group_by(Liquid.Type) %>% summarise(med=median(All.Costs)) %>% ggplot() + geom_col(aes(x = reorder(Liquid.Type, med), y= med, fill=Liquid.Type)) + theme_classic() + coord_flip() + theme(legend.position="none") + xlab("Liquid.Type") + ylab("Median All.Cost") + theme(axis.text.y=element_text(size=7)) + scale_fill_jco()
#median all costs by pipeline type
x %>% group_by(Pipeline.Type) %>% summarise(med=median(All.Costs)) %>% ggplot() + geom_col(aes(x = Pipeline.Type, y= med, fill=Pipeline.Type)) + theme_classic() + coord_flip() + theme(legend.position="none") + xlab("Pipeline.Type") + ylab("Median All.Cost") + theme(axis.text.y=element_text(size=9)) + scale_fill_jco()
#median all costs by cause category
x %>% group_by(Cause.Category) %>% summarise(med=median(All.Costs)) %>% ggplot() + geom_col(aes(x = reorder(Cause.Category, med), y= med, fill=Cause.Category)) + theme_classic() + coord_flip() + theme(legend.position="none") + xlab("Cause.Category") + ylab("Median All.Cost") + theme(axis.text.y=element_text(size=7)) + scale_fill_jco()


#check correlation
dfn = df %>% select (Unintentional.Release_barrels, Liquid.Recovery_barrels, Net.Loss_barrels, All.Costs)
corr=cor(dfn)
corrplot(corr,method="number")

#drop abs with outliers in net loss barrels(NLB) 
cc1=x
zscore2=scores(cc1$Net.Loss_barrels)
nlboutlier = zscore2>3 | zscore2< -3
cc1$nlboutlier = nlboutlier
nlboutlier=cc1[cc1$nlboutlier ==T,]
dim(nlboutlier) 
cc = cc1[cc1$nlboutlier ==F,]
dim(cc) 

#new categorical variable as target variable
AC= cc$All.Costs
high.cost = ifelse(AC>100000, 1,0)
cc$high.cost = high.cost
cc$high.cost = as.factor(cc$high.cost)
Hmisc:: describe(cc$high.cost)

#select features 
df1 = cc %>% select(Accident.Year, Pipeline.Type, Liquid.Type, Accident.State, Cause.Category, Unintentional.Release_barrels, Liquid.Recovery_barrels, Net.Loss_barrels, Liquid.Ignition, Liquid.Explosion, high.cost, Accident.Latitude, Accident.Longitude)

#test and train 80:20
set.seed(111)
y1=sample(1:2736,2189)
xtrain=df1[y1,]
xtest=df1[-y1,]
Hmisc:: describe(df1$high.cost)
Hmisc:: describe(xtrain$high.cost)
Hmisc:: describe(xtest$high.cost)

#prediction
#logistic regression
#model with lowest AIC and highest pseudo r2 out of 7 models tested
model7= glm(high.cost ~ Net.Loss_barrels + Liquid.Ignition + Liquid.Explosion + Pipeline.Type + Liquid.Type + Cause.Category + Accident.Latitude + Accident.Longitude, data=xtrain, family="binomial")
summary(model7)
pR2(model7) 
anova(model7,test="Chisq")
#prediction
probb=predict(model7,xtest,type="response")
probb1=rep(0,547)
probb1[probb>0.5]=1
table(probb1,xtest$high.cost)
confusionMatrix(as.factor(probb1),xtest$high.cost)

#decision tree
treemodel1 = tree(high.cost ~ Net.Loss_barrels + Liquid.Ignition + Liquid.Explosion + Pipeline.Type + Liquid.Type + Cause.Category + Accident.Longitude + Accident.Longitude, xtrain)
plot(treemodel1)
text(treemodel1,pretty=0)
#prediction
treepred=predict(treemodel1, xtest,type="class")
confusionMatrix(treepred, xtest$high.cost)

#clustering 
#subset without outliers in All.Costs, Net.Loss.Barrel and Liquid.Recovery.Barrels
cc2 = x
zscore1=scores(cc2$All.Costs)
costoutlier = zscore1>3 | zscore1< -3
cc2$costoutlier = costoutlier 
zscore2=scores(cc2$Net.Loss_barrels)
nlboutlier = zscore2>3 | zscore2< -3
cc2$nlboutlier= nlboutlier
zscore3=scores(cc2$Liquid.Recovery_barrels)
lrboutlier = zscore3>3 | zscore3< -3
cc2$lrboutlier = lrboutlier
cx= cc2[cc2$costoutlier==F & cc2$nlboutlier==F & cc2$lrboutlier==F, ]
dim(cx)
#scaling
cx2= cx %>% select (All.Costs, Net.Loss_barrels, Liquid.Recovery_barrels)
c2scaled= scale(cx2)
head(c2scaled)

#check optimal clusters - hierarchical clustering
set.seed(1234)
h2= hclust(dist(c2scaled))
plot(h2)
#4 clusters
dend_obj=as.dendrogram(h2)
dend4= color_branches(dend_obj,k=4)
plot(dend4)

#check optimal clusters - elbow method
set.seed(123)
fviz_nbclust(c2scaled,kmeans,method="wss")
#k means clustering with 4 clusters
set.seed(123)
km4= kmeans(c2scaled,centers=4,nstart=50)
km4
fviz_cluster(km4, data=c2scaled, labelsize=0) #cluster plot
with(cc2,pairs(c2scaled,col=(1:4)[km4$cluster])) #pair plot

#summary by cluster id
cx$clusterid=km4$cluster
cx$clusterid=as.factor(cx$cluster)
dfc4= cx %>% select (Liquid.Type,Pipeline.Type,Cause.Category,clusterid)
by(dfc4,dfc4$clusterid,summary)

#Sankey Diagram 4 levels 
links <-
  df %>%
  mutate(row = row_number()) %>%
  gather('column', 'source', -row) %>%
  mutate(column = match(column, names(df))) %>%
  group_by(row) %>%
  arrange(column) %>%
  mutate(target = lead(source)) %>%
  ungroup() %>%
  filter(!is.na(target))
links <-
  links %>%
  mutate(source = paste0(source, '_', column)) %>%
  mutate(target = paste0(target, '_', column + 1)) %>%
  select(source, target)
nodes <- data.frame(name = unique(c(links$source, links$target)))
links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1
links$value <- 1
nodes$name <- sub('_[0-9]+$', '', nodes$name)

sankeyNetwork(Links = links, Nodes = nodes,
 Source = "source", Target = "target",
 Value = "value", NodeID = "name",
 fontSize= 12, nodeWidth = 30)


#Sankey Diagram 3 levels 
s1= df1%>% group_by (Pipeline.Type, Liquid.Type,Cause.Category, high.cost)%>% summarise(Freq=n())
nodes = data.frame("name"=c("Pipeline Accidents","Aboveground","Tank","Underground","CO2", "Crude Oil", "HVL","Refined"))
links = as.data.frame(matrix(c(
0,1,1474,
0,2,299,
0,3,963,
1,4,23,
1,5,721, 
1,6,243,
1,7,487,
2,5,154,
2,6,5,
2,7,140,
3,4,15,
3,5,496,
3,6,153,
3,7,299),
byrow = TRUE, ncol=3))
names(links)=c("source","target","value")

sankeyNetwork(Links = links, Nodes = nodes,
 Source = "source", Target = "target",
 Value = "value", NodeID = "name",
 fontSize= 12, nodeWidth = 30)
