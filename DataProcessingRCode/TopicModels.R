###### Topic Models ###########
###############################

rm(list=ls())
getwd()

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if(!require(stm)){
  install.packages("stm")
  library(stm)
}
if(!require(igraph)){
  install.packages("igraph")
  library(igraph)
}
if(!require(SnowballC)){
  install.packages("SnowballC")
  library(SnowballC)
}
if(!require(tm)){
  install.packages("tm")
  library(tm)
}
if(!require(huge)){
  install.packages("huge")
  library(huge)
}
if(!require(glmnet)){
  install.packages("glmnet")
  library(glmnet)
}
if(!require(Matrix)){
  install.packages("Matrix")

  library("Matrix")
}
if(!require(textir)){
  install.packages("textir")
  library("textir")
}

##load in data

load("final_raw.Rda")
load("final_clean.Rda")

##select a sub-sample with party gender info complete

pgindex<-which(final.raw2$party!="none" & (!is.na(final.raw2$gender)))
sample.pg<-final.raw[pgindex,]
sample.pg.source<-final.raw2[pgindex,]
names(sample.pg.source)

#set.seed(12345)
#subindex<-sample(1:67680,size=20000,replace=FALSE)
#sample<-sample.pg[subindex,]
#meta.sample<-sample.pg.source[subindex,c(8,11)]
#meta.sample$MetaID<-1:20000

meta.pg<-sample.pg.source[,c(8,11)]
sample.pg.source$ID<-1:67680
meta.pg$MetaID<-1:67680

##extract our text
expression="<[^>]*.>"
clean=function(x){
  t=gsub(expression,"",x)
  t2=gsub("(\n)","",t)
  t3=gsub("nbsp","",t2)
  return(t3)
}
#text.sample=sapply(sample[,13],clean)
text.pg=sapply(sample.pg[,13],clean)

###run the model
temp.sample<-textProcessor(documents=text.pg,metadata=meta.pg)
meta<-temp.sample$meta
vocab<-temp.sample$vocab
docs<-temp.sample$documents
out <- prepDocuments(docs, vocab, meta)
#docs1<-out$documents
#vocab1<-out$vocab
#meta1 <-out$meta
docs2<-out$documents
vocab2<-out$vocab
meta2 <-out$meta

length(meta2$party)
head(meta2$party)
length(meta2$gender)

length(docs2)
length(vocab2)
length(meta2)

#save(docs1,file="docs20000.Rda")
#save(vocab1,file="vocab20000.Rda")
#save(meta1,file="meta20000.Rda")

save(docs2,file="docs67680.Rda")
load("docs67680.Rda")
save(vocab2,file="vocab67680.Rda")
load("vocab67680.Rda")
save(meta2,file="meta67680.Rda")
load("meta67680.Rda")
load("meta67680.Rda")

##run the STM model
releasePrevFit2 <- stm(docs2,vocab2,K=10,prevalence=~meta2$party+meta2$gender)
save(releasePrevFit2,file="Model67680.Rda")
load("Model67680.Rda")

#releasePrevFit3 <- stm(docs2,vocab2,K=5,prevalence=~meta2$party+meta2$gender)
#save(releasePrevFit3,file="Model67680_5.Rda")


##select model
#exampleSelect <- selectModel(docs1,vocab1,K=10,prevalence=~meta1$party+meta1$gender, runs=10)
#plotModels(exampleSelect) ##find the model k at the right top corner then use exampleSelect$runout[[k]] to substitute previous Fit model

##find the top words for each topic 
TopicWord<-labelTopics(releasePrevFit2,topics=c(1:10))
save(TopicWord,file="topicWord.Rda")

###find the most relevant document to topic1: findtoughts has two list one is index and one is docs
topic5.release=findThoughts(releasePrevFit2,texts=text.sample[-c(1,2)],topics=7,n=5)
text.sample[5867]

##find the topic for each document

topic.finding=apply(releasePrevFit2$theta,1,which.max)
table(topic.finding)


topic10.thought=findThoughts(releasePrevFit,texts=,topics=10,n=3)

##plot the comparison of different topics in terms of the frequency of the top words 
plot.STM(releasePrevFit,type="perspective",topics=c(1,2))

##plot the topic and their top words and the topics are ranked by the frequency across the documents 
plot.STM(releasePrevFit,type="summary")

##
prep <- estimateEffect(c(1) ~ government, releasePrevFit,metadata=meta)
plot.estimateEffect(prep, covariate="government", model=releasePrevFit,method="pointestimate")

##
mod.out.corr<-topicCorr(releasePrevFit)
plot.topicCorr(mod.out.corr)

