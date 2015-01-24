
#########################################################

##--         Exploratory Analysis                    --##

#########################################################

rm(list=ls(all=T))

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
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
require(scales)
require(ggplot2)
require(RColorBrewer)
color<-brewer.pal(9,"Set1")

load("final_raw.Rda")
load("source.Rda")
load("final_clean.Rda")

##check party,gender subset distribution

names(final.raw2)  #date, locationName, current,government, ortType,stateDelegation, organisations, Placesx<-
#final.raw2$date<-as.Date(as.character(final.raw2$date),"%Y%m%d")
#x<-str_replace_all(final.raw2$gender,")","")
#x[x==""]<-NA
#final.raw2$gender<-x
#save(final.raw2,file="final_clean.Rda")


table(final.raw2$date)
g<-ggplot(data=final.raw2,aes(x=date)) + geom_bar(binwidth=0.1) +
  scale_x_date(breaks="1 week",labels=date_format("%m-%Y")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0)) +
  ggtitle("Media Releases by Date") +
  theme(plot.title=element_text(size=rel(2)))
g1<-ggplot(data=final.raw2,aes(x=date)) + geom_bar(binwidth=0.1) +
  scale_x_date(breaks="4 weeks",labels=date_format("%m-%Y")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0)) +
  ggtitle("Media Releases by Date") +
  theme(plot.title=element_text(size=rel(2)))
g2<-ggplot(data=final.raw2,aes(x=date)) + geom_density(fill=color[1],alpha=0.5) +
  scale_x_date(breaks="4 week",labels=date_format("%m-%Y")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0)) +
  ggtitle("Density of Media Releases by Month") +
  theme(plot.title=element_text(size=rel(2)))

jan<-final.raw2$date[final.raw2$date<="2012-01-31"]
feb<-final.raw2$date[final.raw2$date>"2012-01-31" & final.raw2$date<="2012-02-29"]
mar<-final.raw2$date[final.raw2$date>"2012-02-29" & final.raw2$date<="2012-03-31"]
apr<-final.raw2$date[final.raw2$date>"2012-03-31" & final.raw2$date<="2012-04-30"]
may<-final.raw2$date[final.raw2$date>"2012-04-30" & final.raw2$date<="2012-05-31"]
jun<-final.raw2$date[final.raw2$date>"2012-05-31" & final.raw2$date<="2012-06-30"]
jul<-final.raw2$date[final.raw2$date>"2012-06-30" & final.raw2$date<="2012-07-31"]
aug<-final.raw2$date[final.raw2$date>"2012-07-31" & final.raw2$date<="2012-08-31"]
sep<-final.raw2$date[final.raw2$date>"2012-08-31" & final.raw2$date<="2012-09-30"]
oct<-final.raw2$date[final.raw2$date>"2012-09-30" & final.raw2$date<="2012-10-31"]
nov<-final.raw2$date[final.raw2$date>"2012-10-31" & final.raw2$date<="2012-11-30"]
dec<-final.raw2$date[final.raw2$date>"2012-11-30" & final.raw2$date<="2012-12-31"]

par(mfrow=c(3,4))
plot(table(jan))
plot(table(feb))
plot(table(mar))
plot(table(apr))
plot(table(may))
plot(table(jun))
plot(table(jul))
plot(table(aug))
plot(table(sep))
plot(table(oct))
plot(table(nov))
plot(table(dec))

#our sample
table(final.raw2$party)
partyindex<-which(final.raw2$party!="none")
table(final.raw2$gender)
genderindex<-which(!is.na(final.raw2$gender))
head(genderindex)
pgindex<-which(final.raw2$party!="none" & (!is.na(final.raw2$gender)))
head(pgindex)
length(pgindex)

sample.party<-final.raw2[partyindex,]
sample.gender<-final.raw2[genderindex,]
sample.pg<-final.raw2[pgindex,]   ##this is our subsample with complete info
nrow(sample.pg)
table(sample.pg$party)
table(sample.pg$gender)

g.s<-ggplot(data=sample.pg,aes(x=date)) + geom_bar(binwidth=0.1) +
  scale_x_date(breaks="1 week",labels=date_format("%m-%Y")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0)) +
  ggtitle("Sample Media Releases by Date") +
  theme(plot.title=element_text(size=rel(2)))

g1.s<-ggplot(data=sample.pg,aes(x=date)) + geom_bar(binwidth=0.1) +
  scale_x_date(breaks="4 weeks",labels=date_format("%m-%Y")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0)) +
  ggtitle("Sample Media Releases by Date") +
  theme(plot.title=element_text(size=rel(2)))

g2.s<-ggplot(data=sample.pg,aes(x=date)) + geom_density() +
  scale_x_date(breaks="4 week",labels=date_format("%m-%Y")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0)) +
  ggtitle("Sample Density of Media Releases by Month") +
  theme(plot.title=element_text(size=rel(2)))

##compare

g2 + geom_density(data=sample.pg,aes(x=date),fill=color[2],alpha=0.5) +
  scale_x_date(breaks="4 week",labels=date_format("%m-%Y")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0)) +
  ggtitle("Compare Population and Sample by Date") +
  theme(plot.title=element_text(size=rel(2)))

table(final.raw2$locationName)
par(mfrow=c(1,2))
plot(final.raw2$locationName)
plot(sample.pg$locationName)

ggplot(data=final.raw2) + geom_histogram(aes(x=locationName),fill=color[1],alpha=0.2) +
  geom_histogram(data=sample.pg,aes(x=locationName),fill=color[2],alpha=0.4) +
  theme(axis.text.x=element_text(angle=-90,hjust=0))

ggplot(data=sample.pg) + geom_histogram(aes(x=locationName),fill=color[2],alpha=0.2) +
  theme(axis.text.x=element_text(angle=-90,hjust=0))

par(mfrow=c(1,2))
plot(final.raw2$orgType)
table(final.raw2$orgType)
plot(sample.pg$orgType) 
table(sample.pg$orgType)

par(mfrow=c(1,2))
plot(final.raw2$government)
table(final.raw2$government)
plot(sample.pg$government) 
table(sample.pg$government)

head(pgindex)

table(sample.pg$locationName[sample.pg$government=="Le"
                             table(sample.pg$locationName,sample.pg$party)
                             
                             
                             