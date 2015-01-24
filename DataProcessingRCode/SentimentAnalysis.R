
###### Sentiment Analysis #######
#################################

###calculte sentiment score####

library(plyr)
library(stringr)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


##-----aggregate the sentiment score-------##

##find out the missing values after running the model
missing<-meta.pg$MetaID[!(meta.pg$MetaID %in% meta2$MetaID)]
partition<-sample.pg.source[-missing,]
meta.pg[missing,]  #33064,43348
text.pg[missing]  #they are almost empty!!

load("sentiment_full.Rda")

##match the sentiment score with model results
dim(sentiment_full2)
sentiment_full2[1,]
colnames(sentiment_full2)<-c("score","n.pos","n.neg","n.total","text")
sentiment<-sentiment_full2[-missing,]
sentiment<-as.data.frame(sentiment)
names(sentiment)


##convert the score to numeric
#test<-sentiment$sentiment_score[sentiment$sentiment_score=="-2"][1]
sentiment$score<-as.numeric(as.character(sentiment$score))
sentiment$n.total<-as.numeric(as.character(sentiment$n.total))
sentiment$n.pos<-as.numeric(as.character(sentiment$n.pos))
sentiment$n.neg<-as.numeric(as.character(sentiment$n.neg))
sentiment$topic<-as.factor(topic.finding)
names(sentiment)
range(sentiment$score)

####analysis

#calculate topic sentiment as a weighted averag of document sentiment
topicScore<-sapply(levels(data.final$topic),FUN=function(x){
  sum(data.final$score[data.final$topic==x]*data.final$n.total[data.final$topic==x])/sum(data.final$n.total[data.final$topic==x])
})    
topicScore
save(topicScore,file="topicScore.Rda")
load("topicScore.Rda")


#standardize the topic score
class(topicScore)
mean<-mean(topicScore)
var<-mean(topicScore^2)-(mean(topicScore))^2
sig<-sqrt(var)
topicScore.st<-(topicScore-mean)/sig
save(topicScore.st,file="topicScore_st.Rda")
load("topicScore_st.Rda")

#frequency matrix
freq<-sapply(levels(data.final$topic),FUN=function(x){
  length(data.final$score[data.final$topic==x])
})
save(freq,file="freq.Rda")

#take into count only positive and negative
#sapply(levels(sentiment$topic),FUN=function(x)sum(ifelse(sentiment$score[sentiment$topic==x]>0,1,-1)))  

#partition into parties

data<-cbind(sentiment,partition)
rownames(data)<-NULL
names(data)

data.final<-data[,c("score","topic","text","party","gender","locationName","date","n.total")]
save(data.final,file="data_final.Rda")

#check number of positive and negative documents
sum(data.final$score>=0)
negindex<-which(data.final$score<0)
data$text[head(negindex)]
sum(data.final$score<0)

##analyze party

data.final$party<-as.factor(as.character(data.final$party))
levels(data.final$party)

topicScore.d<-with (subset(data.final,party=="democrat"),
                    sapply(levels(topic),FUN=function(x)sum(score[topic==x]*n.total[topic==x])/sum(n.total[topic==x])))

topicScore.r<-with (subset(data.final,party=="republican"),
                    sapply(levels(topic),FUN=function(x)sum(score[topic==x]*n.total[topic==x])/sum(n.total[topic==x])))

topicScore.i<-with (subset(data.final,party=="independent"),
                    sapply(levels(topic),FUN=function(x)sum(score[topic==x]*n.total[topic==x])/sum(n.total[topic==x])))
topicScore.party<-cbind(topicScore.d,topicScore.r,topicScore.i)
save(topicScore.party,file="topicScore_party.Rda")

#standardize the score
class(topicScore.party)
mean.party<-mean(topicScore.party)
var.party<-mean(topicScore.party^2)-(mean(topicScore.party))^2
sig.party<-sqrt(var.party)
topicScore.party.st<-(topicScore.party-mean.party)/sig.party
save(topicScore.party.st,file="topicScore_party_st.Rda")

#compute the frequency matrix
freq.d<-with (subset(data.final,party=="democrat"),
              sapply(levels(topic),FUN=function(x)length(score[topic==x])))

freq.r<-with (subset(data.final,party=="republican"),
              sapply(levels(topic),FUN=function(x)length(score[topic==x])))

freq.i<-with (subset(data.final,party=="independent"),
              sapply(levels(topic),FUN=function(x)length(score[topic==x])))

freq.party<-cbind(freq.d,freq.r,freq.i)
save(freq.party,file="freq_party.Rda")

##analyze gender
data.final$gender<-as.factor(data.final$gender)
levels(data.final$gender)

topicScore.f<-with (subset(data.final,gender=="female"),
                    sapply(levels(topic),FUN=function(x)sum(score[topic==x]*n.total[topic==x])/sum(n.total[topic==x])))

topicScore.m<-with (subset(data.final,gender=="male"),
                    sapply(levels(topic),FUN=function(x)sum(score[topic==x]*n.total[topic==x])/sum(n.total[topic==x])))

topicScore.gender<-cbind(topicScore.f,topicScore.m)
save(topicScore.gender,file="topicScore_gender.Rda")

#standardize the score
class(topicScore.gender)
mean.gender<-mean(topicScore.gender)
var.gender<-mean(topicScore.gender^2)-(mean(topicScore.gender))^2
sig.gender<-sqrt(var.gender)
topicScore.gender.st<-(topicScore.gender-mean.gender)/sig.gender
save(topicScore.gender.st,file="topicScore_gender_st.Rda")

#compute frequency matrix
freq.f<-with (subset(data.final,gender=="female"),
              sapply(levels(topic),FUN=function(x)length(score[topic==x])))

freq.m<-with (subset(data.final,gender=="male"),
              sapply(levels(topic),FUN=function(x)length(score[topic==x])))

freq.gender<-cbind(freq.f,freq.m)
save(freq.gender,file="freq_gender.Rda")

##analyze region
levels(data.final$locationName)

topicScore.location<-sapply(levels(data.final$locationName),function(x){
  with (subset(data.final,locationName==x),
        sapply(levels(topic),FUN=function(y)sum(score[topic==y]*n.total[topic==y])/sum(n.total[topic==y])))
})
View(topicScore.location)
topicScore.location[5,13]<-0
save(topicScore.location,file="topicScore_location.Rda")

#standardize the score
class(topicScore.location)
mean.location<-mean(topicScore.location)
var.location<-mean(topicScore.location^2)-(mean(topicScore.location))^2
sig.location<-sqrt(var.location)
topicScore.location.st<-(topicScore.location-mean.location)/sig.location
save(topicScore.location.st,file="topicScore_location_st.Rda")

#frequency matrix
freq.location<-sapply(levels(data.final$locationName),function(x){
  with (subset(data.final,locationName==x),
        sapply(levels(topic),FUN=function(y)length(score[topic==y])))
})
save(freq.location,file="freq_location.Rda")
View(freq.location)


##analyze time
head(data.final$date)
#install.packages("zoo")
library(zoo)
#install.packages("chron")
library(chron)

##month analysis
month<-with(month.day.year(data.final$date),zoo(cbind(day,month,year)))
data.final$month<-month$month
data.final$month1<-as.factor(data.final$month)
levels(data.final$month1)
which(data.final$n.total==NaN)
data.final$score[data.final$month1=="11"][1:10]
which(data.final$score==NaN)

topicScore.Month<-sapply(levels(data.final$topic),function(x){
  with (subset(data.final,topic==x),
        sapply(levels(month1),FUN=function(y)sum(score[month1==y]*n.total[month1==y])/sum(n.total[month1==y])))
})
save(topicScore.Month,file="topicScore_Month.Rda")

##standardize the score
class(topicScore.Month)
mean.Month<-mean(topicScore.Month)
var.Month<-mean(topicScore.Month^2)-(mean(topicScore.Month))^2
sig.Month<-sqrt(var.Month)
topicScore.Month.st<-(topicScore.Month-mean.Month)/sig.Month
save(topicScore.Month.st,file="topicScore_Month_st.Rda")

##compute frequency matrix
freq.Month<-sapply(levels(data.final$topic),function(x){
  with (subset(data.final,topic==x),
        sapply(levels(month1),FUN=function(y)length(score[month1==y])))
})
save(freq.Month,file="freq_Month.Rda")

##date analysis
head(data.final$date)
class(data.final$date)
range(data.final$date)
unique(data.final$date)

topicScore.date<-sapply(levels(data.final$topic),function(x){
  with (subset(data.final,topic==x),
        sapply(unique(date),FUN=function(y)sum(score[date==y]*n.total[date==y])/sum(n.total[date==y])))
})
class(topicScore.date)
sapply(1:10,function(i)length(topicScore.date[[i]]))

save(topicScore.date,file="topicScore_date.Rda")

names.date<-sapply(levels(data.final$topic),function(x){
  with (subset(data.final,topic==x),unique(date))
})
class(names.date)

##standardize the score
require("Rcpp")
require(plyr)

topicScore.date.st<-lapply(1:10,FUN=function(i){
  x<-topicScore.date[[i]]
  mean.x<-mean(x)
  var.x<-mean(x^2)-(mean(x))^2
  sig.x<-sqrt(var.x)
  x.st<-(x-mean.x)/sig.x
  names(x.st)<-names.date[[i]]
  return(x.st)
})

class(topicScore.date.st)
topicScore.date.st[[1]]

save(topicScore.date.st,file="topicScore_date_st.Rda")

##compute frequency matrix
freq.date<-sapply(levels(data.final$topic),function(x){
  with (subset(data.final,topic==x),
        sapply(unique(date),FUN=function(y)length(score[date==y])))
})
class(freq.date)
freq.date[[1]]

save(freq.Month,file="freq_date.Rda")






#----------------------------------------
save(data.final,file="data_final.Rda")
load("data_final.Rda")
names(data.final)

data.final$text[1]
