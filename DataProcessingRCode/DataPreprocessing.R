########DataViz Final Projects#########
######### Data Preprocessing ##########

rm(list=ls())
if(!require(rjson)){
  install.packages("rjson")
  library(rjson)
}
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
if(!require(textir)){
  install.packages("textir")
  library("textir")
}


###########################################################
######----preliminary data cleaning--------################
###########################################################


#####---------convert json file into data frame--------#####

#for read in data, generate all the paths
path <- "C:/Users/Rongyao/Documents/GitHub/DataVizFinal/Release_2012"
filenames <- paste0(path, "/",list.files(path, pattern = "json"))

#extract the release date from file name
Release<-list.files(path,pattern="json")
Release<-sapply(Release,function(x)str_sub(x,13,20))

#read in json file, convert into list
mydata = lapply(filenames, function(t) {
  dum=fromJSON(paste(readLines(t), collapse=""))
  return(dum)}) 

#convert each release file into a data frame with tags as columns and articles as rows
#the date information extracted previously will be appended to the end of the columns
#the resulting list will have 366 elements, each of which a data frame  
d=list()
for (q in c(1:366)){
  d[[q]]=data.frame()
  date<-Release[q]
  for (i in c(1:length(mydata[[q]]))){ 
    for (j in c(1:14)){
      if (length(unlist(mydata[[q]][[i]][j]))==1){
        d[[q]][i,j]=unlist(mydata[[q]][[i]][j])
      }else{
        d[[q]][i,j]=paste(mydata[[q]][[i]][j],collapse="@")
      }
    }
    d[[q]][i,15]<-date
  }
}

#stack all the list together
#the resulting data frame will have each article has a row and tags as columns
#tag names are added as column name
final.raw=rbindlist(d)
name=c(names(mydata[[1]][[1]]),"date")
setnames(final.raw,name)
final.raw=as.data.frame(final.raw)

#save the data frame
save(final.raw,file="final_raw.Rda")


load("final_raw.Rda")
View(final.raw)
table(s.8$party)

#####---------clean the data frame--------#####

load("final_raw.Rda")
str(final.raw)
View(final.raw[100:110,])

#check the number of tags in each source column
length<-numeric()
for (i in 1:nrow(final.raw)){
  length[i]<-length(str_locate_all(final.raw[i,"source"],",")[[1]][,1])
}
unique(length)   #7,8,9,10

#####test on a short example with all possible situations####
#############################################################
example<-final.raw$source[c(which(length==7)[1],which(length==8)[1],which(length==9)[1],which(length==10)[1])]
class(example)
View(example)
x<-str_split(example,pattern="\",")    #we have sublist of either length 6 or 7 
class(x)      

#deal with different length
x<-sapply(x,FUN=function(i){
  i.v<-unlist(i)
  if (length(i.v)==6){
    i.v<-c(i.v,i.v[6])
    i.v[6]<-NA
  }
  return(i.v)
})
class(x)
View(x)

#reformat the data, so that each column is a tag
x<-sapply(1:nrow(x),function(i)x[i,])

#split the columns that have two variables
#but before that, pick out those that are completely missing
x[,1]<-unlist(str_extract_all(x[,1],"name = .*"))
new<-ldply(str_split(x[,3],pattern=", "),function(i)data.frame(s3=i[1],s4=i[2]))
x<-cbind(x[,1:2],new,x[,5:7])
View(x)

#change column names
names(x)<-c("name","locationName","current","government","orgType","stateDelegation","gender")

#remove name tags in the content
x.1<-str_replace_all(as.matrix(x),"[[:alpha:]]+ = ","")
View(x.1)
#further cleaning, remove \",), and leading and trailing white spaces
x.1<-str_replace_all(x.1,"\"","")
x.1[,7]<-str_replace_all(x.1[,7],")","")
x.1<-str_trim(x.1,side="both")

#Now this is clean
View(x.1)

#####apply to the whole dataset ####
#############################################################

s<-final.raw$source
length(s)
class(s)
View(s)
s.1<-str_split(s,pattern="\"[[:blank:]]{0,1},")    #we have sublist of either length 6,7 or 8
class(s.1)
unique(sapply(s.1,length))

#deal with different length
s.2<-sapply(s.1,FUN=function(i){
  i.v<-unlist(i)
  if (length(i.v)==7){
    i.v.new<-i.v
  }else if (length(i.v)==6){
    i.v.new<-c(i.v,i.v[6])
    i.v.new[6]<-NA
  }else {
    i.v[1]<-paste(i.v[1],i.v[2],sep="")
    i.v[2:7]<-i.v[3:8]
    i.v.new<-i.v[1:7]
  }
  return(i.v.new)
})
class(s.2)   #matrix
View(s.2)

#reformat the data, so that each column is a tag
s.3<-t(s.2)
View(s.3)

#split the columns that have two variables
#but before that, pick out those that are completely missing
require(plyr)
s.3[,1]<-unlist(str_extract_all(s.3[,1],"name = .*"))
s.3<-as.data.frame(s.3)
new<-ldply(str_split(s.3[,3],pattern=", "),function(i)data.frame(s3=i[1],s4=i[2]))
s.4<-cbind(s.3[,1:2],new,s.3[,4:7])
View(s.4)

#change column names
names(s.4)<-c("name","locationName","current","government","party","orgType","stateDelegation","gender")

#remove name tags in the content
s.5<-str_replace_all(as.matrix(s.4),"[[:alpha:]]+ = ","")
head(s.5)
#further cleaning, remove \",), and leading and trailing white spaces
s.6<-str_replace_all(s.5,"\"","")
s.6[,7]<-str_replace_all(s.6[,7],")","")
s.7<-str_trim(s.6,side="both")
s.8<-as.data.frame(s.7)
#Now this is clean
View(s.8[1:10,])

save(s.8,file="source.Rda")

load("source.Rda")
View(s.8)

######---clean other column-----####
example<-final.raw$keywords[1:10]
k.1<-str_replace_all(example,"c\\(","")
k.2<-str_replace_all(k.1,")","")
k.3<-str_replace_all(k.2,"\"","")
View(k.3)

cleanother<-function(x){
  x<-str_replace_all(x,"c\\(","")
  x<-str_replace_all(x,")","")
  x<-str_replace_all(x,"\"","")
}

names(final.raw)
pp<-sapply(final.raw[,c("keywords","names","organisations","places")],cleanother)
View(pp)
head(pp)
pp<-as.data.frame(pp)
names(pp)<-c("keywords","names","organisations","places")
or<-str_replace_all(pp$organisations,"^list\\($","")
pp$organisations<-or

#####combine cleaned data with the original dataset
names(final.raw)

final.raw1<-final.raw[,c(1,14,15)]
final.raw2<-cbind(final.raw1,s.8,pp)

save(final.raw2,file="final_clean.Rda")
load("C:/Users/Rongyao/Documents/GitHub/DataVizFinal/final_clean.Rda")
View(final.raw2[1:6,])

########################################
####----check data & debug----##########
########################################

names(final.raw2)
unique(final.raw2$orgType)
which(final.raw2$orgType=="South Carolina")
View(final.raw2[which(final.raw2$orgType=="South Carolina"),])
View(final.raw[which(final.raw2$orgType=="South Carolina"),])

bug<-final.raw$source[which(final.raw2$orgType=="South Carolina")]
bug.1<-str_split(bug,pattern="\"[[:blank:]]{0,1},") 