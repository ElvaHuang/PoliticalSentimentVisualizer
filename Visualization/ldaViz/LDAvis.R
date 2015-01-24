library(LDAvis)
phi=releasePrevFit2[[3]][[1]][[1]]
phi=apply(phi,c(1,2),exp)
phi=t(phi)
phi=phi[,c(10,9,5,2,7,6,3,1,4,8)]
token.frequency=releasePrevFit2$settings$dim$wcounts[,2]

vocab=releasePrevFit2[[5]]

topic.NUM=apply(releasePrevFit2$theta,1,which.max)
topic.proportion=as.numeric(table(topic.NUM)/length(topic.NUM))

releaseData=list(phi,token.frequency,vocab,topic.proportion)
names(releaseData)=c("phi","token.frequency","vocab","topic.proportion")


z <- check.inputs(K=10, W=2238, phi=releaseData$phi, token.frequency=releaseData$token.frequency,
                  vocab=releaseData$vocab, topic.proportion=releaseData$topic.proportion)
for (i in 1:length(z)) assign(names(z)[i], z[[i]])
runVis()
