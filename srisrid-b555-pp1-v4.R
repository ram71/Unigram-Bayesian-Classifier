# install.packages("ggplot2")
library(ggplot2)

# set working directory
setwd("E:\\iu\\fa18\\b555\\pp01\\pp1data")


# task 1
# read train data
trainwords = readLines("training_data.txt",warn=FALSE)
trainwords = strsplit(trainwords,split=" ")
trainwords = unlist(trainwords)

# read test data
testwords = readLines("test_data.txt",warn=FALSE)
testwords = strsplit(testwords,split=" ")
testwords = unlist(testwords)

# prepare overall dictionary
dict = data.frame("words"=unique(c(trainwords,testwords)))
dict$alpha = 2

# mle
mle.df = function(trainwords,testwords,dict)
{
  dftrain = data.frame("trainwords"=trainwords,"traincount"=1)
  dftrain = aggregate(traincount~trainwords,FUN=sum,data=dftrain)
  N = sum(dftrain$traincount)
  mle = function(mk,N)
  {
    return(mk/N)
  }
  dftrain$p.estimate = mle(dftrain$traincount,N)
  dftrain = dftrain[,c("trainwords","p.estimate")]
  dftest = data.frame("testwords"=testwords,"testcount"=1)
  dftest = aggregate(testcount~testwords,FUN=sum,data=dftest)
  dftest = merge(dftest,dftrain,by.x="testwords",by.y="trainwords")
  return(dftest)
}

map.df = function(trainwords,testwords,dict)
{
  dftrain = data.frame("trainwords"=trainwords,"traincount"=1)
  dftrain = aggregate(traincount~trainwords,FUN=sum,data=dftrain)
  dftrain = merge(dftrain,dict,by.x="trainwords",by.y="words",all.y=TRUE)
  dftrain[is.na(dftrain$traincount),"traincount"] = 0
  N = sum(dftrain$traincount)
  K = length(dftrain$trainwords)
  alpha0 = sum(dftrain$alpha)
  map = function(mk,alphak,N,alpha0,K)
  {
    return((mk+alphak-1)/(N+alpha0-K))
  }
  dftrain$p.estimate = map(dftrain$traincount,dftrain$alpha,N,alpha0,K)
  dftrain = dftrain[,c("trainwords","p.estimate")]
  dftest = data.frame("testwords"=testwords,"testcount"=1)
  dftest = aggregate(testcount~testwords,FUN=sum,data=dftest)
  dftest = merge(dftest,dftrain,by.x="testwords",by.y="trainwords")
  return(dftest)
}

pred.df = function(trainwords,testwords,dict)
{
  dftrain = data.frame("trainwords"=trainwords,"traincount"=1)
  dftrain = aggregate(traincount~trainwords,FUN=sum,data=dftrain)
  dftrain = merge(dftrain,dict,by.x="trainwords",by.y="words",all.y=TRUE)
  dftrain[is.na(dftrain$traincount),"traincount"] = 0
  N = sum(dftrain$traincount)
  K = length(dftrain$trainwords)
  alpha0 = sum(dftrain$alpha)
  pred = function(mk,alphak,N,alpha0)
  {
    return((mk+alphak)/(N+alpha0))
  }
  dftrain$p.estimate = pred(dftrain$traincount,dftrain$alpha,N,alpha0)
  dftrain = dftrain[,c("trainwords","p.estimate")]
  dftest = data.frame("testwords"=testwords,"testcount"=1)
  dftest = aggregate(testcount~testwords,FUN=sum,data=dftest)
  dftest = merge(dftest,dftrain,by.x="testwords",by.y="trainwords")
  return(dftest)
}

perplexity = function(dftest)
{
  avg.sum.log.prob = sum(dftest$testcount * log(dftest$p.estimate))
  return(exp((-1/sum(dftest$testcount))*avg.sum.log.prob))
}

traindata = list()
nums = c(128,64,16,4,1)
for(i in 1:5)
{
  traindata[[i]] = trainwords[1:(length(trainwords)/nums[i])]
}

mle.perp = rep(0,length(nums))
map.perp = rep(0,length(nums))
pred.perp = rep(0,length(nums))
mle.perp.train = rep(0,length(nums))
map.perp.train = rep(0,length(nums))
pred.perp.train = rep(0,length(nums))

for(i in 1:5)
{
  mle.perp.temp = mle.df(traindata[[i]],testwords,dict)
  mle.perp.temp = mle.perp.temp[mle.perp.temp$p.estimate!=0,]
  mle.perp[i] = perplexity(mle.perp.temp)
  map.perp[i] = perplexity(map.df(traindata[[i]],testwords,dict))
  pred.perp[i] = perplexity(pred.df(traindata[[i]],testwords,dict))
  
  mle.perp.temp.train = mle.df(traindata[[i]],traindata[[i]],dict)
  mle.perp.temp.train = mle.perp.temp.train[mle.perp.temp.train$p.estimate!=0,]
  mle.perp.train[i] = perplexity(mle.perp.temp.train)
  map.perp.train[i] = perplexity(map.df(traindata[[i]],traindata[[i]],dict))
  pred.perp.train[i] = perplexity(pred.df(traindata[[i]],traindata[[i]],dict))
}

dfplot = data.frame("train.data"=rep(1/nums*length(trainwords),3),"test.perplexity"=c(mle.perp,map.perp,pred.perp),"train.perplexity"=c(mle.perp.train,map.perp.train,pred.perp.train),"method"=rep(c("1-mle","2-map","3-predictive.distribution"),each=length(nums)))
ggplot(data=dfplot,aes(x=train.data,y=test.perplexity,color=method)) + geom_point() + geom_line() + ggtitle("test data perplexity vs train data size")
ggplot(data=dfplot,aes(x=train.data,y=train.perplexity,color=method)) + geom_point() + geom_line() + ggtitle("train data perplexity vs train data size")


# task 2
log.gamma = function(x)
{
  if(x==1)
  {
    return(0)
  }
  if(x>1)
  {
    return(sum(log(1:(x-1))))
  }
}

log.gamma = function(x)
{
  if(x==1)
  {
    return(0)
  }
  if(x>1)
  {
    return(sum(log(1:(x-1))))
  }
}

log.evidence = function(dict,trainwords)
{
  dftrain = data.frame("trainwords"=trainwords,"traincount"=1)
  dftrain = aggregate(traincount ~ trainwords, FUN = sum, data= dftrain)
  dfwords = merge(dict,dftrain,by.x="words",by.y="trainwords",all.x=TRUE)
  dfwords[is.na(dfwords$traincount),"traincount"] = 0
  s2 = sum(sapply(dfwords$alpha+dfwords$traincount,log.gamma))
  s4 = sum(sapply(dfwords$alpha,log.gamma))
  alpha0 = sum(dfwords$alpha)
  N = sum(dfwords$traincount)
  s1 = log.gamma(alpha0)
  s3 = log.gamma(alpha0+N)
  return(s1+s2-s3-s4)
}

alphap = 1:10
pred.perp = rep(0,length(alphap))
evidence = rep(0,length(alphap))
for(i in 1:length(alphap))
{
  dict$alpha = alphap[i]
  pred.perp[i] = perplexity(pred.df(trainwords[1:5000],testwords,dict))
  evidence[i] = log.evidence(dict,trainwords[1:5000])
}

ggplot(data.frame(evidence,alphap),aes(x=alphap,y=evidence))+geom_point()+geom_line()
ggplot(data.frame(pred.perp,alphap),aes(x=alphap,y=pred.perp))+geom_point()+geom_line()


# task 3
trainwords = readLines("pg121.txt.clean",warn=FALSE)
trainwords = strsplit(trainwords,split=" ")
trainwords = unlist(trainwords)
trainwords = trainwords[trainwords!=""]
length(trainwords)

testwords1 = readLines("pg141.txt.clean",warn=FALSE)
testwords1 = strsplit(testwords1,split=" ")
testwords1 = unlist(testwords1)
testwords1 = testwords1[testwords1!=""]
length(testwords1)

testwords2 = readLines("pg1400.txt.clean",warn=FALSE)
testwords2 = strsplit(testwords2,split=" ")
testwords2 = unlist(testwords2)
testwords2 = testwords2[testwords2!=""]
length(testwords2)

dict = data.frame("words"=unique(c(trainwords,testwords1,testwords2)))
dict$alpha = 2

perp1 = perplexity(pred.df(trainwords,testwords1,dict))
perp2 = perplexity(pred.df(trainwords,testwords2,dict))
c(perp1,perp2)