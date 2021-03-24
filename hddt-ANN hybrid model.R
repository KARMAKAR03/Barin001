library(dplyr)

mydata<-SDP_dataset
mydata
#set.seed(2019)
maxs = apply(mydata, 2, max)
mins = apply(mydata, 2, min)
dat_scaled= as.data.frame(scale(mydata,center = mins, scale = maxs - mins))


df<-dat_scaled[,1:ncol(mydata)-1]


bound <- floor((nrow(dat_scaled)/5)*4)         #define % of training and test set

dat_scaled<- dat_scaled[sample(nrow(dat_scaled)), ]           #sample rows 
train_scaled <- dat_scaled[1:bound, ]              #get training set
test_scaled<- dat_scaled[(bound+1):nrow(dat_scaled), ] 
test_scaled

#set.seed(2021)
#testrows = sample(1:nrow(mydata),replace=F,size=0.2*nrow(mydata))
#testrows 
#train_scaled = dat_scaled[-testrows,]
df1<-train_scaled[,1:ncol(mydata)-1]
df1
#test_scaled = dat_scaled[testrows,]
df2<-test_scaled[,1:ncol(mydata)-1]
df2





# given a feature vector calculate Hellinger distance
# it takes care of both discrete and continuous attributes
# also returns the "value" of the feature that is used as decision criterion
# and the "type" pf the feature which is either factor as numeric
# ONLY WORKS WITH BINARY LABELS


HDDT_dist <- function(f, y, labels=unique(y)) {  
  i1 <- y==labels[1]
  i0 <- y==labels[2]
  T1 <- sum(i1)
  T0 <- sum(i0)
  val <- NA
  hellinger <- -1
  
  cl <- class(f)  
  if(cl=="factor") {    
    for(v in levels(f)) {
      Tfv1 <- sum(i1 & f==v)
      Tfv0 <- sum(i0 & f==v)
      
      Tfw1 <- T1 - Tfv1
      Tfw0 <- T0 - Tfv0
      cur_value <- ( sqrt(Tfv1 / T1) - sqrt(Tfv0 / T0) )^2 + ( sqrt(Tfw1 / T1) - sqrt(Tfw0 / T0) )^2
      
      if(cur_value > hellinger) {
        hellinger <- cur_value
        val <- v
      }
    }
  }
  else if(cl=="numeric") {
    fs <- sort(unique(f))
    for(v in fs) {
      Tfv1 <- sum(i1 & f<=v)
      Tfv0 <- sum(i0 & f<=v)
      
      Tfw1 <- T1 - Tfv1
      Tfw0 <- T0 - Tfv0
      cur_value <- ( sqrt(Tfv1 / T1) - sqrt(Tfv0 / T0) )^2 + ( sqrt(Tfw1 / T1) - sqrt(Tfw0 / T0) )^2
      
      if(cur_value > hellinger) {
        hellinger <- cur_value
        val <- v
      }
    }
  }
  else stop("unknown class: ", cl)
  
  return(list(d=sqrt(hellinger), v=val, type=cl))
}

HDDT <- function(X, y, C, labels=unique(y)) {
  
  if(is.null(labels) || length(labels)==0) labels <- unique(y)  
  
  node <- list() # when called for first time, this will be the root
  node$C <- C
  node$labels <- labels
  
  if(length(unique(y))==1 || length(y) < C) {
    # calculate counts and frequencies
    # use Laplace smoothing, by adding 1 to count of each label
    y <- c(y, labels)
    node$count <- sort(table(y), decreasing=TRUE)
    node$freq  <- node$count/sum(node$count)
    # get the label of this leaf node
    node$label <- as.integer(names(node$count)[1])
    return(node)
  }
  else { # recursion
    # get Hellinger distance and their max
    # use for loop insread of apply as it will convert data.frame to a matrix and mess up column classes
    # e.g. factor will get coerced into character
    HD <- list()
    for(i in 1:ncol(X)) HD[[i]] <- HDDT_dist(X[,i],y=y,labels=labels)    
    hd <- sapply(HD, function(x) {return(x$d)})
    i  <- which(hd==max(hd))[1] # just taking the first 
    
    # save node attributes
    node$i    <- i
    node$v    <- HD[[i]]$v
    node$type <- HD[[i]]$type
    node$d    <- HD[[i]]$d
    
    if(node$type=="factor") {
      j <- X[,i]==node$v
      node$childLeft  <- HDDT(X[j,], y[j], C, labels)
      node$childRight <- HDDT(X[!j,], y[!j], C, labels)
      
    }
    else if(node$type=="numeric") {
      j <- X[,i]<=node$v
      node$childLeft  <- HDDT(X[j,], y[j], C, labels)
      node$childRight <- HDDT(X[!j,], y[!j], C, labels) 
      
    }
  }
  
  return(node) # returns root node
}




HDDT_predict <- function(root, X) {
  y <- rep(NA, nrow(X))
  
  for(i in 1:nrow(X)) {
    # traverse the tree until we find a leaf node
    node <- root
    while(!is.null(node$v)) {
      if(node$type=="factor") {
        if(X[i,node$i]==node$v) node <- node$childLeft
        else node <- node$childRight
      }
      else if(node$type=="numeric") {
        if(X[i,node$i]<=node$v) node <- node$childLeft
        else node <- node$childRight
      }
      else stop("unknown node type: ", node$type)
    }
    stopifnot(!is.null(node$label))
    #node$freq  <- node$count/sum(node$count)
    y[i] <- node$label
  }
  
  return(y)
}

rep(NA, nrow(df2))
c<-HDDT(df1,train_scaled$Y,0.1*nrow(train_scaled),unique(train_scaled$Y))
c$childLeft
HDDT_predict(c,df2)


tree.pred<-HDDT_predict(c,df2)
tree.pred<-data.frame(tree.pred)
tree.pred$tree.pred <- factor(tree.pred$tree.pred)
length(test_scaled$Y)
test_scaled$Y
#d<-table(test_scaled$c,tree.pred$tree.pred)
d<-table(test_scaled$Y,tree.pred$tree.pred)
d
d[2]
sum(diag(d))/sum(d) #overall accuracy
1-sum(diag(d))/sum(d) #incorrect classification


precision<-d[1]/(d[1]+d[3])
recall<-d[1]/(d[1]+d[2])
recall
specifityvity<-d[4]/(d[3]+d[4])
AUC<-(recall+specifityvity)/2
AUC
F1score<-(2*recall*precision)/(recall+precision)
F1score
#ANN

train.pred = HDDT_predict(c,df1)
test.pred = HDDT_predict(c,df2)

train_scaled$pred = train.pred
train_scaled$c = train_scaled$c
test_scaled$pred = test.pred
test_scaled$c = test_scaled$c
test_scaled
train_scaled



library(neuralnet)


model <- neuralnet(Y~. ,data=train_scaled, hidden = 35, linear.output =  FALSE)
plot(model)
tree.pred <- predict(model, test_scaled)
tree.pred<- as.data.frame(tree.pred)
tree.pred


names(tree.pred)[1] <- "V1"
length(tree.pred$V1)
tree.pred$V1<-replace(tree.pred$V1, tree.pred$V1<0.5, 0)
tree.pred$V1<-replace(tree.pred$V1, tree.pred$V1>0.5, 1)
tree.pred
tree.pred<-data.frame(tree.pred)
tree.pred$V1 <- factor(tree.pred$V1)
tree.pred$V1
length(tree.pred$V1)
length(test_scaled$Y)
d<-table(test_scaled$Y,tree.pred$V1)
d
sum(diag(d))/sum(d) #overall accuracy
1-sum(diag(d))/sum(d) #incorrect classification
precision<-d[1]/(d[1]+d[3])
recall<-d[1]/(d[1]+d[2])
recall
specifityvity<-d[4]/(d[3]+d[4])
AUC<-(recall+specifityvity)/2
AUC
F1score<-(2*recall*precision)/(recall+precision)
F1score

