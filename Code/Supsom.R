library(sampling)
taille=0.7*as.data.frame(table(PEdat$top_dft_auto_pro_corp))[,2]
set.seed(17)
traincand=strata(PEdat, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")
set.seed(15)
#undersampling
id1=sample(1:length(traincand[traincand[,4]==1,4]), 1*length(traincand[traincand[,4]==2,4]))
idtrain=c(traincand[id1,2],traincand[which(traincand[,4]==2),2])
btrain=PEdat[idtrain,-(ncol(PEquan)-2)]
btest=PEdat[-idtrain,-(ncol(PEquan)-2)]
btrain=do.call(cbind,btrain)
btest=do.call(cbind,btest)
dclass=factor(PEquan[,(ncol(PEquan))])
trainingdata <- list(xmap= btrain,ymap = dclass[idtrain])

testdata <- list(xmap= btest,ymap = dclass[-idtrain])
mygrid = somgrid(3, 3,"rectangular")

som.PE <- supersom(trainingdata, grid = mygrid,whatmap = c("xmap", "ymap"),user.weights=c(0.6,0.4),maxNA.fraction = 0.99)
som.prediction <- predict(som.PE, newdata = testdata,whatmap = "xmap")
table(som.prediction$predictions[["ymap"]],dclass[-idtrain])
pre=as.numeric(som.prediction$predictions[["ymap"]])
plot(roc(as.numeric(dclass[-idtrain]),pre),add=TRUE,col="blue")#,print.auc=TRUE)
#example: change dataset'yeast' to less classes
library(kohonen)
data(yeast)
y1=as.list(yeast)
set.seed(17)
training.indices <- sample(nrow(y1$alpha), 500)
training <- rep(FALSE, nrow(y1$alpha))
training[training.indices] <- TRUE
y1$class=as.character(y1$class)
y1$class[y1$class=="M/G1"]=2
y1$class[y1$class=="G1"]=1
y1$class[y1$class=="S"]=2
y1$class[y1$class=="M"]=1
y1$class[!(y1$class%in%c(1))]=2
y1$class=as.factor(y1$class)
yeast.som2 <- supersom(lapply(y1, function(x) subset(x, training)),
                       grid = somgrid(3, 3, "rectangular"),
                       whatmap = c("alpha", "class"), user.weights=c(0.5,0.5),maxNA.fraction = 0.99)
yeast.som2.prediction <-
  predict(yeast.som2,
          newdata = lapply(y1, function(x) subset(x, !training)),
          whatmap = "alpha")
table(yeast.som2.prediction$prediction[["class"]],y1$class[!training])
pre=as.numeric(yeast.som2.prediction$prediction[["class"]])
plot(roc(pre, as.numeric(subset(y1$class, !training))),col="red",print.auc=TRUE)
#initialization
init.class=sample(c(0,1),9,replace=TRUE)
init.btrain=matrix(runif(dim(btrain)[2]*3*3,0,1),nrow=9)
init=list(init.btrain,init.class)





yeast.som2.prediction <-
  predict(yeast.som2,
          newdata = lapply(y1, function(x) subset(x, !training)),
          whatmap = "alpha")
table(y1$class[!training], yeast.som2.prediction$prediction[["class"]])

