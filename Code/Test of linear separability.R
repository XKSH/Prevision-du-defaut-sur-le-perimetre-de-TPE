library(Rglpk)
train=PEdat[traincand[,2],]
train=train[order(train[,246]),]
N1 <- length(which(train[,246]==0))
N2 <- nrow(train)-N1

# the points of sets A and B
train=apply(train,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
#train=do.call(cbind,train)
train=train[,-246]

dim <- ncol(train)
P <-train

# the matrix A defining the lhs of the conditions
A <- cbind(P * c(rep(-1,N1),rep(1,N2)), c(rep(1,N1),rep(-1,N2)))

# the objective function - no optimization necessary
obj <- rep(0, dim+1)

# the vector b defining the rhs of the conditions
b <- rep(-0.1, N1+N2)

# by default GLPK assums positive boundaries for the
# variables. but we need the full set of real numbers.
bounds <- list(
  lower = list(ind = 1:(dim+1), val = rep(-Inf, dim+1)),
  upper = list(ind = 1:(dim+1), val = rep(Inf, dim+1))
)

# solving the linear program
s <- Rglpk_solve_LP(obj, A, rep("<=", N1+N2), b, bounds=bounds)
# status 0 means that a solution was found
if(s$status == 0) {
  cat("Linearly separable.")
} else {
  cat("Not linearly separable.")
}
#Conclusion:$status[1] 1 Not linearly separable.

#convex hull
library(grDevices)
ff=apply(train,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
pc.ff <- prcomp(ff)
library(RColorBrewer)
plot(pc.ff$x[,1:2],col=c(rep("red",N1),rep("green",N2)), main = "Enveloppe convexe  sur le plan ACP", xlab = "CP1", ylab = "CP2")
c1.x=pc.ff$x[1:N1, 1];c1.y=pc.ff$x[1:N1, 2];
c2.x=pc.ff$x[(N1+1):(N1+N2), 1];c2.y=pc.ff$x[(N1+1):(N1+N2), 2];
c1=list(x=c1.x,y=c1.y)
hpts=chull(c1)
hpts <- c(hpts, hpts[1])
lines(c1.x[hpts],c1.y[hpts],col="red")
c2=list(x=c2.x,y=c2.y)
hpts1=chull(c2)
hpts1 <- c(hpts1, hpts1[1])
lines(c2.x[hpts1],c2.y[hpts1],col="green")
#Enveloppe sur la plupart
c1.partial=list(x=c1.x[3>c1.x&c1.x>0&3>c1.y&c1.y>-5],y=c1.y[3>c1.x&c1.x>0&3>c1.y&c1.y>-5])
c2.partial=list(x=c2.x[3>c2.x&c2.x>0&3>c2.y&c2.y>-5],y=c2.y[3>c2.x&c2.x>0&3>c2.y&c2.y>-5])
plot(c1.partial$x,c1.partial$y,col="red", main = "Enveloppe convexe  de la majorité", xlab = "CP1", ylab = "CP2",xlim = c(0,3),ylim=c(-5,3))
points(c2.partial$x,c2.partial$y,col="green")
hpts=chull(c1.partial$x,c1.partial$y)
hpts <- c(hpts, hpts[1])
lines(c1.partial$x[hpts],c1.partial$y[hpts],col="red")
hpts1=chull(c2.partial$x,c2.partial$y)
hpts1 <- c(hpts1, hpts1[1])
lines(c2.partial$x[hpts1],c2.partial$y[hpts1],col="green")