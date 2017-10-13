#Feature selection
#Partie 1
#PCA sur les variables quantitatives
library(ggfortify)
fPEquan=PEquan[,3:(ncol(PEquan)-1)]
ff=apply(fPEquan,2,function(x){x[is.na(x)]=mean(x,na.rm = TRUE); return(x)})
pc.ff <- prcomp(ff)
fPEquan[,ncol(fPEquan)]=as.factor(fPEquan[,ncol(fPEquan)])
colnames(fPEquan)[ncol(fPEquan)]=c("Défaut")
autoplot(pc.ff , data = fPEquan, colour = "Défaut",
         loadings = TRUE, loadings.colour = 'blue')
#Contribution de variance
eig <- (pc.ff $sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig.active <- data.frame(eig = eig, variance = variance,
                                    cumvariance = cumvar)
eig.active=eig.active[which(eig>1),]
barplot(eig.active[, 2], names.arg=1:nrow(eig.active), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
lines(x = 1:nrow(eig.active), 
      eig.active[, 2], 
      type="b", pch=19, col = "red")
#Les variables normalisés peut être calculé comme suivant;sinon cor=TURE dans prcomp
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
}
# Variable correlation/coordinates
loadings <- pc.ff$rotation
sdev <- pc.ff$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))

# Return the index of feature candidates
var.cor=var.cor[,1:44]
var_cand=apply(var.cor,2,function(x){t=which(abs(x)>0.5);return(t)})
var_cand=unlist(var_cand)
var_cand=unique(var_cand)
#Test the correlation of the candidates
cand_cor=cor(ff[,var_cand])

#Partie 2
#Filtration de corrélation haute
fPEquan=do.call(cbind,fPEquan)
pop_cor=cov(fPEquan,use = "pairwise.complete.obs")
pop_cor[!lower.tri(pop_cor)]=0
var_cand.1=apply(pop_cor,2,function(x){t=which(x>0.5);return(t)})
var_cand.1=unlist(var_cand.1)
var_cand.1=setdiff(1:245, unique(var_cand.1))

#Partie 3
#Filtration de ratio de NA
na.prop=c(apply(PEquan,2,function(x){t=sum(is.na(x))/length(x);return(t)}))
var_cand.2=setdiff(1:245, which(na.prop>0.2))

var_cand.3=intersect(var_cand,var_cand.2)
#under sampling

