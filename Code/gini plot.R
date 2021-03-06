#Gini plot
#votre data frame de r�sultat, proba en 1�me colonne, classe r�elle en 2�me colonne  
#res.pre=cbind(c(0.2,0.4,0.1,0.1,0.5,0.7,0.6),c(1,0,1,1,0,0,0))

Gini.plot=function(x,type){
  #Indice de Gini 
  res.pre=x
  res.pre=res.pre[order(res.pre[,1],decreasing=TRUE),]
  pp=seq(0, 1, length.out = 10001)#nombre de point=101
  nd=rep(0,length(pp))
  for(i in 2:length(nd))#sur chaque point, le nombre de d�faut
  { 
    if(floor(pp[i]*length(res.pre[,1]))>0){
      nd[i]=length(which(res.pre[1:(floor(pp[i]*length(res.pre[,1]))),2]==1))
    }
    else{
      nd[i]=0
    }
    
  }
  pd=nd/length(which(res.pre[,2]==1))# la proportion de d�faut sur le point
  aire=sum(pd[-1]+pd[-length(pd)])*1/(((length(pp)-1))*2)
  plot(pp,pd,type="line",col="purple",xlab="% de la population s�lectionn�e",ylab="% des d�fauts s�lectionn�s",yaxt="n",main="Courbe de discrimination")
  axis(2, at=pretty(pd), lab=paste0(pretty(pd) * 100, " %"),  las=TRUE)
  pd1=rep(1,length(pp))
  pd1[which(pp<length(which(res.pre[,2]==1))/length(res.pre[,1]))]=length(res.pre[,1])/length(which(res.pre[,2]==1))*pp[which(pp<length(which(res.pre[,2]==1))/length(res.pre[,1]))]
  lines(pp,pd1,col="orange")
  pd2=rep(1,length(pp))*pp
  lines(pp,pd2,col="green")
  Gini=(aire-0.5)/(0.5-length(which(res.pre[,2]==1))/(2*length(res.pre[,1])))
  legend(0.6, 0.45,title=type,c("Courbe parfaite", paste("Courbe mod�lisation,Gini=",round(Gini,4)), "Courbe al�atoire"), col = c("orange", "purple", "green"),
         lty = c(1, 1, 1),cex=0.6)
}

#res.pre=cbind(train.pre,btrain.complet[,iddf])
#Gini.plot(res.pre,"Base Apprentissage")
