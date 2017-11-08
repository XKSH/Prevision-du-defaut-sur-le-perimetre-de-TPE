# Evaluation de la qulité de clustering
# Evaluation externe
# Paramètres: Un tableau de contingence dont les lignes sont classes et les colonnes sont clusters 
# 1: La pureté
purity=function(obj){
  sum(apply(obj,2,max))/sum(obj)
}
# 2: La NMI (normalized mutual information) 
nmi=function(obj){
  s1=rowSums(obj,na.rm=T)/sum(obj)
  s2=colSums(obj,na.rm=T)/sum(obj)
  s3=obj/sum(obj)
  s4=log(sum(obj)*obj/(sum(obj)^2*s1%*%t(s2)))
  s4[s4==-Inf]=0
  s5=sum(s3*s4)# information mutuelle de classe et cluster(cross entropy)
  I1=-sum(s1[s1!=0]*log(s1[s1!=0]))#entropie de classe
  I2=-sum(s2[s2!=0]*log(s2[s2!=0]))#entropie de cluster
  I12=2*s5/(I1+I2)
  I12
}
#nmi pour faire le choix, obj(dataframe) doit avoir au moins deux colonnes 
# La dernière colonne est la classe
cnmi=function(obj){
  mnmi=rep(0,(dim(obj)[2]-1))
  for(i in 1:(dim(obj)[2]-1)){
    t=table(obj[,i],obj[,ncol(obj)])
    mnmi[i]=nmi(t)
  }
  mnmi
}