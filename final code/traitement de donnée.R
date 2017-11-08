library(FactoClass)
library(glmnet)
library(pROC)
this.dir = dirname("parent.frame(2)$ofile")
setwd(this.dir)
PEquan.org=read.table(file = "avb.txt",header = TRUE,sep="\t",na.strings=c("","NA"))

#le traitment de données
nvquan=c("CATJUR_REF","COSEG_REF","TOP_FILLE_CONSO","top_actif","TOP_LBO_REF_AC","COD_ACT_REF", "COD_ALLUR_CAV_CLI","PERIMETRE",
         "TOP_INCID_SIREN", "COT_BDF_ENTREP","TOP_CLNT_ACTIF_CLI_DITG_2_C","TOP_INCD_12M_SIREN_C")
disj=as.matrix(acm.disjonctif(PEquan.org[,which(colnames(PEquan.org)%in%nvquan)]))
PEquan.org=cbind(PEquan.org[,which(!(colnames(PEquan.org)%in%nvquan))],disj)
PEdat.org=cbind(PEquan.org[,3:(ncol(PEquan.org))] )
PEdat.org= PEdat.org[,colSums(is.na(PEdat.org))<nrow(PEdat.org)]
PEdat.org=PEdat.org[,c(which(!colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")), which(colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")))]
ndisj=ncol(disj)+1

#division de base 
library(sampling)
taille=0.7*as.data.frame(table(PEdat.org$top_dft_auto_pro_corp))[,2]
set.seed(17)
traincand=strata(PEdat.org, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")
btrain=PEdat.org[traincand[,2],]
btest=PEdat.org[-traincand[,2],]
#Normalisation 
btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
btest[,1:(ncol(btrain)-ndisj)]=apply(btest[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))