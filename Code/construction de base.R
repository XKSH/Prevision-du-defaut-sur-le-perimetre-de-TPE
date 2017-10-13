## Continu de "base original_s.R" ligne 36
#obtenir tous les nom de variables
nom.variable=c(colnames(PEquan.org),colnames(PEqual))
nom.variable=t(nom.variable)
write(nom.variable, "variablenames.txt", sep = " ")

c=read.table(file = "variablenames.txt",header = TRUE,sep="\t",na.strings=c("","NA"))

#variable importance
varIMP=read.table(file = "impvariable.txt",header = TRUE,sep=" ",na.strings=c("","NA"))

varIMP=varIMP[order(varIMP[,1],decreasing=TRUE),]
