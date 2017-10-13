# Continu de "base original_s"
require(Rtsne)
require(dplyr)
require(ggplot2)
#tsne sur toutes les variables
tsne_obj <- Rtsne(btrain.complet[,-iddf],pca_center = FALSE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(classe = factor(btrain.complet[,iddf]))
#tsne_data1 =tsne_data[idtrain,] #points de sous-échantillonnage
ggplot(aes(x = X, y = Y), data =tsne_data) +
  geom_point(aes(color = classe))+ggtitle("Distribution des classes")+theme(plot.title = element_text(hjust = 0.5,face="bold"))

#exemple iris
data("iris")
tiris=data.frame(iris)
tiris=unique(tiris)
tsne_obj <- Rtsne(tiris[,1:4])
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(classe = factor(tiris[,5]))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = classe))+ggtitle("Distribution des classes")+theme(plot.title = element_text(hjust = 0.5,face="bold"))

#tsne sur les variables selectionnées
tsne_obj <- Rtsne(btrain.complet[,which(colnames(btrain.complet)%in%c(importance_matrix$Feature))],pca_center = FALSE)
#tsne_obj <- Rtsne(btrain.complet[,1:100],pca_center = FALSE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(classe = factor(btrain.complet[,iddf]))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = classe))+ggtitle("Distribution de deux classes")+theme(plot.title = element_text(hjust = 0.5))



ggplot(aes(x = X, y = Y),data = tsne_data) +
  geom_point(aes(color = classe))+theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                        axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                        axis.title.y=element_blank(),legend.position="none"
                                        )