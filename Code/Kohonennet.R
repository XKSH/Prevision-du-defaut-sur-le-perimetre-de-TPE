kohonenqualigo<-function(seed,nr,nc,alpha0,alphaT,beta0,betaT,dat,n) 
  # seed, dim carte 1, dim carte 2, taux d'apprentissage initial, final, rayon d'apprentissage initial, final, table des données, nombre d'observations lues, type de pas DTW, contrainte de fenêtre DTW
{
  base.test=as.matrix(dat[1:n,]) # x=colonne PCO
  set.seed(seed)
  beta=para.expo(beta0,betaT,n)
  alpha=para.expo(alpha0,alphaT,n)
  # Initialisation aléatoire des vecteurs poids
  profils=matrix(runif(dim(base.test)[2]*nr*nc,0,1),ncol=dim(base.test)[2])
  # Construction de la matrice de voisinage
  neighboo=vois.constr(nr, nc, beta[1]) 
  b=beta[1]
  # Entraînement du réseau Konohen
  for (k in 1:n)
  {
    if (beta[k]%/%1!=b%/%1) {neighboo=vois.constr(nr, nc, beta[k])} 
    # On regarde si la décroissance de béta impacte le nombre de voisins
    b=beta[k]
    dista=apply(profils, 1, function(x, y=base.test) dist(rbind(y[k,],x),method = "minkowski",p=2))
    m<-which(dista==min(dista))[1] # Recherche du min de ces distances;si pas unique, on choisit 1er
    #print(profils)
    # nf?
    #nf=1
    nf=exp(-apply(profils, 1, function(x, y=profils) dist(rbind(y[m,], x))/(beta[k]^2)))
    # Renovellement de l'unité gagante et son voisin
    updt=t((base.test[k,]-t(profils)))*alpha[k]*nf*neighboo[,m]
    updt[is.na(updt)==TRUE]=0
    profils=profils+updt
  }
  return(profils)
}
# Locations des individus et variables sur la carte
kohonenqualiclass<-function(profils,dat,n)
{
  base.test=as.matrix(dat[1:n,])
  m=rep(0,n)
  for (k in 1:n)
  {
    dista=apply(profils, 1, function(x, y=base.test) dist(rbind(y[k,],x)))
    m[k]<-which(dista==min(dista))[1]
  }
  return(m) # Vecteur des neurones vainqueurs
}
