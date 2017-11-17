#############################################################################
## Ce code représente un exemple de prédictions avec des valeurs au
## hasard et les valeurs attendues
#############################################################################
rm(list = ls())
library(Matrix)
library(RCurl)
u.data <- read.csv(text=(getURL('http://www.groupes.polymtl.ca/log6308/Tp/20163/u.data.csv', userpwd='20113:20113')), sep='|', header=T)
head(u.data)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
m <- as.matrix(m)
## m est u.data
m.na <- m
m.na[m==0] <- NA
m.na2 <- m.na

############################################################################### Votes au hasard
m.hasard <- m.na
m.hasard[m>0] <- sample(m.hasard[m>0], sum(m>0))
mae <- function(m1, m2) mean(abs(m1 - m2), na.rm=T)
# La première approche test est celle du vote au hasard.
mae(m.hasard, m.na)

u.mean <- matrix(rowMeans(m.na, na.rm=T), nrow(m), ncol(m))
item.mean <- matrix(colMeans(m.na, na.rm=T), nrow(m), ncol(m), byrow=T)

mae(m.na, u.mean)
mae(m.na, item.mean)

m.expect <- ((u.mean + item.mean)/2)
corner <- function(m, ...) head(t(tail(t(m), ...)), ...)
corner(m.expect)
mae(m.expect, m.na)


#On note  que l'utilisation de la moyenne utilisateur seule fournit un bon score en terme d'erreur. Cependant utiliser la moyenne utilisateur
#comme baseline n'a pas de sens car on donne à chaque item la même note : comment alors recommander un item à un utilisateur ?
#utiliser la moyenne des items est beaucoup plus pertinent, cela revient  à recommander les items les plus populaires.
#m.expect permet d'associer les deux aspects; on obtient d'ailleurs un meilleur score d'erreur.
# On notera que les trois approches donnent une MAE bien moindre que l'approche random.



# Question 2
# Normalisation

#normalisation telle que réalisée dans la partie "expérience pratique" (qui donne le graphe qu'on doit reproduire)
#made by juju
fillingPratique <- function(m.na){
  votes.utilisateurs.moyen <- rowMeans(m.na, na.rm=T)
  `%-=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 - e2))
  votes.items.moyen <- colMeans(m.na, na.rm=T)
  indx <- is.na(t(m.na))
  m.filled <- t(m.na)
  m.filled[indx] <- votes.items.moyen[col(m.filled)][indx]
  m.filled <- t(m.filled)
  m.filled[,1:ncol(m)] %-=% votes.utilisateurs.moyen
  dim(m.filled)
  return(m.filled)
}



#normalisation telle que réalisées dans l'article dans la partie théorique (partie 3)
#made by theo
fillingTheorique <- function(m.na){
  m.filled.th <- m.na
  m.filled.th[is.na(m.filled.th)] <- 0
  m.filled.th <- m.filled.th - item.mean
  return(m.filled.th)
}


#split en test et train set. 
#on renvoie le training set et la matrice bool�enne des indices chang�s 
#(pour pouvoir apr�s calculer la MAE seulement sur les valeurs chang�es)
split <- function(m, ratio){
  i.observed <- which(m > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  length(i.hasard)
  fold.size <- round(length(i.hasard) * ratio)
  i.false <- rep(FALSE, length(m))
  ## Index booléen pour les cellules de test et d'entraînement
  i.test.b <- i.false## Les cellules indexées du replis correspondant sont fixées à TRUE pour le test...
  i.test.b[ i.hasard[1:  fold.size]] <- TRUE## ...et à FALSE pour l'entraînement
  i.train.b <-  !i.test.b
  m.na.train <- m
  m.na.train[i.test.b] <- NA  # on enlève les données de test pour l'entraînement
  return(list(m.na.train, i.test.b))
}

#fonctino réalisant la prédiction des votes pour svd avec un certain nombres de dimensions.
predsvd <- function(nbdim,d,u,v, votes.items.moyen){
  d.reduced <- d[1:nbdim]
  d.squared <- diag(sqrt(d.reduced))
  diag(d.squared)
  usk <- u[,1:nbdim] %*% d.squared
  vsk <- d.squared %*% t(v[,1:nbdim])
  
  #gagne du temps pour l'estimation du vote
  m.reconstruit <- usk %*% vsk
  
  prediction <- function(user,item,votes.items.moyen) {
    if (is.na(m.na[user,item]))
      res <- NA
    else {
      res <- votes.items.moyen[user,item] +  m.reconstruit[user,item]
    }
    return(res)
  }
  test <- sapply(1:943, function(i){#if (i%% 50 == 0) {print(i)};
    
    sapply(1:1682, function(j) prediction(i,j, votes.items.moyen))})
  
  return(t(test))
}

#wrapper qui 1) d�compose en svd et 2) fait les calculs de pr�diction:
predVotes <- function(mat,nbdim,votes.items.moyen){
  svd <- svd(mat)
  return(predsvd(nbdim,svd$d,svd$u,svd$v,votes.items.moyen))
}

dd <- split(m.na,0.1)
testIndices <- dd[[2]]


m.filled <- fillingPratique(dd[[1]])
m.filled.th <- fillingTheorique(dd[[1]])
test <- predVotes(m.filled.th,93, item.mean)
mae(m.na[testIndices] , test[testIndices])

m.svd <- svd(m.filled)
d <- m.svd$d
u <- m.svd$u
v <- m.svd$v

m.svd2 <- svd(m.filled.th)
d2 <- m.svd2$d
u2 <- m.svd2$u
v2 <- m.svd2$v

#corner(t(m.na))
#corner(t(m.filled))
#corner(test)
#corner(t(m.reconstruit))

#En réalisant les tests avec un nombre de dimensions 
#avec la version de m.filled telle qu'indiqué dans la partie théorique, on obtient une erreur nulle pour nbdim = 943. 
#avec la version de m.filled que tu avais faite (qu correspond au truc expérimental), on obtient une MAE de 0.48 en nbdim = 943.
mae(m.na,predsvd(2,d,u,v))
mae(m.na,predsvd(10,d,u,v))
mae(m.na,predsvd(14,d,u,v))
mae(m.na,predsvd(700,d,u,v))
mae(m.na,predsvd(940,d,u,v))


mae(m.na,predsvd(2,d2,u2,v2))
mae(m.na,predsvd(10,d2,u2,v2))
mae(m.na,predsvd(14,d2,u2,v2))
mae(m.na,predsvd(700,d2,u2,v2))
mae(m.na,predsvd(940,d2,u2,v2))




############################################################################### Avec validation croisée basée sur 10 replis (10 folds).
## Le code ne fait qu'un repli.
## Le principe consiste premièrement à créer un vecteur de cellules aléatoires qui couvrent l'ensemble de la matrice.  Ce vecteur est ensuite divisé en 10 replis (sections).  Pour chaque repli, un index booléen est créé pour les données de tests et sa négation correspond aux données d'entraînement.
## Index aléatoire des données de tests
i.observed <- which(m > 0)
i.hasard <- sample(i.observed, length(i.observed))
length(i.hasard)
fold.size <- round(length(i.hasard) / 10)
i.false <- rep(FALSE, length(m))
fold.number <- 1
## Index booléen pour les cellules de test et d'entraînement
i.test.b <- i.false## Les cellules indexées du replis correspondant sont fixées à TRUE pour le test...
i.test.b[ i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE## ...et à FALSE pour l'entraînement
i.train.b <-  !i.test.b
m.na.train <- m.na
m.na.train[i.test.b] <- NA                # on enlève les données de test pour l'entraînement
table(m.na.train)
votes.films.moyens <- colMeans(m.na.train, na.rm=T)
mean(votes.films.moyens)                # des NaN pourraient être créés car certains films n'ont plus aucun vote## Il faudrait alors remplacer ces colonnes par une valeur correspondant à la moyenne générale.
moy.globale <- mean(m.na.train, na.rm=T)
films.sans.votes <- colSums(m.na.train, na.rm=T) == 0
sum(films.sans.votes)                   # si 0 alors pas besoin de faire l'ajustement suivant
m.na[,films.sans.votes] <- moy.globale
votes.films.moyen <- colMeans(m.na.train, na.rm=T)## fin de   l'ajustement
hist(votes.films.moyens)## votes moyens des utilisateurs de test
votes.utilisateurs.moyen <- rowMeans(m.na.train, na.rm=T)## pour faire changement, utilisons la moyenne arithmétique
mean(votes.utilisateurs.moyen)          # véfication si ajustement nécessaire (ici ce ne l'est pas et on continue sans)
hist(votes.utilisateurs.moyen)
votes.attendus <- outer(votes.utilisateurs.moyen, votes.films.moyen, FUN='+') / 2## Histogramme des erreurs Ici �a trace l'histogramme des erreurs pour la baseline (l'approche user.mean+item.mean/2)
hist(votes.attendus[i.test.b] - m[i.test.b])## Erreur absolue moyenne
mae(votes.attendus[i.test.b], m[i.test.b])
mean(abs(votes.attendus[i.test.b] - m[i.test.b]), na.rm=T)## Racine carrée de erreur quadratique moyenne
sqrt(mean((votes.attendus[i.test.b] - m[i.test.b])^2, na.rm=T))
