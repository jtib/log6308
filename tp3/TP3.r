# Lecture des données
rm(list = ls())
library(Matrix)
library(RCurl)
u.data <- read.csv('u.data.csv', sep='|', header=T)
head(u.data)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
m <- as.matrix(m)
## m est u.data
m.na <- m
m.na[m==0] <- NA
m.na2 <- m.na

# Question 1
## Votes au hasard
m.hasard <- m.na
m.hasard[m>0] <- sample(m.hasard[m>0], sum(m>0))
mae <- function(m1, m2) mean(abs(m1 - m2), na.rm=T)
mse <- function(m1, m2) mean((m1 - m2)*(m1 - m2), na.rm=T)
### La première approche test est celle du vote au hasard.
mae(m.hasard, m.na)

## Deuxième et troisième approche : moyenne de l'utilisateur et moyenne de l'item
u.mean <- matrix(rowMeans(m.na, na.rm=T), nrow(m), ncol(m))
item.mean <- matrix(colMeans(m.na, na.rm=T), nrow(m), ncol(m), byrow=T)
mae(m.na, u.mean)
mae(m.na, item.mean)

## Mélange : moyenne de la note moyenne de l'item et celle de l'utilisateur 
m.expect <- ((u.mean + item.mean)/2)
corner <- function(m, ...) head(t(tail(t(m), ...)), ...)
corner(m.expect)
mae(m.expect, m.na)

### On note que l'utilisation de la moyenne utilisateur seule fournit un bon score en terme d'erreur.
### Cependant, utiliser la moyenne utilisateur comme baseline n'a pas de sens car on donne à chaque item la même note :
### comment alors recommander un item à  un utilisateur ?
### Utiliser la moyenne des items est beaucoup plus pertinent, cela revient à recommander les items les plus populaires.
### m.expect permet d'associer les deux aspects; on obtient d'ailleurs un meilleur score d'erreur.
### On notera que les trois approches donnent une MAE bien moindre que l'approche aléatoire.

# Question 2
# Normalisation
## Calcul des votes moyens
votes.utilisateurs.moyen <- rowMeans(m.na, na.rm=T)
votes.items.moyen <- colMeans(m.na, na.rm=T)
## Utilitaires
`%-=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 - e2))
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
## Remplacement des NA par les votes moyens sur les items correspondants
indx <- is.na(t(m.na))
m.filled <- t(m.na)
m.filled[indx] <- votes.items.moyen[col(m.filled)][indx]
m.filled <- t(m.filled)
## Normalisation en soustrayant les votes moyens des utilisateurs correspondants
m.filled[,1:ncol(m)] %-=% votes.utilisateurs.moyen
# SVD
m.svd <- svd(m.filled)

m.svd

# Question 3
## Normalisation, décomposition SVD et prédiction (10 dimensions)

## Normalisation telle que réalisée dans la partie "expérience pratique" (qui donne le graphe qu'on doit reproduire)
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

## Remplacement des NA par les votes moyens sur les items correspondants
indx <- is.na(t(m.na))
m.filled <- t(m.na)
m.filled[indx] <- votes.items.moyen[col(m.filled)][indx]
m.filled <- t(m.filled)
## Normalisation en soustrayant les votes moyens des utilisateurs correspondants
m.filled[,1:ncol(m)] %-=% votes.utilisateurs.moyen
# SVD
m.svd <- svd(m.filled)

## Normalisation telle que réalisée dans l'article dans la partie théorique (partie 3)
fillingTheorique <- function(m.na){
  m.filled.th <- m.na
  m.filled.th[is.na(m.filled.th)] <- 0
  m.filled.th <- m.filled.th - item.mean
  return(m.filled.th)
}

## Fonction réalisant la prédiction des votes après SVD avec un certain nombres de dimensions.
predsvd <- function(nbdim, d, u, v, votes.items.moyen){
  d.reduced <- d[1:nbdim]
  d.squared <- diag(sqrt(d.reduced))
  diag(d.squared)
  usk <- u[,1:nbdim] %*% d.squared
  vsk <- d.squared %*% t(v[,1:nbdim])
  
  ### Gagne du temps pour l'estimation du vote
  m.reconstruit <- usk %*% vsk
  
  prediction <- function(user, item, votes.items.moyen) {
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

## Wrapper qui (1) fait la décomposition SVD et (2) fait les calculs de prédiction :
predVotes <- function(mat,nbdim,votes.items.moyen){
  svd <- svd(mat)
  return(predsvd(nbdim,svd$d,svd$u,svd$v,votes.items.moyen))
}

## Utilitaire : découpage en ensemble d'entraînement et ensemble de validation.
### Renvoie le training set et la matrice booléenne des indices changés 
### (pour pouvoir après calculer la MAE seulement sur les valeurs changées)
split <- function(m, ratio){
  i.observed <- which(m > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  length(i.hasard)
  fold.size <- round(length(i.hasard) * ratio)
  i.false <- rep(FALSE, length(m))
  ## Index booléen pour les cellules de test et d'entraînement
  i.test.b <- i.false ## Les cellules indexées du replis correspondant sont fixées à TRUE pour le test...
  i.test.b[ i.hasard[1:  fold.size]] <- TRUE ## ...et à FALSE pour l'entraînement
  i.train.b <-  !i.test.b
  m.na.train <- m
  m.na.train[i.test.b] <- NA  # on enlève les données de test pour l'entraînement
  return(list(m.na.train, i.test.b))
}

## Découpage et décomposition SVD
dd <- split(m.na,0.1)
testIndices <- dd[[2]]

### Deux normalisations possibles
#### ne fonctionne pas dans cet ordre
m.filled <- fillingPratique(dd[[1]])
#### (sinon, il reste des valeurs manquantes et la décomposition SVD est impossible)
m.na.pr <- fillingPratique(m.na)
dd.pr <- split(m.na.pr, 0.1)
m.filled <- dd.pr[[1]]
testIndices.pr <- dd.pr[[2]]
#### fonctionne en faisant la normalisation avant ou après la séparation
m.filled.th <- fillingTheorique(dd[[1]])

m.svd <- svd(m.filled)
d <- m.svd$d
u <- m.svd$u
v <- m.svd$v

m.svd2 <- svd(m.filled.th)
d2 <- m.svd2$d
u2 <- m.svd2$u
v2 <- m.svd2$v

### Prédiction des votes
test <- predVotes(m.filled.th, 10, item.mean)
test.pr <- predVotes(m.filled, 10, item.mean)

m.na.pr

# Question 4
## Erreur absolue moyenne
mae(m.na[testIndices] , test[testIndices])
mae(m.na[testIndices] , test.pr[testIndices.pr])

## Erreur quadratique moyenne
mse(m.na[testIndices] , test[testIndices])
mse(m.na[testIndices] , test.pr[testIndices.pr])

#############################################################################
## Ce code représente un exemple de prédictions avec des valeurs au
## hasard et les valeurs attendues
#############################################################################

library(Matrix)
library(RCurl)
u.data <- read.csv('u.data.csv', sep='|', header=T)
head(u.data)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')
m <- as.matrix(m)
## m est u.data
m.na <- m
m.na[m==0] <- NA
############################################################################
### Votes au hasard
m.hasard <- m.na
m.hasard[m>0] <- sample(m.hasard[m>0], sum(m>0))
mae <- function(m1, m2) mean(abs(m1 - m2), na.rm=T)
mae(m.hasard, m.na)
0

# Question 5

## Fonction pour la prédiction
predict.SVD = function(nb.dim, M) {
  M.svd <- svd(M)
  ## Matrices U, S et V
  U <- M.svd$u
  S <- diag(m.svd$d)
  V <- m.svd$v
  ## Matrice Sk
  Sk <- diag(M.svd$d[1:nb.dim])
  Sk.sqrt <- diag(sqrt(diag(Sk)))
  ## Uk et Vk
  Uk <- U[,1:k]
  Vk <- V[,1:k]
  UkSk <- Uk%*%Sk.sqrt
  SkVk <- Sk.sqrt%*%t(Vk)
  UkSkVk <- UkSk%*%SkVk
  ## Dénormalisation
  Pk <- UkSkVk[,1682] %+=% votes.utilisateurs.moyen
  return(Pk)
}

# # Matrices U, S et V
# U <- m.svd$u
# S <- diag(m.svd$d)
# V <- m.svd$v
# # Matrices Sk
# compute.Sk <- function(k){
#     return(diag(m.svd$d[1:k]))
# }
# list.Sk <- list()
# aux <- c(2,5:21,25,50,100)
# list.Sk <- lapply(aux, compute.Sk)
# length(list.Sk)
# 
# # Racine carrée de chaque Sk
# # Note : diag n'est pas une opération très efficace.
# list.Sk.roots <- lapply(list.Sk, function(x) diag(sqrt(diag(x))))
# 
# # Uk*Sk^1/2 et Sk^1/2*Vk
# list.Uk <- lapply(aux, function(k) U[,1:k])
# list.Vk <- lapply(aux, function(k) V[,1:k])
# list.UkSk <- mapply(function(M,N) M%*%N, list.Uk, list.Sk.roots)
# list.SkVk <- mapply(function(M,N) M%*%t(N), list.Sk.roots, list.Vk)
# # UkSkVk'
# list.UkSkVk <- list()
# list.UkSkVk <- mapply(function(M,N) M%*%N, list.UkSk, list.SkVk, SIMPLIFY=F)
# # Dénormalisation
# tmp <- 1:21
# P <- lapply(tmp, function(k) {list.UkSkVk[[k]][,1:1682] %+=% votes.utilisateurs.moyen; list.UkSkVk[[k]]})

# Prediction (MAE)
aux <- c(2,5:21,25,50,100)
P <- lapply(aux, predict.SVD, M=m.filled)
votes.attendus <- outer(votes.utilisateurs.moyen, votes.items.moyen, FUN='+')/2# pas sûre du tout qu'on doive comparer à ça
err <- lapply(tmp, function(k) mae(votes.attendus, P[[k]]))
plot(unlist(err))
# d'autant que comme ça on a une erreur strictement croissante selon le nombre de dimensions, pareil avec m.hasard
# ce qui est normal : on compare à la moyenne ; moins on a de dimensions, moins on a de détails, moins on a d'écart à la moyenne
# mais ça ne nous apporte pas l'information nécessaire pour trouver la dimension idéale


# Question 6
#############################################################################
## Avec validation croisée basée sur 10 replis (10 folds).
## Le code ne fait qu'un repli. <- non, maintenant il en fait 10
## Le principe consiste premièrement à créer un vecteur de cellules aléatoires qui couvrent l'ensemble de la matrice.
## Ce vecteur est ensuite divisé en 10 replis (sections).
## Pour chaque repli, un index booléen est créé pour les données de tests et sa négation correspond aux données d'entraînement.
## Index aléatoire des données de tests
i.observed <- which(m > 0)
i.hasard <- sample(i.observed, length(i.observed))
length(i.hasard)
fold.size <- round(length(i.hasard) / 10)
## Pour les erreurs
err.mae <- matrix(data = NA, nrow = 10, ncol = 2)
err.rmse <- matrix(data = NA, nrow = 10, ncol = 1)
## 10 replis
for (i in 1:10) { 
i.false <- rep(FALSE, length(m))
fold.number <- 1
## Index booléen pour les cellules de test et d'entraînement
i.test.b <- i.false
## Les cellules indexées du repli correspondant sont fixées à TRUE pour le test...
i.test.b[ i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)] ] <- TRUE
## ...et à FALSE pour l'entraînement
i.train.b <-  !i.test.b
m.na.train <- m.na
m.na.train[i.test.b] <- NA # on enlève les données de test pour l'entraînement
table(m.na.train)
votes.films.moyens <- colMeans(m.na.train, na.rm=T)
mean(votes.films.moyens) # des NaN pourraient être créés car certains films n'ont plus aucun vote## Il faudrait alors remplacer ces colonnes par une valeur correspondant à la moyenne générale.
moy.globale <- mean(m.na.train, na.rm=T)
films.sans.votes <- colSums(m.na.train, na.rm=T) == 0
sum(films.sans.votes) # si 0 alors pas besoin de faire l'ajustement suivant
m.na[,films.sans.votes] <- moy.globale
votes.films.moyen <- colMeans(m.na.train, na.rm=T)
## fin de l'ajustement
hist(votes.films.moyens)
## votes moyens des utilisateurs de test
votes.utilisateurs.moyen <- rowMeans(m.na.train, na.rm=T)
## pour faire changement, utilisons la moyenne arithmétique
mean(votes.utilisateurs.moyen) # véfication si ajustement nécessaire (ici ce ne l'est pas et on continue sans)
hist(votes.utilisateurs.moyen)caca
votes.attendus <- outer(votes.utilisateurs.moyen, votes.films.moyen, FUN='+') / 2
## Histogramme des erreurs
hist(votes.attendus[i.test.b] - m[i.test.b])
## Erreur absolue moyenne
err.mae[i,1] <- mae(votes.attendus[i.test.b], m[i.test.b])
err.mae[i,2] <- mean(abs(votes.attendus[i.test.b] - m[i.test.b]), na.rm=T)
## Racine carrée de erreur quadratique moyenne
err.rmse[i,1] <- sqrt(mean((votes.attendus[i.test.b] - m[i.test.b])^2, na.rm=T))
}
