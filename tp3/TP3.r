# Lecture des donnÃ©es
rm(list = ls())
library(Matrix)
library(RCurl)
u.data <- read.csv('u.data.csv', sep = '|', header = T)
head(u.data)
m <- sparseMatrix(u.data[, 1], u.data[, 2], x = u.data[, 3])
rownames(m) <- paste('u', 1:nrow(m), sep = '')
colnames(m) <- paste('i', 1:ncol(m), sep = '')
m <- as.matrix(m)
## m est u.data
m.na <- m
m.na[m == 0] <- NA
m.na2 <- m.na

# Question 1
## Votes au hasard
m.hasard <- m.na
m.hasard[m > 0] <- sample(m.hasard[m > 0], sum(m > 0))
mae <- function(m1, m2)
  mean(abs(m1 - m2), na.rm = T)
mse <- function(m1, m2)
  mean((m1 - m2) * (m1 - m2), na.rm = T)
### La premiÃ¨re approche test est celle du vote au hasard.
mae(m.hasard, m.na)

## DeuxiÃ¨me et troisiÃ¨me approche : moyenne de l'utilisateur et moyenne de l'item
u.mean <- matrix(rowMeans(m.na, na.rm = T), nrow(m), ncol(m))
item.mean <-
  matrix(colMeans(m.na, na.rm = T), nrow(m), ncol(m), byrow = T)
mae(m.na, u.mean)
mae(m.na, item.mean)

## MÃ©lange : moyenne de la note moyenne de l'item et celle de l'utilisateur
m.expect <- ((u.mean + item.mean) / 2)
corner <- function(m, ...)
  head(t(tail(t(m), ...)), ...)
corner(m.expect)
mae(m.expect, m.na)

### On note que l'utilisation de la moyenne utilisateur seule fournit un bon score en terme d'erreur.
### Cependant, utiliser la moyenne utilisateur comme baseline n'a pas de sens car on donne Ã Â chaque item la mÃªme note :
### comment alors recommander un item Ã Â  un utilisateur ?
### Utiliser la moyenne des items est beaucoup plus pertinent, cela revient Ã Â recommander les items les plus populaires.
### m.expect permet d'associer les deux aspects; on obtient d'ailleurs un meilleur score d'erreur.
### On notera que les trois approches donnent une MAE bien moindre que l'approche alÃ©atoire.

# Question 2
# Normalisation
## Calcul des votes moyens
votes.utilisateurs.moyen <- rowMeans(m.na, na.rm = T)
votes.items.moyen <- colMeans(m.na, na.rm = T)
## Utilitaires
`%-=%` = function(e1, e2)
  eval.parent(substitute(e1 <- e1 - e2))
`%+=%` = function(e1, e2)
  eval.parent(substitute(e1 <- e1 + e2))
## Remplacement des NA par les votes moyens sur les items correspondants
indx <- is.na(t(m.na))
m.filled <- t(m.na)
m.filled[indx] <- votes.items.moyen[col(m.filled)][indx]
m.filled <- t(m.filled)
## Normalisation en soustrayant les votes moyens des utilisateurs correspondants
m.filled[, 1:ncol(m)] %-=% votes.utilisateurs.moyen
# SVD
m.svd <- svd(m.filled)

m.svd

# Question 3
## Normalisation, dÃ©composition SVD et prÃ©diction (10 dimensions)

## Normalisation telle que rÃ©alisÃ©e dans la partie "expÃ©rience pratique" (qui donne le graphe qu'on doit reproduire)
fillingPratique <- function(m.na) {
  votes.utilisateurs.moyen <- rowMeans(m.na, na.rm = T)
  `%-=%` = function(e1, e2)
    eval.parent(substitute(e1 <- e1 - e2))
  votes.items.moyen <- colMeans(m.na, na.rm = T)
  indx <- is.na(t(m.na))
  m.filled <- t(m.na)
  m.filled[indx] <- votes.items.moyen[col(m.filled)][indx]
  m.filled <- t(m.filled)
  m.filled[, 1:ncol(m)] %-=% votes.utilisateurs.moyen
  dim(m.filled)
  return(m.filled)
}

## Remplacement des NA par les votes moyens sur les items correspondants
indx <- is.na(t(m.na))
m.filled <- t(m.na)
m.filled[indx] <- votes.items.moyen[col(m.filled)][indx]
m.filled <- t(m.filled)
## Normalisation en soustrayant les votes moyens des utilisateurs correspondants
m.filled[, 1:ncol(m)] %-=% votes.utilisateurs.moyen
# SVD
m.svd <- svd(m.filled)



## Fonction rÃ©alisant la prÃ©diction des votes aprÃ¨s SVD avec un certain nombres de dimensions.
predsvd <- function(nbdim, d, u, v, votes.items.moyen) {
  d.reduced <- d[1:nbdim]
  d.squared <- diag(sqrt(d.reduced))
  diag(d.squared)
  usk <- u[, 1:nbdim] %*% d.squared
  vsk <- d.squared %*% t(v[, 1:nbdim])
  
  ### Gagne du temps pour l'estimation du vote
  m.reconstruit <- usk %*% vsk
  
  prediction <- function(user, item, votes.items.moyen) {
    if (is.na(m.na[user, item]))
      res <- NA
    else {
      #on reconstruit tel que dans l'article
      res <-
        votes.items.moyen[user, item] +  m.reconstruit[user, item]
    }
    return(res)
  }
  test <- sapply(1:943, function(i) {
    #if (i%% 50 == 0) {print(i)};
    
    sapply(1:1682, function(j)
      prediction(i, j, votes.items.moyen))
  })
  
  return(t(test))
}

## Wrapper qui (1) fait la dÃ©composition SVD et (2) fait les calculs de prÃ©diction :
predVotes <- function(mat, nbdim, votes.items.moyen) {
  svd <- svd(mat)
  return(predsvd(nbdim, svd$d, svd$u, svd$v, votes.items.moyen))
}

## Utilitaire : dÃ©coupage en ensemble d'entraÃ®nement et ensemble de validation.
### Renvoie le training set et la matrice boolÃ©enne des indices changÃ©s
### (pour pouvoir aprÃ¨s calculer la MAE seulement sur les valeurs changÃ©es)
split <- function(m, ratio) {
  i.observed <- which(m > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  length(i.hasard)
  fold.size <- round(length(i.hasard) * ratio)
  i.false <- rep(FALSE, length(m))
  ## Index boolÃ©en pour les cellules de test et d'entraÃ®nement
  i.test.b <-
    i.false ## Les cellules indexÃ©es du replis correspondant sont fixÃ©es Ã Â TRUE pour le test...
  i.test.b[i.hasard[1:fold.size]] <-
    TRUE ## ...et Ã  FALSE pour l'entraÃ®nement
  i.train.b <-  !i.test.b
  m.na.train <- m
  m.na.train[i.test.b] <-
    NA  # on enlÃ¨ve les donnÃ©es de test pour l'entraÃ®nement
  return(list(m.na.train, i.test.b))
}


m.na.pr <- fillingPratique(m.na)
dd.pr <- split(m.na.pr, 0.1)
m.filled <- dd.pr[[1]]
m.filled[is.na(m.filled)] <- mean(m.filled, na.rm = T)
testIndices.pr <- dd.pr[[2]]


### PrÃ©diction des votes
test.pr <- predVotes(m.filled, 10, item.mean)

m.na.pr

# Question 4
## Erreur absolue moyenne
mae(m.na[testIndices.pr] , test.pr[testIndices.pr])

## Erreur quadratique moyenne
mse(m.na[testIndices.pr] , test.pr[testIndices.pr])

#Les valeurs semblent élevées comparées à la baseline.
#Cepedant la baseline de la question 1 était réalisée sans séparation entre
#ensemble de test et ensemble d'entraînement; le résultat de notre SVD serait certainement meilleur dans ces conditions.
#On est toutefois bien au dessus des performances du résultat au hasard

# Question 5

## Fonction pour la prÃ©diction
predict.SVD = function(nb.dim, M) {
  M.svd <- svd(M)
  k <- nb.dim
  ## Matrices U, S et V
  U <- M.svd$u
  S <- diag(m.svd$d)
  V <- m.svd$v
  ## Matrice Sk
  Sk <- diag(M.svd$d[1:nb.dim])
  Sk.sqrt <- diag(sqrt(diag(Sk)))
  ## Uk et Vk
  Uk <- U[, 1:k]
  Vk <- V[, 1:k]
  UkSk <- Uk %*% Sk.sqrt
  SkVk <- Sk.sqrt %*% t(Vk)
  UkSkVk <- UkSk %*% SkVk
  ## DÃ©normalisation
  Pk <- UkSkVk %+=% votes.utilisateurs.moyen
  return(Pk)
}

splitSet <-split(m.filled, 0.1)
m.filled.train <- splitSet[[1]]
test.indices <- splitSet[[2]]
m.filled.train <- fillingPratique(m.filled.train)

# Prediction (MAE)
aux <- c(2, 5:21, 25, 50)
auxInd <- c(1:20)
P <- lapply(aux, FUN = function(x) {print(x);predVotes(m.filled.train,x,item.mean) })
P <- lapply(aux, predict.SVD, M = m.filled.train)

err <- lapply(auxInd, function(x) mae(m.na[test.indices], P[[x]][test.indices]))
plot(aux, unlist(err))
#La forme de la courbe est globalement la même que celle donnée dans l'article.
#On note un nombre de dimensions idéal de 9, qui est un peu plus élevé que la valeur de 14 trouvée dans l'article,
#mais qui n'est pas aberrante non plus. La présence de pics (à nbDim = 11 par exemple) est cependant curieuse,
#on s'attendait à une fonction décroissante puis croissante et plutôt très régulière.

#attention, la forme de la courbe dépend du résultat de la fonction split et peut parfois
#être "mauvaise", en présentant des pics brusques pour certaines valeurs. La tendance générale
#reste cependant (sur plusieurs essais) à une valeur optimale entre 8 et 18 


# Question 6 et 7 
#############################################################################
## Avec validation croisÃ©e basÃ©e sur 10 replis (10 folds).
## Le code ne fait qu'un repli. <- non, maintenant il en fait 10
## Le principe consiste premiÃ¨rement Ã  crÃ©er un vecteur de cellules alÃ©atoires qui couvrent l'ensemble de la matrice.
## Ce vecteur est ensuite divisÃ© en 10 replis (sections).
## Pour chaque repli, un index boolÃ©en est crÃ©Ã© pour les donnÃ©es de tests et sa nÃ©gation correspond aux donnÃ©es d'entraÃ®nement.
## Index alÃ©atoire des donnÃ©es de tests
i.observed <- which(m > 0)
i.hasard <- sample(i.observed, length(i.observed))
length(i.hasard)
fold.size <- round(length(i.hasard) / 10)

#pour la question 7
m.zero <- m.na
m.zero[is.na(m.na)]<-0


## Pour les erreurs
err.mae <- matrix(data = NA, nrow = 10, ncol = 1)
err.rmse <- matrix(data = NA, nrow = 10, ncol = 1)
err.mae.svd <- matrix(data = NA, nrow = 10, ncol = 1)
err.rmse.svd <- matrix(data = NA, nrow = 10, ncol = 1)
err.mae.q7 <- matrix(data = NA, nrow = 10, ncol = 1)
err.rmse.q7 <- matrix(data = NA, nrow = 10, ncol = 1)
## 10 replis
nbDim <- 9
for (i in 1:10) {
  i.false <- rep(FALSE, length(m))
  fold.number <- i
  ## Index boolÃ©en pour les cellules de test et d'entraÃ®nement
  i.test.b <- i.false
  ## Les cellules indexÃ©es du repli correspondant sont fixÃ©es Ã  TRUE pour le test...
  i.test.b[i.hasard[((fold.number - 1) * fold.size):((fold.number) * fold.size)]] <-
    TRUE
  ## ...et Ã  FALSE pour l'entraÃ®nement
  i.train.b <-  !i.test.b
  m.na.train <- m.na
  m.na.train[i.test.b] <-
    NA # on enlÃ¨ve les donnÃ©es de test pour l'entraÃ®nement
  table(m.na.train)
  votes.films.moyens <- colMeans(m.na.train, na.rm = T)
  mean(votes.films.moyens) # des NaN pourraient Ãªtre crÃ©Ã©s car certains films n'ont plus aucun vote
  ## Il faudrait alors remplacer ces colonnes par une valeur correspondant Ã  la moyenne gÃ©nÃ©rale.
  moy.globale <- mean(m.na.train, na.rm = T)
  films.sans.votes <- colSums(m.na.train, na.rm = T) == 0
  sum(films.sans.votes) # si 0 alors pas besoin de faire l'ajustement suivant
  m.na.train[, films.sans.votes] <- moy.globale
  votes.films.moyen <- colMeans(m.na.train, na.rm = T)
  m.na.train.filled <- fillingPratique(m.na.train)
  ## fin de l'ajustement
  hist(votes.films.moyens)
  ## votes moyens des utilisateurs de test
  votes.utilisateurs.moyen <- rowMeans(m.na.train, na.rm = T)
  ## pour faire changement, utilisons la moyenne arithmÃ©tique
  mean(votes.utilisateurs.moyen) # vÃ©fication si ajustement nÃ©cessaire (ici ce ne l'est pas et on continue sans)
  hist(votes.utilisateurs.moyen)
  #votes attendus pour la baseline
  votes.attendus <-
    outer(votes.utilisateurs.moyen, votes.films.moyen, FUN = '+') / 2
  #votes attendus pour la SVD
  votes.attendus.svd <- predict.SVD(nbDim, M = m.na.train.filled)
  ## Histogramme des erreurs
  hist(votes.attendus[i.test.b] - m[i.test.b])
  hist(votes.attendus.svd[i.test.b] - m[i.test.b])
  
  #partie utile pour la question 7
  #donne la corrélation entre utilisateurs
  user.cor = cor(t(m.na.train), use='pairwise.complete.obs', method='pearson')
  user.cor[is.na(user.cor)] <- 0
  #normalisation des poids
  user.cor.norm <- t(t(user.cor)/colSums(user.cor))
  
  #on a remplacé les NA par des 0 pour le calcul de la moyenne et le produit matriciel
  #calcule la moyenne pondérée pour chaque item pour chaque utilisateur
  m_pred <- user.cor.norm%*%m.zero
  #gestion des cas limites
  m_pred[m_pred < 1 ] <- 1
  m_pred[m_pred > 5] <- 5
 
  
  err.mae.q7<-  mae(m_pred[i.test.b],m.na[i.test.b])
  err.rmse.q7 <- sqrt(mean((m_pred[i.test.b] - m[i.test.b]) ^ 2, na.rm = T))
  
  ## Erreur absolue moyenne
  err.mae[i, 1] <- mean(abs(votes.attendus[i.test.b] - m[i.test.b]), na.rm = T)
  err.mae.svd[i, 1] <- mean(abs(votes.attendus.svd[i.test.b] - m[i.test.b]), na.rm = T)
  ## Racine carrÃ©e de erreur quadratique moyenne
  err.rmse.svd[i, 1] <- sqrt(mean((votes.attendus.svd[i.test.b] - m[i.test.b]) ^ 2, na.rm = T))
  err.rmse[i, 1] <- sqrt(mean((votes.attendus[i.test.b] - m[i.test.b]) ^ 2, na.rm = T))
  print(i)
}

err.mae
err.mae.svd
err.mae.q7
err.rmse
err.rmse.svd
err.rmse.q7



#Avec une valeur de 9 pour la svd, on note une mae aux alentours de 1, ce qui est relativement élevé comparé à la baseline. 
#On remarque également que la valeur d'environ 0.8 que nous avions obtenu à la question précédente est en réalité non représentative.
#La différence pourrait également venir du prétraitement (la manière dont on gère les valeurs manquantes)









