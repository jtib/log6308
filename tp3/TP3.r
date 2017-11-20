# Lecture des données
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
### La première approche test est celle du vote au hasard.
mae(m.hasard, m.na)

## Deuxième et troisième approche : moyenne de l'utilisateur et moyenne de l'item
u.mean <- matrix(rowMeans(m.na, na.rm = T), nrow(m), ncol(m))
item.mean <-
  matrix(colMeans(m.na, na.rm = T), nrow(m), ncol(m), byrow = T)
mae(m.na, u.mean)
mae(m.na, item.mean)

## Mélange : moyenne de la note moyenne de l'item et celle de l'utilisateur
m.expect <- ((u.mean + item.mean) / 2)
corner <- function(m, ...)
  head(t(tail(t(m), ...)), ...)
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
## Normalisation, décomposition SVD et prédiction (10 dimensions)

## Normalisation telle que réalisée dans la partie "expérience pratique" (qui donne le graphe qu'on doit reproduire)
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



## Fonction réalisant la prédiction des votes après SVD avec un certain nombres de dimensions.
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

## Wrapper qui (1) fait la décomposition SVD et (2) fait les calculs de prédiction :
predVotes <- function(mat, nbdim, votes.items.moyen) {
  svd <- svd(mat)
  return(predsvd(nbdim, svd$d, svd$u, svd$v, votes.items.moyen))
}

## Utilitaire : découpage en ensemble d'entraînement et ensemble de validation.
### Renvoie le training set et la matrice booléenne des indices changés
### (pour pouvoir après calculer la MAE seulement sur les valeurs changées)
split <- function(m, ratio) {
  i.observed <- which(m > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  length(i.hasard)
  fold.size <- round(length(i.hasard) * ratio)
  i.false <- rep(FALSE, length(m))
  ## Index booléen pour les cellules de test et d'entraînement
  i.test.b <-
    i.false ## Les cellules indexées du replis correspondant sont fixées à TRUE pour le test...
  i.test.b[i.hasard[1:fold.size]] <-
    TRUE ## ...et à FALSE pour l'entraînement
  i.train.b <-  !i.test.b
  m.na.train <- m
  m.na.train[i.test.b] <-
    NA  # on enlève les données de test pour l'entraînement
  return(list(m.na.train, i.test.b))
}


m.na.pr <- fillingPratique(m.na)
dd.pr <- split(m.na.pr, 0.1)
m.filled <- dd.pr[[1]]
m.filled[is.na(m.filled)] <- mean(m.filled, na.rm = T)
testIndices.pr <- dd.pr[[2]]


### Prédiction des votes
test.pr <- predVotes(m.filled, 10, item.mean)

m.na.pr

# Question 4
## Erreur absolue moyenne
mae(m.na[testIndices.pr] , test.pr[testIndices.pr])

## Erreur quadratique moyenne
mse(m.na[testIndices.pr] , test.pr[testIndices.pr])

#Les valeurs semblent �lev�es compar�es � la baseline.
#Cepedant la baseline de la question 1 �tait r�alis�e sans s�paration entre
#ensemble de test et ensemble d'entra�nement; le r�sultat de notre SVD serait certainement meilleur dans ces conditions.
#On est toutefois bien au dessus des performances du r�sultat au hasard

# Question 5

## Fonction pour la prédiction
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
  ## Dénormalisation
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
#La forme de la courbe est globalement la m�me que celle donn�e dans l'article.
#On note un nombre de dimensions id�al de 9, qui est un peu plus �lev� que la valeur de 14 trouv�e dans l'article,
#mais qui n'est pas aberrante non plus. La pr�sence de pics (� nbDim = 11 par exemple) est cependant curieuse,
#on s'attendait � une fonction d�croissante puis croissante et plut�t tr�s r�guli�re.

#attention, la forme de la courbe d�pend du r�sultat de la fonction split et peut parfois
#�tre "mauvaise", en pr�sentant des pics brusques pour certaines valeurs. La tendance g�n�rale
#reste cependant (sur plusieurs essais) � une valeur optimale entre 8 et 18 


# Question 6 et 7 
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
  ## Index booléen pour les cellules de test et d'entraînement
  i.test.b <- i.false
  ## Les cellules indexées du repli correspondant sont fixées à TRUE pour le test...
  i.test.b[i.hasard[((fold.number - 1) * fold.size):((fold.number) * fold.size)]] <-
    TRUE
  ## ...et à FALSE pour l'entraînement
  i.train.b <-  !i.test.b
  m.na.train <- m.na
  m.na.train[i.test.b] <-
    NA # on enlève les données de test pour l'entraînement
  table(m.na.train)
  votes.films.moyens <- colMeans(m.na.train, na.rm = T)
  mean(votes.films.moyens) # des NaN pourraient être créés car certains films n'ont plus aucun vote
  ## Il faudrait alors remplacer ces colonnes par une valeur correspondant à la moyenne générale.
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
  ## pour faire changement, utilisons la moyenne arithmétique
  mean(votes.utilisateurs.moyen) # véfication si ajustement nécessaire (ici ce ne l'est pas et on continue sans)
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
  #donne la corr�lation entre utilisateurs
  user.cor = cor(t(m.na.train), use='pairwise.complete.obs', method='pearson')
  user.cor[is.na(user.cor)] <- 0
  #normalisation des poids
  user.cor.norm <- t(t(user.cor)/colSums(user.cor))
  
  #on a remplac� les NA par des 0 pour le calcul de la moyenne et le produit matriciel
  #calcule la moyenne pond�r�e pour chaque item pour chaque utilisateur
  m_pred <- user.cor.norm%*%m.zero
  #gestion des cas limites
  m_pred[m_pred < 1 ] <- 1
  m_pred[m_pred > 5] <- 5
 
  
  err.mae.q7<-  mae(m_pred[i.test.b],m.na[i.test.b])
  err.rmse.q7 <- sqrt(mean((m_pred[i.test.b] - m[i.test.b]) ^ 2, na.rm = T))
  
  ## Erreur absolue moyenne
  err.mae[i, 1] <- mean(abs(votes.attendus[i.test.b] - m[i.test.b]), na.rm = T)
  err.mae.svd[i, 1] <- mean(abs(votes.attendus.svd[i.test.b] - m[i.test.b]), na.rm = T)
  ## Racine carrée de erreur quadratique moyenne
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



#Avec une valeur de 9 pour la svd, on note une mae aux alentours de 1, ce qui est relativement �lev� compar� � la baseline. 
#On remarque �galement que la valeur d'environ 0.8 que nous avions obtenu � la question pr�c�dente est en r�alit� non repr�sentative.
#La diff�rence pourrait �galement venir du pr�traitement (la mani�re dont on g�re les valeurs manquantes)









