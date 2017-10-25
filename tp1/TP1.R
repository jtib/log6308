# Bibliotheques
list.of.packages <- c("RCurl", "dplyr", "lsa", "Matrix", "Metrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(RCurl)
library(dplyr)
library(lsa)
library(Matrix)
library(Metrics)

# Lecture des donnees
u.data <- read.csv("u.data.csv", sep='|', header=T)
u.user <- read.csv("u.user.csv", sep='|', header=T)
u.item <- read.csv('u.item.csv', sep='|', header=T)
v.u <- merge(u.data, u.user, by.x='user.id', by.y='id')

# Question 1
# Moyenne des votes par profession et par age
q1_job <- v.u %>% group_by(job) %>% summarise(mean=mean(rating))
q1_age <- v.u %>% group_by(age) %>% summarise(mean=mean(rating))


# Question 2
# 10 films les plus similaires a "Star Trek: The Final Frontier (1989)"
# Creation de la matrice de votes
m.sparse <- sparseMatrix(u.data[,1], u.data[,2], x=u.data[,3])
rownames(m.sparse) <- paste('u', 1:nrow(m.sparse), sep='')
colnames(m.sparse) <- paste('i', 1:ncol(m.sparse), sep='')
# Mesure du cosinus
# Fonction donnee : cosinus entre un vecteur v et chaque colonne de la matrice m
cosinus.vm <- function(v,m.sparse) { n <- sqrt(colSums(m.sparse^2)); (v %*% m.sparse)/(n * sqrt(sum(v^2))) }
# Le film considere a l'indice 450
m_cos <- cosinus.vm(m.sparse[,450], m.sparse)
# Fonctions donnees : index des premieres valeurs maximales/minimales d'une matrice
max.nindex <- function(M, n=5) {
    i <- order(M, decreasing=TRUE)
    return(i[1:n])
}
min.nindex <- function(M, n=5) {
    i <- order(M)
    return(i[1:n])
}
q2_cos <- max.nindex(m_cos, n=11)
# Correlation avec la matrice de votes
# Utilisation de la matrice nonsparse des votes
m.nonsparse <- as.matrix(m.sparse)
m.nonsparse[m.nonsparse==0] <- NA
m_cor = cor(m.nonsparse, use='pairwise.complete.obs', method='pearson')
q2_cor <- max.nindex(m_cor[,450], n=11)


# Question 3
# Calcul du vote des utilisateurs pour un film (approche item-item)
n.voisins <- 20 + 1
votes.communs <- (colSums((m.sparse[,450] * m.sparse) > 0)) # nombre de votes communs
distance.450 <- sqrt(colSums((m.sparse[,450] - m.sparse)^2)) # valeurs manquantes a 0 (ce qu'on prend pour cette question)
# Calcul des voisins
(i.distance.450 <- min.nindex(distance.450, n.voisins))
# Calcul des poids
# On met aussi les valeurs manquantes a 0
poids.450 <- cosinus.vm(m.sparse[,450], m.sparse)
# Pour chaque note manquante pour ce film, on prend les notes données par l'utilisateur aux 20 films les plus proches et on multiplie chacune par le poids correspondant calculé à l'étape précédente

# Utilisateurs qui n'ont pas mis de note au film 450
users.no.450 <- which(is.na(m.nonsparse[,450]))
# Votes sur les 20 films voisins des utilisateurs qui n'ont pas mis de note au film 450
votes.no.450.voisins <- m.nonsparse[users.no.450, i.distance.450[-1]]
# Initialisation
votes.calc.450 <- m.nonsparse[,450]
# Calcul
tmp_1 <- t(apply(votes.no.450.voisins, FUN=function(x) poids.450[i.distance.450[-1]]*x, MARGIN=1))
tmp_2 <- matrix(nrow=nrow(tmp_1), ncol=ncol(tmp_1), 1)
tmp_2[is.na(tmp_1)] <- 0
poids.450.rep <- matrix(nrow=nrow(tmp_1), ncol=20, poids.450[i.distance.450[-1]], byrow=TRUE)
votes.calc.450 <- rowSums(tmp_1, na.rm=TRUE)/rowSums(tmp_2*poids.450.rep)

# Affichage
print("Votes calculés pour le film 450")
print(votes.calc.450)
# Note : on a maintenant des notes pour ce film pour 92 utilisateurs, au lieu de 63 avant.
# Ce n'est toujours pas beaucoup par rapport au nombre total d'utilisateurs (943).
# Beaucoup d'utilisateurs n'ont jamais donné de note à un des 20 films les plus proches du n°450.

# Question 4
# Erreur quadratique moyenne (racine)
# Prediction pour ceux qui ont deja mis une note
# Utilisateurs qui ont mis une note au film 450
users.base.450 <- which(!is.na(m.nonsparse[,450]))
# Votes pour les 20 voisins des utilisateurs qui ont mis une note au film 450
votes.450.voisins <- m.nonsparse[users.base.450, i.distance.450[-1]]
# Base de comparaison
votes.base.450 <- m.nonsparse[,450][!is.na(m.nonsparse[,450])]
# Initialisation
votes.compare.450 <- rep(NA,length(votes.base.450))
# Calcul
## Numerateur
tmp <- t(apply(votes.450.voisins, FUN=function(x) poids.450[i.distance.450[-1]]*x, MARGIN=1))
tmp0 <- matrix(nrow=nrow(tmp), ncol=ncol(tmp), 1)
tmp0[is.na(tmp)] <- 0
poids.450.rep.err <- matrix(nrow=nrow(tmp), ncol=20, poids.450[i.distance.450[-1]], byrow=TRUE)
## Quotient
votes.compare.450 <- rowSums(tmp, na.rm=TRUE)/rowSums(tmp0*poids.450.rep.err)

# Affichage
print("RMSE")
erreur <- rmse(votes.base.450[!is.na(votes.compare.450)], votes.compare.450[!is.na(votes.compare.450)])
print(erreur)
# L'erreur est d'environ 0.9, ce qui pour des notes de 1 à 5 est assez important mais peu surprenant vu le peu de données disponibles.
