##############################################
##                                          ##
##             LOG 6308 - TP 1              ##
##                                          ##
##    Juliette Tibayrenc - Théo Rubenach    ##
##    1800292 - 1800314                     ##
##                                          ##
############################################## 

rm(list = ls())
library(Matrix)
library(dplyr)
library(Metrics)
library(lsa)

print("Attention le workspace est à adapter !")
setwd("D:/POLYMTL/LOG6308")

#Lecture des données
u.data <- read.csv(file='u.data.csv', sep='|', header=T)
u.user <- read.csv(file='u.user.csv', sep='|', header=T)
u.item <- read.csv(file='u.item.csv', sep='|', header=T)
v.u <- merge(u.data, u.user, by.x='user.id', by.y='id')
#Creation de la matrice de votes
m.sparse <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
#On crée une version où les valeurs manquantes, mises à 0 par défaut, sont remplacées par NA
m.nonsparse <- as.matrix(m.sparse)
m.nonsparse[m.nonsparse==0] <- NA

rownames(m.sparse) <- paste('u', 1:nrow(m.sparse), sep='')
colnames(m.sparse) <- paste('i', 1:ncol(m.sparse), sep='')


################################################################
### Question 1
## Moyenne des votes par profession et par age
################################################################

#dplyr permet une solution rapide et lisible en une ligne
q1_job <- v.u %>% group_by(job) %>% summarise(mean=mean(rating))
q1_age <- v.u %>% group_by(age) %>% summarise(mean=mean(rating))
print(q1_job)
print(q1_age)


#################################################################
# Question 2
# 10 films les plus similaires a "Star Trek: The Final Frontier (1989)"
#################################################################

# Fonctions donnees : index des premieres valeurs maximales/minimales d'une matrice
max.nindex <- function(M, n=5) {
  i <- order(M, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(M, n=5) {
  i <- order(M)
  return(i[1:n])
}

############
# Mesure du cosinus
# Fonction donnee : cosinus entre un vecteur v et chaque colonne de la matrice m
#on utilise la matrice sparse; la matrice avec des NA ne donnerait que des NA (ou presque) en sortie de la mesure du cosinus.
cosinus.vm <- function(v,m.sparse) { n <- sqrt(colSums(m.sparse^2)); (v %*% m.sparse)/(n * sqrt(sum(v^2))) }
# Le film considere a l'indice 450
m_cos <- cosinus.vm(m.sparse[,450], m.sparse)

q2_cos <- max.nindex(m_cos, n=11)
#films les plus proches selon le cosinus
print(q2_cos)
print(u.item[q2_cos,2])


############
# Correlation avec la matrice de votes
# Utilisation de la matrice nonsparse des votes
m_cor = cor(m.nonsparse, use='pairwise.complete.obs', method='pearson')
q2_cor <- max.nindex(m_cor[,450], n=11)
#films les plus proches selon la corélation
print(q2_cor)
print(u.item[q2_cor,2])

#############
# Utilisation de la matrice sparse des votes
m_cor_sparse = cor(as.matrix(m.sparse), use='pairwise.complete.obs', method='pearson')
q2_cor_sparse <- max.nindex(m_cor_sparse[,450], n=11)
#films les plus proches selon la corélation
print(q2_cor_sparse)
print(u.item[q2_cor_sparse,2])

#Ici, le cosinus et la corrélation donnent les mêmes résultats si on laisse les valeurs manquantes à 0.
#En revanche, les résultats sont très différents si on met les valeurs manquantes à NA pour l'un et pas pour l'autre (un seul film sur 10 en commun).
#Cela semble assez logique au vu de la proportion très élevée de NA dans la matrice.
#Notons que R nous prévient que la corrélation calculée avec la matrice avec NA (non sparse) est problématique.
#En effet, l'écart-type vaut 0 pour certains films, ce qui entraîne une division par 0, gérée on ne sait comment par la fonction.
#Ceci peut expliquer les grosses différences dans les résultats des deux versions de la gestion des valeurs manquantes.
#Comme on constate en plus une grande similarité entre la corrélation avec m.sparse et le cosinus, il semble plus raisonnable de choisir cette version de la corrélation.



########################################################
# Question 3
# Utilisez une approche item-item pour calculer le vote au film "Star Trek V: The Final Frontier (1989)" des utilisateurs qui n'ont pas de vote pour celui-ci.
########################################################
n.voisins <- 20 + 1
votes.communs <- (colSums((m.sparse[,450] * m.sparse) > 0)) # nombre de votes communs
distance.450 <- sqrt(colSums((m.sparse[,450] - m.sparse)^2)) # valeurs manquantes a 0 (ce qu'on prend pour cette question)
# Calcul des voisins
(i.distance.450 <- min.nindex(distance.450, n.voisins))
# Calcul des poids
# On met aussi les valeurs manquantes a 0
poids.450 <- cosinus.vm(m.sparse[,450], m.sparse)
# Pour chaque note manquante pour ce film, on prend les notes données par l'utilisateur aux 20 films les plus proches 
#et on multiplie chacune par le poids correspondant calculé à l'étape précédente

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
# Beaucoup d'utilisateurs n'ont donné de note à aucun des 20 films les plus proches du n°450, la valeur prédite est donc NA.


#########################################
# Question 4
# Calculez l'erreur quadratique moyenne des prédiction de l'approche item-item à la question précédente en la comparant aux valeurs observées. 
#########################################

# Calcul de la valeur prédite pour les utilisateurs qui ont deja mis une note (pour pouvoir comparer)
# Utilisateurs qui ont mis une note au film 450 :
users.base.450 <- which(!is.na(m.nonsparse[,450]))
# Votes pour les 20 voisins des utilisateurs qui ont mis une note au film 450
votes.450.voisins <- m.nonsparse[users.base.450, i.distance.450[-1]]
# Base de comparaison
votes.base.450 <- m.nonsparse[,450][!is.na(m.nonsparse[,450])]
# Initialisation
votes.compare.450 <- rep(NA,length(votes.base.450))
# Calcul où on utilise une pondération des voisins par la distance.
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
#notons que nous avons calculé la RMSE et non pas la MSE. La MSE serait un peu moindre, (0.9^2) mais la RMSE présente l'avantage d'être
#homogène aux valeurs des données (comme un écart type à la place d'une variance).




#####################################################
##Question 5
##Un utilisateur a coté la note la plus faible (1) à tous les films de Star Wars et la note la plus forte (5) à tous les films de Star Trek.
##Quels 10 films lui conseillez-vous? Utilisez une approche utilisateur-utilisateur pour la réponse et 20 voisins rapprochés.
####################################################"

#récupération des indices des films
indices.star.wars <- c(172,181)
indices.star.trek <- grep("trek",
                          as.character(u.item$movie.title), 
                          ignore.case=T) 
indices.starfilms <- c(indices.star.trek, indices.star.wars)

#vecteurs des votes de l'utilisateur sur les films en question
starUserVotes <- c( matrix(5,length(indices.star.trek),1), matrix(1,length(indices.star.wars),1))
 
#fonction pour calculer la distance euclidienne
d_euclidienne <- function(a_user, b_user){
                          sqrt(sum((a_user-b_user)^2))
}
#fonction calculant la distance euclidienne entre un utilisateur quelconque et notre fan de SF
dbis <- function(b_user){d_euclidienne(starUserVotes,b_user)}


#vecteur des distances de chaque utilisateur avec notre fan de Science Fiction (pour trouver les plus proches voisins)
distanceVector <- apply(as.matrix(m.sparse[,indices.starfilms]), dbis, MARGIN = 1)


#on récupère les 20 plus proches voisins, leur identifiant, leur score de distance et leurs votes
topDistances <- sort(distanceVector)[1:20]
topNeighbors <- names(topDistances)
neighborsVotes <- m.sparse[topNeighbors,]


#fonction à appliquer sur les colonnes pour obtenir une moyenne pondérée (par la distance) des votes des plus proches voisins pour chaque film
filmScore <- function(film, distances, topNeighbors){
    sum(film[topNeighbors]*distances[topNeighbors])/sum(distances[topNeighbors])
}

#application de la fonction
meilleursNotesFilms <- apply(m.sparse, function(x) filmScore(x,topDistances,topNeighbors), MARGIN = 2) 
#on retire les NA
MeilleursFilms <- sort(meilleursNotesFilms[!is.na(meilleursNotesFilms)],decreasing = TRUE, index.return = TRUE)$ix
#films à ne pas suggérer car ce sont les films star wars et star trek

#résultats en enlevant les films star trek et star wars
MeilleursFilms2 <- MeilleursFilms[!MeilleursFilms %in% indices.starfilms][1:10]
print(as.character(u.item[MeilleursFilms2,2]))

#Dans un premier temps, nous avions essayé de réaliser ce calcul sur la matrice non sparse (ie avec les NA).
#Nous avons constaté que seuls deux films ressortent (en dehors des films star trek et star wars qui se retrouvent évidemment dans les plus proches voisins)
#on ne peut donc retirer tous les NA, on utilise donc m.sparse, qui renvoie suffisamment de films pour avoir des plus proches voisins.
#Cela n'est pas dérangeant puisqu'on calcule la distance sur 10 films sur lesquels l'utilisateur fan de SF a voté; 
#les valeurs nulles ne viendront donc pas gêner le calcul de la distance 
#(contrairement à un cas de figureoù on aurait aucune valeur commune entre deux utilisateurs, ce qui peut facilement se produire)



########################################################
## Question 6
##Je suis un nouvel utilisateur. Vous connaissez ma profession, mon sexe et mon âge. 
##Développez un algorithme bayésien pour recommander 10 films sur la base de ces trois catégories.
########################################################

#on va utiliser la formule des probabilités totales en supposant l'indépendance des facteurs (bien que ce ne soit probablement pas le cas)
#on aurait sinon de trop petits échantillons voire aucun...
#on aura donc P(note_i_j | age_i, sexe_i, profession_i) = K *  P( note_i_j ) * P(age_i | v_i_j) * P(sexe_i | v_i_j) * P(profession_i | v_i_j) 
#où i est l'utilisateur, j le film et K une constante ne changeant rien car indépendante du vote (elle impactera tous les résultats de la même façon)


#v.notes est le vecteur de distribution de probabilités des notes pour un unique film, avec correction de Laplace pour gérer les cas limites.
f.notes <- function(v.notes) {
  notes <- 1:5
  res <- apply(as.matrix(notes),MARGIN =1, FUN = function(note) length(v.notes[v.notes==note]))
  return((1+res)/(5+sum(res))) #5 est le nombre de notes possibles
}


#cette fonction permet d'avoir les distributions de probas pour chaque note pour chaque film
f.film <- function(data){
  r <- apply(data[,],MARGIN = 2, FUN = f.notes)
  return(r)
}

#distributions de chaque note pour chaque film
p.notes.probas <- f.film(m.sparse)

#fonction interne utilisées dans fplop, pour obtenir les notes d'un film
f.plop <- function(data,notes){
  res <- apply(as.matrix(notes),MARGIN =1, FUN = function(note) length(data[data==note]))
  return(res)
}
#nombre de notes de chaque type pour chaque film, dans les données fournies 
#(similaire à f.film, mais celle-ci renvoie une probabilité et pas un nombre)
f.fplop <- function(data){
  notes <- 1:5
  r<- apply(data[,],MARGIN = 2, FUN = f.plop, note = notes)
}
#utilisé dans les fonctions suivantes
p.notes.cardinal <- f.fplop(m.sparse)

#Renvoie la distribution de probas d'un job donné en fonction de la note attribuée : 
#P(job = j | note ) et ce pour chaque film
#avec correction de Laplace pour éviter les NaN, 21 étant le nombre de professions représentées dans la base de données
f.job <- function(j){ 
  notes <- 1:5
  u.j <- (u.user %>% filter(u.user$job == j))$id
  u.j.note <- m.sparse[u.j,]
  u.j.card <- f.fplop(u.j.note)
  u.j.proba <-( u.j.card+1)/(p.notes.cardinal+21) #il y a 21 métiers dans la base de données
return(u.j.proba)
}

#Fonction similaire à f.job, mais pour le genre.
#P(genre = "M" | note) ou P(genre = "F" |note) pour chaque film
f.gender <- function(g){ 
  u.j <- (u.user %>% filter(u.user$gender == g))$id
  u.j.note <- m.sparse[u.j,]
  u.j.card <- f.fplop(u.j.note)
  u.j.proba <- (u.j.card+1)/(p.notes.cardinal+2)# 2 genres
  return(u.j.proba)
}

#Troisième fonction dans le même style pour renvoyer P(age |note) pour chaque film.
#on teste par décennies (de 0 à 9 ans, de 10 à 19, etc...)
#P( 10*i <= age < 10*(i+1) | notes ), avec i entre 0 et 7.
#Ce système pourrait être affiné (par exemple avec une fenêtre glissante centrée sur l'âge de l'individu)
#il ne faut cependant pas trop réduire la fenêtre pour éviter d'avoir des sous-ensembles trop petits et donc non représentatifs
f.age <- function(a){ 
  u.j <- (u.user %>% filter(((u.user$age)%/%10) == a%/%10))$id
  u.j.note <- m.sparse[u.j,]
  u.j.card <- f.fplop(u.j.note)
  u.j.proba <- (u.j.card+1)/(p.notes.cardinal+8) #l'âge allant dans la base de données de 7 à 73 ans, il y a 8 décennies possibles
  return(u.j.proba)
}

#fonction finale déterminant dix films à recommender à une personne en fonction de son âge, de son genre et de sa profession
f.bayes <- function(a,g,p){
  p.age <- f.age(a)
  p.gender <- f.gender(g)
  p.profession <- f.job(p)
  #distribution de probabilités de chaque note (indépendamment de toute autre variable) pour chaque film
  p.notes <- f.film(m.sparse)
  
  #formule des probabilités totales
  p.filmScoresProbas <- p.gender*p.profession*p.age*p.notes
  
  #transformation de la distribution de probabilités prédite par le modèle en un score pour le film
  #on fait la moyenne des 5 notes possibles pondérées par leurs probabilités respectives
  notes = 1:5
  p.inter <- p.filmScoresProbas * notes
  p.filmsScore <- colSums(p.inter)
  #récupération de l'index des 10 meilleurs résultats
  recommendationIndices <-  sort(p.filmsScore, decreasing = TRUE, index.return = TRUE)$ix[1:10]
  #on renvoie les titres
  recommendations <- u.item[recommendationIndices,2]
  return(recommendations)
  }

#exemple de l'influence du genre sur les résultats : notre modèle montre quelques stéréotypes qui doivent se trouver dans les données:
#(Roméo et Juliette ou Bed of Roses pour les filles, Austin Power et Reservoir Dogs pour les garçons...
#Il semble donc qu'il fonctionne plutôt bien, dans la mesure où les trois critères sont exploités exactement de la même manière. 
maleExample <- f.bayes(22,"M","student")
femaleExample <- f.bayes(22,"F","student")
print(as.character(maleExample))
print(as.character(femaleExample))


#Bonne relache !

