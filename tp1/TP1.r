##############################################
##                                          ##
##             LOG 6308 - TP 1              ##
##                                          ##
##    Juliette Tibayrenc - Th�o Rubenach    ##
##    1800292 - 1800314                     ##
##                                          ##
############################################## 

rm(list = ls())
library(Matrix)
library(dplyr)
library(Metrics)
library(lsa)

print("Attention le workspace est � adapter !")
setwd("D:/POLYMTL/LOG6308")

#Lecture des donn�es
u.data <- read.csv(file='u.data.csv', sep='|', header=T)
u.user <- read.csv(file='u.user.csv', sep='|', header=T)
u.item <- read.csv(file='u.item.csv', sep='|', header=T)
v.u <- merge(u.data, u.user, by.x='user.id', by.y='id')
#Creation de la matrice de votes
m.sparse <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
#On cr�e une version o� les valeurs manquantes, mises � 0 par d�faut, sont remplac�es par NA
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
#films les plus proches selon la cor�lation
print(q2_cor)
print(u.item[q2_cor,2])

#############
# Utilisation de la matrice sparse des votes
m_cor_sparse = cor(as.matrix(m.sparse), use='pairwise.complete.obs', method='pearson')
q2_cor_sparse <- max.nindex(m_cor_sparse[,450], n=11)
#films les plus proches selon la cor�lation
print(q2_cor_sparse)
print(u.item[q2_cor_sparse,2])

#Ici, le cosinus et la corr�lation donnent les m�mes r�sultats si on laisse les valeurs manquantes � 0.
#En revanche, les r�sultats sont tr�s diff�rents si on met les valeurs manquantes � NA pour l'un et pas pour l'autre (un seul film sur 10 en commun).
#Cela semble assez logique au vu de la proportion tr�s �lev�e de NA dans la matrice.
#Notons que R nous pr�vient que la corr�lation calcul�e avec la matrice avec NA (non sparse) est probl�matique.
#En effet, l'�cart-type vaut 0 pour certains films, ce qui entra�ne une division par 0, g�r�e on ne sait comment par la fonction.
#Ceci peut expliquer les grosses diff�rences dans les r�sultats des deux versions de la gestion des valeurs manquantes.
#Comme on constate en plus une grande similarit� entre la corr�lation avec m.sparse et le cosinus, il semble plus raisonnable de choisir cette version de la corr�lation.



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
# Pour chaque note manquante pour ce film, on prend les notes donn�es par l'utilisateur aux 20 films les plus proches 
#et on multiplie chacune par le poids correspondant calcul� � l'�tape pr�c�dente

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
print("Votes calcul�s pour le film 450")
print(votes.calc.450)
# Note : on a maintenant des notes pour ce film pour 92 utilisateurs, au lieu de 63 avant.
# Ce n'est toujours pas beaucoup par rapport au nombre total d'utilisateurs (943).
# Beaucoup d'utilisateurs n'ont donn� de note � aucun des 20 films les plus proches du n�450, la valeur pr�dite est donc NA.


#########################################
# Question 4
# Calculez l'erreur quadratique moyenne des pr�diction de l'approche item-item � la question pr�c�dente en la comparant aux valeurs observ�es. 
#########################################

# Calcul de la valeur pr�dite pour les utilisateurs qui ont deja mis une note (pour pouvoir comparer)
# Utilisateurs qui ont mis une note au film 450 :
users.base.450 <- which(!is.na(m.nonsparse[,450]))
# Votes pour les 20 voisins des utilisateurs qui ont mis une note au film 450
votes.450.voisins <- m.nonsparse[users.base.450, i.distance.450[-1]]
# Base de comparaison
votes.base.450 <- m.nonsparse[,450][!is.na(m.nonsparse[,450])]
# Initialisation
votes.compare.450 <- rep(NA,length(votes.base.450))
# Calcul o� on utilise une pond�ration des voisins par la distance.
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
# L'erreur est d'environ 0.9, ce qui pour des notes de 1 � 5 est assez important mais peu surprenant vu le peu de donn�es disponibles.
#notons que nous avons calcul� la RMSE et non pas la MSE. La MSE serait un peu moindre, (0.9^2) mais la RMSE pr�sente l'avantage d'�tre
#homog�ne aux valeurs des donn�es (comme un �cart type � la place d'une variance).




#####################################################
##Question 5
##Un utilisateur a cot� la note la plus faible (1) � tous les films de Star Wars et la note la plus forte (5) � tous les films de Star Trek.
##Quels 10 films lui conseillez-vous? Utilisez une approche utilisateur-utilisateur pour la r�ponse et 20 voisins rapproch�s.
####################################################"

#r�cup�ration des indices des films
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


#on r�cup�re les 20 plus proches voisins, leur identifiant, leur score de distance et leurs votes
topDistances <- sort(distanceVector)[1:20]
topNeighbors <- names(topDistances)
neighborsVotes <- m.sparse[topNeighbors,]


#fonction � appliquer sur les colonnes pour obtenir une moyenne pond�r�e (par la distance) des votes des plus proches voisins pour chaque film
filmScore <- function(film, distances, topNeighbors){
    sum(film[topNeighbors]*distances[topNeighbors])/sum(distances[topNeighbors])
}

#application de la fonction
meilleursNotesFilms <- apply(m.sparse, function(x) filmScore(x,topDistances,topNeighbors), MARGIN = 2) 
#on retire les NA
MeilleursFilms <- sort(meilleursNotesFilms[!is.na(meilleursNotesFilms)],decreasing = TRUE, index.return = TRUE)$ix
#films � ne pas sugg�rer car ce sont les films star wars et star trek

#r�sultats en enlevant les films star trek et star wars
MeilleursFilms2 <- MeilleursFilms[!MeilleursFilms %in% indices.starfilms][1:10]
print(as.character(u.item[MeilleursFilms2,2]))

#Dans un premier temps, nous avions essay� de r�aliser ce calcul sur la matrice non sparse (ie avec les NA).
#Nous avons constat� que seuls deux films ressortent (en dehors des films star trek et star wars qui se retrouvent �videmment dans les plus proches voisins)
#on ne peut donc retirer tous les NA, on utilise donc m.sparse, qui renvoie suffisamment de films pour avoir des plus proches voisins.
#Cela n'est pas d�rangeant puisqu'on calcule la distance sur 10 films sur lesquels l'utilisateur fan de SF a vot�; 
#les valeurs nulles ne viendront donc pas g�ner le calcul de la distance 
#(contrairement � un cas de figureo� on aurait aucune valeur commune entre deux utilisateurs, ce qui peut facilement se produire)



########################################################
## Question 6
##Je suis un nouvel utilisateur. Vous connaissez ma profession, mon sexe et mon �ge. 
##D�veloppez un algorithme bay�sien pour recommander 10 films sur la base de ces trois cat�gories.
########################################################

#on va utiliser la formule des probabilit�s totales en supposant l'ind�pendance des facteurs (bien que ce ne soit probablement pas le cas)
#on aurait sinon de trop petits �chantillons voire aucun...
#on aura donc P(note_i_j | age_i, sexe_i, profession_i) = K *  P( note_i_j ) * P(age_i | v_i_j) * P(sexe_i | v_i_j) * P(profession_i | v_i_j) 
#o� i est l'utilisateur, j le film et K une constante ne changeant rien car ind�pendante du vote (elle impactera tous les r�sultats de la m�me fa�on)


#v.notes est le vecteur de distribution de probabilit�s des notes pour un unique film, avec correction de Laplace pour g�rer les cas limites.
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

#fonction interne utilis�es dans fplop, pour obtenir les notes d'un film
f.plop <- function(data,notes){
  res <- apply(as.matrix(notes),MARGIN =1, FUN = function(note) length(data[data==note]))
  return(res)
}
#nombre de notes de chaque type pour chaque film, dans les donn�es fournies 
#(similaire � f.film, mais celle-ci renvoie une probabilit� et pas un nombre)
f.fplop <- function(data){
  notes <- 1:5
  r<- apply(data[,],MARGIN = 2, FUN = f.plop, note = notes)
}
#utilis� dans les fonctions suivantes
p.notes.cardinal <- f.fplop(m.sparse)

#Renvoie la distribution de probas d'un job donn� en fonction de la note attribu�e : 
#P(job = j | note ) et ce pour chaque film
#avec correction de Laplace pour �viter les NaN, 21 �tant le nombre de professions repr�sent�es dans la base de donn�es
f.job <- function(j){ 
  notes <- 1:5
  u.j <- (u.user %>% filter(u.user$job == j))$id
  u.j.note <- m.sparse[u.j,]
  u.j.card <- f.fplop(u.j.note)
  u.j.proba <-( u.j.card+1)/(p.notes.cardinal+21) #il y a 21 m�tiers dans la base de donn�es
return(u.j.proba)
}

#Fonction similaire � f.job, mais pour le genre.
#P(genre = "M" | note) ou P(genre = "F" |note) pour chaque film
f.gender <- function(g){ 
  u.j <- (u.user %>% filter(u.user$gender == g))$id
  u.j.note <- m.sparse[u.j,]
  u.j.card <- f.fplop(u.j.note)
  u.j.proba <- (u.j.card+1)/(p.notes.cardinal+2)# 2 genres
  return(u.j.proba)
}

#Troisi�me fonction dans le m�me style pour renvoyer P(age |note) pour chaque film.
#on teste par d�cennies (de 0 � 9 ans, de 10 � 19, etc...)
#P( 10*i <= age < 10*(i+1) | notes ), avec i entre 0 et 7.
#Ce syst�me pourrait �tre affin� (par exemple avec une fen�tre glissante centr�e sur l'�ge de l'individu)
#il ne faut cependant pas trop r�duire la fen�tre pour �viter d'avoir des sous-ensembles trop petits et donc non repr�sentatifs
f.age <- function(a){ 
  u.j <- (u.user %>% filter(((u.user$age)%/%10) == a%/%10))$id
  u.j.note <- m.sparse[u.j,]
  u.j.card <- f.fplop(u.j.note)
  u.j.proba <- (u.j.card+1)/(p.notes.cardinal+8) #l'�ge allant dans la base de donn�es de 7 � 73 ans, il y a 8 d�cennies possibles
  return(u.j.proba)
}

#fonction finale d�terminant dix films � recommender � une personne en fonction de son �ge, de son genre et de sa profession
f.bayes <- function(a,g,p){
  p.age <- f.age(a)
  p.gender <- f.gender(g)
  p.profession <- f.job(p)
  #distribution de probabilit�s de chaque note (ind�pendamment de toute autre variable) pour chaque film
  p.notes <- f.film(m.sparse)
  
  #formule des probabilit�s totales
  p.filmScoresProbas <- p.gender*p.profession*p.age*p.notes
  
  #transformation de la distribution de probabilit�s pr�dite par le mod�le en un score pour le film
  #on fait la moyenne des 5 notes possibles pond�r�es par leurs probabilit�s respectives
  notes = 1:5
  p.inter <- p.filmScoresProbas * notes
  p.filmsScore <- colSums(p.inter)
  #r�cup�ration de l'index des 10 meilleurs r�sultats
  recommendationIndices <-  sort(p.filmsScore, decreasing = TRUE, index.return = TRUE)$ix[1:10]
  #on renvoie les titres
  recommendations <- u.item[recommendationIndices,2]
  return(recommendations)
  }

#exemple de l'influence du genre sur les r�sultats : notre mod�le montre quelques st�r�otypes qui doivent se trouver dans les donn�es:
#(Rom�o et Juliette ou Bed of Roses pour les filles, Austin Power et Reservoir Dogs pour les gar�ons...
#Il semble donc qu'il fonctionne plut�t bien, dans la mesure o� les trois crit�res sont exploit�s exactement de la m�me mani�re. 
maleExample <- f.bayes(22,"M","student")
femaleExample <- f.bayes(22,"F","student")
print(as.character(maleExample))
print(as.character(femaleExample))


#Bonne relache !

