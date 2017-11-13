rm(list = ls())
library(Matrix)

# modifie un peu le pageRank par rapport � d=0.85 (utilis� pour les pages web habituelles),
# � tol�rance �gale pour la convergence de l'algorithme PageRank, mais les r�sultats principaux restent les m�mes
d = 0.6

m = read.table("citeseer.rtable")
m.matrix <- data.matrix(m)
m.matrix
# Erreur tol�r�e de l'algorithme
eps <- 0.001

# Fonction pageRank
pageRank <- function(mat, D, pr.init) {
  N <- ncol(mat)
  pr.prev <- pr.init
  cs <- colSums(mat)
  cs[cs==0] <- 1
  pr <- (1-D)/N + (D * mat %*% (pr.prev/cs))
  while(abs(max(pr-pr.prev))>=eps) {
    pr.prev <- pr
    pr <- (1-D)/N + (D * mat %*% (pr.prev/cs))
  }
  return(pr)
}

# Calcul des pageRanks des articles en prenant toute les donn�es en compte
n <- ncol(m)
pr.first.all <- rep(1,n)
ranks.all <- pageRank(m.matrix, d, pr.first.all)
ranks.all

# Il suffit � pr�sent de prendre, parmi les citations r�f�renc�es par l'article 422908, celles qui ont le meilleur pageRank.
# Domaine de recherche
S <- m.matrix['422908',]
S <- S[S==1]
S
# Matrice correspondante
colonnes.S <- strsplit(names(S), ',')
lignes.S <- lapply(names(S), function(str) strsplit(str, 'X')[[1]][2])
tmp <- subset(m.matrix, rownames(m.matrix) %in% lignes.S)
mat.S <- subset(tmp,, colnames(tmp) %in% colonnes.S)
# Calcul des pageRanks pour l'ensemble S
n.S <- ncol(mat.S)
pr.first.S <- rep(1,n.S)
ranks.S <- pageRank(mat.S, d, pr.first.S)
ranks.S
# On ordonne
ranks.S.order <- ranks.S[order(-ranks.S),,drop=FALSE]

# Vu le peu de citations pr�sentes dans la base de donn�es, on se limitera � recommander les 5 premi�res.
names(ranks.S.order[1:5,])

# Variante : utilisation du domaine S', qui inclut les citations des citations
# La matrice d'adjacence �lev�e au carr� par multiplication bool�enne repr�sente
# la composition de la relation d'origine par elle-m�me
m.matrix.square <- m.matrix %&% m.matrix
# Union des citations et des citations de citations
m.prime <- m.matrix | m.matrix.square
# Retrait de la diagonale
diag(m.prime) <- FALSE
m.prime

# Domaine de recherche
S.prime <- m.prime['422908',]
S.prime <- S.prime[S.prime==1]
S.prime

# Matrice correspondante
colonnes.S.prime <- strsplit(names(S.prime), ',')
lignes.S.prime <- lapply(names(S.prime), function(str) strsplit(str, 'X')[[1]][2])
tmp.prime <- subset(as.matrix(m.prime), rownames(m.prime) %in% lignes.S.prime)
mat.S.prime <- subset(tmp.prime,, colnames(tmp.prime) %in% colonnes.S.prime)

# Calcul des pageRanks pour l'ensemble S
n.S.prime <- ncol(mat.S.prime)
pr.first.S.prime <- rep(1,n.S.prime)
ranks.S.prime <- pageRank(mat.S.prime, d, pr.first.S.prime)
ranks.S.prime

# On ordonne
ranks.S.prime.order <- ranks.S.prime[order(-ranks.S.prime),,drop=FALSE]

# Et on renvoie les 5 premi�res
names(ranks.S.prime.order[1:5,])
# Pour comparaison
names(ranks.S.order[1:10,])
#row.names(ranks.S.order) <- paste('X', row.names(ranks.S.order), sep='')
length(names(ranks.S.order[1:10,])[names(ranks.S.order[1:10,]) %in% names(ranks.S.prime.order[1:10,])])
# 8 recommandations en commun
# La plupart des recommandations restent communes, mais deux nouvelles apparaissent.
# Ceci s'explique sans doute par la matrice utilis�e, qui ne contient que peu d'informations,
# et, en cons�quence, les articles cit�s par celui examin� ne sont pas forc�ment tr�s li�s entre eux,
# bien qu'appartenant � un domaine tr�s proche.
# On en conclut que l'�tendue du sous-ensemble de d�part permettrait de recommander des citations plus anciennes,
# potentiellement plus importantes dans le domaine scientifique consid�r� mais moins directement reli�es � l'article
# d'origine, mais que les donn�es disponibles sont tr�s insuffisantes pour conclure sur l'impact d'utilisation de S'
# de fa�on d�finie (et puis l'exp�rience a �t� faite avec un seul article d'origine, ce qui est de toute fa�on un nombre
# insuffisant d'exemples pour conclure en termes quantitatifs).




############################
#Question 2

m2 = as.matrix(m)


avgCitationParArticle = mean(rowSums(m))

#notons qu'ici les utilisateurs sont aussi les items.

#notre mesure de similarit� recommandera des articles qui citent les m�me articles que celui qui nous int�resse.
#ceci est logique car il est possible de recommander d'autres articles � partir d'un article r�cent. On utilisera donc les lignes de la matrice.
#Utiliser les colonnes am�nerait � recommander les articles co-cit�s par l'article qui nous int�resse, autrement dit les articles �galement cit�s par les articles qui citent notre article courant.
#cette derni�re approche semble plus adapt�e � des articles anciens. 

#on souhaite tester plusieurs mesures de similarit�s : la corr�lation de Pearson, le cosinus et une troisi�me mesure bas�e sur une modification de la distance de Hamming.

#Similarit� de deux lignes selon la corr�lation de Pearson
corSimL <- function(item_no1,item_no2, mat = m2){
  item1 = mat[item_no1,]
  item2 = mat[item_no2,]
  if( sum(item1)== 0 || sum(item2)==0)
    return(0)
  else
    return(cor(item1,item2,use = 'pairwise.complete.obs', method = 'pearson')) 
}


#Similarit� de deux lignes en utilisant le cosinus
cosSimL <- function(item_no1,item_no2, mat = m2) { 
  item1 = mat[item_no1,]
  item2 = mat[item_no2,]
  if( sum(item1*item2)== 0)
    return(0)
  else
    sum(item1 * item2)/( sqrt(sum(item1^2) * sum(item2^2))  ) 
}


#Similarit� de deux lignes en utilisant la distance de Hamming, mais divis�e par le nombre d'indices o� l'un des deux vecteurs est non nuls
#cela permet de favoriser les articles avec plus de citations, qui semblent donc plus pertinents.
#autrement dit  0 0 0 1 1 et 1 0 0 0 1 auront une distance de 2/3 (3 indices o� l'un des deux vecteurs est non nul).
#on multiplie par  -1 pour pouvoir trier les r�sultats de la m�me mani�re qu'avec la corr�lation et le cosinus
hammingSimL <- function(item_no1,item_no2, mat = m2) { 
  item1 = mat[item_no1,]
  item2 = mat[item_no2,]
  if( sum(item1)== 0 || sum(item2)==0)
    return(-10000)
  else
    a = abs(item1[item1+item2>0]-item2[item1+item2>0]) 
  return(-sum(a/length(a)))
}


#corr�lation de pearson pour deux colonnes
corSimC <- function(item_no1,item_no2, mat){
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1)== 0 || sum(item2)==0)
    return(0)
  else
    return(cor(item1,item2,use = 'pairwise.complete.obs', method = 'pearson')) 
}

#similarit� par cosinus pour deux colonnes
cosSimC <- function(item_no1,item_no2, mat) { 
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1*item2)== 0)
    return(0)
  else
    sum(item1 * item2)/( sqrt(sum(item1^2) * sum(item2^2))  ) 
}


#similarit� en utilisant la distance de Hamming modifi�e pour deux colonnes
hammingSimC <- function(item_no1,item_no2, mat) { 
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1)== 0 || sum(item2)==0)
    return(-10000)
  else
    a = abs(item1[item1+item2>0]-item2[item1+item2>0]) 
  return(-sum(a/length(a)))
}


#calcule la similarit� d'une colonne ou d'une lignes avec les autres
similarities <- function(item_no,method_of_similarity, mat = m2){
  res = seq(1,1090);
  similarities = sapply( res, FUN = function(x) method_of_similarity(x,item_no, mat))
  return(c(similarities))
}



#renvoie les articles les plus "proches" en fonction du nombre et de la m�thode choisie, selon les colonnes
recommandations <- function(article_line, number_of_recommendations = 5,method_of_similarity = cosSimL, mat = m2) {
  scores = similarities(article_line,method_of_similarity, mat)
  res=sort(scores, decreasing = TRUE, index.return = TRUE)
  res <- setNames(res$x, names(m)[res$ix])[1:number_of_recommendations+1]
  
  if (res[1]==article_line)
    return(res[2:length(res)])
  else
    return(res[1:length(res)])
}


#wrapper qui transforme les r�sultats de la recommendation ie les num�ros des bonnes colonnes en leur nom (ex : "X422908")
recommandationsNames <- function(article_name, number_of_recommendations = 5, method_of_similarity = cosSimilarity, mat = m2) {
  al = match(article_name, names(m))
  reco_lines = recommandations(al,number_of_recommendations, method_of_similarity, mat)
  return(reco_lines)
}

recommandationsNames("X422908",10,corSimL)
rCosL1 <- recommandationsNames("X422908",10,cosSimL)
rHamL1 <- recommandationsNames("X422908",10,hammingSimL)
#utiliser les colonnes revient � lancer notre algorithme sur la transpos�e de la matrice de citations.
recommandationsNames("X422908",10,corSimC)
rCosC1 <- recommandationsNames("X422908",10,cosSimC)
rHamC1 <- recommandationsNames("X422908",10,hammingSimC)


#On note que pour l'article d'int�r�t, la premi�re recommandation reste la m�me que ce soit en traitant les lignes ou les colonnes, ce qui est rassurant.
#les m�thodes de calcul de similarit� donnent des r�sultats identiques entre corr�lation et cosinus, ce qui n'est pas suprenant (voir TP1)
#la distance modifi�e de Hamming donne des r�sultats un peu diff�rents au del� de la premi�re recommandation. 
#Les v�rifications num�riques montrent que les articles r�alisant de bons scores selon le cosinus ou la corr�lation ne "scorent" pas mal avec la distance de Hamming, mais d'autres sont l�g�rement devant et inversement. 
#Comme les donn�es sont tr�s vides (en moyenne moins de trois coefficients non nuls par ligne), il y a fort � parier que beaucoup d'articles ont le m�me score. Donc le classement se fait plut�t par "batch" de plusieurs articles. 
#On peut le v�rifier en comptant le nombre de valeurs diff�rentes dans le vecteur de scores :

similariteCos <- similarities(747,cosSimL)
length(unique(similariteCos))

#on otient une cinquantaine, ce qui, rapport� � nos 1090 points, semble valider notre hypoth�se.

#Le probl�me est donc la pauvret� des donn�es. On pourrait utiliser la transpos�e de la matrice d'adjacence, ainsi que (comme dans PageRank), le carr� de la matrice d'adjacence,
#pour atteindre les citations du deuxi�me ordre.

#cr�ation d'une matrice contenant tout ce que nous avons �voqu�, concat�n�, afin d'utiliser les trois informations en m�me temps.
m3 <- m2 %*% m2
mean(rowSums(m3))
mTranspose <- t(m2)
bigM <- cbind(m2,mTranspose,m3)


rCosBig <- recommandationsNames("X422908",10,hammingSimL,bigM)
recommandationsNames("X422908",10,corSimL,bigM)
rHamBig <- recommandationsNames("X422908",10,cosSimL,bigM)

similariteCos <- similarities(747,hammingSimL, bigM)
length(unique(similariteCos))
#on voit ici qu'on a multipli� par 4 le nombre de valeurs prises par les similarit�s. On peut donc raisonnablement penser que l'on a r�duit le ph�nom�ne de "palliers".

PageRankNames <- names(ranks.S.order[1:10,])
PNR <- sapply(PageRankNames, FUN = function(x) paste("X",x,sep=''))

length(PNR[PNR %in% names(rCosBig)])
length(PNR[PNR %in% names(rHamBig)])
length(PNR[PNR %in% names(rCosL1)])
length(PNR[PNR %in% names(rCosC1)])
length(PNR[PNR %in% names(rHamL1)])
length(PNR[PNR %in% names(rHamC1)])
#Sur cet exemple particulier, on remarque que les mesures de similarit� renvoyant les r�sultats les plus proches de PageRank sont ceux utilisant les citations communes.
#Cela fait sens puisque que l'algorithme d�velopp� � la question 1 recommande les scores les plus haut PARMI les articles cit�s par "X422908". 
#Or l'approche colonne elle se base sur les articles citant "X422908". Il est donc normal que les recommandations de l'aglrithme de similarit� soient plus �lev�es en utilisant les lignes.
#On notera �galement que l'utilisation coinjointe de la matrice, de sa transpos�e et de son carr� ne rapproche pas les r�sultats de ceux de PageRank. 
#On ne peut cependant conclure que dans l'absolu ses recommandations ne sont pas meilleures.


#Deuxi�me version plus pouss�e de l'approche item-item : on calcule les similarit�s de la base d'articles avec tous les articles cit�s par notre article courant.
#Cette approche part du principe que les citations de l'article sont pertinentes, et cherche donc � trouver des articles similaires.
#Il est possible que certains des meilleures recommandations obtenues par cette m�thode soient certains des articles cit�s par l'article de base,
#mais ce n'est pas d�rangeant : rien ne dit que l'utilisateur a lu toutes les r�f�rences ! De plus, la base de donn�es �tant tr�s creuse, retirer ces r�sultats pourrait 
#d�t�riorer la qualit� de la recommandation (il n'existe probablement pas 20 articles pertinents � recommander dans la base de donn�es...)


#Ce choix est motiv� par la validation crois�e demand�e � la question 3. En effet, notre algorithme de base cherche les articles ayant les citations les plus proches. 
#Le probl�me est qu'avec une moyenne de citations inf�rieure � 3, en retirer une des trois pour la validation crois�e change sensiblement le vecteur de donn�es.
#Cette nouvelle approche devrait �tre bien plus robuste � la validation crois�e.

#Les fonctions sont sensiblement les m�mes, mais appliqu�es � tous les articles cit�s par notre article courant, on en fait ensuite la somme.

articlesCites <- m[,m["X422908"]==1]


similaritesParCitation <- function(articleName, method_of_similarity = cosSimL,mat = m2){
  articlesCites <- m[,m[articleName]==1]
  indices = match(names(articlesCites),names(m))
  
  scores = sapply(indices, FUN = function(x) similarities(x,method_of_similarity,mat))
  return(rowSums(scores))
  
}

recommandationsParVoisinage <- function(article_name, number_of_recommendations = 5,method_of_similarity = cosSimL, mat = m2) {
  scores = similaritesParCitation(article_name,method_of_similarity, mat)
  res=sort(scores, decreasing = TRUE, index.return = TRUE)
  res <- setNames(res$x, names(m)[res$ix])[1:number_of_recommendations+1]
  
  if (res[1]==article_name)
    return(res[2:length(res)])
  else
    return(res[1:length(res)-1])
}

rVoisinage <- recommandationsParVoisinage("X422908",10,hammingSimL)

#On constate que la nouvelle approche, si elle renvoie des r�sultats assez diff�rents de la premi�re, n'est pas plus proche de PageRank.
length(rCosL1[names(rCosL1) %in% names(rVoisinage)])
length(PNR[PNR %in% names(rVoisinage)])

#fonction utilis�e pour supprimer un 1 du vecteur (au hasard)
replace.rand <- function(vec){
  ind <- names(vec[vec==1][sample(length(vec[vec==1]),1)])
  vec[vec==1][ind] <- 0
  return(vec)
}



###########################"
## Question 3




# Question 3
# S�paration du jeu de donn�es
# On utilise l'approche expliqu�e dans l'article conseill�
# (90%/10%, retrait d'une citation pour chaque �l�ment de l'ensemble de test)

division2 <- function(mat, train.perc){
  N <- nrow(mat)
  train.nb <- round(train.perc*N/100)
  nonnull <- match(row.names(m[-rowSums(m)!=0,]),row.names(m))
  #indices o� on change un truc
  test <- sample(nonnull, train.nb)
  set.train <- mat[-test,]
  set.test.full <- mat[test,]
  mat[test,] <- t(apply(mat[test,], 1, replace.rand)) 
  # Retrait d'une citation
  # Ajout des articles incomplets � l'ensemble d'entra�nement
  return(list(mat, test, set.test.full))
}

division2 <- function(mat, train.perc){
  N <- nrow(mat)
  train.nb <- round(train.perc*N/100)
  
  #indices o� on change un truc
  test <- sample.int(nrow(mat), train.nb)
  set.train <- mat[-test,]
  set.test.full <- mat[test,]
  mat[test,] <- t(apply(mat[test,], 1, replace.rand)) 
  # Retrait d'une citation
  # Ajout des articles incomplets � l'ensemble d'entra�nement
  return(list(mat, test, set.test.full))
}

mtest <- m
l = division2(mtest,10)
trainSet <- as.matrix(l[[1]])
namesOfTrainSet <- names(m)[l[[2]]]
#top pr�diction sur l'ensemble de test
estimatedPredict <- sapply(namesOfTrainSet , FUN = function(x) recommandationsNames(x ,1,cosSimL,trainSet))

namesTrainSet <- sapply(names(estimatedPredict), FUN = function(x) {strsplit(x,'[.]')[[1]][1]})
#nom de l'article pr�dit
predIndice <- sapply(names(estimatedPredict), FUN = function(x) strsplit(x,'[.]')[[1]][2])
#on retire le X pour obtenir le bon nom de colonne
predIndice2 <- sapply(predIndice, function(x) substr(x,2,nchar(x)))
sort(estimatedPredict)

#Reconstruction de la matrice pour comparaison
mReconstruit <- m
a <- predIndice2
for(zz in predIndice2)
{
  for(j in namesOfTrainSet){
    if (estimatedPredict[match(j,namesOfTrainSet)] != 0)
    {mReconstruit[zz,j] = 1}
  }
}


#Comparaison : nombre de valeurs modifi�es dans l'ensemble de test
sum(m - trainSet)

#Comparaison : nombre de valeurs incorrectement pr�dites
sum(m - mReconstruit)
#Comparaison : nombre de valeurs modifi�es (utile � des fins de v�rification)
sum(mReconstruit-trainSet)

#Apr�s plusieurs lancements de cette derni�re partie de l'algorithme, on constate que la reconstruction est toujours parfaite, ce qui semble surprenant.
