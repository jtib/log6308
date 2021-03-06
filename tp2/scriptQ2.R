rm(list = ls())
library(Matrix)
setwd("D:/POLYMTL/LOG6308/TP2")
m = read.table("citeseer.rtable")
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

cosSimL(7,9)
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
    return(res[2:end])
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

length(rec.S.10[rec.S.10 %in% rec.S.prime.10])
length(rec.S.10[rec.S.10 %in% names(rCosBig)])
length(rec.S.10[rec.S.10 %in% names(rHamBig)])
length(rec.S.10[rec.S.10 %in% names(rCosL1)])
length(rec.S.10[rec.S.10 %in% names(rCosC1)])
length(rec.S.10[rec.S.10 %in% names(rHamL1)])
length(rec.S.10[rec.S.10 %in% names(rHamC1)])
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

articlesCites <- m[,m["X422908"]==1]



similaritesParCitation <- function(articleName, method_of_similarity = cosSimL,mat = m2){
  articlesCites <- m[,m[articleName]==1]
  indices = match(names(articlesCites),names(m))
  
  scores = sapply(indices, FUN = function(x) similarities(x,method_of_similarity,mat))
  return(rowSums(scores))
  
}


#Ce choix est motiv� par la validation crois�e demand�e � la question 3. En effet, notre algorithme de base cherche les articles ayant les citations les plus proches. 
#Le probl�me est qu'avec une moyenne de citations inf�rieure � 3, en retirer une des trois pour la validation crois�e change sensiblement le vecteur de donn�es.
#Cette nouvelle approche devrait �tre bien plus robuste � la validation crois�e.

recommandationsParVoisinage <- function(article_name, number_of_recommendations = 5,method_of_similarity = cosSimL, mat = m2) {
  scores = similaritesParCitation(article_name,method_of_similarity, mat)
  res=sort(scores, decreasing = TRUE, index.return = TRUE)
  res <- setNames(res$x, names(m)[res$ix])[1:number_of_recommendations+1]
  
  if (res[1]==article_name)
    return(res[2:end])
  else
    return(res[1:length(res)])
}

rVoisinage <- recommandationsParVoisinage("X422908",10,hammingSimL)

length(rCosL1[names(rCosL1) %in% names(rVoisinage)])

length(rec.S.10[rec.S.10 %in% rec.S.prime.10])
length(rec.S.10[rec.S.10 %in% names(rCosBig)])
length(rec.S.10[rec.S.10 %in% names(rHamBig)])
length(rec.S.10[rec.S.10 %in% names(rCosL1)])
length(rec.S.10[rec.S.10 %in% names(rCosC1)])
length(rec.S.10[rec.S.10 %in% names(rHamL1)])
length(rec.S.10[rec.S.10 %in% names(rHamC1)])
length(rec.S.10[rec.S.10 %in% names(rVoisinage)])

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
estimatedPredict <- sapply(namesOfTrainSet , FUN = function(x) recommandationsNames(x ,1,cosSimL,trainSet))

namesTrainSet <- sapply(names(estimatedPredict), FUN = function(x) {strsplit(x,'[.]')[[1]][1]})
predIndice <- sapply(names(estimatedPredict), FUN = function(x) strsplit(x,'[.]')[[1]][2])
#on retire le X pour obtenir le bon nom de colonne
predIndice2 <- sapply(predIndice, function(x) substr(x,2,nchar(x)))

#indices <- sapply(names(estimatedPredict), function(x) {match(x,names(m) )})


mReconstruit <- m
a <- predIndice2
for(i in a)
{
  for(j in mReconstruit){
    mReconstruit[i,j] <- 1
  }
}

#ceci ne marche pas je sais pas pk :
#mReconstruit[a,namesTrainSet] <- 1 

sum(m - trainSet)
sum(m - mReconstruit)
sum(mReconstruit-trainSet)

function()
