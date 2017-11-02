rm(list = ls())
library(Matrix)
setwd("D:/POLYMTL/LOG6308/TP2")
m = read.table("citeseer.rtable")
m2 = as.matrix(m)

avgCitationParArticle = mean(rowSums(m))


#notons qu'ici les utilisateurs sont aussi les items.

#notre mesure de similarité recommandera des articles qui citent les même articles que celui qui nous intéresse.
#ceci est logique car il est possible de recommander d'autres articles à partir d'un article récent. On utilisera donc les lignes de la matrice.
#Utiliser les colonnes amènerait à recommander les articles co-cités par l'article qui nous intéresse, autrement dit les articles également cités par les articles qui citent notre article courant.
#cette dernière approche semble plus adaptée à des articles anciens. 


#on souhaite tester plusieurs mesures de similarités : la corrélation de Pearson, le cosinus et une troisième mesure basée sur une modification de la distance de Hamming.


#Similarité de deux lignes selon la corrélation de Pearson
corSimL <- function(item_no1,item_no2){
  item1 = m2[item_no1,]
  item2 = m2[item_no2,]
  if( sum(item1)== 0 || sum(item2)==0)
    return(0)
  else
    return(cor(item1,item2,use = 'pairwise.complete.obs', method = 'pearson')) 
}


#Similarité de deux lignes en utilisant le cosinus
cosSimL <- function(item_no1,item_no2) { 
  item1 = m2[item_no1,]
  item2 = m2[item_no2,]
  if( sum(item1*item2)== 0)
    return(0)
  else
    sum(item1 * item2)/( sqrt(sum(item1^2) * sum(item2^2))  ) 
  }


#Similarité de deux lignes en utilisant la distance de Hamming, mais divisée par le nombre d'indices où l'un des deux vecteurs est non nuls
#cela permet de favoriser les articles avec plus de citations, qui semblent donc plus pertinents.
#autrement dit  0 0 0 1 1 et 1 0 0 0 1 auront une distance de 2/3 (3 indices où l'un des deux vecteurs est non nul). 
hammingSimL <- function(item_no1,item_no2) { 
  item1 = m2[item_no1,]
  item2 = m2[item_no2,]
  if( sum(item1)== 0 || sum(item2)==0)
    return(-10000)
  else
    a = abs(item1[item1+item2>0]-item2[item1+item2>0]) 
    return(-sum(a/length(a)))
}


#corrélation de pearson pour deux colonnes
corSimC <- function(item_no1,item_no2){
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1)== 0 || sum(item2)==0)
    return(0)
  else
    return(cor(item1,item2,use = 'pairwise.complete.obs', method = 'pearson')) 
}

#similarité par cosinus pour deux colonnes
cosSimC <- function(item_no1,item_no2) { 
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1*item2)== 0)
    return(0)
  else
    sum(item1 * item2)/( sqrt(sum(item1^2) * sum(item2^2))  ) 
}


#similarité en utilisant la distance de Hamming modifiée pour deux colonnes
hammingSimC <- function(item_no1,item_no2) { 
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1)== 0 || sum(item2)==0)
    return(-10000)
  else
    a = abs(item1[item1+item2>0]-item2[item1+item2>0]) 
  return(-sum(a/length(a)))
}


#calcule la similarité d'une colonne ou d'une lignes avec les autres
similarities <- function(item_no,method_of_similarity){
 res = seq(1,1090);
  similarities = sapply( res, FUN = function(x) method_of_similarity(x,item_no))
 return(c(similarities))
}



#renvoie les articles les plus "proches" en fonction du nombre et de la méthode choisie, selon les colonnes
recommandations <- function(article_line, number_of_recommendations = 5,method_of_similarity = cosSimL) {
  scores = similarities(article_line,method_of_similarity)
  res=sort(scores, decreasing = TRUE, index.return = TRUE)$ix[1:number_of_recommendations+1]
  if (res[1]==article_line)
    return(res[2:end])
  else
    return(res[1:length(res)])
}


#wrapper qui transforme les résultats de la recommendation ie les numéros des bonnes colonnes en leur nom (ex : "X422908")
recommandationsNames <- function(article_name, number_of_recommendations = 5, method_of_similarity = cosSimilarity) {
  al = match(article_name, names(m))
  reco_lines = recommandations(al,number_of_recommendations, method_of_similarity)
  reco_names = names(m)[reco_lines]
  return(reco_names)
}

recommandationsNames("X422908",8,corSimL)
recommandationsNames("X422908",8,cosSimL)
recommandationsNames("X422908",8,hammingSimL)
recommandationsNames("X422908",8,corSimC)
recommandationsNames("X422908",8,cosSimC)
recommandationsNames("X422908",8,hammingSimC)

#article d'intérêt
match("X422908",names(m))
#top partout :
match("X96767",names(m))
#top 3 en lignes pour cos et cor mais pas hamming
match("X149673", names(m))
#top 2 en lignes pour hamming mais pas cos et cor
match("X155792", names(m))
#top 2 pour les colonnes mais absent dans les lignes
match("X496938", names(m))

#On note que pour l'article d'intérêt, la première recommandation reste la même que ce soit en traitant les lignes ou les colonnes, ce qui est rassurant.
#les méthodes de calcul de similarité donnent des résultats identiques entre corrélation et cosinus, ce qui n'est pas suprenant (voir TP1)
#la distance modifiée de Hamming donne des résultats un peu différents au delà de la première recommandation. 
#Les vérifications numériques montrent que les articles réalisant de bons scores selon le cosinus ou la corrélation ne "scorent" pas mal avec la distance de Hamming, mais d'autres sont légèrement devant et inversement. 
#Comme les données sont très vides (en moyenne moins de trois coefficients non nuls par ligne), il y a fort à parier que beaucoup d'articles ont le même score. Donc le classement se fait plutôt par "batch" de plusieurs articles. 
#On peut le vérifier en comptant le nombre de valeurs différentes dans le vecteur de scores :

similariteCos <- similarities(747,cosSimL)
length(unique(similariteCos))

#on otient une cinquantaine, ce qui, rapporté à nos 1090 points, semble valider notre hypothèse.

