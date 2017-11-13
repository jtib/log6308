############################
## LOG 6308 - TP 2
## Tibayrenc Juliette - Rubenach Théo
## matricules 1800292 - 1800314
##########################"#



###################
##Question 1

rm(list = ls())
library(Matrix)

  # modifie un peu le pageRank par rapport à d=0.85 (utilisé pour les pages web habituelles),
  # à tolérance égale pour la convergence de l'algorithme PageRank, mais les résultats principaux restent les mêmes
  d = 0.6

  m = read.table("citeseer.rtable")
m.matrix <- data.matrix(m)
m.matrix
  # Erreur tolérée de l'algorithme
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

  # Calcul des pageRanks des articles en prenant toute les données en compte
  n <- ncol(m)
pr.first.all <- rep(1,n)
ranks.all <- pageRank(m.matrix, d, pr.first.all)
ranks.all

  # Il suffit à présent de prendre, parmi les citations référencées par l'article 422908, celles qui ont le meilleur pageRank.
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

  # Vu le peu de citations présentes dans la base de données, on se limitera à recommander les 5 premières.
  names(ranks.S.order[1:5,])

  # Variante : utilisation du domaine S', qui inclut les citations des citations
  # La matrice d'adjacence élevée au carré par multiplication booléenne représente
  # la composition de la relation d'origine par elle-même
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

  # Et on renvoie les 5 premières
  names(ranks.S.prime.order[1:5,])
  # Pour comparaison
  names(ranks.S.order[1:10,])
#row.names(ranks.S.order) <- paste('X', row.names(ranks.S.order), sep='')
length(names(ranks.S.order[1:10,])[names(ranks.S.order[1:10,]) %in% names(ranks.S.prime.order[1:10,])])
# 8 recommandations en commun
  # La plupart des recommandations restent communes, mais deux nouvelles apparaissent.
  # Ceci s'explique sans doute par la matrice utilisée, qui ne contient que peu d'informations,
  # et, en conséquence, les articles cités par celui examiné ne sont pas forcément très liés entre eux,
  # bien qu'appartenant à un domaine très proche.
  # On en conclut que l'étendue du sous-ensemble de départ permettrait de recommander des citations plus anciennes,
  # potentiellement plus importantes dans le domaine scientifique considéré mais moins directement reliées à l'article
  # d'origine, mais que les données disponibles sont très insuffisantes pour conclure sur l'impact d'utilisation de S'
  # de façon définie (et puis l'expérience a été faite avec un seul article d'origine, ce qui est de toute façon un nombre
  # insuffisant d'exemples pour conclure en termes quantitatifs).
  



############################
#Question 2

m2 = as.matrix(m)


avgCitationParArticle = mean(rowSums(m))

#notons qu'ici les utilisateurs sont aussi les items.

#notre mesure de similarité recommandera des articles qui citent les même articles que celui qui nous intéresse.
#ceci est logique car il est possible de recommander d'autres articles à partir d'un article récent. On utilisera donc les lignes de la matrice.
#Utiliser les colonnes amènerait à recommander les articles co-cités par l'article qui nous intéresse, autrement dit les articles également cités par les articles qui citent notre article courant.
#cette dernière approche semble plus adaptée à des articles anciens. 

#on souhaite tester plusieurs mesures de similarités : la corrélation de Pearson, le cosinus et une troisième mesure basée sur une modification de la distance de Hamming.

#Similarité de deux lignes selon la corrélation de Pearson
corSimL <- function(item_no1,item_no2, mat = m2){
  item1 = mat[item_no1,]
  item2 = mat[item_no2,]
  if( sum(item1)== 0 || sum(item2)==0)
    return(0)
  else
    return(cor(item1,item2,use = 'pairwise.complete.obs', method = 'pearson')) 
}


#Similarité de deux lignes en utilisant le cosinus
cosSimL <- function(item_no1,item_no2, mat = m2) { 
  item1 = mat[item_no1,]
  item2 = mat[item_no2,]
  if( sum(item1*item2)== 0)
    return(0)
  else
    sum(item1 * item2)/( sqrt(sum(item1^2) * sum(item2^2))  ) 
}


#Similarité de deux lignes en utilisant la distance de Hamming, mais divisée par le nombre d'indices où l'un des deux vecteurs est non nuls
#cela permet de favoriser les articles avec plus de citations, qui semblent donc plus pertinents.
#autrement dit  0 0 0 1 1 et 1 0 0 0 1 auront une distance de 2/3 (3 indices où l'un des deux vecteurs est non nul).
#on multiplie par  -1 pour pouvoir trier les résultats de la même manière qu'avec la corrélation et le cosinus
hammingSimL <- function(item_no1,item_no2, mat = m2) { 
  item1 = mat[item_no1,]
  item2 = mat[item_no2,]
  if( sum(item1)== 0 || sum(item2)==0)
    return(-10000)
  else
    a = abs(item1[item1+item2>0]-item2[item1+item2>0]) 
  return(-sum(a/length(a)))
}


#corrélation de pearson pour deux colonnes
corSimC <- function(item_no1,item_no2, mat){
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1)== 0 || sum(item2)==0)
    return(0)
  else
    return(cor(item1,item2,use = 'pairwise.complete.obs', method = 'pearson')) 
}

#similarité par cosinus pour deux colonnes
cosSimC <- function(item_no1,item_no2, mat) { 
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1*item2)== 0)
    return(0)
  else
    sum(item1 * item2)/( sqrt(sum(item1^2) * sum(item2^2))  ) 
}


#similarité en utilisant la distance de Hamming modifiée pour deux colonnes
hammingSimC <- function(item_no1,item_no2, mat) { 
  item1 = m[item_no1]
  item2 = m[item_no2]
  if( sum(item1)== 0 || sum(item2)==0)
    return(-10000)
  else
    a = abs(item1[item1+item2>0]-item2[item1+item2>0]) 
  return(-sum(a/length(a)))
}


#calcule la similarité d'une colonne ou d'une lignes avec les autres
similarities <- function(item_no,method_of_similarity, mat = m2){
  res = seq(1,1090);
  similarities = sapply( res, FUN = function(x) method_of_similarity(x,item_no, mat))
  return(c(similarities))
}



#renvoie les articles les plus "proches" en fonction du nombre et de la méthode choisie, selon les colonnes
recommandations <- function(article_line, number_of_recommendations = 5,method_of_similarity = cosSimL, mat = m2) {
  scores = similarities(article_line,method_of_similarity, mat)
  res=sort(scores, decreasing = TRUE, index.return = TRUE)
  res <- setNames(res$x, names(m)[res$ix])[1:number_of_recommendations+1]
  
  if (res[1]==article_line)
    return(res[2:length(res)])
  else
    return(res[1:length(res)])
}


#wrapper qui transforme les résultats de la recommendation ie les numéros des bonnes colonnes en leur nom (ex : "X422908")
recommandationsNames <- function(article_name, number_of_recommendations = 5, method_of_similarity = cosSimilarity, mat = m2) {
  al = match(article_name, names(m))
  reco_lines = recommandations(al,number_of_recommendations, method_of_similarity, mat)
  return(reco_lines)
}

recommandationsNames("X422908",10,corSimL)
rCosL1 <- recommandationsNames("X422908",10,cosSimL)
rHamL1 <- recommandationsNames("X422908",10,hammingSimL)
#utiliser les colonnes revient à lancer notre algorithme sur la transposée de la matrice de citations.
recommandationsNames("X422908",10,corSimC)
rCosC1 <- recommandationsNames("X422908",10,cosSimC)
rHamC1 <- recommandationsNames("X422908",10,hammingSimC)


#On note que pour l'article d'intérêt, la première recommandation reste la même que ce soit en traitant les lignes ou les colonnes, ce qui est rassurant.
#les méthodes de calcul de similarité donnent des résultats identiques entre corrélation et cosinus, ce qui n'est pas suprenant (voir TP1)
#la distance modifiée de Hamming donne des résultats un peu différents au delà de la première recommandation. 
#Les vérifications numériques montrent que les articles réalisant de bons scores selon le cosinus ou la corrélation ne "scorent" pas mal avec la distance de Hamming, mais d'autres sont légèrement devant et inversement. 
#Comme les données sont très vides (en moyenne moins de trois coefficients non nuls par ligne), il y a fort à parier que beaucoup d'articles ont le même score. Donc le classement se fait plutôt par "batch" de plusieurs articles. 
#On peut le vérifier en comptant le nombre de valeurs différentes dans le vecteur de scores :

similariteCos <- similarities(747,cosSimL)
length(unique(similariteCos))

#on otient une cinquantaine, ce qui, rapporté à nos 1090 points, semble valider notre hypothèse.

#Le problème est donc la pauvreté des données. On pourrait utiliser la transposée de la matrice d'adjacence, ainsi que (comme dans PageRank), le carré de la matrice d'adjacence,
#pour atteindre les citations du deuxième ordre.

#création d'une matrice contenant tout ce que nous avons évoqué, concaténé, afin d'utiliser les trois informations en même temps.
m3 <- m2 %*% m2
mean(rowSums(m3))
mTranspose <- t(m2)
bigM <- cbind(m2,mTranspose,m3)


rCosBig <- recommandationsNames("X422908",10,hammingSimL,bigM)
recommandationsNames("X422908",10,corSimL,bigM)
rHamBig <- recommandationsNames("X422908",10,cosSimL,bigM)

similariteCos <- similarities(747,hammingSimL, bigM)
length(unique(similariteCos))
#on voit ici qu'on a multiplié par 4 le nombre de valeurs prises par les similarités. On peut donc raisonnablement penser que l'on a réduit le phénomène de "palliers".

PageRankNames <- names(ranks.S.order[1:10,])
PNR <- sapply(PageRankNames, FUN = function(x) paste("X",x,sep=''))

length(PNR[PNR %in% names(rCosBig)])
length(PNR[PNR %in% names(rHamBig)])
length(PNR[PNR %in% names(rCosL1)])
length(PNR[PNR %in% names(rCosC1)])
length(PNR[PNR %in% names(rHamL1)])
length(PNR[PNR %in% names(rHamC1)])
#Sur cet exemple particulier, on remarque que les mesures de similarité renvoyant les résultats les plus proches de PageRank sont ceux utilisant les citations communes.
#Cela fait sens puisque que l'algorithme développé à la question 1 recommande les scores les plus haut PARMI les articles cités par "X422908". 
#Or l'approche colonne elle se base sur les articles citant "X422908". Il est donc normal que les recommandations de l'aglrithme de similarité soient plus élevées en utilisant les lignes.
#On notera également que l'utilisation coinjointe de la matrice, de sa transposée et de son carré ne rapproche pas les résultats de ceux de PageRank. 
#On ne peut cependant conclure que dans l'absolu ses recommandations ne sont pas meilleures.


#Deuxième version plus poussée de l'approche item-item : on calcule les similarités de la base d'articles avec tous les articles cités par notre article courant.
#Cette approche part du principe que les citations de l'article sont pertinentes, et cherche donc à trouver des articles similaires.
#Il est possible que certains des meilleures recommandations obtenues par cette méthode soient certains des articles cités par l'article de base,
#mais ce n'est pas dérangeant : rien ne dit que l'utilisateur a lu toutes les références ! De plus, la base de données étant très creuse, retirer ces résultats pourrait 
#détériorer la qualité de la recommandation (il n'existe probablement pas 20 articles pertinents à recommander dans la base de données...)


#Ce choix est motivé par la validation croisée demandée à la question 3. En effet, notre algorithme de base cherche les articles ayant les citations les plus proches. 
#Le problème est qu'avec une moyenne de citations inférieure à 3, en retirer une des trois pour la validation croisée change sensiblement le vecteur de données.
#Cette nouvelle approche devrait être bien plus robuste à la validation croisée.

#Les fonctions sont sensiblement les mêmes, mais appliquées à tous les articles cités par notre article courant, on en fait ensuite la somme.

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
rVoisinage
#On constate que la nouvelle approche, si elle renvoie des résultats assez différents de la première, n'est pas plus proche de PageRank.
length(rCosL1[names(rCosL1) %in% names(rVoisinage)])
length(PNR[PNR %in% names(rVoisinage)])

#fonction utilisée pour supprimer un 1 du vecteur (au hasard)
replace.rand <- function(vec){
  ind <- names(vec[vec==1][sample(length(vec[vec==1]),1)])
  vec[vec==1][ind] <- 0
  return(vec)
}



###########################"
## Question 3




# Question 3
# Séparation du jeu de données
# On utilise l'approche expliquée dans l'article conseillé
# (90%/10%, retrait d'une citation pour chaque élément de l'ensemble de test)



division2 <- function(mat, train.perc){
  N <- nrow(mat)
  train.nb <- round(train.perc*N/100)
  
  #indices où on change un truc
  test <- sample.int(nrow(mat), train.nb)
  set.train <- mat[-test,]
  set.test.full <- mat[test,]
  mat[test,] <- t(apply(mat[test,], 1, replace.rand)) 
  # Retrait d'une citation
  # Ajout des articles incomplets à l'ensemble d'entraînement
  return(list(mat, test, set.test.full))
}

crossValidation <- function(mat=m,pourcentageTest = 10,nbIteration = 10){
  finalPerf = 0
  for(i in 1:10){
  mtest <- m
  l = division2(mtest,10)
  trainSet <- as.matrix(l[[1]])
  namesOfTrainSet <- names(m)[l[[2]]]
  #top prédiction sur l'ensemble de test
  estimatedPredict <- sapply(namesOfTrainSet , FUN = function(x) recommandationsNames(x ,1,cosSimL,trainSet))

  namesTrainSet <- sapply(names(estimatedPredict), FUN = function(x) {strsplit(x,'[.]')[[1]][1]})
  #nom de l'article prédit
  predIndice <- sapply(names(estimatedPredict), FUN = function(x) strsplit(x,'[.]')[[1]][2])
  #on retire le X pour obtenir le bon nom de colonne
  predIndice2 <- sapply(predIndice, function(x) substr(x,2,nchar(x)))

  #Reconstruction de la matrice pour comparaison
  mReconstruit <- m
  a <- 0
  #devant le manque de temps, nous nous sommes permis une boucle for, dans la mesure où elle est appliquée à des données de petite taille.
  for(zz in predIndice2)
    {
    if (zz %in% c("100967")){
      for(j in namesTrainSet){
    
        mReconstruit[zz,j] = 1
      
    
        break
      }
    }
  }


  #Comparaison : nombre de valeurs modifiées dans l'ensemble de test (n'est pas égal à la taille de l'ensemble de test car les lignes vides ne sont pas modifiées)
  sum(m - trainSet)

  #Comparaison : pourcentage de références correctement prédites
  sum(mReconstruit - m )
  performance <- 1 - abs(sum(mReconstruit - m ))/length(predIndice2)
  finalPerf = finalPerf +  performance
  print("itération")
  }
  return(finalPerf/nbIteration)
}

crossValidation(m,10,10)


#test du nombre de lignes nulles dans la matrice
nrow(m[rowSums(m)==0,])
#après plusieurs lancements, la proportion d'articles correctement retrouvés est toujours environ de 50 pourcents. Ce chiffre paraît faible de prime abord, mais il faut garder à l'esprit
#que des lignes vides (environ un tiers) peuvent être choisies dans l'ensemble de test, et ces lignes se verront attribuer une citation par l'algorithme
#cette citation sera toujours le premier article du corpus, puisque l'article vide aura un score de similarité de 0 avec tous les articles.
#On aura donc un tiers de résultats faux, environ, par lancement. De plus, il est possible que notre algorithme ne trouve aucun autre item similaire, auquel cas il renverra également 0.

#En corrigeant ce problème, comme dans la version actuellement présentée, on arrive à un score étonnamment bon de 99% quasiment à chaque fois.
#Cela veut donc dire que quand il existe de plus proches voisins, on retrouve quasiment tout le temps la bonne citation; en revanche quand on ne trouve pas de plus proche voisin, l'algorithme est perdu.
#Cela pourrait s'expliquer par les lignes n'ayant qu'un seul 1 : en enlevant une valeur non nulle, on se retrouve avec une ligne morte; l'algorithme ne pourra donc pas être utile.

#Etant donné le temps que met l'algorithme de validation croisée à tourner avec notre version de la similarité de base, nous n'avons pas fait de validation croisée sur la version avancée qui est significativement plus gourmande en calculs.
#Cependant, dans la mesure où celle-ci performait de manière similaire aux versions de base dans la comparaison avec PageRank, il y a fort à parier que les résultats seront aussi bons.
#Il faut toutefois rester très prudent sur cette hypothèse dans la mesure où il est possible que notre algorithme de similarité de base soit particulièrement adapté au protocole de validation croisée que nous avons utilisé ici; c'est un biais possible 
#dont on ne peut facilement infirmer ou confirmer la présence. Ce biais expliquerait cela dit les performances surprenantes de notre algorithme...
