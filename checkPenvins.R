#=====================================================#
#Pour utiliser ce script, assurez-vous que le fichier 
#"checkPenvins.R" se trouve dans votre repertoire 
#courant, puis copiez-collez la commande ci-dessous dans R :
#source("checkPenvins.R")
#-----------------------------------------------------#
#Vous pouvez egalement utiliser le menu Fichier > Sourcer du code R... > 
#et selectionner le script
#-----------------------------------------------------#
#Il ne vous reste plus qu'a lire votre fichier de donnees 
#avec read.table() et a utiliser :
#checkPenvins(nomdutableau)
#-----------------------------------------------------#

# "Juste pour la gloire !"

#=====================================================#
#Fonction checkPenvins
#-----------------------------------------------------#
#Fonction principale du script appelant toutes les autres
#-----------------------------------------------------#

# Enregistre tous les messages dans un fichier txt "test_summary.txt"
checkPenvins <- function(dataset){
  testSummary <- file("tests_summary.txt", open = "wt")
  sink(file = testSummary, append = TRUE, type="output")
  sink(file = testSummary, append = TRUE, type="message")
  
  # Message de bienvenue
  cat("#-----------------------------------------------------#\nBienvenue dans le verificateur de fichiers Penvins 2017.\n#-----------------------------------------------------#\n\n")
 
  #-----------------------------------------------------#
  # Etape 1
  #-----------------------------------------------------#
  checkDataFrame(dataset)
  
  #-----------------------------------------------------#
  # Etape 2
  #-----------------------------------------------------#
  
  
  #-----------------------------------------------------#
  # Etape 3
  #-----------------------------------------------------#
  nomsRefQuad = c("transect","resp","date","coef","mode","d.chenal","d.mer","alt","surf","p.roc","p.moul","p.huit","p.bala","p.alg","p.enc","p.eau","s.flaq","d.flaq") #Noms et ordres des colonnes attendus pour le fichier quad
  nomsRefInd = c("transect","resp","date","coef","mode","d.chenal","d.mer","alt","surf","p.roc","p.moul","p.huit","p.bala","p.alg","p.enc","p.eau","s.flaq","d.flaq") #Noms et ordres des colonnes attendus pour le fichier ind
  
  if (checkQuad(dataset)) {
    checkColNames(dataset, nomsRefQuad)
  } else {
    if (checkInd(dataset)) {
      checkColNames(dataset, nomsRefInd)
    }
  }

  
  #=====================================================#
  # Fin du script
  #-----------------------------------------------------#
  cat("\nFin de l'analyse.", file = testSummary)
  sink(type="output") # stop sinking
  sink(type="message") # stop sinking
  # affichage du fichier texte des messages et erreurs
  file.show("tests_summary.txt")
  close(testSummary) # Ferme le fichier txt
}

#-----------------------------------------------------#
#Etape 1 : Verifier si mydata est de classe data.frame
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la classe de mydata
#-----------------------------------------------------#
checkDataFrame <- function(mydata) {
  if (is.data.frame(mydata)) {
    cat("Le fichier", deparse(substitute(mydata)), "est bien de type data.frame.")
  } else {
    cat("ERROR : Le fichier n'est pas de type data.frame :", deparse(substitute(mydata)), "est un", class(mydata))
    sink(type = "message")
    file.show("tests_summary.txt")
    stop("Le fichier n'est pas de type data.frame  :", deparse(substitute(mydata)), "est un", class(mydata))
  }
}

#-----------------------------------------------------#
#Etape 2 : Verifier que le tableau possede bien 34 ou 43 colonnes
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer le nombre incorrect de colonnes
#-----------------------------------------------------#

#-----------------------------------------------------#
# Vérifie que le fichier mydata est bien de type Quad en vérifiant le nombre de colonnes attendues
checkQuad <- function(mydata) {
  nbColQuad = 34
  if (ncol(mydata)==nbColQuad){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#-----------------------------------------------------#
# Vérifie que le fichier file est bien de type Ind en vérifiant le nombre de colonnes attendues
checkInd <- function(mydata) {
  nbColInd = 43
  if (ncol(mydata)==nbColInd){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#-----------------------------------------------------#
#Etape 3 : Verifier que les noms de colonnes sont corrects
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer les erreurs et les noms attendus
#-----------------------------------------------------#

# La fonction checkColNames() vérifie que les nomes des colonnes du fichier dataset en argument correspondent bien à la liste nomsRef en argument 2
checkColNames <- function(dataset, nomsRef) {
  cat("\n\nETAPE 3 : Verification des noms des colonnes :\n")
  col = names(dataset) # "col" stocke les noms des colonnes du fichier
  verif=TRUE # si verif=TRUE à la fin du test, les noms des colonnes sont conformes
  for (i in 1:length(dataset)){
    if (col[i] != nomsRef[i]){
      verif=FALSE # si il y a une erreur, ce n'est plus conforme, verif change d'etat
      warning("Le nom de la colonne", i, "n'est pas correct.")
      cat("Remplacer le nom de la colonne",i, "par",nomsRef[i],".\n")} # sinon on affiche un message permettant de remplacer les noms non conformes
    }
  cat("Fin de la verification du nom des colonnes\n")
  if (verif==FALSE) {
    cat("ERROR : La fonction checkPenvins s'est terminee prematurement.")
    sink(type = "message")
    file.show("tests_summary.txt")
    stop("Il y a une/des erreur(s) dans les noms de colonnes.") # message d'erreur indiquant qu'il y a une ou des erreurs et stoppant la fonction
  } else {cat("Toutes les colonnes sont correctes.\n")}
}


#-----------------------------------------------------#
#Etape 4 : Verifier que les classes des colonnes sont correctes
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer les erreurs et les classes attendues
#-----------------------------------------------------#


#-----------------------------------------------------#
#Etape 5 : Verifier dans les colonnes de classe "factor"
#que la ou les modalites du facteur correspondent a celles attendues
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la colonne, les numeros
#et contenus des lignes qui ne sont pas conformes, ainsi que le contenu attendu
#-----------------------------------------------------#


#=====================================================#
# Etape 7 : Verification des ratios larg/haut et peri/larg dans biom.txt
#-----------------------------------------------------#
# verifier que les ratios larg/haut et peri/larg du fichier biometrique qui ne sont pas NA
#sont situes respectivement dans des intervalles min-max realistes que vous choisirez.

# Fonction checkRatio()
# Vérifie le ratio attendu entre deux valeurs
# prend en arguments 5 valeurs :
# valeur A, valeur B, ratio min attendu, ratio max attendu, numéro de la ligne, espece
checkRatio <- function(varOne, varTwo, ratioMin, ratioMax, ligneNb, espece){
  varOne=as.numeric(varOne)
  varTwo=as.numeric(varTwo)
  # vérifie que les deux valeurs sont des valeurs numériques et par conséquent ne sont pas des "NA"
  if (!is.na(varOne) & !is.na(varTwo)){
    ratio = varOne/varTwo
    if (ratio<ratioMin | ratio>ratioMax){
      warning("Ligne", ligneNb, "pour l'espece", espece, ": Le ratio", ratio, "sort de l'intervalle attendu", ratioMin, ":",ratioMax)
    }
  } else{ #dans le cas ou l'une des valeurs est NA
    warning("Ligne", ligneNb, "pour l'espece", espece, ": valeur NA !")
  }
}

# Initialisation de l'étape 7 : uniquement si le fichier est reconnu de type Ind
#if (checkInd(mydata)){ # Vérification du fichier : type ind
  
  # ratio larg/haut
  
  
  # ratio peri/larg
  
  # Fin de l'etape 7
  
#}

#=====================================================#
#Fonction checkfactor
#-----------------------------------------------------#
#Utilisee dans l'etape 5, elle verifie que les modalites
#d'une colonne de classe factor correspondent a celles attendues
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la colonne, les numeros
#et contenus des lignes qui ne sont pas conformes, ainsi que le contenu attendu
#checkfactor(col, ref)
#col : colonne de classe factor tiree du tableau de donn?es
#ref : vecteur contenant les modalites de reference, toutes les lignes doivent comporter une de ces modalites
#-----------------------------------------------------#

checkfactor <- function(col, ref) {
	testok <- TRUE

	for (i in 1:length(col)) {
		if (!(col[i] %in% ref)) {
			testok <- FALSE
			if (length(ref) == 1) #Le message d'erreur s'adapte au nombre de modalites
				cat("Erreur dans la colonne ", col[1], " ? la ligne ", i, ".\nLa modalite ", col[i], " ne correspond pas a la modalite attendue : ", ref)
			} else {
				cat("Erreur dans la colonne ", col[1], " ? la ligne ", i, ".\nLa modalite ", col[i], " ne correspond pas aux modalites attendues : ", ref)
			}
		}
	if (testok) {
		cat("Les modalites des facteurs sont correctes.")
	}
}

#=====================================================#
#Fonction checknumeric
#-----------------------------------------------------#
#Utilisee dans l'etape 6, elle verifie que les valeurs
#d'une colonne de classe numeric ou integer correspondent a des valeurs
#precises attendues ou sont comprises dans l'intervalle attendu
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la colonne, les numeros
#et contenus des lignes qui ne sont pas conformes, ainsi que l'intervalle attendu
#checknumeric(col, method, values)
#col : colonne de classe numeric ou integer tiree du tableau de donnees
#method : 
#  - si "disc" : les valeurs de la colonne devront correspondre exactement a une des valeurs donnees
#  - si "cont" : les valeurs de la colonne devront etre comprises entre les valeurs donnees
#values : valeurs attendues exactement (si "disc") ou bornes de l'intervalle attendu (si "cont")
#-----------------------------------------------------#