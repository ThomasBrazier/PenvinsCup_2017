#=====================================================#
#Pour utiliser ce script, assurez-vous que le fichier 
#"checkPenvins_EFCE_2017.R" se trouve dans votre repertoire 
#courant, puis copiez-collez la commande ci-dessous dans R :
#source("checkPenvins_EFCE_2017.R")
#-----------------------------------------------------#
#Vous pouvez egalement utiliser le menu Fichier > Sourcer du code R... > 
#et selectionner le script
#-----------------------------------------------------#
#Il ne vous reste plus qu'a lire votre fichier de donnees 
#avec read.table() et a utiliser :
#checkPenvins(nomdutableau)
#-----------------------------------------------------#

#=====================================================#
#Fonction checkPenvins
#-----------------------------------------------------#
#Fonction principale du script appelant toutes les autres
#-----------------------------------------------------#

checkPenvins <- function(mydata) {

cat("Bienvenue dans le verificateur de fichiers Penvins 2017.")
}

#-----------------------------------------------------#
# Enregistre tous les messages dans un fichier txt "test_summary.txt"
testSummary <- file("tests_summary.txt", open = "w")
sink(file = testSummary, append = TRUE, type="message", split = TRUE)

#-----------------------------------------------------#
# Vérifie que le fichier file est bien de type Quad en vérifiant le nombre de colonnes attendues
checkQuad <- function(file) {
  nbColQuad = 29
    if (ncol(file)==nbColQuad){
      return(TRUE)
    } else{
      return(FALSE)
    }
}

#-----------------------------------------------------#
# Vérifie que le fichier file est bien de type Ind en vérifiant le nombre de colonnes attendues
checkInd <- function(file) {
  nbColInd = 39
  if (ncol(file)==nbColInd){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#-----------------------------------------------------#
#Etape 1 : Verifier si mydata est de classe data.frame
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la classe de mydata
#-----------------------------------------------------#

if (class(mydata) == "data.frame") {
	cat("Tableau charge avec succes !\nL'objet est bien de classe ", class(mydata), ".\nAnalyse des donnees...\n\n")
} else {
	stop("L'objet charge est de classe ", class(mydata), " et ne peut etre lu par ce script.\n Verifiez que votre objet est un tableau.\n\n")
}

#-----------------------------------------------------#
#Etape 2 : Verifier que le tableau possede bien 27 colonnes
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer le nombre incorrect de colonnes
#-----------------------------------------------------#

cat("Analyse du nombre de colonnes...\n\n")

if (ncol(mydata) == 27) {
	cat("Votre tableau comporte bien 27 colonnes.\n\n")
} else {
	stop("Votre tableau comporte ", ncol(mydata), " colonnes au lieu de 27.\nCorrigez votre fichier et recommencez.\n\n")
}

#-----------------------------------------------------#
#Etape 3 : Verifier que les noms de colonnes sont corrects
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer les erreurs et les noms attendus
#-----------------------------------------------------#

cat("Analyse des noms de colonnes...\n\n")

refname = c("cidre", "galette") #Liste des noms de colonnes attendus
testok = TRUE #Variable egale a TRUE si les noms sont conformes ou FALSE sinon

for (i in seq_along(mydata)) {
	if (names(mydata)[i] != refname[i]) {
		cat("Le nom de la colonne ", names(mydata)[i], " est incorrect. Remplacer par ", refname[i],".\n\n")
		testok = FALSE
	}
}

if (testok) {
	cat("Les noms de colonnes sont corrects.")
} else {
	stop("Certains noms de colonnes ne sont pas conformes.\nVerifiez les references donnees ci-dessus.")
}

#-----------------------------------------------------#
#Etape 4 : Verifier que les classes des colonnes sont correctes
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer les erreurs et les classes attendues
#-----------------------------------------------------#

cat("Analyse des classes des colonnes...\n\n")

refclass = c("cidre", "galette") #Liste des classes de colonnes attendues
testok = TRUE #Variable egale a TRUE si les noms sont conformes ou FALSE sinon

for (i in seq_along(mydata)) {
	if (class(mydata[i]) != refclass[i]) {
		cat("La classe de la colonne ", names(mydata)[i], " est incorrecte.\nLes donnees sont de classe ", (class(mydata[i]), "\net devraient etre de classe ", refclass[i],".\n\n")
		testok = FALSE
	}
}

if (testok) {
	cat("Les classes des colonnes sont correctes.")
} else {
	stop("Certaines classes de colonnes ne sont pas conformes.\nVerifiez le format des donnees.")
}

cat("Le contenu des colonnes va etre analyse en detail.\n")
readline("Appuyez sur Entree pour continuer.") #Permet a l'utilisateur de respirer entre les blocs de texte.

#-----------------------------------------------------#
#Etape 5 : Verifier dans les colonnes de classe "factor"
#que la ou les modalites du facteur correspondent a celles attendues
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la colonne, les numeros
#et contenus des lignes qui ne sont pas conformes, ainsi que le contenu attendu
#-----------------------------------------------------#

cat("Analyse de la conformite des noms des facteurs...\n\n")

for (i in seq_along(mydata)) {
	if (mydata[i][1] == "group" | mydata[i][1] == "resp") { #Colonnes ne devant contenir qu'une modalite
		checkfactor(mydata[i], mydata[i][2]) 		  #Appelle la fonction dediee a cet epineux probleme
	}  
	if (mydata[i][1] == "month") {
		checkfactor(mydata[i], c("sept"))
	}  	
	if (mydata[i][1] == "mode") {
		checkfactor(mydata[i], c("a", "b"))
	}  
}

}

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
if (checkInd(mydata)){ # Vérification du fichier : type ind
  
  # ratio larg/haut
  
  
  # ratio peri/larg
  
  # Fin de l'etape 7
  
}

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

	for (i in 2:length(col)) { #La boucle commence en deuxieme ligne pour ne pas examiner le titre
		if !(col[i] %in% ref) {
			testok <- FALSE
			if (length(ref) == 1) #Le message d'erreur s'adapte au nombre de modalites
				cat("Erreur dans la colonne ", col[1], " ? la ligne ", i, ".\nLa modalite ", col[i], " ne correspond pas a la modalite attendue : ", ref)
			} else {
				cat("Erreur dans la colonne ", col[1], " ? la ligne ", i, ".\nLa modalite ", col[i], " ne correspond pas aux modalites attendues : ", ref)
			}
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

checknumeric <- function(col, method, values) {
	testok <- TRUE

	if method == "disc"	
		for (i in 2:length(col)) { #La boucle commence en deuxieme ligne pour ne pas examiner le titre
			if !(col[i] %in% values) {
				testok <- FALSE
				if (length(values) == 1) #Le message d'erreur s'adapte au nombre de valeurs
					cat("Erreur dans la colonne ", col[1], " a la ligne ", i, ".\nLa valeur  ", col[i], " ne correspond pas a la valeur attendue : ", values)
				} else {
					cat("Erreur dans la colonne ", col[1], " a la ligne ", i, ".\nLa valeur ", col[i], " ne correspond pas aux valeurs attendues : ", values)
				}
			}
		}

	if method == "cont"	
		for (i in 2:length(col)) { #La boucle commence en deuxieme ligne pour ne pas examiner le titre
			if !(col[i]>=values[1] & col[i]<=values[2]) {
				testok <- FALSE
				cat("Erreur dans la colonne ", col[1], " a la ligne ", i, ".\nLa valeur  ", col[i], " est en dehors de l'intervalle attendu : ", values[1], "-", values[2])
			}
		}

	if (testok) {
		cat("Les valeurs numeriques sont correctes.")
	} 
}
}

#=====================================================#
# Fin du script
#-----------------------------------------------------#
sink() # stop sinking
cat("Fin de l'analyse.",file = testSummary)
close(testSummary) # Ferme le fichier txt