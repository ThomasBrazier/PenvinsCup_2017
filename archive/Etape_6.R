#-----------------------------------------------------#
# Etape 6
#-----------------------------------------------------#

# Tests en serie avec la fonction checknumeric() decrite plus bas

# colonne 4 : coefficient de maree
checknumeric(dataset, 4, "disc", c(97, 99), identical = TRUE)
# colonne 6 : distance au chenal
checknumeric(dataset, 6, "disc", c(0, 10, 20, 30, 40, 50, 70, 90, 110), identical = TRUE)
# colonne 7 : distance a la mer
dmermax = 100 #Distance maximale a la mer, a definir
checknumeric(dataset, 7, "cont", c(0, dmermax), integer = TRUE)
# colonne 8 : altitude en m
altmax = 40 # Altitude maximale, a definir
checknumeric(dataset, 8, "cont", c(0, altmax))
# colonne 9 : surface en m2
surfmin = 0.1#Surface de quadrat minimale, a definir
surfmax = 1 #Surface de quadrat maximale, a definir (1 = 5*0.2 = 5*quadrat)
checknumeric(dataset, 9, "cont", c(surfmin, surfmax))

# colonnes 10:16 : nombre entier entre 0 et 100 (pourcentage)
for(i in 10:16) {
	checknumeric(dataset, i, "cont", c(0, 100), integer = TRUE)
}
# colonne 17 : surface de la flaque
sflaqmin = 0.1#Surface de flaque minimale, a definir
sflaqmax = 40 #Surface de flaque maximale, a definir
checknumeric(dataset, 17, "cont", c(sflaqmin, sflaqmax), possibleNA = TRUE)
# colonne 18 : distance a la flaque la plus proche
dflaqmin = 0.1 #Distance a la flaque minimale, a definir
dflaqmax = 40 #Distance a la flaque maximale, a definir
checknumeric(dataset, 18, "cont", c(dflaqmin, dflaqmax), possibleNA = TRUE)
# colonnes 19:34 : nombre maximu d'individus
nbindmax = 200 #Nombre maximum d'individus d'une espece releves sur un quadrat, a definir
# Les NAs sont acceptes : donnees manquantes/non relevees
for(i in 19:34) {
	checknumeric(dataset, i, "cont", c(0, nbindmax), integer = TRUE, possibleNA = TRUE)
}

if (checkInd(dataset)) {
  #Partie a completer pour les fichiers individu
  # Hauteur : col 36
  
  # Largeur : col 37
  
  # Peristome : col 38
  
  # Masse : col 43
  
}

#=====================================================#
#Fonction checknumeric
#-----------------------------------------------------#
#Utilis?e dans l'?tape 6, elle v?rifie que les valeurs
#d'une colonne de classe numeric ou integer correspondent ? des valeurs
#pr?cises attendues ou sont comprises dans l'intervalle attendu
#Si oui : message de confirmation 
#Si non : arr?ter le script et indiquer la colonne, les num?ros
#et contenus des lignes qui ne sont pas conformes, ainsi que l'intervalle attendu
#checknumeric(mydata, col, method, values, integer, identical, possibleNA)
#mydata : objet de classe data.frame contenant les donn?es
#col : id de la colonne de classe numeric ou integer tir?e du tableau de donn?es
#method : 
#  - si "disc" : les valeurs de la colonne devront correspondre exactement ? une des valeurs donn?es
#  - si "cont" : les valeurs de la colonne devront ?tre comprises entre les valeurs donn?es
#values : valeurs attendues exactement (si "disc") ou bornes de l'intervalle attendu (si "cont")
#integer : FALSE par d?faut. Si TRUE, les valeurs doivent ?tre des nombres entiers.
#identical : FALSE par d?faut. Si TRUE, les valeurs doivent toutes ?tre identiques
#possibleNA : FALSE par d?faut. Si TRUE, valeurs NA autoris?es
#-----------------------------------------------------#

checknumeric <- function(mydata, col, method, values, integer = FALSE, identical = FALSE, possibleNA = FALSE) {
	testok <- TRUE

	if (method == "disc") {	
		for (i in 1:nrow(mydata)) {
			if (is.na(mydata[i,col])) {
				if (!possibleNA) {
					testok <- FALSE
					cat("Erreur dans la colonne ", names(mydata)[col], " ? la ligne ", i, ".\nValeur NA inattendue.\n\n")
			} else {
				if (!mydata[i,col] %in% values) {
					testok <- FALSE
					if (length(values) == 1) #Le message d'erreur s'adapte au nombre de valeurs
						cat("Erreur dans la colonne ", names(mydata)[col], " ? la ligne ", i, ".\nLa valeur  ", mydata[i,col], " ne correspond pas ? la valeur attendue : ", values, ".\n\n")
					} else {
						cat("Erreur dans la colonne ", names(mydata)[col], " ? la ligne ", i, ".\nLa valeur ", mydata[i,col], " ne correspond pas aux valeurs attendues : ", values, ".\n\n")
					}
				}
			}
		}
	}

	if (method == "cont") {
		for (i in 1:nrow(mydata)) {
			if (is.na(mydata[i,col])) {
				if (!possibleNA) {
					testok <- FALSE
					cat("Erreur dans la colonne ", names(mydata)[col], " ? la ligne ", i, ".\nValeur NA inattendue.\n\n")
				}
			} else {
				if (integer & (class(!mydata[i,col]) == "integer")) {
					testok <- FALSE
					cat("Erreur dans la colonne ", names(mydata)[col], " ? la ligne ", i, ".\nLa valeur  ", mydata[i,col], " devrait ?tre un nombre entier.\n\n")
				}
				if (!(mydata[i,col]>=values[1] & mydata[i,col]<=values[2])) {
					testok <- FALSE
					cat("Erreur dans la colonne ", names(mydata)[col], " ? la ligne ", i, ".\nLa valeur  ", mydata[i,col], " est en dehors de l'intervalle attendu : ", values[1], "-", values[2], ".\n\n")
				}
			}
		}
	}

	if (identical) {
		#Cette commande examine tous les ?l?ments de la colonne et retourne
		#un vecteur contenant une seule fois chaque valeur trouv?e.
		#S'il est de taille sup?rieure ? 1, toutes les valeurs ne sont pas identiques !
		if (sapply(mydata[col], function(x) length(unique(x))>1)) {
			testok <- FALSE
			cat("Erreur dans la colonne ", names(mydata)[col], ".\nToutes les valeurs devraient ?tre identiques.\n\n")
		}
	}		

	if (testok) {
		cat("Aucune anomalie d?tect?e dans les colonnes num?riques.\n\n")
	}
}
