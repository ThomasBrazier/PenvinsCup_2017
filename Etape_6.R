#-----------------------------------------------------#
# Etape 6
#-----------------------------------------------------# 
checknumeric(dataset, 4, "disc", c(97, 99), identical = TRUE)

checknumeric(dataset, 6, "disc", c(0, 10, 20, 30, 40, 50, 70, 90, 110), identical = TRUE)

dmermax = 100 #Distance maximale à la mer, à définir
checknumeric(dataset, 7, "cont", c(0, dmermax), integer = TRUE)

altmax = 40 #Altitude maximale, à définir
checknumeric(dataset, 8, "cont", c(0, altmax))

surfmin = 0.1#Surface de quadrat minimale, à définir
surfmax = 40 #Surface de quadrat maximale, à définir
checknumeric(dataset, 9, "cont", c(surfmin, surfmax))

for(i in 10:16) {
	checknumeric(dataset, i, "cont", c(0, 100), integer = TRUE)
}

sflaqmin = 0.1#Surface de flaque minimale, à définir
sflaqmax = 40 #Surface de flaque maximale, à définir
checknumeric(dataset, 17, "cont", c(sflaqmin, sflaqmax), possibleNA = TRUE)

dflaqmin = 0.1#Distance à la flaque minimale, à définir
dflaqmax = 40 #Distance à la flaque maximale, à définir
checknumeric(dataset, 18, "cont", c(dflaqmin, dflaqmax), possibleNA = TRUE)

nbindmax = 200#Nombre maximum d'individus d'une espèce relevés sur un quadrat, à définir
for(i in 19:34) {
	checknumeric(dataset, i, "cont", c(0, nbindmax), integer = TRUE)
}

if (checkInd(dataset)) {
#Partie à compléter pour les fichiers individu
}

#=====================================================#
#Fonction checknumeric
#-----------------------------------------------------#
#Utilisée dans l'étape 6, elle vérifie que les valeurs
#d'une colonne de classe numeric ou integer correspondent à des valeurs
#précises attendues ou sont comprises dans l'intervalle attendu
#Si oui : message de confirmation 
#Si non : arrêter le script et indiquer la colonne, les numéros
#et contenus des lignes qui ne sont pas conformes, ainsi que l'intervalle attendu
#checknumeric(mydata, col, method, values, integer, identical, possibleNA)
#mydata : objet de classe data.frame contenant les données
#col : id de la colonne de classe numeric ou integer tirée du tableau de données
#method : 
#  - si "disc" : les valeurs de la colonne devront correspondre exactement à une des valeurs données
#  - si "cont" : les valeurs de la colonne devront être comprises entre les valeurs données
#values : valeurs attendues exactement (si "disc") ou bornes de l'intervalle attendu (si "cont")
#integer : FALSE par défaut. Si TRUE, les valeurs doivent être des nombres entiers.
#identical : FALSE par défaut. Si TRUE, les valeurs doivent toutes être identiques
#possibleNA : FALSE par défaut. Si TRUE, valeurs NA autorisées
#-----------------------------------------------------#

checknumeric <- function(mydata, col, method, values, integer = FALSE, identical = FALSE, possibleNA = FALSE) {
	testok <- TRUE

	if (method == "disc") {	
		for (i in 1:nrow(mydata)) {
			if (is.na(mydata[i,col])) {
				if (!possibleNA) {
					testok <- FALSE
					cat("Erreur dans la colonne ", names(mydata)[col], " à la ligne ", i, ".\nValeur NA inattendue.\n\n")
			} else {
				if (!mydata[i,col] %in% values) {
					testok <- FALSE
					if (length(values) == 1) #Le message d'erreur s'adapte au nombre de valeurs
						cat("Erreur dans la colonne ", names(mydata)[col], " à la ligne ", i, ".\nLa valeur  ", mydata[i,col], " ne correspond pas à la valeur attendue : ", values, ".\n\n")
					} else {
						cat("Erreur dans la colonne ", names(mydata)[col], " à la ligne ", i, ".\nLa valeur ", mydata[i,col], " ne correspond pas aux valeurs attendues : ", values, ".\n\n")
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
					cat("Erreur dans la colonne ", names(mydata)[col], " à la ligne ", i, ".\nValeur NA inattendue.\n\n")
				}
			} else {
				if (integer & (class(!mydata[i,col]) == "integer")) {
					testok <- FALSE
					cat("Erreur dans la colonne ", names(mydata)[col], " à la ligne ", i, ".\nLa valeur  ", mydata[i,col], " devrait être un nombre entier.\n\n")
				}
				if (!(mydata[i,col]>=values[1] & mydata[i,col]<=values[2])) {
					testok <- FALSE
					cat("Erreur dans la colonne ", names(mydata)[col], " à la ligne ", i, ".\nLa valeur  ", mydata[i,col], " est en dehors de l'intervalle attendu : ", values[1], "-", values[2], ".\n\n")
				}
			}
		}
	}

	if (identical) {
		#Cette commande examine tous les éléments de la colonne et retourne
		#un vecteur contenant une seule fois chaque valeur trouvée.
		#S'il est de taille supérieure à 1, toutes les valeurs ne sont pas identiques !
		if (sapply(mydata[col], function(x) length(unique(x))>1)) {
			testok <- FALSE
			cat("Erreur dans la colonne ", names(mydata)[col], ".\nToutes les valeurs devraient être identiques.\n\n")
		}
	}		

	if (testok) {
		cat("Aucune anomalie détectée dans les colonnes numériques.\n\n")
	}
}
