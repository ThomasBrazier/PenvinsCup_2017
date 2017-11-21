#-----------------------------------------------------#
# Etape 6
#-----------------------------------------------------# 

  numeric_error_count <- 0 #variable GLOBALE comptant le nombre de colonnes numeriques contenant des erreurs
  
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
  surfmin <- 0.1#Surface de quadrat minimale, a definir
  surfmax <- 1  #Surface de quadrat maximale, a definir (1 = 5*0.2 = 5*quadrat)
  checknumeric(dataset, 9, "cont", c(surfmin, surfmax))
  
  # colonnes 10:16 : nombre entier entre 0 et 100 (pourcentage)
  for(i in 10:16) {
    checknumeric(dataset, i, "cont", c(0, 100), integer = TRUE)
  }
  
  # colonne 17 : surface de la flaque
  sflaqmin <- 0.1#Surface de flaque minimale, a definir
  sflaqmax <- 40 #Surface de flaque maximale, a definir
  checknumeric(dataset, 17, "cont", c(sflaqmin, sflaqmax), possibleNA = TRUE)
  
  # colonne 18 : distance a la flaque la plus proche
  dflaqmin <- 0.1 #Distance a la flaque minimale, a definir
  dflaqmax <- 40 #Distance a la flaque maximale, a definir
  checknumeric(dataset, 18, "cont", c(dflaqmin, dflaqmax), possibleNA = TRUE)
  
  # colonnes 19:34 : nombre maximu d'individus
  nbindmax <- 200 #Nombre maximum d'individus d'une espece releves sur un quadrat, a definir
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

  if (numeric_error_count == 0) {
	cat("Aucune anomalie detectee dans les colonnes numeriques.\n\n")
  } else {
	if (numeric_error_count == 1) {
		cat("Une colonne numerique contient des erreurs.\nVerifiez les avertissements ci-dessus.\n\n")
  	} else {
		cat(numeric_error_count, " colonnes numeriques contiennent des erreurs.\nVerifiez les avertissements ci-dessus.\n\n")
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
#checknumeric(mydata, col, method, values, integer, identical, possibleNA)
#mydata : objet de classe data.frame contenant les donnees
#col : id de la colonne de classe numeric ou integer tiree du tableau de donnees
#method : 
#  - si "disc" : les valeurs de la colonne devront correspondre exactement a une des valeurs donnees
#  - si "cont" : les valeurs de la colonne devront etre comprises entre les valeurs donnees
#values : valeurs attendues exactement (si "disc") ou bornes de l'intervalle attendu (si "cont")
#integer : FALSE par defaut. Si TRUE, les valeurs doivent etre des nombres entiers.
#identical : FALSE par defaut. Si TRUE, les valeurs doivent toutes etre identiques
#possibleNA : FALSE par defaut. Si TRUE, valeurs NA autorisees
#-----------------------------------------------------#

checknumeric <- function(mydata, col, method, values, integer = FALSE, identical = FALSE, possibleNA = FALSE) {
	testok <- TRUE

	if (method == "disc") {	#Valeurs correspondant exactement aux valeurs de reference
		for (i in 1:nrow(mydata)) {
			if (is.na(mydata[i,col])) {
				if (!possibleNA) { #Tout NA doit etre signale si les NA ne sont pas autorises
					testok <- FALSE
					warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ".\nValeur NA inattendue.\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
			} else {
				if (!mydata[i,col] %in% values) { #Si la valeur a la ligne i n'est pas comprise dans le vecteur values
					testok <- FALSE
					if (length(values) == 1) #Le message d'erreur s'adapte au nombre de valeurs
						warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ".\nLa valeur  ", mydata[i,col], " ne correspond pas a la valeur attendue : ", values, ".\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
					} else {
						warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ".\nLa valeur ", mydata[i,col], " ne correspond pas aux valeurs attendues : ", values, ".\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
					}
				}
			}
		}
	}

	if (method == "cont") { #Valeurs devant etre comprises dans un intervalle
		for (i in 1:nrow(mydata)) {
			if (is.na(mydata[i,col])) {
				if (!possibleNA) { #Tout NA doit etre signale si les NA ne sont pas autorises
					testok <- FALSE
					warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ".\nValeur NA inattendue.\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
				}
			} else {
				if (integer & (as.integer(mydata[i,col]) != mydata[i,col])) { #Si des nombres entiers sont attendus, les nombres decimaux doivent etre signales
					testok <- FALSE
					warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ".\nLa valeur  ", mydata[i,col], " devrait etre un nombre entier.\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
				}
				if (!(mydata[i,col]>=values[1] & mydata[i,col]<=values[2])) { #Si la valeur a la ligne i sort de l'intervalle attendu
					testok <- FALSE
					warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ".\nLa valeur  ", mydata[i,col], " est en dehors de l'intervalle attendu : ", values[1], "-", values[2], ".\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
				}
			}
		}
	}

	if (identical) { #Valeurs identiques attendues sur la colonne
		if (sapply(mydata[col], function(x) length(unique(x))>1)) {
			#Cette commande examine tous les elements de la colonne et retourne
			#un vecteur contenant une seule fois chaque valeur trouvee.
			#S'il est de taille superieure a 1, toutes les valeurs ne sont pas identiques !
			testok <- FALSE
			warning(c("Erreur dans la colonne ", names(mydata)[col], ".\nToutes les valeurs devraient etre identiques.\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
		}
	}		

	numeric_error_count <<- numeric_error_count + !testok 
	#Si testok == TRUE, renvoie FALSE donc 0 : pas d'erreur supplementaire detectee
	#Si testok == FALSE, renvoie TRUE donc 1 : 1 colonne supplementaire contient une erreur
	#<<- permet de modifier la variable globale
}
