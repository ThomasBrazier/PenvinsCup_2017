#=====================================================#
#Fonction checkPenvins
#-----------------------------------------------------#
#Fonction principale du script appelant toutes les autres
#-----------------------------------------------------#

# "Juste pour la gloire !"

checkPenvins <- function(dataset, bilan = FALSE){
  
  bilan <<- bilan # creation d'un fichier txt d'erreur (optionnel), FALSE par defaut
  if (bilan == TRUE) {
    testSummary <- file("tests_summary.txt", open = "wt") # ouvre un fichier txt qui recueillera tous les messages, au lieu d'un affichage en consoles
    sink(file = testSummary, append = TRUE, type="output") # lance la recuperation des warnings dans un fichier txt
    sink(file = testSummary, append = TRUE, type="message") # lance la recuperation des messages dans un fichier txt
  }
  
  nm <<- deparse(substitute(dataset))  # recupere le nom de l'objet teste en variable globale (accessible meme dans les fonctions)
  count_warn <<- 0 # variable globale, init a 0, qui compte sur l'ensemble du script le nombre d'erreurs

  cat("#-----------------------------------------------------#\nBienvenue dans le verificateur de fichiers Penvins 2017.\n#-----------------------------------------------------#\n\n")
  cat("\n\nETAPE 1 : Verification de la classe de l'objet :\n")
  checkDataFrame(dataset)

  cat("\n\nETAPE 2 : Verification du nombre de colonnes :\n")
  if (checkInd(dataset)) {
    cat("Le fichier",  nm,"a 43 colonnes et est de type données biométriques sur les individus.\n")
  } else {
    if (checkQuad(dataset)) {
      cat("Le fichier", nm,"a 34 colonnes et est de type données de quadrat.\n")
    } else {
      if (bilan) {
        cat("ERROR : Le fichier", nm,"n'a pas le nombre de colonnes attendu. ", ncol(dataset)," colonnes comptees.\n")
        sink(type = "message")
        file.show("tests_summary.txt")
      }
      stop("Le fichier ", nm," n'a pas le nombre de colonnes attendu. ", ncol(dataset)," colonnes comptees.\n")
    }
  }

  cat("\n\nETAPE 3 : Verification du nom des colonnes :\n")
  checkColNames(dataset)
  
  cat("\n\nETAPE 4 : Verification de la classe des colonnes :\n")
  checkClass(dataset)

  cat("\n\nETAPE 5 : Verification des modalites pour chaque colonne de type factor :\n")
  for (p in c("transect", "resp", "date", "mode")) {  # fonction checkFactor() pour les colonnes communes aux fichiers Ind et Quad
    checkFactor(dataset, p)
    if (length(levels(dataset[[p]])) != 1) { # ces 4 colonnes ne doivent comporter qu'une unique modalite
      count_warn <<- count_warn + 1
      warning("La colonne ne doit posseder qu'une seule modalite. Veuillez verifier et choisir une modalite unique pour l'ensemble de la colonne", p, ".\n", call. = FALSE, noBreaks. = TRUE, immediate. = T)
    }
  }
  if (checkInd(dataset)) {  # uniquement sur fichiers Ind, 5 colonnes biometrie supplementaires
    for (p in c("sp", "pred", "text", "coul", "epizo")) {
      checkFactor(dataset, p)
    }
  }

  cat("\n\nETAPE 6 : Verification des donnees numeriques qui doivent etre contenues dans l'intervalle attendu :\n")  
  # Tests en serie avec la fonction checknumeric() decrite plus bas
  error_count <- 0 #variable comptant le nombre de colonnes numeriques contenant des erreurs
  
  # colonne 4 : coefficient de maree
  error_count <- error_count + checknumeric(dataset, 4, "disc", c(97, 99), identical = TRUE)
  
  # colonne 6 : distance au chenal
  error_count <- error_count + checknumeric(dataset, 6, "disc", c(0, 10, 20, 30, 40, 50, 70, 90, 110), identical = TRUE)
  
  # colonne 7 : distance a la mer
  dmermax = 100 #Distance maximale a la mer, a definir
  error_count <- error_count + checknumeric(dataset, 7, "cont", c(0, dmermax), integer = TRUE)
  
  # colonne 8 : altitude en m
  altmax = 40 # Altitude maximale, a definir
  error_count <- error_count + checknumeric(dataset, 8, "cont", c(0, altmax))
  
  # colonne 9 : surface en m2
  surfmin <- 0.1#Surface de quadrat minimale, a definir
  surfmax <- 1  #Surface de quadrat maximale, a definir (1 = 5*0.2 = 5*quadrat)
  error_count <- error_count + checknumeric(dataset, 9, "cont", c(surfmin, surfmax))
  
  # colonnes 10:16 : nombre entier entre 0 et 100 (pourcentage)
  for(i in 10:16) {
    error_count <- error_count + checknumeric(dataset, i, "cont", c(0, 100), integer = TRUE)
  }
  
  # colonne 17 : surface de la flaque
  sflaqmin <- 0.1#Surface de flaque minimale, a definir
  sflaqmax <- 40 #Surface de flaque maximale, a definir
  error_count <-error_count + checknumeric(dataset, 17, "cont", c(sflaqmin, sflaqmax), possibleNA = TRUE)
  
  # colonne 18 : distance a la flaque la plus proche
  dflaqmin <- 0.1 #Distance a la flaque minimale, a definir
  dflaqmax <- 40 #Distance a la flaque maximale, a definir
  error_count <- error_count + checknumeric(dataset, 18, "cont", c(dflaqmin, dflaqmax), possibleNA = TRUE)
  
  # colonnes 19:34 : nombre maximu d'individus
  nbindmax <- 200 #Nombre maximum d'individus d'une espece releves sur un quadrat, a definir
  # Les NAs sont acceptes : donnees manquantes/non relevees
  for(i in 19:34) {
    error_count <- error_count + checknumeric(dataset, i, "cont", c(0, nbindmax), integer = TRUE, possibleNA = TRUE)
  }
  
  if (checkInd(dataset)) {
    #Partie a completer pour les fichiers individu
    # Hauteur : col 36
    
    # Largeur : col 37
    
    # Peristome : col 38
    
    # Masse : col 43
    
  }
  
  if (error_count == 0) {
    cat("Aucune anomalie detectee dans les colonnes numeriques.\n\n")
  } else {
    if (error_count == 1) {
      cat("Une colonne numerique contient des erreurs.\nVerifiez les avertissements ci-dessus.\n\n")
    } else {
      cat(error_count, " colonnes numeriques contiennent des erreurs.\nVerifiez les avertissements ci-dessus.\n\n")
    }
  }
  
  if (checkInd(dataset)){ # Vérification du type de fichier : type Ind attendu uniquement
    cat("\n\nETAPE 7 : Verification des ratios des mesures biometriques :\n")
    checkRatio(dataset, 1)    # ratio larg/haut, option 1
    checkRatio(dataset, 2)    # ratio peri/larg, option 2
  }

  if (count_warn == 0) { # Message de conclusion, en fonction de la conformite du fichier ou bien du nombre d'erreurs relevees
    cat("\nCe fichier est conforme a ce qui etait attendu. Il est prêt a etre utilise pour le projet.\n")
  } else {
    if (count_warn == 1) {
      cat("\nUne erreur a ete relevee pendant l'analyse. Veuillez proceder a sa correction...\n")
    } else {
    cat("\n", count_warn, " erreurs ont ete relevees pendant l'analyse. Veuillez proceder a leur correction...\n")
    }
  }
  cat("\nFin de l'analyse.\n")
  if (bilan) { # fin du sink et affichage du bilan si l'option est activee
    sink(type="output") # stop sinking des warnings
    sink(type="message") # stop sinking des messages
    file.show("tests_summary.txt") # affichage du fichier texte des messages et erreurs
    close(testSummary) # ferme le fichier txt
  }
  #return(count_warn) # la fonction renvoie le nombre d'erreurs quand elle se termine
}



#=====================================================#
# FONCTIONS
#=====================================================#

#-----------------------------------------------------#
#Etape 1 : Verifier si mydata est de classe data.frame
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la classe de mydata
#-----------------------------------------------------#
checkDataFrame <- function(mydata) { # Etape 1 : Verifier si mydata est de classe data.frame
  if (is.data.frame(mydata)) { # teste si le fichier est bien un data.frame
    cat("Le fichier", nm, "est bien de type data.frame.\n")
  } else { # sinon affiche une erreur et stoppe la fonction
    if (bilan) {
      cat("ERROR : Le fichier n'est pas de type data.frame : ", nm, " est un ", class(mydata), ".\n")
      sink(type = "message")
      file.show("tests_summary.txt")
    }
    stop("Le fichier n'est pas de type data.frame  : ", nm, " est un ", class(mydata), ".\n")
  }
}

#-----------------------------------------------------#
#Etape 2 : Verifier que le tableau possede bien 34 ou 43 colonnes
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer le nombre incorrect de colonnes
#-----------------------------------------------------#

#-----------------------------------------------------#

checkQuad <- function(mydata) { # Vérifie que le fichier mydata est bien de type Quad en comptant le nombre de colonnes
  if (ncol(mydata) == 34){ return(TRUE) } else{ return(FALSE) }} # un fichier quad doit contenir 34 colonnes

#-----------------------------------------------------#

checkInd <- function(mydata) { # Vérifie que le fichier mydata est bien de type Ind en comptant le nombre de colonnes
  if (ncol(mydata) == 43){ return(TRUE) } else{ return(FALSE) }} # un fichier ind doit contenir 43 colonnes


#-----------------------------------------------------#
#Etape 3 : Verifier que les noms de colonnes sont corrects
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer les erreurs et les noms attendus
#-----------------------------------------------------#

# La fonction checkColNames() vérifie que les nomes des colonnes du fichier dataset en argument correspondent bien à la liste nomsRef en argument 2

checkColNames <- function(mydata) {
  nomsRef = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap", "sp", "haut", "larg", "peri", "pred", "coul", "text", "epizo", "masse") #Noms et ordres des colonnes attendus pour le fichier
  col = names(mydata) # "col" stocke les noms des colonnes du fichier observes
  verif = 0 # si verif toujours egal a 0 a la fin du test, les noms des colonnes sont conformes
  for (i in 1:length(mydata)) {
    if (col[i] != nomsRef[i]) {
      verif=+1 # si il y a une erreur, ce n'est plus conforme, verif change d'etat -> FALSE
      warning("Le nom de la colonne ", i, " n'est pas correct.", call. = FALSE, noBreaks. = TRUE, immediate. = TRUE)
      cat("Remplacer le nom de la colonne",i, "par",nomsRef[i],".\n")} # affiche un message donnant la solution pour remplacer les noms non conformes
    }
    if (verif != 0) { # en fin de test, s'assure de la conformite du data frame, stoppe la fonction en cas de non conformite
      if (bilan) {
        cat("ERROR : La fonction checkPenvins s'est terminee prematurement. Il y a ", verif," erreur(s) dans les noms de colonne.")
        sink(type = "message")
        file.show("tests_summary.txt")
      }
    stop("Il y a ", verif," erreur(s) dans les noms de colonnes.", call. = FALSE) # message d'erreur indiquant qu'il y a une ou des erreurs et stoppant la fonction
  } else {cat("Tous les noms de colonne sont correctes.\n")}
}


#-----------------------------------------------------#
# Etape 4 :
# Verifier que les colonnes sont bien de la classe attendue :
# factor (des lettres), integer (nombres entiers) ou numeric (nombres décimaux)
# selon le cas. Si non, afficher un message d'erreur signalant le nom et la classe des colonnes qui posent probleme
# en citant a chaque fois la classe anormale detectee et la classe qui etait attendue,
# puis stopper la fonction. S'il n'y a pas d'erreur,
# afficher un message d'information indiquant que les classes des colonnes sont correctes.
#-----------------------------------------------------#

# Fonction checkClass()
# prend en argument :
# mydata : un fichier data.frame de type ind ou quad
checkClass <- function(mydata) {
  success = TRUE # si aucune erreur relevee, success restera TRUE jusqu'a la fin de la fonction
  ClassExpect = c("factor", "factor", "factor", "integer", "factor", "integer", "integer", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "factor", "numeric", "numeric", "numeric", "factor", "factor", "factor", "factor", "numeric") # Liste des classes attendues pour le fichier Ind ou quad, classees selon numero de colonne
  
  # parcourt les colonnes une par une pour verifier la classe
  for (i in 1:length(mydata)) {
    if (class(mydata[,i]) != ClassExpect[i]) { # verifie si la classe de la colonne i est differente de la classe attendue
      success = FALSE # bascule success en FALSE pour declencher un stop() a la fin de la fonction
      warning("La classe de la colonne ", i, " (", names(mydata)[i], ") est ", class(mydata[,i]), " la ou ", ClassExpect[i], " etait attendu.\n", call. = FALSE, noBreaks. = TRUE, immediate. = TRUE)
    }
  }
  
  if (success) {
    cat("Toutes les classes des colonnes sont correctes.\n")
  } else {
    if (bilan) {
      cat("ERROR : Certaines colonnes ne sont pas de la classe attendue.\n")
      sink(type = "message")
      file.show("tests_summary.txt")
    }
    stop("Le fichier n'est pas conforme : Certaines colonnes ne sont pas de la classe attendue.\n", call. = FALSE)
  }
}


#=====================================================#
#Etape 5 : Verifier dans les colonnes de classe "factor"
#que la ou les modalites du facteur correspondent a celles attendues
#Si oui : message de confirmation 
#Si non : arreter le script et indiquer la colonne, les numeros
#et contenus des lignes qui ne sont pas conformes, ainsi que le contenu attendu
#-----------------------------------------------------#

#-----------------------------------------------------#
# Fonction checkFactor()

# Utilisee dans l'etape 5, elle verifie que les modalites
# d'une colonne de classe factor correspondent a celles attendues
# Si oui : message de confirmation
# Si non : arreter le script et indiquer la colonne, les numeros
# et contenus des lignes qui ne sont pas conformes, ainsi que le contenu attendu
# checkFactor(col, ref)
# col : nom d'une colonne de classe factor tiree du tableau de donnees sous forme data[,i] ou data$name
# ref : vecteur contenant les modalites de reference, toutes les lignes doivent comporter une de ces modalites


checkFactor <- function(mydata, nameCol) {
  # recupere toutes les modalites de la colonne, a partir du nom de la colonne
  ref = list(transect = c(paste("a", c(0, 10, 20, 30, 40, 50, 70, 90, 110), sep = ""), paste("b", c(0, 10, 20, 30, 40, 50, 70, 90, 110), sep = "")),
          resp = c("raphael.leprince", "nicolas.leroux.1", "audrey.fabarez", "morgane.milin", "vincent.carre", "justine.charlet-de-sauvage", "capucine.hemonnet-dal", "sasha.donnier", "heloise.villesseche", "julien.curassier", "laura.bellec", "nathan.viel", "thomas.brazier", "loic.menut", "eloise.couthouis", "thibaud.tournadre", "camille.pilisi"),
          date = c("20septembre2017", "21septembre2017"), mode = c("a", "b"),
          sp = c("Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap"),
          pred = c(NA, "oui", "non"), coul = c(NA, "clair", "sombre", "rayures"), text = c(NA, "lisse", "rugueux", "bosses"), epizo = c(NA, "oui", "non"))
  col = which(colnames(mydata) == nameCol)
  mods = levels(mydata[,col])
  if (NA %in% mydata[,col]) { # ajoute les NA a la liste des modalites
    mods =c(mods, NA)
  }
  if (min(mods %in% ref[[nameCol]]) == 1) { # si la comparaison entre mods et ref ne renvoie que des TRUE -> fin de la fonction
    cat("Toutes les modalites de la colonne ",  colnames(mydata)[col]," sont correctes.\n")
  } else { # sinon, recherche de l'erreur
    for (i in 1:nrow(mydata[col])) { # pour chaque ligne de la colonne
      if (!(mydata[i, col] %in% ref[[nameCol]])) {
        count_warn <<- count_warn + 1
        warning(c("Erreur dans la colonne ", colnames(mydata)[col], " a la ligne ", i, ".\nLa modalite ", as.character(mydata[i, col]), " ne correspond pas aux modalites attendues : ", paste(ref[[nameCol]], collapse = ", "), ".\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
      }
    }
    warning(c("Erreur dans la colonne ", colnames(mydata)[col], ", une ou des modalites sont fausses.\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
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
  
  return(!testok) 
  #Si testok == TRUE, renvoie FALSE donc 0 : pas d'erreur supplementaire detectee
  #Si testok == FALSE, renvoie TRUE donc 1 : 1 colonne supplementaire contient une erreur
}
#-----------------------------------------------------#



#-----------------------------------------------------#
# Etape 7 : Verification des ratios larg/haut et peri/larg dans biom.txt
#-----------------------------------------------------#
# verifier que les ratios larg/haut et peri/larg du fichier biometrique qui ne sont pas NA
#sont situes respectivement dans des intervalles min-max realistes que vous choisirez.

# Fonction checkRatio()
# Vérifie le ratio attendu entre deux variables
# prend 2 arguments :
# un data frame de type ind et le type de test => larg/haut (1) ou peri/larg (2)
checkRatio <- function(mydata, type = 1){
  success = TRUE  # success restera TRUE uniquement si aucun warning n'est releve
  if (type == 1) { # recupere les numeros de colonne des variables larg et haut ou peri, ici pour une verification de larg/haut
    v1 = 36 # colonne largeur
    v2 = 37 # colonne hauteur
    ty = "largeur/hauteur"
    MinMax = c(0.35,3)    # vecteur MinMax = ratio min et ratio max attendu
  } else {
    if (type == 2) { # pour une verification de peri/larg
      v1 = 38 # colonne peristome
      v2 = 36 # colonne largeur
      ty = "peristome/largeur"
      MinMax = c(0.1,1.5) # vecteur MinMax = ratio min et ratio max attendu
    }
  }
  for (i in 1:nrow(mydata)) { # comparaison des ratios ligne par ligne : parcourt chaque ligne des deux colonnes
    if (!is.na(mydata[i,v1]) & !is.na(mydata[i,v2])){   # verifie que les deux valeurs ne sont pas des NA et sont ainsi comparables
      ratio = mydata[i,v1]/mydata[i,v2] # calcule le ratio pour la ligne i
      if (ratio<MinMax[1] | ratio>MinMax[2]){
        success = FALSE # echec du test : la valeur sort de l'intervalle attendu
        count_warn <<- count_warn + 1
        warning(c("Ligne ", i, " pour l'espece ", as.character(mydata[i,35]), " : Le ratio ", ty, " = ", ratio, " sort de l'intervalle attendu ", MinMax[1], ":", MinMax[2]), call. = FALSE, noBreaks. = TRUE, immediate. = T)
      }
    } else{ #dans le cas ou l'une des valeurs est NA
      success = FALSE
      count_warn <<- count_warn + 1
      warning(c("Ligne ", i, " pour l'espece ", as.character(mydata[i,35]), " : valeur NA !"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
    }
  }
  if (success == TRUE) { # si success est reste TRUE, message de reussite
    cat("Tous les ratios ", ty, " sont corrects.")
  }
}