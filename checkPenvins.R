#=====================================================#
# Fonction checkPenvins
#=====================================================#

# "Juste pour la gloire !"

checkPenvins <- function(dataset, bilan = FALSE){
  bilan <<- bilan # la variable globale "bilan", si elle est programmee à TRUE par l'utilisateur, active toutes les lignes pour la fabrication du fichier d'erreur summary.txt
  if (bilan == TRUE) { # creation d'un fichier txt d'erreur (optionnel), FALSE par defaut
    testSummary <- file("tests_summary.txt", open = "wt") # ouvre un fichier txt qui recueillera tous les messages, au lieu d'un affichage en consoles
    sink(file = testSummary, append = TRUE, type="output") # lance la recuperation des warnings dans ce fichier txt
    sink(file = testSummary, append = TRUE, type="message") # lance la recuperation des messages dans ce fichier txt
  }
  # recuperation et declaration des variables globales
  nm <<- deparse(substitute(dataset))  # recupere le nom de l'objet fournit a la fonction et l'affecte en variable globale (accessible meme dans les sous-fonctions)
  count_warn <<- 0 # variable globale, init a 0, qui compte le nombre d'erreurs sur l'ensemble du script

  cat("#-----------------------------------------------------#\nBienvenue dans le verificateur de fichiers Penvins 2017.\n#-----------------------------------------------------#\n")
  cat("\n\nETAPE 1 : Verification de la classe de l'objet :\n")
  checkDataFrame(dataset) # l'objet doit etre de classe data.frame, sinon le script s'arrete

  cat("\n\nETAPE 2 : Verification du nombre de colonnes :\n")
  if (checkInd(dataset)) {cat("Le fichier",  nm,"contient 43 colonnes, ce qui est correct pour un fichier de type donnees biometriques sur les individus.\n")} # si fichier compte 43 colonnes, il est reconnu de type Individu
  else {
    if (checkQuad(dataset)) {cat("Le fichier", nm,"contient 34 colonnes, ce qui est correct pour un fichier de type donnees de quadrats.\n")} # si fichier compte 34 colonnes, il est reconnu de type Quad
    else { # fichier n'etant ni Ind, ni Quad, declenchement de l'erreur qui stoppera le script
      if (bilan) {
        cat("Erreur : le fichier", nm,"n'a pas le nombre de colonnes attendu. ", ncol(dataset)," colonnes comptees. On attendait 34 colonnes pour un fichier Quadrat ou 43 colonnes pour un fichier Ind.\nVeuillez corriger le fichier avant de poursuivre...\n")
        sink(type = "message")
        file.show("tests_summary.txt")
      }
      stop("Le fichier ", nm," n'a pas le nombre de colonnes attendu. ", ncol(dataset)," colonnes comptees. On attendait 34 colonnes pour un fichier Quadrat ou 43 colonnes pour un fichier Ind.\nVeuillez corriger le fichier avant de poursuivre...\n") # fin prematuree du script
    }
  }

  cat("\n\nETAPE 3 : Verification du nom des colonnes :\n")
  checkColNames(dataset)
  
  cat("\n\nETAPE 4 : Verification de la classe des colonnes :\n")
  checkClass(dataset)

  cat("\n\nETAPE 5 : Verification des modalites pour chaque colonne de type factor :\n")
  for (p in c("transect", "resp", "date", "mode")) {  # fonction checkFactor() pour les colonnes communes aux fichiers Ind et Quad
    checkFactor(dataset, p)
    if (length(levels(dataset[[p]])) != 1) { # ces 4 colonnes ne doivent comporter qu'une unique modalite, sinon declenchement d'une erreur
      count_warn <<- count_warn + 1
      warning("La colonne ne doit posseder qu'une seule modalite. Veuillez verifier et choisir une modalite unique pour l'ensemble de la colonne", p, ".\n", call. = FALSE, noBreaks. = TRUE, immediate. = T)
    }
  }
  if (checkInd(dataset)) {  # uniquement pour les fichiers Ind, 5 colonnes biometriques supplementaires sont a tester
    for (p in c("sp", "pred", "text", "coul", "epizo")) {checkFactor(dataset, p)}
  }

  cat("\n\nETAPE 6 : Verification des donnees numeriques, qui doivent etre contenues dans un intervalle attendu :\n") # Tests en serie avec la fonction checknumeric() decrite plus bas
  numeric_error_count <<- 0 # variable GLOBALE comptant le nombre de colonnes numeriques contenant des erreurs
  # Quand nous n'avions pas de references connues pour l'intervalle attendu, les valeurs ont ete fixees apres etude de leur distribution et confirmation avec les responsables de groupe de la veracite des valeurs extremes
  checknumeric(dataset, 4, "disc", c(97, 99), identical = TRUE)  # colonne 4 : coefficient de maree
  checknumeric(dataset, 6, "disc", c(0, 10, 20, 30, 40, 50, 70, 90, 110), identical = TRUE) # colonne 6 : distance au chenal
  checknumeric(dataset, 7, "cont", c(0, 96), integer = TRUE) # colonne 7 : distance a la mer
  checknumeric(dataset, 8, "cont", c(0, 7)) # colonne 8 : altitude en m
  checknumeric(dataset, 9, "cont", c(0.1, 1))# colonne 9 : surface en m2
  for(i in 10:16) {checknumeric(dataset, i, "cont", c(0, 100), integer = TRUE)} # colonnes 10:16 : nombre entier entre 0 et 100 (pourcentage)
  checknumeric(dataset, 17, "cont", c(0.05, 45), possibleNA = TRUE) # colonne 17 : surface de la flaque, comprise entre 0.05 et 45m
  checknumeric(dataset, 18, "cont", c(0.1, 40), possibleNA = TRUE) # colonne 18 : distance a la flaque la plus proche, de 0.1 a 40m
  for(i in 19:34) {checknumeric(dataset, i, "cont", c(0, 155), integer = TRUE, possibleNA = TRUE)} # colonnes 19:34 : nombre maximu d'individus
  if (checkInd(dataset)) { # ces tests se font seulement pour les fichiers individus
    checknumeric(dataset, 36, "cont", c(0.5, 36), possibleNA = TRUE) # Hauteur : col 36
    checknumeric(dataset, 37, "cont", c(1.5, 25), possibleNA = TRUE) # Largeur : col 37  
    checknumeric(dataset, 38, "cont", c(0.1, 15), possibleNA = TRUE) # Peristome : col 38   
    checknumeric(dataset, 43, "cont", c(0.001, 10), possibleNA = TRUE) # Masse : col 43  
  }
  if (numeric_error_count == 0) {cat("Aucune anomalie detectee dans les colonnes numeriques.\n")}
  else {
    if (numeric_error_count == 1) {cat("Une colonne numerique contient des erreurs. Veuillez verifier les avertissements ci-dessus.\n")}
    else {cat(numeric_error_count, "colonnes numeriques contiennent des erreurs. Veuillez verifier les avertissements ci-dessus.\n")}
  }
  
  if (checkInd(dataset)){ # Verification du type de fichier : cette etape 7 ne s'execute que sur un fichier de type individus
    cat("\n\nETAPE 7 : Verification des ratios entre mesures biometriques :\nRatios largeur/hauteur :\n")
    checkRatio(dataset, "largeur/hauteur")    # ratio larg/haut, option 1
    cat("Ratios peristome/largeur :\n")
    checkRatio(dataset, "peristome/largeur")    # ratio peri/larg, option 2
    cat("Verification de l'erreur de mesure de la masse :\n")
    for (i in 1:length(dataset$masse)) { # verifie l'erreur de la masse mesuree selon le ratio etabli par Andreas Prinzing et demande a l'utilisateur de compenser l'erreur de mesure au besoin
      if (!is.na(dataset$masse[i]) & !is.na(dataset$larg[i]) & !is.na(dataset$haut[i])) { # uniquement pour les lignes qui ne contiennent pas de NA
        if ((dataset$masse[i]/(((((dataset$larg[i]/2)^2)*pi)*dataset$haut[i])/3)) > 0.009) { # si la valeur du rapport calcule depasse le seuil de 0.009, la mesure necessite une correction
          count_warn <<- count_warn + 1
          warning(c("Ligne ", i, " pour la mesure de la masse : le rapport etant superieur a 0.009, il faut diviser la masse par 10."), call. = FALSE, noBreaks. = TRUE, immediate. = T)
        }
      }
    }
  }
  # Le temps du bilan : message de conclusion personnalise en fonction du nombre d'erreurs dans le fichier
  if (count_warn == 0) {cat("\nCe fichier est conforme a ce qui etait attendu. Il est pret a etre utilise pour le projet.\n")} # Message de conclusion en cas de succes a toutes les etapes (il ne servira pas souvent celui la)
  else {
    if (count_warn == 1) {cat("\nUne erreur a ete relevee pendant l'analyse. Veuillez proceder a sa correction...\n")} # si une seule erreur, message au singulier ! sinon pluriel... avec le nombre d'erreurs
    else {cat("\n", count_warn, " erreurs ont ete relevees pendant l'analyse. Veuillez proceder a leur correction...\n")} ## "Penvins, c'est zero ! Des erreurs, des erreurs, des erreurs, ça m'enerve !", librement inspire d'Attila le Hun (Kaamelott) ##
  }
  cat("\nLa verification du fichier", nm, "est terminee.\n") # THE END
  if (bilan) { # fin du sink et affichage du bilan si l'option a ete activee
    sink(type="output") # stop sinking des warnings
    sink(type="message") # stop sinking des messages
    file.show("tests_summary.txt") # affichage du fichier texte des messages et erreurs
    close(testSummary) # ferme le fichier txt
  }
}

#=====================================================#
# FONCTIONS
#=====================================================#
checkDataFrame <- function(mydata) { # ETAPE 1 : Verifier si mydata est bien de classe data.frame
  if (is.data.frame(mydata)) {cat("Le fichier", nm, "est bien de type data.frame.\n")} # teste si le fichier est bien un data.frame
  else { # sinon affiche une erreur et stoppe la fonction
    if (bilan) { # extinction du sink avant fermeture de la fonction
      cat("Erreur : le fichier n'est pas de type data.frame : ", nm, " est un ", class(mydata), ".\nVeuillez corriger le fichier avant de poursuivre...\n")
      sink(type = "message")
      file.show("tests_summary.txt")
    }
    stop("Le fichier n'est pas de type data.frame  : ", nm, " est un ", class(mydata), ".\nVeuillez corriger le fichier avant de poursuivre...\n") # fin prematuree de la fonction
  }
}

checkQuad <- function(mydata) { # ETAPE 2 : Verifie que le fichier mydata est bien de type Quad en comptant le nombre de colonnes
  if (ncol(mydata) == 34){ return(TRUE) } else{ return(FALSE) }} # un fichier quad doit contenir 34 colonnes
checkInd <- function(mydata) { # ETAPE 2 bis : Verifie que le fichier mydata est bien de type Ind en comptant le nombre de colonnes
  if (ncol(mydata) == 43){ return(TRUE) } else{ return(FALSE) }} # un fichier ind doit contenir 43 colonnes

checkColNames <- function(mydata) { # ETAPE 3 : Verifier que les noms de colonnes sont corrects
  nomsRef = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap", "sp", "haut", "larg", "peri", "pred", "coul", "text", "epizo", "masse") #Noms et ordres des colonnes attendus pour le fichier
  col = names(mydata) # "col" stocke les noms observes des colonnes du fichier
  verif = 0 # si verif est toujours egal a 0 a la fin du test, les noms des colonnes sont conformes
  for (i in 1:length(mydata)) { # pour chaque colonne du fichier
    if (col[i] != nomsRef[i]) { # compare le nom de la colonne au nom attendu selon la liste nomsRef (même indice i pour les deux listes)
      verif = verif + 1 # si il y a une erreur, ce n'est plus conforme, verif incrémente change d'etat -> FALSE (>1)
      warning("Le nom de la colonne ", i, " (", col[i],") n'est pas correct.", call. = FALSE, noBreaks. = TRUE, immediate. = TRUE)
      cat("Veuillez remplacer le nom de la colonne",i, "par'", nomsRef[i],"'.\n")} # affiche un message donnant la solution pour remplacer les noms non conformes
  }
    if (verif != 0) { # en fin de test, s'assure de la conformite du data frame, stoppe la fonction en cas de non conformite
      if (bilan) {
        cat("Erreur : Il y a ", verif," erreur(s) dans les noms de colonne. Veuillez corriger le fichier avant de poursuivre...\n")
        sink(type = "message")
        file.show("tests_summary.txt")
      }
    stop("Il y a ", verif," erreur(s) dans les noms de colonne. Veuillez corriger le fichier avant de poursuivre...\n", call. = FALSE) # message d'erreur indiquant qu'il y a une ou des erreurs et stoppant la fonction
  } else {cat("Tous les noms des colonnes sont corrects.\n")}
}

checkClass <- function(mydata) { # ETAPE 4 : Verifier que les colonnes sont bien de la classe attendue
  success = TRUE # si aucune erreur n'est relevee, success restera TRUE jusqu'a la fin de la fonction
  ClassExpect = c("factor", "factor", "factor", "integer", "factor", "integer", "integer", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "numeric", "numeric", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "integer", "factor", "numeric", "numeric", "numeric", "factor", "factor", "factor", "factor", "numeric") # Liste des classes attendues pour le fichier Ind ou quad, classees selon numero de colonne
  if (all(is.na(mydata$s.flaq))) {mydata$s.flaq = as.numeric(mydata$s.flaq)} # exception pour la colonne s.flaq si pas de flaque relevee -> la colonne ne contient que des NA, col Factor est convertie en col Numeric
  if (all(is.na(mydata$d.flaq))) {mydata$d.flaq = as.numeric(mydata$d.flaq)} # exception pour la colonne d.flaq de meme que ligne precedente
  for (i in 1:length(mydata)) { # parcourt les colonnes une par une pour verifier la classe
    if (class(mydata[,i]) != ClassExpect[i]) { # verifie si la classe de la colonne i est differente de la classe attendue
      success = FALSE # bascule success en FALSE pour declencher un stop() a la fin de la fonction
      warning("La classe de la colonne ", i, " (", names(mydata)[i], ") est ", class(mydata[,i]), " la ou ", ClassExpect[i], " etait attendu.\n", call. = FALSE, noBreaks. = TRUE, immediate. = TRUE)
    }
  }
  if (success) {cat("Toutes les classes des colonnes sont correctes.\n")}
  else {
    if (bilan) {
      cat("Erreur : certaines colonnes ne sont pas de la classe attendue.\nVeuillez corriger le fichier avant de poursuivre...\n")
      sink(type = "message")
      file.show("tests_summary.txt")
    }
    stop("Le fichier n'est pas conforme : certaines colonnes ne sont pas de la classe attendue.\nVeuillez corriger le fichier avant de poursuivre...\n", call. = FALSE)
  }
}

checkFactor <- function(mydata, nameCol) { # ETAPE 5 : Verifier dans les colonnes de classe "factor"
  ref = list(transect = c(paste("a", c(0, 10, 20, 30, 40, 50, 70, 90, 110), sep = ""), paste("b", c(0, 10, 20, 30, 40, 50, 70, 90, 110), sep = "")),
          resp = c("raphael.leprince", "nicolas.leroux.1", "audrey.fabarez", "morgane.milin", "vincent.carre", "justine.charlet-de-sauvage", "capucine.hemonnet-dal", "sasha.donnier", "heloise.villesseche", "julien.curassier", "laura.bellec", "nathan.viel", "thomas.brazier", "loic.menut", "eloise.couthouis", "thibaud.tournadre", "camille.pilisi"),
          date = c("20septembre2017", "21septembre2017"), mode = c("a", "b"),
          sp = c("Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap"),
          pred = c(NA, "oui", "non"), coul = c(NA, "clair", "sombre", "rayures"), text = c(NA, "lisse", "rugueux", "bosses"), epizo = c(NA, "oui", "non")) # liste de toutes les modalites attendues, classees par nom de colonne
  col = which(colnames(mydata) == nameCol) # recupere le numero de la colonne a partir de son nom passe en argument
  mods = levels(mydata[,col])  # recupere toutes les modalites de la colonne, a partir du numero de la colonne
  if (NA %in% mydata[,col]) {mods =c(mods, NA)} # ajoute les NA a la liste des modalites, si des NA sont detectes dans la colonne
  if (min(mods %in% ref[[nameCol]]) == 1) {cat("Toutes les modalites de la colonne ",  colnames(mydata)[col]," sont correctes.\n")} # si la comparaison entre mods et ref ne renvoie que des TRUE -> fin de la fonction
  else { # sinon, recherche de l'erreur
    for (i in 1:nrow(mydata[col])) { # pour chaque ligne de la colonne
      if (!(mydata[i, col] %in% ref[[nameCol]])) { # compare valeur observee et valeur attendue
        count_warn <<- count_warn + 1
        warning(c("Erreur dans la colonne ", colnames(mydata)[col], " a la ligne ", i, ".\nLa modalite ", as.character(mydata[i, col]), " ne correspond pas aux modalites attendues : ", paste(ref[[nameCol]], collapse = ", "), ".\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
      }
    }
    warning(c("Erreur dans la colonne ", colnames(mydata)[col], ", une ou des modalites sont fausses.\n"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
  }
}

checknumeric <- function(mydata, col, method, values, integer = FALSE, identical = FALSE, possibleNA = FALSE) { # ETAPE 6 : verifie que les valeurs d'une colonne de classe numeric ou integer correspondent a des valeurs precises attendues ou sont comprises dans l'intervalle attendu
  testok <- TRUE # variable de succes du test
  if (method == "disc") {	#Valeurs correspondant exactement aux valeurs de reference
    for (i in 1:nrow(mydata)) {
      if (is.na(mydata[i,col])) {
        if (!possibleNA) { #Tout NA doit etre signale si les NA ne sont pas autorises
          testok <- FALSE
          warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, " : valeur NA inattendue !"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
        } else {
          if (!mydata[i,col] %in% values) { #Si la valeur a la ligne i n'est pas comprise dans le vecteur values
            testok <- FALSE
            if (length(values) == 1) #Le message d'erreur s'adapte au nombre de valeurs
              warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ", la valeur  ", mydata[i,col], " ne correspond pas a la valeur attendue : ", values, "."), call. = FALSE, noBreaks. = TRUE, immediate. = T)
          } else {
            warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ", la valeur ", mydata[i,col], " ne correspond pas aux valeurs attendues : ", values, "."), call. = FALSE, noBreaks. = TRUE, immediate. = T)
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
          count_warn <<- count_warn + 1
          warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, " : valeur NA inattendue !"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
        }
      } else {
        if (integer & (as.integer(mydata[i,col]) != mydata[i,col])) { #Si des nombres entiers sont attendus, les nombres decimaux doivent etre signales
          testok <- FALSE
          count_warn <<- count_warn + 1
          warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ", la valeur  ", mydata[i,col], " devrait etre un nombre entier."), call. = FALSE, noBreaks. = TRUE, immediate. = T)
        }
        if (!(mydata[i,col]>=values[1] & mydata[i,col]<=values[2])) { #Si la valeur a la ligne i sort de l'intervalle attendu
          testok <- FALSE
          count_warn <<- count_warn + 1
          warning(c("Erreur dans la colonne ", names(mydata)[col], " a la ligne ", i, ", la valeur  ", mydata[i,col], " est en dehors de l'intervalle attendu : ", values[1], "-", values[2], "."), call. = FALSE, noBreaks. = TRUE, immediate. = T)
        }
      }
    }
  }
  if (identical) { #Valeurs identiques attendues sur la colonne
    if (sapply(mydata[col], function(x) length(unique(x))>1)) { #Cette commande examine tous les elements de la colonne et retourne un vecteur contenant une seule fois chaque valeur trouvee. S'il est de taille superieure a 1, toutes les valeurs ne sont pas identiques !
      testok <- FALSE
      count_warn <<- count_warn + 1
      warning(c("Erreur dans la colonne ", names(mydata)[col], " : toutes les valeurs devraient etre identiques."), call. = FALSE, noBreaks. = TRUE, immediate. = T)
    }
  }		
  numeric_error_count <<- numeric_error_count + !testok  #Si testok == FALSE, renvoie TRUE donc 1 : incremente le compte des erreurs d'1 colonne supplementaire qui contient une erreur
}

checkRatio <- function(mydata, type = "largeur/hauteur"){ # ETAPE 7 : Verification des ratios larg/haut et peri/larg dans un fichier Ind, avec 2 arguments : un data frame de type ind et le type de test => larg/haut (1) ou peri/larg (2)
  success = TRUE  # success restera TRUE uniquement si aucun warning n'est observe
  if (type == "largeur/hauteur") { # recupere toutes les variables pour une verification de larg/haut
    v = c(36,37) # un vecteur contenant (largeur,hauteur)
    MinMax = c(0.35,3)    # vecteur MinMax = ratio min et ratio max attendu
  } else {
    if (type == "peristome/largeur") { # pour une verification de peri/larg
      v = c(38,36) # un vecteur contenant (peristome,largeur)
      MinMax = c(0.1,1.5) # vecteur MinMax = ratio min et ratio max attendu
    }
  }
  for (i in 1:nrow(mydata)) { # comparaison des ratios ligne par ligne : parcourt chaque ligne des deux colonnes
    if (!is.na(mydata[i,v[1]]) & !is.na(mydata[i,v[2]])){   # verifie qu'aucune des deux valeurs n'est NA. Condition pour calculer le ratio
      ratio = mydata[i,v[1]]/mydata[i,v[2]] # calcule le ratio pour la ligne i
      if (ratio<MinMax[1] | ratio>MinMax[2]){ # si le ratio de la ligne i sort de l'intervalle
        success = FALSE # echec du test : la valeur sort de l'intervalle attendu
        count_warn <<- count_warn + 1
        warning(c("Ligne ", i, " pour l'espece ", as.character(mydata[i,35]), " : Le ratio ", type, " = ", round(ratio, digits = 2), " sort de l'intervalle attendu [", MinMax[1], ",", MinMax[2], "]"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
      }
    } else{ #dans le cas ou l'une des valeurs est NA
      success = FALSE
      count_warn <<- count_warn + 1
      warning(c("Ligne ", i, " pour le ratio ", type," sur l'espece ", as.character(mydata[i,35]), " : valeur NA !"), call. = FALSE, noBreaks. = TRUE, immediate. = T)
    }
  }
  if (success == TRUE) {cat("Tous les ratios ", type, " sont corrects.\n")} # si success est reste TRUE, message de reussite
}