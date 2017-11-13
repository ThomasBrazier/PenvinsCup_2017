ind <- read.table("data/b30indCORR.txt", header = TRUE)
quad <- read.table("data/b30quadCORR.txt", header = TRUE)
head(quad)
head(ind)
str(ind)
str(quad)

source("checkPenvins.R")

# Voir dans le readme la gestion du sink() !!!!
# Cette fonction permet de recuperer tous les messages qui s'affichent normalement dans la console pour les ecrire dans un fichier texte
# Lancer le sink()
testSummary <- file("tests_summary.txt", open = "wt")
sink(file = testSummary, append = TRUE, type="output")
sink(file = testSummary, append = TRUE, type="message")

# Arreter le sink()
sink(type="output") # stop sinking
sink(type="message") # stop sinking
close(testSummary)

### Fonctions individuelles


# etape 1
checkDataFrame(ind)
checkDataFrame(quad)
# etape 2
checkQuad(quad)
checkInd(ind)
# etape 3
nomsRefInd = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap", "sp", "haut", "larg", "peri", "pred", "coul", "text", "epizo", "masse")
nomsRefQuad = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap")
checkColNames(ind, nomsRefInd)
checkColNames(quad, nomsRefQuad)

# etape 4
checkClass(ind)
checkClass(quad)

# etape 5
checkFactor(dataset$transect, c())
checkFactor(dataset$resp, c())
checkFactor(dataset$date, c("20septembre2017", "21septembre2017"))
checkFactor(dataset$mode, c("a", "b"))
checkFactor(dataset$sp, c())
checkFactor(dataset$pred, c(NA, "oui", "non"))
checkFactor(dataset$coul, c(NA, "clair", "sombre", "rayures"))
checkFactor(dataset$text, c(NA, "lisse", "rugueux", "bosses"))
checkFactor(dataset$epizo, c(NA, "oui", "non"))

# etape 6
checknumeric()

# etape 7
checkRatio(ind, 1)
checkRatio(ind, 2)


### Fonction finale
checkPenvins(quad)

### Affichage du fichier d'erreur
file.show("tests_summary.txt")
