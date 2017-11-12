ind <- read.table("data/b30indCORR.txt", header = TRUE)
quad <- read.table("data/b30quadCORR.txt", header = TRUE)
head(quad)
head(ind)
str(ind)
str(quad)

source("checkPenvins.R")

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
nomsRefQuad = c("transect","resp","date","coef","mode","d.chenal","d.mer","alt","surf","p.roc","p.moul","p.huit","p.bala","p.alg","p.enc","p.eau","s.flaq","d.flaq") #Noms et ordres des colonnes attendus pour le fichier quad
nomsRefInd = c("transect","resp","date","coef","mode","d.chenal","d.mer","alt","surf","p.roc","p.moul","p.huit","p.bala","p.alg","p.enc","p.eau","s.flaq","d.flaq") #Noms et ordres des colonnes attendus pour le fichier ind
checkColNames(ind, nomsRefInd)
checkColNames(quad, nomsRefQuad)

# etape 7
checkRatio(ind, 1)
checkRatio(ind, 2)


### Fonction finale
checkPenvins(quad)

### Affichage du fichier d'erreur
file.show("tests_summary.txt")
