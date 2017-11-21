ind <- read.table("data/a0indCORR2.txt", header = TRUE)
quad <- read.table("data/b30quadCORR2.txt", header = TRUE)
head(quad)
head(ind)
str(ind)
str(quad)

source("checkPenvins.R")
checkPenvins(ind)
warnings()

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



# AVEC fichier txt d'erreurs
checkPenvins(ind, bilan = T)

### Affichage du fichier d'erreur
file.show("tests_summary.txt")

