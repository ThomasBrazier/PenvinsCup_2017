#V?rifier que les noms de colonnes sont corrects
  cat("V?rification des noms des colonnes \n")
  fichier=readline("Quel est le nom du fichier ? v?rifier?") 
  data=read.table(fichier,header=TRUE) #Charge les fichiers
  col = names(data) #"col" stocke les noms des colonnes du fichier
  nomsref = c("transect","resp","date","coef","mode","d.chenal","d.mer","alt","surf","p.roc","p.moul","p.huit","p.bala","p.encr","p.alg","p.eau","s.flaq","d.flaq","sp","haut","larg","peri","pred","coul","text","epizoaire","masse") #Noms et ordres des colonnes ok
  #nomsref stocke les noms attendus dans le fichier
  verif=TRUE #si verif=TRUE, les noms des colonnes sont conformes
  for (i in 1:length(data)){
    if (col[i] != nomsref[i]){
      verif=FALSE #si il y a une erreur, ce n'est plus conforme
      stop("il y a une/des erreur(s) dans les noms de colonnes")#message d'erreur indiquant qu'il y a une ou des erreurs
    }else{verif=TRUE} #si pas de diff?rences, les noms sont confrmes
  }
  if (verif){ #si les noms sont conformes
    cat("Les noms des colonnes sont conformes") #alors on affiche un message indiquant qu'ils le sont
  }else{
    for (i in 1:length(data)){
      if (col[i] != nomsref[i]){
        cat("remplacer le nom de la colonne n?",i, "par",nomsref[i],".\n")} #sinon on affiche un message permettant de remplacer les noms non conformes
    }
  }
cat("fin de la v?rification du nom des colonnes")
