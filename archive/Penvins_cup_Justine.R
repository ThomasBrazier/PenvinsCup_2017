			##########################
			### SCRIPT PENVINS CUP ###
			##########################



##Etape 1 : Vérifier que l'objet est de classe data.frame.
#########
#MANQUE : STOPPER LA FONCTION ET DONNER LE NOM DE L'OBJET
#fonction objects()


data=read.table(fichier,header=TRUE)
if (class(data)=="data.frame"){
	cat("Le fichier respecte le format correct, à savoir ",class(data),"\n")
}else{
	cat("La classe du fichier ",    ,"est incorrecte : ",class(data),"\nIl doit être de classe data.frame.\nVeuillez corriger pour poursuivre.\n")
 }

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

##Etape 2 : Compter les colonnes de l'objet à examiner.
#########
#FONCTIONNE. AJOUTER LES COMMENTAIRES 


if (length(data)==34 |length(data)==43){
	cat("Le fichier contient ",length(data)," colonnes.\nC'est correct.\n")
	if(length(data)==34){
		cat("Il doit s'agir d'un fichier quadrats de type 'quad'.\n")
	}else{
		cat("Il doit s'agir d'un fichier individus de type 'ind'.\n")
	 }
}else{
	stop("Ce fichier contient un nombre anormal de colonnes : ",length(data),"\nUn fichier de type quadrats doit contenir 34 colonnes et un fichier de type individus 43 colonnes.\nVeuillez corriger votre fichier avant de poursuivre.\n")
 }

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Etape 3 : Vérifier les intitulés de colonnes.
#########
#POURQUOI ERREUR DANS LA TOUTE DERNIERE ACCOLADE DE FERMETURE ?	
#ENLEVER L'ESPACE ENTRE LE MESSAGE D'ERREUR D'INTITULE ET LE POINT FINAL.
#ERREUR INTITULE ET ERREUR DE L'ARRET DU PROGRAMME : REDONDANT ?


if (length(data)==34){		#fichier de type quad
	cat("Vérification des intitulés de colonnes du fichier quadrats :\n")
	refquad<-c("transect","resp","date","coef","mode","d.chenal","d.mer","alt","surf","p.roc","p.moul","p.huit","p.bala","p.alg","p.encr","p.eau","s.flaq","d.flaq","Bitret","Gibcin","Gibsp.","Gibtum","Litlit","Litobt","Litrud","Litsax","Monlin","Nasinc","Naspyg","Nasret","Oceeri","Patsp.","Rispar","Thalap")
	if(identical(names(data),refquad)){
		cat("Les noms des colonnes sont corrects.\n")
	}else{for (i in 1:length(data)){
			if (names(data)[i]!=refquad[i]){
				cat("Erreur d'intitulé : la colonne",names(data)[i],"est à renommer par",refquad[i],".\n")
			}
		}
	 stop("Veuillez corriger votre fichier avant de poursuivre.")
	 }
}else{length(data)==43		#fichier de type ind
	cat("Vérification des intitulés de colonnes du fichier individus :\n")
	refind<-c("transect","resp","date","coef","mode","d.chenal","d.mer","alt","surf","p.roc","p.moul","p.huit","p.bala","p.alg","p.encr","p.eau","s.flaq","d.flaq","Bitret","Gibcin","Gibsp.","Gibtum","Litlit","Litobt","Litrud","Litsax","Monlin","Nasinc","Naspyg","Nasret","Oceeri","Patsp.","Rispar","Thalap","sp","haut","larg","peri","pred","coul","text","epizo","masse")
	if(identical(names(data),refind)){
		cat("Les noms des colonnes sont corrects.\n")
	}else{for (i in 1:length(data)){
			if (names(data)[i]!=refind[i]){
				cat("Erreur d'intitulé : la colonne",names(data)[i],"est à renommer par",refind[i],".\n")
			}
		}
	 stop("Veuillez corriger votre fichier avant de poursuivre.")
	 }
}else{		#Si le fichier n'est pas de type quad ou ind (i.e. ne contient pas 34 ou 43 colonnes).
	cat("Le fichier ne comporte pas un nombre correct de colonnes.\nRetournez à l'étape de vérification précédente.\n")

 }
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Etape 4 : Vérifier la classe des colonnes.
#########

if (length(data)==34){
	refquadclass<-c("factor","factor","factor","integer","factor","integer","integer","numeric","numeric","integer","integer","integer","integer","integer","integer","integer","numeric","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor")
	if(identical(class(data),refquadclass)){
		cat("Les classes des colonnes sont corrects.\n")
	}else{for (i in 1:length(data)){
			if (class(data[,i])!=refquadclass[i]){
				cat("Erreur de classe : la colonne",names(data)[i],"est de classe",class(data[,i])"au lieu de",refquadclass[i],".\n")
			}
		}
	 stop("Veuillez corriger votre fichier avant de poursuivre.")
	 }
}else{length(data)==43





for(i in 1:length(data)){
class(data[,i])


refquadclass<-c("factor","factor","factor","integer","factor","integer","integer","numeric","numeric","integer","integer","integer","integer","integer","integer","integer","numeric","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor")


refindclass<-c("factor","factor","factor","integer","factor","integer","integer","numeric","numeric","integer","integer","integer","integer","integer","integer","integer","numeric","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor","factor")





