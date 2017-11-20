# Projet Penvins

# Data assembler

# Working directory
setwd("~/Documents/GitHub/PenvinsCup_2017")
source("checkPenvins.R")

#####################
# Running a serie of checkPenvins over each file
for (f in 1:length(list.files(path = "~/Documents/Github/PenvinsCup_2017/data"))) {
  #checkPenvins(list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f])
}

######################
# Assembling Ind file
indTableAll = data.frame() # empty table for individuals
# Compile all .txt ind files into one data.frame
for (f in 1:length(list.files(path = "~/Documents/Github/PenvinsCup_2017/data"))) {
  cat("Proceeding to next file...\n")
  if (grepl("ind", list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f])) { # only if this is an ind file
    tmp = read.table(paste("data/",list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f], sep = ""), header = TRUE)
    cat("Processing", as.character(tmp[2,1]), "ind by", as.character(tmp[2,2]),".\n")
    if (ncol(tmp) == 43) { # verifying compatibility of the file by number of col
      colnames(tmp) = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap", "sp", "haut", "larg", "peri", "pred", "coul", "text", "epizo", "masse") # remove any name of column with correct name
      indTableAll = rbind(indTableAll, tmp) # add temp data.frame to global data.frame
    } else {
      warning("File ind", tmp[1], "by", tmp[2],"not added to global data frame : not the required number of columns.\n")
    }
  }
}
cat("All Ind files in One")


###################
# Assembling Quad file
quadTableAll = data.frame() # empty table for quadrats
# Compile all .txt quad files into one data.frame
for (f in 1:length(list.files(path = "~/Documents/Github/PenvinsCup_2017/data"))) {
  cat("Proceeding to next file...\n")
  if (grepl("ind", list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f])) { # only if this is an ind file
    tmp = read.table(paste("data/",list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f], sep = ""), header = TRUE)
    cat("Processing", as.character(tmp[2,1]), "ind by", as.character(tmp[2,2]),".\n")
    if (ncol(tmp) == 43) { # verifying compatibility of the file by number of col
      colnames(tmp) = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap") # remove any name of column with correct name
      quadTableAll = rbind(quadTableAll, tmp) # add temp data.frame to global data.frame
    } else {
      warning("File quad", tmp[1], "by", tmp[2],"not added to global data frame : not the required number of columns.\n")
    }
  }
}

cat("All Quad files in One")

####################
# Finishing

# Necessary verifications
str(indTableAll)
str(quadTableAll)
quadTableAll = quadTableAll[-35:-43] # remove any column of type Ind from the Quad file

# checkPenvins
checkPenvins(indTableAll)
checkPenvins(quadTableAll)


cat("End of the work")
