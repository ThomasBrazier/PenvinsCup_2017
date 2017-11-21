# Projet Penvins

# Data assembler

# Working directory
setwd("~/Documents/GitHub/PenvinsCup_2017")
source("checkPenvins.R")

#####################

# Running a serie of checkPenvins over each file
for (f in 1:length(list.files(path = "~/Documents/Github/PenvinsCup_2017/data"))) {
  tmp = read.table(paste("data/",list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f], sep = ""), header = TRUE)
  checkPenvins(tmp, bilan = FALSE)
  readline(prompt="Press [enter] to continue")
}

# Running only one checkPenvins at once
f = 6 # index of file
print(list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f])
tmp = read.table(paste("data/",list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f], sep = ""), header = TRUE)
checkPenvins(tmp, bilan = FALSE)

# Not working
# 3, 4

######################
# Assembling Ind file
ind = data.frame() # empty table for individuals
# Compile all .txt ind files into one data.frame
for (f in 1:length(list.files(path = "~/Documents/Github/PenvinsCup_2017/data"))) {
  cat("Proceeding to next file...\n")
  if (grepl("ind", list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f])) { # only if this is an ind file
    tmp = read.table(paste("data/",list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f], sep = ""), header = TRUE)
    cat("Processing", as.character(tmp[2,1]), "ind by", as.character(tmp[2,2]),".\n")
    if (ncol(tmp) == 43) { # verifying compatibility of the file by number of col
      colnames(tmp) = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap", "sp", "haut", "larg", "peri", "pred", "coul", "text", "epizo", "masse") # remove any name of column with correct name
      ind = rbind(ind, tmp) # add temp data.frame to global data.frame
    } else {
      warning("File ind", tmp[1], "by", tmp[2],"not added to global data frame : not the required number of columns.\n")
    }
  }
}
cat("All Ind files in One")


###################
# Assembling Quad file
quad = data.frame() # empty table for quadrats
# Compile all .txt quad files into one data.frame
for (f in 1:length(list.files(path = "~/Documents/Github/PenvinsCup_2017/data"))) {
  cat("Proceeding to next file...\n")
  if (grepl("ind", list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f])) { # only if this is an ind file
    tmp = read.table(paste("data/",list.files(path = "~/Documents/Github/PenvinsCup_2017/data")[f], sep = ""), header = TRUE)
    cat("Processing", as.character(tmp[2,1]), "ind by", as.character(tmp[2,2]),".\n")
    if (ncol(tmp) == 43) { # verifying compatibility of the file by number of col
      colnames(tmp) = c("transect", "resp", "date", "coef", "mode", "d.chenal", "d.mer", "alt", "surf", "p.roc", "p.moul", "p.huit", "p.bala", "p.alg", "p.encr", "p.eau", "s.flaq", "d.flaq", "Bitret", "Gibcin", "Gibsp.", "Gibtum", "Litlit", "Litobt", "Litrud", "Litsax", "Monlin", "Nasinc", "Naspyg", "Nasret", "Oceeri", "Patsp.", "Rispar", "Thalap") # remove any name of column with correct name
      quad = rbind(quad, tmp) # add temp data.frame to global data.frame
    } else {
      warning("File quad", tmp[1], "by", tmp[2],"not added to global data frame : not the required number of columns.\n")
    }
  }
}

cat("All Quad files in One")

####################
# Finishing

# Necessary verifications
str(ind)
str(quad)
quad = quad[-35:-43] # remove any column of type Ind from the Quad file

# checkPenvins
checkPenvins(ind)
checkPenvins(quad)


# ratios :
# liste de tous les ratios
# sort
# on elimine (coupe) quand les ratios décollent d'un coup vers valeur extreme (plot ?)
# quantile ?
for (i in 1:length(ind$haut)) {
  ind$ratioLH[i] = ind$larg[i]/ind$haut[i]
}
for (i in 1:length(ind$larg)) {
  ind$ratioPL[i] = ind$peri[i]/ind$larg[i]
}
str(ind)
plot(sort(ind$ratioLH)[4209:4409])
plot(sort(ind$ratioLH)[1:200])
plot(sort(ind$ratioPL)[1:200])
plot(sort(ind$ratioPL)[4209:4409])

# intervalles
# coef
# 97 ou 99

# d.chenal entre 0 et max = 110
plot(sort(ind$d.chenal))
plot(sort(ind$d.chenal)[4009:4409])


# d.mer entre 0 et max = 96
plot(sort(ind$d.mer))
plot(sort(ind$d.mer)[4209:4409])

# alt entre 0 et max = 7m ????
plot(sort(ind$alt))

# surf : surf min = 0.2 et max = 0.6 cad 3 fois la surface d'un quadrat
plot(sort(ind$surf))

# denombrement : nb max = 150
plot(sort(ind$Litlit))
sort(ind$Litlit)[4200:4409]

# s.flaq min = 0.1 (demi quadrat) et max = 10 m2
plot(sort(ind$s.flaq))

# d.flaq min = 0.1 (juste a coté) et max = 10m (2 x rayon d'action de chaque placette) ??
plot(sort(ind$d.flaq))

# haut min = 1.5 et max = 36
plot(sort(ind$haut))
plot(sort(ind$haut)[1:200])
plot(sort(ind$haut)[4209:4409])

# larg min = 1.5 et max = 21
plot(sort(ind$larg))
plot(sort(ind$larg)[1:200])
plot(sort(ind$larg)[4209:4409])

# peri min = 0.1 (valeur de precision min du pied a coulisse) et max = 15
plot(sort(ind$peri))
plot(sort(ind$peri)[1:200])
sort(ind$peri)[1:200]
abline(h=0.01)
plot(sort(ind$peri)[4009:4409])
sort(ind$peri)[4009:4409]

# masse min = 0.001 et max = 10
plot(sort(ind$masse))
plot(sort(ind$masse)[1:200])
sort(ind$masse)[1:200]
plot(sort(ind$masse)[4009:4409])
sort(ind$masse)[4009:4409]

cat("End of the work")
