

getwd()
setwd("Put dir path here")  # set dir

library(tidyverse)
library(limma)
library(dplyr)
library(mixOmics)
library(igraph)

#####################  Data cleaning for statistical analysis #######################

#Short Chain Fatty Acid data
SCFA <- data.frame(read.delim(file="SCFA_file.txt", sep="\t"))
rownames(SCFA) <- SCFA[,1]
SCFA <- SCFA[,2:ncol(SCFA)]

#Plasma lipid data
PLipid <- data.frame(read.delim(file="PlasmaLipids_file.txt", sep="\t"))
rownames(PLipid) <- PLipid[,1]
PLipid <- PLipid[,2:ncol(PLipid)]

#descripted behaviour data
Behav <- data.frame(read.delim(file="Behaviour_file.txt", sep="\t"))
rownames(Behav) <- Behav[,1]
Behav <- Behav[,2:ncol(Behav)]

###### nRNA gene expression data for 5 brain regions  ############
NS_R1 <- data.frame(read.delim(file="NanoString_Gene_Epression_Region_1.txt", sep="\t"))
rownames(NS_R1) <- NS_R1[,1]
NS_R1 <- NS_R1[,2:ncol(NS_R1)]

NS_R2 <- data.frame(read.delim(file="TNanoString_Gene_Epression_Region_2.txt", sep="\t"))
rownames(NS_R2) <- NS_R2[,1]
NS_R2 <- NS_R2[,2:ncol(NS_R2)]

NS_R3 <- data.frame(read.delim(file="NanoString_Gene_Epression_Region_3.txt", sep="\t"))
rownames(NS_R3) <- NS_R3[,1]
NS_R3 <- NS_R3[,2:ncol(NS_R3)]

NS_R4 <- data.frame(read.delim(file="NanoString_Gene_Epression_Region_4.txt", sep="\t"))
rownames(NS_R4) <- NS_R4[,1]
NS_R4 <- NS_R4[,2:ncol(NS_R4)]

NS_R5 <- data.frame(read.delim(file="NanoString_Gene_Epression_Region_5.txt", sep="\t"))
rownames(NS_R5) <- NS_R5[,1]
NS_R5 <- NS_R5[,2:ncol(NS_R5)]

######## Brain Lipid metabolomics data from 5 regions  ##########
Lipid_R1 <- data.frame(read.delim(file="Region_1_Lipid.txt", sep="\t"))
rownames(Lipid_R1) <- Lipid_R1[,1]
Lipid_R1 <- Lipid_R1[,2:ncol(Lipid_R1)]

Lipid_R2 <- data.frame(read.delim(file="Region_2_Lipid.txt", sep="\t"))
rownames(Lipid_R2) <- Lipid_R2[,1]
Lipid_R2 <- Lipid_R2[,2:ncol(Lipid_R2)]

Lipid_R3 <- data.frame(read.delim(file="Region_3_Lipid.txt", sep="\t"))
rownames(Lipid_R3) <- Lipid_R3[,1]
Lipid_R3 <- Lipid_R3[,2:ncol(Lipid_R3)]

Lipid_R4 <- data.frame(read.delim(file="Region_4_Lipid.txt", sep="\t"))
rownames(Lipid_R4) <- Lipid_R4[,1]
Lipid_R4 <- Lipid_R4[,2:ncol(Lipid_R4)]

Lipid_R5 <- data.frame(read.delim(file="Region_5_Lipid.txt", sep="\t"))
rownames(Lipid_R5) <- Lipid_R5[,1]
Lipid_R5 <- Lipid_R5[,2:ncol(Lipid_R5)]

# Ceacal microbiotia DNA data, indentified at Genus level
L6 <- data.frame(read.delim(file="ncbi_genus.txt", sep="\t"))
rownames(L6) <- L6[,1]
L6 <- L6[,2:ncol(L6)]
colnames(L6) <- strsplit2(colnames(L6),"_P")[,1]

L6.micro <- L6
L6.names <- strsplit2(rownames(L6),";")

i <- 1
for(i in 1:ncol(L6.names)) {
  
  L6.names[,ncol(L6.names)] <- ifelse(L6.names[,ncol(L6.names)] == "", L6.names[,ncol(L6.names)-i],L6.names[,ncol(L6.names)]) 
}
head(L6.names)


rownames(L6.micro) <- paste(L6.names[,4], L6.names[,ncol(L6.names)], sep="; ")
L6.micro <- L6.micro[rownames(L6.micro) != "; NCBI",]
L6.micro <- L6.micro[rownames(L6.micro) != "; cellular organisms",]
L6.micro <- L6.micro[rownames(L6.micro) != "; Bacteria",]
L6.micro <- L6.micro[rownames(L6.micro) != "; Not assigned",]
L6.micro <- L6.micro[rownames(L6.micro) != "environmental samples <bacteria,superkingdom Bacteria>; environmental samples <bacteria,superkingdom Bacteria>",]

L6.micro <- prop.table(as.matrix(L6.micro),2)
dim(L6.micro)

# removel of sample animal data to match all datasets 
### L6 Taxa
L6.micro <- L6.micro[, colnames(L6) != "sub_01"]  # subject number  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_11"]  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_15"]  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_31"]  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_55"]  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_57"]  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_88"]  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_93"]  
L6.micro <- L6.micro[, colnames(L6.micro) != "sub_97"]  
dim(L6.micro)

### Brain NS_R1 
NS_R1.micro <- NS_R1
dim(NS_R1.micro)
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1) != "sub_01"]    
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_11"]
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_15"]  
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_31"]  
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_55"]  
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_57"]  
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_88"] 
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_93"]
NS_R1.micro <- NS_R1.micro[, colnames(NS_R1.micro) != "sub_97"]  
dim(NS_R1.micro)

### Brain NS_R2
NS_R2.micro <- NS_R2
dim(NS_R2.micro)
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2) != "sub_01"]    
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_11"]
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_15"]  
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_31"]  
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_55"]  
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_57"]  
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_88"] 
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_93"]
NS_R2.micro <- NS_R2.micro[, colnames(NS_R2.micro) != "sub_97"]  
dim(NS_R2.micro)

### Brain NS_R3 
NS_R3.micro <- NS_R3
dim(NS_R3.micro)
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3) != "sub_01"]    
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_11"]
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_15"]  
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_31"]  
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_55"]  
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_57"]  
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_88"] 
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_93"]
NS_R3.micro <- NS_R3.micro[, colnames(NS_R3.micro) != "sub_97"]  
dim(NS_R3.micro)

### Brain NS_R4
NS_R4.micro <- NS_R4
dim(NS_R4.micro)
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4) != "sub_01"]    
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_11"]
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_15"]  
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_31"]  
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_55"]  
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_57"]  
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_88"] 
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_93"]
NS_R4.micro <- NS_R4.micro[, colnames(NS_R4.micro) != "sub_97"]  
dim(NS_R4.micro)

### Brain NS_R5
NS_R5.micro <- NS_R5
dim(NS_R5.micro)
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5) != "sub_01"]    
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_11"]
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_15"]  
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_31"]  
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_55"]  
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_57"]  
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_88"] 
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_93"]
NS_R5.micro <- NS_R5.micro[, colnames(NS_R5.micro) != "sub_97"]  
dim(NS_R5.micro)

### Brain Lipid_R1 
Lipid_R1.micro <- Lipid_R1
dim(Lipid_R1.micro)
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1) != "sub_01"]    
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_11"]
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_15"]  
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_31"]  
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_55"]  
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_57"]  
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_88"] 
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_93"]
Lipid_R1.micro <- Lipid_R1.micro[, colnames(Lipid_R1.micro) != "sub_97"]  
dim(Lipid_R1.micro)

### Brain Lipid_R2
Lipid_R2.micro <- Lipid_R2
dim(Lipid_R2.micro)
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2) != "sub_01"]    
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_11"]
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_15"]  
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_31"]  
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_55"]  
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_57"]  
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_88"] 
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_93"]
Lipid_R2.micro <- Lipid_R2.micro[, colnames(Lipid_R2.micro) != "sub_97"]  
dim(Lipid_R2.micro)

### Brain Lipid_R3 
Lipid_R3.micro <- Lipid_R3
dim(Lipid_R3.micro)
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3) != "sub_01"]    
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_11"]
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_15"]  
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_31"]  
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_55"]  
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_57"]  
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_88"] 
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_93"]
Lipid_R3.micro <- Lipid_R3.micro[, colnames(Lipid_R3.micro) != "sub_97"]  
dim(Lipid_R3.micro)

### Brain Lipid_R4
Lipid_R4.micro <- Lipid_R4
dim(Lipid_R4.micro)
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4) != "sub_01"]    
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_11"]
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_15"]  
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_31"]  
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_55"]  
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_57"]  
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_88"] 
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_93"]
Lipid_R4.micro <- Lipid_R4.micro[, colnames(Lipid_R4.micro) != "sub_97"]  
dim(Lipid_R4.micro)

### Brain Lipid_R5
Lipid_R5.micro <- Lipid_R5
dim(Lipid_R5.micro)
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5) != "sub_01"]    
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_11"]
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_15"]  
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_31"]  
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_55"]  
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_57"]  
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_88"] 
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_93"]
Lipid_R5.micro <- Lipid_R5.micro[, colnames(Lipid_R5.micro) != "sub_97"]  
dim(Lipid_R5.micro)

# Caecum SCFA 
SCFA.micro <- SCFA
dim(SCFA.micro)
SCFA.micro <- SCFA.micro[, colnames(SCFA) != "sub_01"]   
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_11"]  
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_15"]
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_31"]  
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_55"]  
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_57"]  
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_88"]  
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_93"]
SCFA.micro <- SCFA.micro[, colnames(SCFA.micro) != "sub_97"]  
dim(SCFA.micro)

### Plasma Lipids 
PLipid.micro <- PLipid
dim(PLipid.micro)
PLipid.micro <- PLipid.micro[, colnames(PLipid) != "sub_01"]
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_11"]  
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_15"]
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_31"]  
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_55"]  
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_57"]  
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_88"]  
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_93"]
PLipid.micro <- PLipid.micro[, colnames(PLipid.micro) != "sub_97"]  
dim(PLipid.micro)

### Behaviour
Behav.micro <- Behav
dim(Behav.micro)
Behav.micro <- Behav.micro[, colnames(Behav) != "sub_01"]
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_11"]  
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_15"]
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_31"]  
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_55"]  
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_57"]  
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_88"]  
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_93"]
Behav.micro <- Behav.micro[, colnames(Behav.micro) != "sub_97"]  
dim(Behav.micro)

#########################

mapping <- data.frame(read.delim(file="_CompAna_mapping.txt", sep="\t",stringsAsFactors=F))
mapping <- mapping[order(mapping[,1]),]
head(mapping)
rownames(mapping) <- mapping$RatID

### mapping data 
mapping.micro <- mapping
dim(mapping.micro)
mapping.micro <- mapping.micro[rownames(mapping) != "sub_01",]  
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_11",]  
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_15",]
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_31",]  
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_31",] 
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_57",] 
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_88",]  
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_93",]
mapping.micro <- mapping.micro[rownames(mapping.micro) != "sub_97",]  
dim(mapping.micro)

rownames(mapping.micro)
colnames(Behav.micro)
colnames(NS_R1.micro)
colnames(NS_R2.micro)
colnames(NS_R3.micro)
colnames(NS_R4.micro)
colnames(NS_R5.micro)
colnames(Lipid_R1.micro)
colnames(Lipid_R2.micro)
colnames(Lipid_R3.micro)
colnames(Lipid_R4.micro)
colnames(Lipid_R5.micro)
colnames(SCFA.micro)
colnames(PLipid.micro)
colnames(L6.micro)

table(mapping.micro$Treatment)

###############         Statistical analysis ####################

colnames(L6.micro) <- mapping.micro$RatID
colnames(L6.micro)
L6.micro_T <- t(L6.micro)
L6.micro_T <- L6.micro_T[,colSums(L6.micro_T) > 0]
dim(L6.micro_T)
L6.log <-logratio.transfo(L6.micro_T, logratio = "CLR", offset = 1)
X_L6 <- L6.log
dim(X_L6)
Y_L6 <- X_L6

Behav.micro <- Behav.micro + 1
Behav_log <-logratio.transfo(Behav.micro, logratio = "CLR", offset = 1)
Y_Behav <- t(Behav_log)
dim(Y_Behav)
X_Behav <- Y_Behav

NS_R1_log <-logratio.transfo(NS_R1.micro, logratio = "CLR", offset = 1)
Y_NS_R1 <- t(NS_R1_log)
dim(Y_NS_R1)
X_NS_R1 <- Y_NS_R1

NS_R2_log <-logratio.transfo(NS_R2.micro, logratio = "CLR", offset = 1)
Y_NS_R2 <- t(NS_R2_log)
dim(Y_NS_R2)
X_NS_R2 <- Y_NS_R2

NS_R3_log <-logratio.transfo(NS_R3.micro, logratio = "CLR", offset = 1)
Y_NS_R3 <- t(NS_R3_log)
dim(Y_NS_R3)
X_NS_R3 <- Y_NS_R3

NS_R4_log <-logratio.transfo(NS_R4.micro, logratio = "CLR", offset = 1)
Y_NS_R4 <- t(NS_R4_log)
X_NS_R4 <- Y_NS_R4

NS_R5_log <-logratio.transfo(NS_R5.micro, logratio = "CLR", offset = 1)
Y_NS_R5 <- t(NS_R5_log)
dim(Y_NS_R5)
X_NS_R5 <- Y_NS_R5

Lipid_R1_log <-logratio.transfo(Lipid_R1.micro, logratio = "CLR", offset = 1)
Y_Lipid_R1 <- t(Lipid_R1_log)
dim(Y_Lipid_R1)
X_Lipid_R1 <- Y_Lipid_R1

Lipid_R2_log <-logratio.transfo(Lipid_R2.micro, logratio = "CLR", offset = 1)
Y_Lipid_R2 <- t(Lipid_R2_log)
dim(Y_Lipid_R2)
X_Lipid_R2 <- Y_Lipid_R2

Lipid_R3_log <-logratio.transfo(Lipid_R3.micro, logratio = "CLR", offset = 1)
Y_Lipid_R3 <- t(Lipid_R3_log)
dim(Y_Lipid_R3)
X_Lipid_R3 <- Y_Lipid_R3

Lipid_R4_log <-logratio.transfo(Lipid_R4.micro, logratio = "CLR", offset = 1)
Y_Lipid_R4 <- t(Lipid_R4_log)
dim(Y_Lipid_R4)
X_Lipid_R4 <- Y_Lipid_R4

Lipid_R5_log <-logratio.transfo(Lipid_R5.micro, logratio = "CLR", offset = 1)
Y_Lipid_R5 <- t(Lipid_R5_log)
dim(Y_Lipid_R5)
X_Lipid_R5 <- Y_Lipid_R5

SCFA_log <-logratio.transfo(SCFA.micro, logratio = "CLR", offset = 1)
Y_SCFA <- t(SCFA_log)
dim(Y_SCFA)
X_SCFA <- Y_SCFA

PLipid_log <-logratio.transfo(PLipid.micro, logratio = "CLR", offset = 1)
Y_PLipid <- t(PLipid_log)
dim(Y_PLipid)
X_PLipid <- Y_PLipid

# PCA
prcomp.L6 <- prcomp(X_L6, center = TRUE, scale = TRUE)
summary(prcomp.L6)
eigs <- prcomp.L6$sdev^2
eigs / sum(eigs)
L6var.explained <- eigs / sum(eigs)
L6var.explained <- L6var.explained*100
plot(L6var.explained, main = "PCA Caecum microbiota (Level 6:Genus)")

prcomp.SCFA <- prcomp(X_SCFA, center = TRUE, scale = TRUE)
summary(prcomp.SCFA)
eigs <- prcomp.SCFA$sdev^2
eigs / sum(eigs)
SCFAvar.explained <- eigs / sum(eigs)
SCFAvar.explained <- SCFAvar.explained*100
plot(SCFAvar.explained, main = "PCA Caecum SCFA")

prcomp.PLipid <- prcomp(X_PLipid, center = TRUE, scale = TRUE)
summary(prcomp.PLipid)
eigs <- prcomp.PLipid$sdev^2
eigs / sum(eigs)
PLvar.explained <- eigs / sum(eigs)
PLvar.explained <- PLvar.explained*100
plot(PLvar.explained, main = "PCA Plasma Lipids")

prcomp.NS_R1 <- prcomp(X_NS_R1, center = TRUE, scale = TRUE)
summary(prcomp.NS_R1)
eigs <- prcomp.NS_R1$sdev^2
eigs / sum(eigs)
NS_R1var.explained <- eigs / sum(eigs)
NS_R1var.explained <- NS_R1var.explained*100
plot(NS_R1var.explained, main = "PCA NS Geneset for Region 1")

prcomp.NS_R2 <- prcomp(X_NS_R2, center = TRUE, scale = TRUE)
summary(prcomp.NS_R2)
eigs <- prcomp.NS_R2$sdev^2
eigs / sum(eigs)
NS_R2var.explained <- eigs / sum(eigs)
NS_R2var.explained <- NS_R2var.explained*100
plot(NS_R2var.explained, main = "PCA NS Geneset for Region 2")

prcomp.NS_R3 <- prcomp(X_NS_R3, center = TRUE, scale = TRUE)
summary(prcomp.NS_R3)
eigs <- prcomp.NS_R3$sdev^2
eigs / sum(eigs)
NS_R3var.explained <- eigs / sum(eigs)
NS_R3var.explained <- NS_R3var.explained*100
plot(NS_R3var.explained, main = "PCA NS Geneset for Region 3")

prcomp.NS_R4 <- prcomp(X_NS_R4, center = TRUE, scale = TRUE)
summary(prcomp.NS_R4)
eigs <- prcomp.NS_R4$sdev^2
eigs / sum(eigs)
NS_R4var.explained <- eigs / sum(eigs)
NS_R4var.explained <- NS_R4var.explained*100
plot(NS_R4var.explained, main = "PCA NS Geneset for Region 4")

prcomp.NS_R5 <- prcomp(X_NS_R5, center = TRUE, scale = TRUE)
summary(prcomp.NS_R5)
eigs <- prcomp.NS_R5$sdev^2
eigs / sum(eigs)
NS_R5var.explained <- eigs / sum(eigs)
NS_R5var.explained <- NS_R5var.explained*100
plot(NS_R5var.explained, main = "PCA NS Geneset for Region 5")

prcomp.Lipid_R1 <- prcomp(X_Lipid_R1, center = TRUE, scale = TRUE)
summary(prcomp.Lipid_R1)
eigs <- prcomp.Lipid_R1$sdev^2
eigs / sum(eigs)
Lipid_R1var.explained <- eigs / sum(eigs)
Lipid_R1var.explained <- Lipid_R1var.explained*100
plot(Lipid_R1var.explained, main = "PCA Lipids for Region 1")

prcomp.Lipid_R2 <- prcomp(X_Lipid_R2, center = TRUE, scale = TRUE)
summary(prcomp.Lipid_R2)
eigs <- prcomp.Lipid_R2$sdev^2
eigs / sum(eigs)
Lipid_R2var.explained <- eigs / sum(eigs)
Lipid_R2var.explained <- Lipid_R2var.explained*100
plot(Lipid_R2var.explained, main = "PCA Lipids for Region 2")

prcomp.Lipid_R3 <- prcomp(X_Lipid_R3, center = TRUE, scale = TRUE)
summary(prcomp.Lipid_R3)
eigs <- prcomp.Lipid_R3$sdev^2
eigs / sum(eigs)
Lipid_R3var.explained <- eigs / sum(eigs)
Lipid_R3var.explained <- Lipid_R3var.explained*100
plot(Lipid_R3var.explained, main = "PCA Lipids for Region 3")

prcomp.Lipid_R4 <- prcomp(X_Lipid_R4, center = TRUE, scale = TRUE)
summary(prcomp.Lipid_R4)
eigs <- prcomp.Lipid_R4$sdev^2
eigs / sum(eigs)
Lipid_R4var.explained <- eigs / sum(eigs)
Lipid_R4var.explained <- Lipid_R4var.explained*100
plot(Lipid_R4var.explained, main = "PCA Lipids for Region 4")

prcomp.Lipid_R5 <- prcomp(X_Lipid_R5, center = TRUE, scale = TRUE)
summary(prcomp.Lipid_R5)
eigs <- prcomp.Lipid_R5$sdev^2
eigs / sum(eigs)
Lipid_R5var.explained <- eigs / sum(eigs)
Lipid_R5var.explained <- Lipid_R5var.explained*100
plot(Lipid_R5var.explained, main = "PCA Lipids for Region 5")

prcomp.Behav <- prcomp(X_Behav, center = TRUE, scale = TRUE)
summary(prcomp.Behav)
eigs <- prcomp.Behav$sdev^2
eigs / sum(eigs)
Behvar.explained <- eigs / sum(eigs)
Behvar.explained <- Behvar.explained*100
plot(Behvar.explained, main = "PCA Behaviour for OFT, EPM & NOR(3 days)")

pca.par <- par(mfrow = c(2,3))
plot(Behvar.explained, main = "Behaviour")
plot(NS_R1var.explained, main = "NS Geneset for Region 1") 
plot(NS_R2var.explained, main = "NS Geneset for Region 2") 
plot(NS_R3var.explained, main = "NS Geneset for Region 3") 
plot(NS_R4var.explained, main = "NS Geneset for Region 4") 
plot(NS_R5var.explained, main = "NS Geneset for Region 5") 
pca.par <- par(mfrow = c(2,3))
plot(Lipid_R1var.explained, main = "Lipids for Region 1") 
plot(Lipid_R2var.explained, main = "Lipids for Region 2") 
plot(Lipid_R3var.explained, main = "Lipids for Region 3") 
plot(Lipid_R4var.explained, main = "Lipids for Region 4") 
plot(Lipid_R5var.explained, main = "Lipids for Region 5") 

plot(SCFAvar.explained, main = "Caecum SCFA") 
plot(PLvar.explained, main = "Plasma Lipids")
plot(L6var.explained, main = "Caecum microbiota (Level 6:Genus)")

# SPLS
#note: regression does not survive the threshold: 0.5 
#NS_R1_Behav.spls <- spls(X_NS_R1, Y_Behav, ncomp = 5, mode = 'regression', scale=F, near.zero.var = TRUE) 
NS_R1_Behav.spls <- spls(X_NS_R1, Y_Behav, ncomp = 5, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(NS_R1_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 1 ", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
cim(NS_R1_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 1 ", margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
NS_R1_Behav.result <- network(NS_R1_Behav.spls, cutoff = 0.5,
                           color.node = c("mistyrose", "lightcyan"),
                           shape.node = c("rectangle", "circle"),
                           color.edge = c("blue", "red"),
                           lty.edge = "solid", lwd.edge = 1,
                           show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(NS_R1_Behav.result$gR, file = "NS_R1_Behav_network.gml", format = "gml")
write.graph(NS_R1_Behav.result$gR, file = "NS_R1_Behav_network0.5.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 nor does Canonical
#NS_R2_Behav.spls <- spls(X_NS_R2, Y_Behav, ncomp = 5, mode = 'regression', scale=F, near.zero.var = TRUE) 
#NS_R2_Behav.spls <- spls(X_NS_R2, Y_Behav, ncomp = 5, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(NS_R2_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 2", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
cim(NS_R2_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 2", margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
NS_R2_Behav.result <- network(NS_R2_Behav.spls, cutoff = 0.5,
                           color.node = c("mistyrose", "lightcyan"),
                           shape.node = c("rectangle", "circle"),
                           color.edge = c("blue", "red"),
                           lty.edge = "solid", lwd.edge = 1,
                           show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(NS_R2_Behav.result$gR, file = "NS_R2_Behav_network.gml", format = "gml")
write.graph(NS_R2_Behav.result$gR, file = "NS_R2_Behav_network.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 
#NS_R3_Behav.spls <- spls(X_NS_R3, Y_Behav, ncomp = 5, mode = 'regression', scale=F, near.zero.var = TRUE) 
NS_R3_Behav.spls <- spls(X_NS_R3, Y_Behav, ncomp = 5, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(NS_R3_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 3", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
cim(NS_R3_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 3", margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
NS_R3_Behav.result <- network(NS_R3_Behav.spls, cutoff = 0.5,
                           color.node = c("mistyrose", "lightcyan"),
                           shape.node = c("rectangle", "circle"),
                           color.edge = c("blue", "red"),
                           lty.edge = "solid", lwd.edge = 1,
                           show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(NS_R3_Behav.result$gR, file = "NS_R3_Behav_network.gml", format = "gml")
write.graph(NS_R3_Behav.result$gR, file = "NS_R3_Behav_network0.5.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 
#NS_R4_Behav.spls <- spls(X_NS_R4, Y_Behav, ncomp = 5, mode = 'regression', scale=F, near.zero.var = TRUE) 
NS_R4_Behav.spls <- spls(X_NS_R4, Y_Behav, ncomp = 5, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(NS_R4_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 4", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
cim(NS_R4_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 4", margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
NS_R4_Behav.result <- network(NS_R4_Behav.spls, cutoff = 0.5,
                           color.node = c("mistyrose", "lightcyan"),
                           shape.node = c("rectangle", "circle"),
                           color.edge = c("blue", "red"),
                           lty.edge = "solid", lwd.edge = 1,
                           show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(NS_R4_Behav.result$gR, file = "NS_R4_Behav_network.gml", format = "gml")
write.graph(NS_R4_Behav.result$gR, file = "NS_R4_Behav_network0.5.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 nor does Canonical
#NS_R5_Behav.spls <- spls(X_NS_R5, Y_Behav, ncomp = 4, mode = 'regression', scale=F, near.zero.var = TRUE) 
NS_R5_Behav.spls <- spls(X_NS_R5, Y_Behav, ncomp = 4, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(NS_R5_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 5", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
cim(NS_R5_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "NanoString Geneset for Region 5", margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
NS_R5_Behav.result <- network(NS_R5_Behav.spls, cutoff = 0.7,
                           color.node = c("mistyrose", "lightcyan"),
                           shape.node = c("rectangle", "circle"),
                           color.edge = c("blue", "red"),
                           lty.edge = "solid", lwd.edge = 1,
                           show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(NS_R5_Behav.result$gR, file = "NS_R5_Behav_network.gml", format = "gml")
write.graph(NS_R5_Behav.result$gR, file = "NS_R5_Behav_network0.5.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 nor does Canonical
#Lipid_R1_Behav.spls <- spls(X_Lipid_R1, Y_Behav, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
#Lipid_R1_Behav.spls <- spls(X_Lipid_R1, Y_Behav, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(Lipid_R1_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "R1gdala Lipids", margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
Lipid_R1_Behav.result <- network(Lipid_R1_Behav.spls, cutoff = 0.6,
                               color.node = c("mistyrose", "lightcyan"),
                               shape.node = c("rectangle", "circle"),
                               color.edge = c("blue", "red"),
                               lty.edge = "solid", lwd.edge = 1,
                               show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(Lipid_R1_Behav.result$gR, file = "Lipid_R1_Behav_network.gml", format = "gml")
write.graph(Lipid_R1_Behav.result$gR, file = "Lipid_R1_Behav_network.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 nor does Canonical
#Lipid_R2_Behav.spls <- spls(X_Lipid_R2, Y_Behav, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
#Lipid_R2_Behav.spls <- spls(X_Lipid_R2, Y_Behav, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(Lipid_R2_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "R2ebellum Lipids", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
Lipid_R2_Behav.result <- network(Lipid_R2_Behav.spls, cutoff = 0.6,
                               color.node = c("mistyrose", "lightcyan"),
                               shape.node = c("rectangle", "circle"),
                               color.edge = c("blue", "red"),
                               lty.edge = "solid", lwd.edge = 1,
                               show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(Lipid_R2_Behav.result$gR, file = "Lipid_R2_Behav_network.gml", format = "gml")
write.graph(Lipid_R2_Behav.result$gR, file = "Lipid_R2_Behav_network.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 nor does Canonical
#Lipid_R3_Behav.spls <- spls(X_Lipid_R3, Y_Behav, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
#Lipid_R3_Behav.spls <- spls(X_Lipid_R3, Y_Behav, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(Lipid_R3_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "R3pocampus Lipids", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
Lipid_R3_Behav.result <- network(Lipid_R3_Behav.spls, cutoff = 0.6,
                               color.node = c("mistyrose", "lightcyan"),
                               shape.node = c("rectangle", "circle"),
                               color.edge = c("blue", "red"),
                               lty.edge = "solid", lwd.edge = 1,
                               show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(Lipid_R3_Behav.result$gR, file = "Lipid_R3_Behav_network.gml", format = "gml")
write.graph(Lipid_R3_Behav.result$gR, file = "Lipid_R3_Behav_network.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 nor does Canonical
#Lipid_R4_Behav.spls <- spls(X_Lipid_R4, Y_Behav, ncomp = 2, mode = 'regression', scale=F, near.zero.var = TRUE) 
#Lipid_R4_Behav.spls <- spls(X_Lipid_R4, Y_Behav, ncomp = 2, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(Lipid_R4_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "R4othalamus Lipids", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
ipid_R4_Behav.result <- network(Lipid_R4_Behav.spls, cutoff = 0.6,
                               color.node = c("mistyrose", "lightcyan"),
                               shape.node = c("rectangle", "circle"),
                               color.edge = c("blue", "red"),
                               lty.edge = "solid", lwd.edge = 1,
                               show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(Lipid_R4_Behav.result$gR, file = "Lipid_R4_Behav_network.gml", format = "gml")
write.graph(Lipid_R4_Behav.result$gR, file = "Lipid_R4_Behav_network.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5 nor does Canonical
#Lipid_R5_Behav.spls <- spls(X_Lipid_R5, Y_Behav, ncomp = 4, mode = 'regression', scale=F, near.zero.var = TRUE) 
#Lipid_R5_Behav.spls <- spls(X_Lipid_R5, Y_Behav, ncomp = 4, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(Lipid_R5_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "R5 Lipids", cutoff = 0.5, margins = c(15, 10)) #this cutoff is the highest the cim() function will tolorate on this data
Lipid_R5_Behav.result <- network(Lipid_R5_Behav.spls, cutoff = 0.6,
                               color.node = c("mistyrose", "lightcyan"),
                               shape.node = c("rectangle", "circle"),
                               color.edge = c("blue", "red"),
                               lty.edge = "solid", lwd.edge = 1,
                               show.edge.labels = TRUE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(Lipid_R5_Behav.result$gR, file = "Lipid_R5_Behav_network.gml", format = "gml")
write.graph(Lipid_R5_Behav.result$gR, file = "Lipid_R5_Behav_network_.graphml", format = "graphml")

#note: regression does not survive the threshold: 0.5
#L6_Behav.spls <- spls(X_L6, Y_Behav, ncomp = 7, mode = 'regression', scale=F, near.zero.var = TRUE) 
#L6_Behav.spls <- spls(X_L6, Y_Behav, ncomp = 7, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(L6_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "Caecum microbiota (Level 6:Genus)", margins = c(8, 18)) #this cutoff is the highest the cim() function will tolorate on this data
cim(L6_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", ylab= "Caecum microbiota (Level 6:Genus)", cutoff = 0.5, margins = c(8, 18)) #this cutoff is the highest the cim() function will tolorate on this data

L6_NS.result <- network(L6_Behav.spls, comp = 1:3, cutoff = 0.5,
                        color.node = c("mistyrose", "lightcyan"),
                        shape.node = c("rectangle", "circle"),
                        color.edge = c("blue", "red"),
                        lty.edge = "solid", lwd.edge = 1,
                        show.edge.labels = FALSE, interactive = F)

# Write graph to file that can be opened in Cytoscape
write.graph(L6_NS.result$gR, file = "L6_NS_network_0.5.gml", format = "gml")
write.graph(L6_NS.result$gR, file = "L6_NS_network_0.5.graphml", format = "gml")

#note: regression does not survive the threshold: 0.5
#L6_NS.spls <- spls(X_L6, Y_NS, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
L6_NS.spls <- spls(X_L6, Y_NS, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(L6_NS.spls, xlab = "NanoString Geneset for 5 Brain Regions", ylab= "Caecum microbiota (Level 6:Genus)", cutoff = 0.5, margins = c(8, 18)) #this cutoff is the highest the cim() function will tolorate on this data
L6_NS.result <- network(L6_NS.spls, comp = 1:3, cutoff = 0.5,
                        color.node = c("mistyrose", "lightcyan"),
                        shape.node = c("rectangle", "circle"),
                        color.edge = c("blue", "red"),
                        lty.edge = "solid", lwd.edge = 1,
                        show.edge.labels = FALSE, interactive = F)

# Write graph to file that can be opened in Cytoscape
write.graph(L6_NS.result$gR, file = "L6_NS_network_0.5.gml", format = "gml")
write.graph(L6_NS.result$gR, file = "L6_NS_network_0.5.graphml", format = "gml")

#note: regression does not survive the threshold: 0.5
L6_SCFA.spls <- spls(X_L6, Y_SCFA, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
L6_SCFA.spls <- spls(X_L6, Y_SCFA, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(L6_SCFA.spls, xlab = "Caecum SCFA", ylab= "Caecum microbiota (Level 6:Genus)", cutoff = 0.5, margins = c(7, 18)) #this cutoff is the highest the cim() function will tolorate on this data
L6_SCFA.result <- network(L6_SCFA.spls, comp = 1:3, cutoff = 0.5,
                          color.node = c("mistyrose", "lightcyan"),
                          shape.node = c("rectangle", "circle"),
                          color.edge = c("blue", "red"),
                          lty.edge = "solid", lwd.edge = 1,
                          show.edge.labels = FALSE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(L6_SCFA.result$gR, file = "L6_SCFA_network_0.5.gml", format = "gml")
write.graph(L6_SCFA.result$gR, file = "L6_SCFA_network_0.5.graphml", format = "gml")

#note: regression does not survive the threshold: 0.5
NS_SCFA.spls <- spls(X_NS, Y_SCFA, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
NS_SCFA.spls <- spls(X_NS, Y_SCFA, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(NS_SCFA.spls, xlab = "Caecum SCFA",  ylab= "NanoString Geneset for 5 Brain Regions", cutoff = 0.5, margins = c(7, 9)) #this cutoff is the highest the cim() function will tolorate on this data
NS_SCFA.result <- network(NS_SCFA.spls, comp = 1:3, cutoff = 0.5,
                          color.node = c("mistyrose", "lightcyan"),
                          shape.node = c("rectangle", "circle"),
                          color.edge = c("blue", "red"),
                          lty.edge = "solid", lwd.edge = 1,
                          show.edge.labels = FALSE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(NS_SCFA.result$gR, file = "NS_SCFA_network_0.5.gml", format = "gml")
write.graph(NS_SCFA.result$gR, file = "NS_SCFA_network_0.5.graphml", format = "gml")

# regression works at threshold = 0.5
#BLipid_NS.spls <- spls(X_BLipid, Y_NS, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
BLipid_NS.spls <- spls(X_BLipid, Y_NS, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(BLipid_NS.spls, xlab = "NanoString Geneset for 5 Brain Regions", ylab= "Lipids for 5 Brain regions", cutoff = 0.5, margins = c(8, 23)) #this cutoff is the highest the cim() function will tolorate on this data
BLipid_NS.result <- network(BLipid_NS.spls, comp = 1:3, cutoff = 0.5,
                            color.node = c("mistyrose", "lightcyan"),
                            shape.node = c("rectangle", "circle"),
                            color.edge = c("blue", "red"),
                            lty.edge = "solid", lwd.edge = 1,
                            show.edge.labels = FALSE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(BLipid_NS.result$gR, file = "BLipid_NS_network_0.5.gml", format = "gml")
write.graph(BLipid_NS.result$gR, file = "BLipid_NS4_network_0.5.graphml", format = "gml")

#note: regression does not survive the threshold: 0.5
#BLipid_SCFA.spls <- spls(X_BLipid, Y_SCFA, ncomp = 3, mode = 'regression', scale=F, near.zero.var = TRUE) 
BLipid_SCFA.spls <- spls(X_BLipid, Y_SCFA, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(BLipid_SCFA.spls, xlab = "Caecum SCFA", ylab= "Lipids for 5 Brain regions", cutoff = 0.5, margins = c(7, 28)) #this cutoff is the highest the cim() function will tolorate on this data
BLipid_SCFA.result <- network(BLipid_SCFA.spls, comp = 1:3, cutoff = 0.5,
                              color.node = c("mistyrose", "lightcyan"),
                              shape.node = c("rectangle", "circle"),
                              color.edge = c("blue", "red"),
                              lty.edge = "solid", lwd.edge = 1,
                              show.edge.labels = FALSE, interactive = F)

# Write graph to file that can be opened in Cytoscape
#write.graph(BLipid_SCFA.result$gR, file = "BLipid_SCFA_network_0.5.gml", format = "gml")
write.graph(BLipid_SCFA.result$gR, file = "BLipid_SCFA_network_0.5.graphml", format = "graphml")

L6_PLipid.spls <- spls(X_L6, Y_PLipid, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(L6_PLipid.spls, xlab = "Plasma Lipids", cutoff = 0.45, margins = c(10, 24)) #this cutoff is the highest the cim() function will tolorate on this data
#network(L6_PLipid.spls, cutoff = 0.7)

PLipid_SCFA.spls <- spls(X_PLipid, Y_SCFA, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(PLipid_SCFA.spls, xlab = "Caecum SCFA", cutoff = 0.4, margins = c(7, 10)) #this cutoff is the highest the cim() function will tolorate on this data
#network(PLipid_SCFA.spls, cutoff = 0.7)

L6_Behav.spls <- spls(X_L6, Y_Behav, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(L6_Behav.spls, xlab = "Behaviour: OFT, EPM & NOR", cutoff = 0.3, margins = c(14, 20)) #this cutoff is the highest the cim() function will tolorate on this data
#network(NS_BLipid.spls, cutoff = 0.7)

PLipid_Behav.spls <- spls(X_Behav, Y_PLipid, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(PLipid_Behav.spls, xlab = "Plasma Lipids", cutoff = 0.3, margins = c(10, 12)) #this cutoff is the highest the cim() function will tolorate on this data
#network(PLipid_SCFA.spls, cutoff = 0.7)

SCFA_Behav.spls <- spls(X_Behav, Y_SCFA, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(SCFA_Behav.spls, xlab = "Caecum SCFA", cutoff = 0.35, margins = c(7, 15)) #this cutoff is the highest the cim() function will tolorate on this data
#network(PLipid_SCFA.spls, cutoff = 0.7)

L6_Behav_VD.spls <- spls(X_L6, Y_Behav_VD, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(L6_Behav_VD.spls, xlab = "Velocity(mean & max) and Total Distance: OFT, EPM & NOR", cutoff = 0.3, margins = c(14, 20)) #this cutoff is the highest the cim() function will tolorate on this data
#network(NS_BLipid.spls, cutoff = 0.7)

PLipid_Behav_VD.spls <- spls(X_Behav_VD, Y_PLipid, ncomp = 3, mode = 'canonical', scale=F, near.zero.var = TRUE) 
cim(PLipid_Behav_VD.spls, xlab = "Plasma Lipids", cutoff = 0.4, margins = c(10, 12)) #this cutoff is the highest the cim() function will tolorate on this data
#network(PLipid_SCFA.spls, cutoff = 0.7)





png(" PCA Caecum Microbiota .png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,10),xpd=T) 
plot(pca.X_L6, main = "PCA Caecum microbiota (Level 6:Genus)")
dev.off()

png(" PCA NanoString Geneset.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,10),xpd=T) 
plot(pca.NS, main = "PCA NS Geneset for 5 Brain regions") 
dev.off()

png(" PCA Caecum SCFA.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,10),xpd=T) 
plot(pca.SCFA, main = "PCA Caecum SCFA") 
dev.off()

png(" PCA Plasma Lipids .png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,10),xpd=T) 
plot(pca.PLipid, main = "PCA Plasma Lipids")
dev.off()

png(" sPLS Heatmap Caecum Microbiota & NanoString Geneset.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,30),xpd=T) 
cim(L6_NS.spls, xlab = "NS Geneset for 5 Brain Regions",cutoff = 0.45, margins = c(8, 22))
dev.off()

png(" sPLS Heatmap Caecum Microbiota & Caecum SCFA.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,30),xpd=T) 
cim(L6_SCFA.spls, xlab = "Caecum SCFA", cutoff = 0.4, margins = c(6, 23))
dev.off()

png(" sPLS Heatmap Caecum Microbiota & Plasma Lipids.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,30),xpd=T) 
cim(L6_PLipid.spls, xlab = "Plasma Lipids", cutoff = 0.5, margins = c(9, 20))
dev.off()

png(" sPLS Heatmap NanoString Geneset & Plasma Lipids.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,30),xpd=T) 
cim(NS_PLipid.spls, xlab = "Plasma Lipids", cutoff = 0.45, margins = c(10, 7))
dev.off()

png(" sPLS Heatmap NanoString Geneset & Caecum SCFA.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,30),xpd=T) 
cim(NS_SCFA.spls, xlab = "Caecum SCFA", cutoff = 0.4, margins = c(6, 7))
dev.off()

png(" sPLS Heatmap Plasma Lipids & Caecum SCFA.png", height=4000, width=4000, pointsize=80)
par(mar=c(5,6,5,30),xpd=T) 
cim(PLipid_SCFA.spls, xlab = "Caecum SCFA", cutoff = 0.3, margins = c(6, 8))
dev.off()

