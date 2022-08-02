getwd()
setwd("C:/Users/")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("genefilter")
BiocManager::install("mixOmics")
BiocManager::install("limma")
BiocManager::install("edgeR")
BiocManager::install("metagenomeSeq")
install.packages('caret')
BiocManager::install("KEGGgraph")
install.packages("table")
install.packages("tibble")
install.packages("statmod")

library(statmod)
library(fields)
library(agricolae)
library(rgl)
library(mixOmics)
library(gplots)
library(genefilter)
library(vegan)
library(limma)
library(plot3D)
library(RColorBrewer)
library(randomForest)
library(lmPerm)
library(plotrix)
library(FSA)
library(edgeR)
library(metagenomeSeq)
library(dplyr)
library(caret)
library(e1071)
library(KEGGgraph)
library(data.table)
library(tibble)
mapping <- data.frame(read.delim(file="_microbiome_mapping.txt", sep="\t",stringsAsFactors=F))
mapping <- mapping[order(mapping[,1]),]
head(mapping)


################# Data cleaning for statistical analysis    ###########################


#############
############# Functional analysis (genes are assigned to functional roles)
############

SEED <- data.frame(read.delim(file="_ncbi_SEED_L4.txt", sep="\t"))
dim(SEED)
colnames(SEED)

rownames(SEED) <- SEED[,1]
SEED <- SEED[,2:ncol(SEED)]
dim(SEED)
colnames(SEED)

colnames(SEED) <- strsplit2(colnames(SEED),"_P")[,1]

SEED.names <- strsplit2(rownames(SEED),";")
rownames(SEED.names) <- rownames(SEED)

SEED.names["SEED;Not assigned;",]
SEED.names["SEED;Not assigned;",3] <- "Not assigned"
SEED.names["SEED;Not assigned;",4] <- "Not assigned"
SEED.names["SEED;Not assigned;",5] <- "Not assigned"

SEED.L1 <- SEED[SEED.names[,3] == "",]
SEED.L1 <- rbind(SEED.L1, SEED["SEED;Not assigned;",])
SEED.L1 <- SEED.L1[rownames(SEED.L1) !="SEED;",]

SEED.L2 <- SEED[SEED.names[,3] != "" & SEED.names[,4] == "",]
SEED.L2 <- rbind(SEED.L2, SEED["SEED;Not assigned;",])
SEED.L2 <- SEED.L2[rownames(SEED.L2) !="SEED;",]

SEED.L3 <- SEED[SEED.names[,4] != "" & SEED.names[,5] == "",]
SEED.L3 <- rbind(SEED.L3, SEED["SEED;Not assigned;",])
SEED.L3 <- SEED.L3[rownames(SEED.L3) !="SEED;",]

SEED.L4 <- SEED[SEED.names[,5] != "",]
SEED.L4 <- SEED.L4[rownames(SEED.L4) !="SEED;",]

SEED.L4_2 <- SEED.L4
SEED.L4_2.names <- strsplit2(rownames(SEED.L4_2),";")
colnames(SEED.L4_2.names) <- c("Fun1", "Fun2", "Fun3", "Fun4", "Fun5")
colnames(SEED.L4_2.names)
rownames(SEED.L4_2.names) <- rownames(SEED.L4_2)
SEED.L4_3 <- cbind(SEED.L4_2, SEED.L4_2.names)
dim(SEED.L4_3)


colSums(SEED.L1)
colSums(SEED.L2)
colSums(SEED.L3)
colSums(SEED.L4)
colnames(SEED.L4_3)

write.table(SEED.L4_3, file = paste("_SEED_matrix.txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)

###########################
#############
############# Pathway analysis  
############

KEGG <- data.frame(read.delim(file="_ncbi_KEGG_L4.txt", sep="\t"))
rownames(KEGG) <- KEGG[,1]
KEGG <- KEGG[,2:ncol(KEGG)]
colnames(KEGG) <- strsplit2(colnames(KEGG),"_P")[,1]

KEGG.names <- strsplit2(rownames(KEGG),";")
rownames(KEGG.names) <- rownames(KEGG)

KEGG.names["KEGG;Not assigned;",]
KEGG.names["KEGG;Not assigned;",3] <- "Not assigned"
KEGG.names["KEGG;Not assigned;",4] <- "Not assigned"
KEGG.names["KEGG;Not assigned;",5] <- "Not assigned"

KEGG.L1 <- KEGG[KEGG.names[,3] == "",]
KEGG.L1 <- rbind(KEGG.L1, KEGG["KEGG;Not assigned;",])
KEGG.L1 <- KEGG.L1[rownames(KEGG.L1) !="KEGG;",]

KEGG.L2 <- KEGG[KEGG.names[,3] != "" & KEGG.names[,4] == "",]
KEGG.L2 <- rbind(KEGG.L2, KEGG["KEGG;Not assigned;",])
KEGG.L2 <- KEGG.L2[rownames(KEGG.L2) !="KEGG;",]

KEGG.L3 <- KEGG[KEGG.names[,4] != "" & KEGG.names[,5] == "",]
KEGG.L3 <- rbind(KEGG.L3, KEGG["KEGG;Not assigned;",])
KEGG.L3 <- KEGG.L3[rownames(KEGG.L3) !="KEGG;",]

KEGG.L4 <- KEGG[KEGG.names[,5] != "",]
KEGG.L4 <- KEGG.L4[rownames(KEGG.L4) !="KEGG;",]

KEGG.L4_2 <- KEGG.L4
KEGG.L4_2.names <- strsplit2(rownames(KEGG.L4_2),";")
colnames(KEGG.L4_2.names) <- c("Fun1", "Fun2", "Fun3", "Fun4", "Fun5")
colnames(KEGG.L4_2.names)
rownames(KEGG.L4_2.names) <- rownames(KEGG.L4_2)
KEGG.L4_3 <- cbind(KEGG.L4_2, KEGG.L4_2.names)
dim(KEGG.L4_3)

colSums(KEGG.L1)
colSums(KEGG.L2)
colSums(KEGG.L3)
colSums(KEGG.L4)
colnames(KEGG.L4_3)
rownames(KEGG.L4)

write.table(KEGG.L4_3, file = paste("_KEGG_matrix.txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)

###########################

BRITE <- data.frame(read.delim(file="_ncbi_KEGG_Brite_L4.txt", sep="\t"))
rownames(BRITE) <- BRITE[,1]
BRITE <- BRITE[,2:ncol(BRITE)]
colnames(BRITE) <- strsplit2(colnames(BRITE),"_P")[,1]

BRITE.names <- strsplit2(rownames(BRITE),";")
rownames(BRITE.names) <- rownames(BRITE)

BRITE.names["KEGG;Not assigned;",]
BRITE.names["KEGG;Not assigned;",3] <- "Not assigned"
BRITE.names["KEGG;Not assigned;",4] <- "Not assigned"
BRITE.names["KEGG;Not assigned;",5] <- "Not assigned"

BRITE.L1 <- BRITE[BRITE.names[,3] == "",]
BRITE.L1 <- rbind(BRITE.L1, BRITE["KEGG;Not assigned;",])

BRITE.L2 <- BRITE[BRITE.names[,3] != "" & BRITE.names[,4] == "",]
BRITE.L2 <- rbind(BRITE.L2, BRITE["KEGG;Not assigned;",])

BRITE.L3 <- BRITE[BRITE.names[,4] != "" & BRITE.names[,5] == "",]
BRITE.L3 <- rbind(BRITE.L3, BRITE["KEGG;Not assigned;",])

BRITE.L4 <- BRITE[BRITE.names[,5] != "",]

BRITE.L4_2 <- BRITE.L4
BRITE.L4_2.names <- strsplit2(rownames(BRITE.L4_2),";")
colnames(BRITE.L4_2.names) <- c("Fun1", "Fun2", "Fun3", "Fun4", "Fun5")
colnames(BRITE.L4_2.names)
rownames(BRITE.L4_2.names) <- rownames(BRITE.L4_2)
BRITE.L4_3 <- cbind(BRITE.L4_2, BRITE.L4_2.names)
dim(BRITE.L4_3)

colSums(BRITE.L1)
colSums(BRITE.L2)
colSums(BRITE.L3)
colSums(BRITE.L4)
colnames(BRITE.L4_3)

write.table(BRITE.L4_3, file = paste("_BRITE_matrix.txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)


############# Plotting data at higher level to look for areas of interest

############################
############################  SEED   (SL2)
############################

plot.micro <- SEED.L2
plot.names <- strsplit2(rownames(plot.micro),";")

i <- 1
for(i in 1:ncol(plot.names)) {
  
  plot.names[,ncol(plot.names)] <- ifelse(plot.names[,ncol(plot.names)] == "", plot.names[,ncol(plot.names)-i],plot.names[,ncol(plot.names)]) 
}
head(plot.names)

plot.micro <- plot.micro[,order(mapping$Description)]
plot.mapping <- mapping[order(mapping$Description),]
table(mapping$Description)

rownames(plot.micro) <- paste(plot.names[,2], plot.names[,ncol(plot.names)], sep="; ")
plot.micro <- plot.micro[rownames(plot.micro) != "; SEED",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Cell Envelope",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Cellular Processes",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Clustering-based subsystems",]
plot.micro <- plot.micro[rownames(plot.micro) != "; DNA Processing",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Energy",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Experimental Subsystems",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Membrane Transport",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Carbohydrates",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Miscellaneous",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Protein Processing",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Regulation And Cell Signaling",]
plot.micro <- plot.micro[rownames(plot.micro) != "; RNA Processing",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Secondary Carbohydrates",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Stress Response, Defense",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Not assigned",]

plot.micro <- prop.table(as.matrix(plot.micro),2)
dim(plot.micro)

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  ## Pick some colours for each taxa you want to plot. this does not always work.
#plot.micro.cols <- col_vector[sample(length(plot.micro[,1]), replace = FALSE)]  ## Pick some colours for each taxa you want to plot. this will

png("_ncbi_Seed_L2_barplot.png", height=10000, width=18000, pointsize=50)

par(mar=c(5,6,5,35),xpd=T) 
barplot(as.matrix(plot.micro)*100,horiz=F,las=2,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=plot.mapping$Description,cex.names=0.8,main="")
axis(2, at = c(0,20,40,60,80,100) ,las=1, cex.axis = 1,lwd=1)
mtext("Percent", side=2, line=1.5, cex=1, at=50)
legend(x=ncol(plot.micro)*1.23,y=100,legend=rev(rownames(plot.micro)), bty="n", cex=1.5,col=rev(plot.micro.cols), pch=15)

dev.off()


plot.micro.mean <- aggregate(t(plot.micro),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.mean) <- plot.micro.mean$Group.1
plot.micro.mean <- subset(plot.micro.mean, select =-Group.1)
plot.micro.mean <- t(plot.micro.mean)

write.table(plot.micro.mean, file = paste("_SEED_mean_L2_",".txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)

png("_ncbi_Seed_L2_barplot_mean.png", height=8000, width=8000, pointsize=50)

par(mar=c(5,6,5,27),xpd=T) 
barplot(as.matrix(plot.micro.mean)*100,horiz=F,las=1,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.mean),cex.names=1.2,main="")
axis(2, at = c(0,20,40,60,80,100) ,las=1, cex.axis = 1,lwd=1)
mtext("Percent", side=2, line=1.5, cex=1, at=50)
legend(x=ncol(plot.micro.mean)*1.23,y=100,legend=rev(rownames(plot.micro)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

#######################
####################### KEGG   (KL2)
#######################


plot.micro <- KEGG.L2
plot.names <- strsplit2(rownames(plot.micro),";")

i <- 1
for(i in 1:ncol(plot.names)) {
  
  plot.names[,ncol(plot.names)] <- ifelse(plot.names[,ncol(plot.names)] == "", plot.names[,ncol(plot.names)-i],plot.names[,ncol(plot.names)]) 
}
head(plot.names)

plot.micro <- plot.micro[,order(mapping$Description)]
plot.mapping <- mapping[order(mapping$Description),]
table(mapping$Description)

rownames(plot.micro) <- paste(plot.names[,3], plot.names[,ncol(plot.names)], sep="; ")
plot.micro <- plot.micro[rownames(plot.micro) != "; KEGG",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Carbohydrates",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Environmental Information Processing",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Cellular Processes",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Not assigned",]

plot.micro <- prop.table(as.matrix(plot.micro),2)
dim(plot.micro)

#plot.micro[rowSums(plot.micro == "Not assigned; Not assigned")==0, , drop = FALSE]
#dim(plot.micro)

#plot.names[!grepl("Not assigned", plot.names$V1),]
#plot.names = my_dataframe[my_dataframe.employee_name != 'chad']

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  ## Pick some colours for each taxa you want to plot
plot.micro.cols <- col_vector[sample(length(plot.micro[,1]), replace = FALSE)]  ## Pick some colours for each taxa you want to plot

png("_ncbi_Kegg_L2_barplot.png", height=10000, width=14000, pointsize=50)

par(mar=c(5,6,5,35),xpd=T) 
barplot(as.matrix(plot.micro)*100,horiz=F,las=2,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=plot.mapping$Description,cex.names=0.8,main="")
axis(2, at = c(0,20,40,60,80,100) ,las=1, cex.axis = 1,lwd=1)
mtext("Percent", side=2, line=1.5, cex=1, at=50)
legend(x=ncol(plot.micro)*1.23,y=100,legend=rev(rownames(plot.micro)), bty="n", cex=1.5,col=rev(plot.micro.cols), pch=15)

dev.off()


plot.micro.mean <- aggregate(t(plot.micro),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.mean) <- plot.micro.mean$Group.1
plot.micro.mean <- subset(plot.micro.mean, select =-Group.1)
plot.micro.mean <- t(plot.micro.mean)


png("_ncbi_Kegg_L2_barplot_mean.png", height=8000, width=8000, pointsize=50)

par(mar=c(5,6,5,27),xpd=T) 
barplot(as.matrix(plot.micro.mean)*100,horiz=F,las=1,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.mean),cex.names=1.2,main="")
axis(2, at = c(0,20,40,60,80,100) ,las=1, cex.axis = 1,lwd=1)
mtext("Percent", side=2, line=1.5, cex=1, at=50)
legend(x=ncol(plot.micro.mean)*1.23,y=100,legend=rev(rownames(plot.micro)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()




#######################
####################### BRITE   (BL2)
#######################


plot.micro <- BRITE.L2
plot.names <- strsplit2(rownames(plot.micro),";")

i <- 1
for(i in 1:ncol(plot.names)) {
  
  plot.names[,ncol(plot.names)] <- ifelse(plot.names[,ncol(plot.names)] == "", plot.names[,ncol(plot.names)-i],plot.names[,ncol(plot.names)]) 
}
head(plot.names)

plot.micro <- plot.micro[,order(mapping$Description)]
plot.mapping <- mapping[order(mapping$Description),]
table(mapping$Description)

rownames(plot.micro) <- paste(plot.names[,3], plot.names[,ncol(plot.names)], sep="; ")
plot.micro <- plot.micro[rownames(plot.micro) != "; KEGG",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Brite Hierarchies",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Not Included in Pathway or Brite",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Not assigned",]

plot.micro <- prop.table(as.matrix(plot.micro),2)
dim(plot.micro)

#plot.micro[rowSums(plot.micro == "Not assigned; Not assigned")==0, , drop = FALSE]
#dim(plot.micro)

#plot.names[!grepl("Not assigned", plot.names$V1),]
#plot.names = my_dataframe[my_dataframe.employee_name != 'chad']

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  ## Pick some colours for each taxa you want to plot
plot.micro.cols <- col_vector[sample(length(plot.micro[,1]), replace = FALSE)]  ## Pick some colours for each taxa you want to plot

png("_ncbi_Brite_L2_barplot.png", height=10000, width=14000, pointsize=50)

par(mar=c(5,6,5,35),xpd=T) 
barplot(as.matrix(plot.micro)*100,horiz=F,las=2,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=plot.mapping$Description,cex.names=0.8,main="")
axis(2, at = c(0,20,40,60,80,100) ,las=1, cex.axis = 1,lwd=1)
mtext("Percent", side=2, line=1.5, cex=1, at=50)
legend(x=ncol(plot.micro)*1.23,y=100,legend=rev(rownames(plot.micro)), bty="n", cex=1.5,col=rev(plot.micro.cols), pch=15)

dev.off()


plot.micro.mean <- aggregate(t(plot.micro),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.mean) <- plot.micro.mean$Group.1
plot.micro.mean <- subset(plot.micro.mean, select =-Group.1)
plot.micro.mean <- t(plot.micro.mean)


png("_ncbi_Brite_L2_barplot_mean.png", height=8000, width=8000, pointsize=50)

par(mar=c(5,6,5,27),xpd=T) 
barplot(as.matrix(plot.micro.mean)*100,horiz=F,las=1,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.mean),cex.names=1.2,main="")
axis(2, at = c(0,20,40,60,80,100) ,las=1, cex.axis = 1,lwd=1)
mtext("Percent", side=2, line=1.5, cex=1, at=50)
legend(x=ncol(plot.micro.mean)*1.23,y=100,legend=rev(rownames(plot.micro)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()



################  Closer examination of areas of interest  ##############################

#######################
#######################  SEED examination of larger functions SEED.L4
#######################
#Protein Processing; Protein Synthesis
#Metabolism; Amino Acids and Derivatives
#Metabolism; Carbohydrates
#Stress Response, Defense, Virulence; Stress Response, Defense and Virulence
#Metabolism; Cofactors, Vitamins, Prosthetic Groups
#DNA Processing; DNA repair
#Energy; Energy and Precursor Metabolites Generation
#Metabolism; Nucleosides and Nucleotides
#RNA Processing; RNA processing and modification
#Cellular Processes; Cell Cycle, Cell Division and Death

############################## Protein Synthesis (Protein Processing; Protein Synthesis)

plot.micro.Protein_Synthesis <- SEED.L4
plot.micro.Protein_Synthesis  <- prop.table(as.matrix(plot.micro.Protein_Synthesis ),2)

plot.micro.Protein_Synthesis <- plot.micro.Protein_Synthesis[grep("Protein Synthesis", rownames(plot.micro.Protein_Synthesis)),]
plot.names.Protein_Synthesis <- strsplit2(rownames(plot.micro.Protein_Synthesis),";")
head(plot.names.Protein_Synthesis)

plot.micro.Protein_Synthesis <- plot.micro.Protein_Synthesis[,order(mapping$Description)]
plot.mapping.Protein_Synthesis <- mapping[order(mapping$Description),]
table(plot.mapping.Protein_Synthesis$Description)

rownames(plot.micro.Protein_Synthesis) <- plot.names.Protein_Synthesis[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.Protein_Synthesis[,1]))  ## Pick some colours for each taxa you want to plot

plot.micro.Protein_Synthesis.mean <- aggregate(t(plot.micro.Protein_Synthesis),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Protein_Synthesis.mean) <- plot.micro.Protein_Synthesis.mean$Group.1
plot.micro.Protein_Synthesis.mean <- subset(plot.micro.Protein_Synthesis.mean, select =-Group.1)
plot.micro.Protein_Synthesis.mean <- t(plot.micro.Protein_Synthesis.mean)

png("_SEED_L4_Protein_Synthesis_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Protein_Synthesis.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Protein_Synthesis.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,3),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=1.5, cex=1, at=1.5)
legend(x=ncol(plot.micro.Protein_Synthesis.mean)*1.23,y=2,legend=rev(rownames(plot.micro.Protein_Synthesis.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

 
############################## Stress Response, Defense and Virulence  (Stress Response, Defense, Virulence; Stress Response, Defense and Virulence)
plot.micro.Stress_Response <- SEED.L4
plot.micro.Stress_Response  <- prop.table(as.matrix(plot.micro.Stress_Response ),2)

plot.micro.Stress_Response <- plot.micro.Stress_Response[grep("Stress Response, Defense and Virulence", rownames(plot.micro.Stress_Response)),]
plot.names.Stress_Response <- strsplit2(rownames(plot.micro.Stress_Response),";")
head(plot.names.Stress_Response)

plot.micro.Stress_Response <- plot.micro.Stress_Response[,order(mapping$Description)]
plot.mapping.Stress_Response <- mapping[order(mapping$Description),]
table(plot.mapping.Stress_Response$Description)

rownames(plot.micro.Stress_Response) <- plot.names.Stress_Response[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#plot.micro.cols <- sample(col_vector, length(plot.micro.Stress_Response[,1]))  ## Pick some colours for each taxa you want to plot
plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Stress_Response.mean <- aggregate(t(plot.micro.Stress_Response),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Stress_Response.mean) <- plot.micro.Stress_Response.mean$Group.1
plot.micro.Stress_Response.mean <- subset(plot.micro.Stress_Response.mean, select =-Group.1)
plot.micro.Stress_Response.mean <- t(plot.micro.Stress_Response.mean)

png("_SEED_L4_Stress_Response_barplot_mean.png", height=6000, width=5000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Stress_Response.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Stress_Response.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,2.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.Stress_Response.mean)*1.23,y=2.5,legend=rev(rownames(plot.micro.Stress_Response.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()


############################## Carbohydrates (Metabolism; Carbohydrates)
plot.micro.Carbohydrates <- SEED.L4
plot.micro.Carbohydrates  <- prop.table(as.matrix(plot.micro.Carbohydrates ),2)

plot.micro.Carbohydrates <- plot.micro.Carbohydrates[grep("Carbohydrates", rownames(plot.micro.Carbohydrates)),]
plot.names.Carbohydrates <- strsplit2(rownames(plot.micro.Carbohydrates),";")
head(plot.names.Carbohydrates)

plot.micro.Carbohydrates <- plot.micro.Carbohydrates[,order(mapping$Description)]
plot.mapping.Carbohydrates <- mapping[order(mapping$Description),]
table(plot.mapping.Carbohydrates$Description)

rownames(plot.micro.Carbohydrates) <- plot.names.Carbohydrates[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.Carbohydrates[,1]))  ## Pick some colours for each taxa you want to plot
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Carbohydrates.mean <- aggregate(t(plot.micro.Carbohydrates),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Carbohydrates.mean) <- plot.micro.Carbohydrates.mean$Group.1
plot.micro.Carbohydrates.mean <- subset(plot.micro.Carbohydrates.mean, select =-Group.1)
plot.micro.Carbohydrates.mean <- t(plot.micro.Carbohydrates.mean)

png("_SEED_L4_Carbohydrates_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Carbohydrates.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Carbohydrates.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,3),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.Carbohydrates.mean)*1.23,y=2.5,legend=rev(rownames(plot.micro.Carbohydrates.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

############################## Amino Acids and Derivatives (Metabolism; Amino Acids and Derivatives)
plot.micro.Amino_Acids <- SEED.L4
plot.micro.Amino_Acids  <- prop.table(as.matrix(plot.micro.Amino_Acids ),2)

plot.micro.Amino_Acids <- plot.micro.Amino_Acids[grep("Amino Acids and Derivatives", rownames(plot.micro.Amino_Acids)),]
plot.names.Amino_Acids <- strsplit2(rownames(plot.micro.Amino_Acids),";")
head(plot.names.Amino_Acids)

plot.micro.Amino_Acids <- plot.micro.Amino_Acids[,order(mapping$Description)]
plot.mapping.Amino_Acids <- mapping[order(mapping$Description),]
table(plot.mapping.Amino_Acids$Description)

rownames(plot.micro.Amino_Acids) <- plot.names.Amino_Acids[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.Amino_Acids[,1]))  ## Pick some colours for each taxa you want to plot
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Amino_Acids.mean <- aggregate(t(plot.micro.Amino_Acids),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Amino_Acids.mean) <- plot.micro.Amino_Acids.mean$Group.1
plot.micro.Amino_Acids.mean <- subset(plot.micro.Amino_Acids.mean, select =-Group.1)
plot.micro.Amino_Acids.mean <- t(plot.micro.Amino_Acids.mean)

png("_SEED_L4_Amino_Acids_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Amino_Acids.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Amino_Acids.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,3),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.Amino_Acids.mean)*1.23,y=3,legend=rev(rownames(plot.micro.Amino_Acids.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()


############################## Cofactors, Vitamins, Prosthetic Groups  (Metabolism; Cofactors, Vitamins, Prosthetic Groups)
plot.micro.Cofact_vits <- SEED.L4
plot.micro.Cofact_vits  <- prop.table(as.matrix(plot.micro.Cofact_vits ),2)

plot.micro.Cofact_vits <- plot.micro.Cofact_vits[grep("Cofactors, Vitamins, Prosthetic Groups", rownames(plot.micro.Cofact_vits)),]
plot.names.Cofact_vits <- strsplit2(rownames(plot.micro.Cofact_vits),";")
head(plot.names.Cofact_vits)

plot.micro.Cofact_vits <- plot.micro.Cofact_vits[,order(mapping$Description)]
plot.mapping.Cofact_vits <- mapping[order(mapping$Description),]
table(plot.mapping.Cofact_vits$Description)

rownames(plot.micro.Cofact_vits) <- plot.names.Cofact_vits[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.Cofact_vits[,1]))  ## Pick some colours for each taxa you want to plot
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Cofact_vits.mean <- aggregate(t(plot.micro.Cofact_vits),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Cofact_vits.mean) <- plot.micro.Cofact_vits.mean$Group.1
plot.micro.Cofact_vits.mean <- subset(plot.micro.Cofact_vits.mean, select =-Group.1)
plot.micro.Cofact_vits.mean <- t(plot.micro.Cofact_vits.mean)

png("_SEED_L4_Cofact_vits_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Cofact_vits.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Cofact_vits.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,2.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.Cofact_vits.mean)*1.23,y=2.5,legend=rev(rownames(plot.micro.Cofact_vits.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

##############################  DNA repair (DNA Processing; DNA repair)
plot.micro.DNA_repair <- SEED.L4
plot.micro.DNA_repair  <- prop.table(as.matrix(plot.micro.DNA_repair ),2)

plot.micro.DNA_repair <- plot.micro.DNA_repair[grep("DNA repair", rownames(plot.micro.DNA_repair)),]
plot.names.DNA_repair <- strsplit2(rownames(plot.micro.DNA_repair),";")
head(plot.names.DNA_repair)

plot.micro.DNA_repair <- plot.micro.DNA_repair[,order(mapping$Description)]
plot.mapping.DNA_repair <- mapping[order(mapping$Description),]
table(plot.mapping.DNA_repair$Description)

rownames(plot.micro.DNA_repair) <- plot.names.DNA_repair[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.DNA_repair[,1]))  ## Pick some colours for each taxa you want to plot
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.DNA_repair.mean <- aggregate(t(plot.micro.DNA_repair),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.DNA_repair.mean) <- plot.micro.DNA_repair.mean$Group.1
plot.micro.DNA_repair.mean <- subset(plot.micro.DNA_repair.mean, select =-Group.1)
plot.micro.DNA_repair.mean <- t(plot.micro.DNA_repair.mean)

png("_SEED_L4_DNA_repair_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.DNA_repair.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.DNA_repair.mean),cex.names=1.2,main="")
axis(2, at = c(0,0.1),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.DNA_repair.mean)*1.23,y=0.1,legend=rev(rownames(plot.micro.DNA_repair.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

##############################  Energy and Precursor Metabolites Generation (Energy; Energy and Precursor Metabolites Generation)
plot.micro.Energy_metabolites <- SEED.L4
plot.micro.Energy_metabolites  <- prop.table(as.matrix(plot.micro.Energy_metabolites ),2)

plot.micro.Energy_metabolites <- plot.micro.Energy_metabolites[grep("Energy and Precursor Metabolites Generation", rownames(plot.micro.Energy_metabolites)),]
plot.names.Energy_metabolites <- strsplit2(rownames(plot.micro.Energy_metabolites),";")
head(plot.names.Energy_metabolites)

plot.micro.Energy_metabolites <- plot.micro.Energy_metabolites[,order(mapping$Description)]
plot.mapping.Energy_metabolites <- mapping[order(mapping$Description),]
table(plot.mapping.Energy_metabolites$Description)

rownames(plot.micro.Energy_metabolites) <- plot.names.Energy_metabolites[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.Energy_metabolites[,1]))  ## Pick some colours for each taxa you want to plot
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Energy_metabolites.mean <- aggregate(t(plot.micro.Energy_metabolites),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Energy_metabolites.mean) <- plot.micro.Energy_metabolites.mean$Group.1
plot.micro.Energy_metabolites.mean <- subset(plot.micro.Energy_metabolites.mean, select =-Group.1)
plot.micro.Energy_metabolites.mean <- t(plot.micro.Energy_metabolites.mean)

png("_SEED_L4_Energy_metabolites_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Energy_metabolites.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Energy_metabolites.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,2.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.Energy_metabolites.mean)*1.23,y=2.5,legend=rev(rownames(plot.micro.Energy_metabolites.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

############################## Nucleosides and Nucleotides (Metabolism; Nucleosides and Nucleotides)
plot.micro.Nucleoside <- SEED.L4
plot.micro.Nucleoside  <- prop.table(as.matrix(plot.micro.Nucleoside ),2)

plot.micro.Nucleoside <- plot.micro.Nucleoside[grep("Nucleosides and Nucleotides", rownames(plot.micro.Nucleoside)),]
plot.names.Nucleoside <- strsplit2(rownames(plot.micro.Nucleoside),";")
head(plot.names.Nucleoside)

plot.micro.Nucleoside <- plot.micro.Nucleoside[,order(mapping$Description)]
plot.mapping.Nucleoside <- mapping[order(mapping$Description),]
table(plot.mapping.Nucleoside$Description)

rownames(plot.micro.Nucleoside) <- plot.names.Nucleoside[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.Nucleoside[,1]))  ## Pick some colours for each taxa you want to plot
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Nucleoside.mean <- aggregate(t(plot.micro.Nucleoside),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Nucleoside.mean) <- plot.micro.Nucleoside.mean$Group.1
plot.micro.Nucleoside.mean <- subset(plot.micro.Nucleoside.mean, select =-Group.1)
plot.micro.Nucleoside.mean <- t(plot.micro.Nucleoside.mean)

png("_SEED_L4_Nucleoside_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Nucleoside.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Nucleoside.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,2.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.Nucleoside.mean)*1.23,y=2.5,legend=rev(rownames(plot.micro.Nucleoside.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

############################## RNA processing and modification (RNA Processing; RNA processing and modification)
plot.micro.RNA_process <- SEED.L4
plot.micro.RNA_process  <- prop.table(as.matrix(plot.micro.RNA_process ),2)

plot.micro.RNA_process <- plot.micro.RNA_process[grep("RNA processing and modification", rownames(plot.micro.RNA_process)),]
plot.names.RNA_process <- strsplit2(rownames(plot.micro.RNA_process),";")
head(plot.names.RNA_process)

plot.micro.RNA_process <- plot.micro.RNA_process[,order(mapping$Description)]
plot.mapping.RNA_process <- mapping[order(mapping$Description),]
table(plot.mapping.RNA_process$Description)

rownames(plot.micro.RNA_process) <- plot.names.RNA_process[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.RNA_process[,1]))  ## Pick some colours for each taxa you want to plot
#plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.RNA_process.mean <- aggregate(t(plot.micro.RNA_process),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.RNA_process.mean) <- plot.micro.RNA_process.mean$Group.1
plot.micro.RNA_process.mean <- subset(plot.micro.RNA_process.mean, select =-Group.1)
plot.micro.RNA_process.mean <- t(plot.micro.RNA_process.mean)

png("_SEED_L4_RNA_process_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.RNA_process.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.RNA_process.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,2.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.RNA_process.mean)*1.23,y=2.5,legend=rev(rownames(plot.micro.RNA_process.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()


############################## Cell Cycle, Cell Division and Death  (Cellular Processes; Cell Cycle, Cell Division and Death)
plot.micro.Cell <- SEED.L4
plot.micro.Cell  <- prop.table(as.matrix(plot.micro.Cell ),2)

plot.micro.Cell <- plot.micro.Cell[grep("Cell Cycle, Cell Division and Death", rownames(plot.micro.Cell)),]
plot.names.Cell <- strsplit2(rownames(plot.micro.Cell),";")
head(plot.names.Cell)

plot.micro.Cell <- plot.micro.Cell[,order(mapping$Description)]
plot.mapping.Cell <- mapping[order(mapping$Description),]
table(plot.mapping.Cell$Description)

rownames(plot.micro.Cell) <- plot.names.Cell[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.Cell[,1]))  ## Pick some colours for each taxa you want to plot

plot.micro.Cell.mean <- aggregate(t(plot.micro.Cell),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Cell.mean) <- plot.micro.Cell.mean$Group.1
plot.micro.Cell.mean <- subset(plot.micro.Cell.mean, select =-Group.1)
plot.micro.Cell.mean <- t(plot.micro.Cell.mean)

png("_SEED_L4_Cell_barplot_mean.png", height=4000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Cell.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Cell.mean),cex.names=1.2,main="")
axis(2, at = c(0,0.2),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=1.5, cex=1, at=1.5)
legend(x=ncol(plot.micro.Cell.mean)*1.23,y=0.1,legend=rev(rownames(plot.micro.Cell.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()

#######################
#######################  KEGG examination of larger functions KEGG.L4
#######################
############################## Carbohydrate metabolism
plot.micro.Carb_met <- KEGG.L4
plot.micro.Carb_met  <- prop.table(as.matrix(plot.micro.Carb_met ),2)

plot.micro.Carb_met <- plot.micro.Carb_met[grep("Carbohydrate metabolism", rownames(plot.micro.Carb_met)),]
plot.names.Carb_met <- strsplit2(rownames(plot.micro.Carb_met),";")
head(plot.names.Carb_met)

plot.micro.Carb_met <- plot.micro.Carb_met[,order(mapping$Description)]
plot.mapping.Carb_met <- mapping[order(mapping$Description),]
table(plot.mapping.Carb_met$Description)

rownames(plot.micro.Carb_met) <- plot.names.Carb_met[,3]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#plot.micro.cols <- sample(col_vector, length(plot.micro.Carb_met[,1]))  ## Pick some colours for each taxa you want to plot#
plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Carb_met.mean <- aggregate(t(plot.micro.Carb_met),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Carb_met.mean) <- plot.micro.Carb_met.mean$Group.1
plot.micro.Carb_met.mean <- subset(plot.micro.Carb_met.mean, select =-Group.1)
plot.micro.Carb_met.mean <- t(plot.micro.Carb_met.mean)

png("_KEGG_L4_Carb_met_barplot_mean.png", height=12000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Carb_met.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Carb_met.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,3,4,5,5.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=1.5, cex=1, at=1.5)
legend(x=ncol(plot.micro.Carb_met.mean)*1.23,y=5.5,legend=rev(rownames(plot.micro.Carb_met.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()


############################## Amino acid metabolism
plot.micro.A_acid_met <- KEGG.L4
plot.micro.A_acid_met  <- prop.table(as.matrix(plot.micro.A_acid_met ),2)

plot.micro.A_acid_met <- plot.micro.A_acid_met[grep("Amino acid metabolism", rownames(plot.micro.A_acid_met)),]
plot.names.A_acid_met <- strsplit2(rownames(plot.micro.A_acid_met),";")
head(plot.names.A_acid_met)

plot.micro.A_acid_met <- plot.micro.A_acid_met[,order(mapping$Description)]
plot.mapping.A_acid_met <- mapping[order(mapping$Description),]
table(plot.mapping.A_acid_met$Description)

rownames(plot.micro.A_acid_met) <- plot.names.A_acid_met[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#plot.micro.cols <- sample(col_vector, length(plot.micro.A_acid_met[,1]))  ## Pick some colours for each taxa you want to plot
plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)

plot.micro.A_acid_met.mean <- aggregate(t(plot.micro.A_acid_met),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.A_acid_met.mean) <- plot.micro.A_acid_met.mean$Group.1
plot.micro.A_acid_met.mean <- subset(plot.micro.A_acid_met.mean, select =-Group.1)
plot.micro.A_acid_met.mean <- t(plot.micro.A_acid_met.mean)

png("_KEGG_L4_A_acid_met_barplot_mean.png", height=10000, width=5000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.A_acid_met.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.A_acid_met.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,2.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.A_acid_met.mean)*1.23,y=2.5,legend=rev(rownames(plot.micro.A_acid_met.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()


#######################
#######################  BRITE examination of larger functions KEGG.L4
#######################
############################## genetic information processing
plot.micro.Gen_info <- BRITE.L4
head(plot.micro.Gen_info)
plot.micro.Gen_info  <- prop.table(as.matrix(plot.micro.Gen_info ),2)
head(plot.micro.Gen_info)
plot.micro.Gen_info <- plot.micro.Gen_info[grep("genetic information processing", rownames(plot.micro.Gen_info)),]
head(plot.micro.Gen_info)
plot.names.Gen_info <- strsplit2(rownames(plot.micro.Gen_info),";")
head(plot.names.Gen_info)


plot.micro.Gen_info <- plot.micro.Gen_info[,order(mapping$Description)]
plot.mapping.Gen_info <- mapping[order(mapping$Description),]
table(plot.mapping.Gen_info$Description)

rownames(plot.micro.Gen_info) <- plot.names.Gen_info[,3]  

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#plot.micro.cols <- sample(col_vector, length(plot.micro.Gen_info[,1]))  ## Pick some colours for each taxa you want to plot#
plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)  

plot.micro.Gen_info.mean <- aggregate(t(plot.micro.Gen_info),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Gen_info.mean) <- plot.micro.Gen_info.mean$Group.1
plot.micro.Gen_info.mean <- subset(plot.micro.Gen_info.mean, select =-Group.1)
plot.micro.Gen_info.mean <- t(plot.micro.Gen_info.mean)

png("_BRITE_L4_Gen_info_barplot_mean.png", height=12000, width=4000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Gen_info.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Gen_info.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,3,4,5,6),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=1.5, cex=1, at=1.5)
legend(x=ncol(plot.micro.Gen_info.mean)*1.23,y=6,legend=rev(rownames(plot.micro.Gen_info.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()


############################## signaling and cellular processes;
plot.micro.Sign_cell <- BRITE.L4
plot.micro.Sign_cell  <- prop.table(as.matrix(plot.micro.Sign_cell ),2)
plot.micro.Sign_cell <- plot.micro.Sign_cell[grep("signaling and cellular processes", rownames(plot.micro.Sign_cell)),]
plot.names.Sign_cell <- strsplit2(rownames(plot.micro.Sign_cell),";")
head(plot.names.Sign_cell)

plot.micro.Sign_cell <- plot.micro.Sign_cell[,order(mapping$Description)]
plot.mapping.Sign_cell <- mapping[order(mapping$Description),]
table(plot.mapping.Sign_cell$Description)

rownames(plot.micro.Sign_cell) <- plot.names.Sign_cell[,4]  #by column ?

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#plot.micro.cols <- sample(col_vector, length(plot.micro.Sign_cell[,1]))  ## Pick some colours for each taxa you want to plot
plot.micro.cols <- sample(col_vector, length(plot.micro[,1]), replace = TRUE)

plot.micro.Sign_cell.mean <- aggregate(t(plot.micro.Sign_cell),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.Sign_cell.mean) <- plot.micro.Sign_cell.mean$Group.1
plot.micro.Sign_cell.mean <- subset(plot.micro.Sign_cell.mean, select =-Group.1)
plot.micro.Sign_cell.mean <- t(plot.micro.Sign_cell.mean)

png("_BRITE_L4_Sign_cell_barplot_mean.png", height=10000, width=5000, pointsize=40)

par(mar=c(5,6,5,40),xpd=T) 
barplot(as.matrix(plot.micro.Sign_cell.mean)*100,horiz=F,las=1,axes=F,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.Sign_cell.mean),cex.names=1.2,main="")
axis(2, at = c(0,1,2,3,4,5,6,6.5),las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=2, cex=1, at=1.5)
legend(x=ncol(plot.micro.Sign_cell.mean)*1.23,y=6.5,legend=rev(rownames(plot.micro.Sign_cell.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()




##### ANALYSIS ######

#################
################   Choose level for analyses and rename taxa into manageable bites
#################

comparison.counts <- SEED.L4
dim(comparison.counts)

################
################  Permutation ANOVA
################
################

##  Filter to remove very low abundance taxa

comparison.counts <- comparison.counts[rowSums(comparison.counts) > 20,] # 20 is a chosen threshold when there  are lower numbers use >50 for larger number of genes  
dim(comparison.counts)

i <- 1
filter.rare <- 0
for(i in 1:nrow(comparison.counts))  {
  filter.rare[i] <- length(comparison.counts[i,][comparison.counts[i,] > 1])  }
filter.rare

comparison.counts <- comparison.counts[filter.rare > 5,]

dim(comparison.counts)


##  Convert to proportions

comparison.prop <- prop.table(as.matrix(comparison.counts), 2)

colSums(comparison.prop)

##########

fact1 <- as.factor(mapping$treat1)
fact2 <- as.factor(mapping$treat2)

groups <- as.factor(mapping$Description)

group.size <- table(groups)
group.size

means.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(means.table) <- paste(levels(groups),".mean",sep="")

sem.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(sem.table) <- paste(levels(groups),".sem",sep="")

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  means.table[i,] <- tapply(as.numeric(comparison.prop[i,]),groups,mean)*100  }


i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  sem.table[i,] <- tapply(as.numeric(comparison.prop[i,]),groups,sd)*100 /sqrt(group.size) }


stats.table <- as.data.frame(matrix(1, ncol = 6, nrow = nrow(comparison.prop)))
colnames(stats.table) <- c("p.value.treat1","p.value.treat2","p.value.interaction","fdr.treat1","fdr.treat2","fdr.interaction")

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  
  reptest <- as.numeric(comparison.prop[i,])
  stat.test <- unlist(summary(aovp(reptest ~ fact1 * fact2, perm="Prob", maxIter=1000000)))
  stats.table$p.value.treat1[i] <- stat.test["Pr(Prob)1"] 
  stats.table$p.value.treat2[i] <- stat.test["Pr(Prob)2"]
  stats.table$p.value.interaction[i] <- stat.test["Pr(Prob)3"]
}

stats.table$fdr.treat1 <- p.adjust(stats.table$p.value.treat1, method = "BH", n=nrow(stats.table))
stats.table$fdr.treat2 <- p.adjust(stats.table$p.value.treat2, method = "BH", n=nrow(stats.table))
stats.table$fdr.interaction <- p.adjust(stats.table$p.value.interaction, method = "BH", n=nrow(stats.table))


lsd.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(lsd.table) <- levels(groups)

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  
  reptest <- as.numeric(comparison.prop[i,])
  ANOVA <- aov(reptest~groups)
  
  comparison <- LSD.test(ANOVA,"groups",group=T)
  comparison$groups$groups <- as.character(comparison$groups$groups)
  comparison$groups <- comparison$groups[order(rownames(comparison$groups)),]
  
  lsd.table[i,] <- as.character(comparison$groups$groups)   }

stats.table <- cbind(Classification=rownames(comparison.prop),means.table,sem.table,stats.table, lsd.table)

stats.table[stats.table$p.value.treat1 < 0.05,]
stats.table[stats.table$p.value.treat2 < 0.05,]
stats.table[stats.table$fdr.treat1 < 0.05,]
stats.table[stats.table$fdr.treat2 < 0.05,]


write.table(stats.table, file = "_SEED_ncbi_L4_stats_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)


P_sig_treat1 <- stats.table[(stats.table[,'p.value.treat1']< 0.05),]
P_sig_treat2 <- stats.table[(stats.table[,'p.value.treat2']< 0.05),]
P_sig_interaction <- stats.table[(stats.table[,'p.value.interaction']< 0.05),]

FDR_treat1 <- stats.table[(stats.table[,'fdr.treat1']< 0.05),]
FDR_treat2 <- stats.table[(stats.table[,'fdr.treat2']< 0.05),]
FDR_interaction <- stats.table[(stats.table[,'fdr.interaction']< 0.05),]


write.table(FDR_treat1, file = "_SEED_ncbi_L4_stats_FDR_treat1_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)


for(row_num in 1:nrow(FDR_treat1)){
  #print(row_num)
  row_name<-FDR_treat1[row_num, "Classification"]
  #print(row_name)
  for(i in 1:nrow(comparison.prop)){
    #print(i)
    row_comparison<-(rownames(comparison.prop)[i])
    #print(row_comparison)
    if (row_name == row_comparison){
      boxplot(comparison.prop[i,]~mapping$treat1, main=stats.table$Classification[i])
      #print()
    }
  }  
}


###############
###############  Boxplot
###############


for(row_num in 1:nrow(FDR_treat1)){
  #print(row_num)
  row_name<-FDR_treat1[row_num, "Classification"]
  #print(row_name)
  for(i in 1:nrow(comparison.prop)){
    #print(i)
    row_comparison<-(rownames(comparison.prop)[i])
    #print(row_comparison)
    mypath <- file.path(paste("_SEED_ncbi_L4_", stats.table$Classification[i], ".png", sep = ""))
    if (row_name == row_comparison){
      png(filename=mypath,height=5000, width=5000, pointsize=50)   
      par(mar=c(12,5,5,4))
      boxplot(as.numeric(comparison.prop[i,])*100~mapping$treat1,frame=F,xaxt="n",ylab="Comparison",xlab="",outline=F,boxwex=0.5,main=stats.table$Classification[i],las=1)
      stripchart(as.numeric(comparison.prop[i,])*100~mapping$treat1, vertical=T,frame=F, pch=1,xaxt="n", ylab="Comparison",lwd=5,las=1, xlim=c(0.5,4.1),
                 method = "jitter",jitter=0.05, add=T)
      axis(side=1, at=c(1:2),labels=levels(as.factor(mapping$treat1)),cex.axis=1.2, las=2)
      box(bty="l",lwd=3)
      dev.off()
    }
  }  
}


SEED_plots <- stats.table[stats.table$fdr.treat1 < 0.05,]
dim(SEED_plots)
SEED_plots

########################
#########################

#######
#############
##################  PCA
############################################
#############################################################


pca.res <- prcomp(t(comparison.prop),scale=T)

eigs <- pca.res$sdev^2
eigs / sum(eigs)
var.explained <- eigs / sum(eigs)
var.explained <- var.explained*100


cols <- c("darkorange3","darkorchid2","cornflowerblue","chartreuse4")[as.factor(mapping$Description)]


plot3d(pca.res$x[,1:3],col=cols,type="s",size=1)



png("_SEED_ncbi_L4_PCA.png",height=4000,width=4000,pointsize=80)

scatter3D(pca.res$x[,3],pca.res$x[,1],pca.res$x[,2], col = cols, colvar = NA, colkey = F, pch=16,theta=30,phi=30,bty="g",cex=1.5,lwd=8,
          xlab=paste("\nPC1 (",round(as.numeric(var.explained[1]),1),"%)",sep=""),
          ylab=paste("\nPC2 (",round(as.numeric(var.explained[2]),1),"%)",sep=""),
          zlab=paste("\nPC3 (",round(as.numeric(var.explained[3]),1),"%)",sep=""))
legend(x=0.27,y=-0.39, levels(as.factor(mapping$Description)),bty="n", pch = 19, col = c("darkorange3","darkorchid2","cornflowerblue","chartreuse4"), cex=1.1)


dev.off()

################
################
##########################################  EdgeR glmLRT 
################
################


group <- as.factor(mapping$Description) 
group.design <- model.matrix(~group)

comparison.counts <- SEED.L4

comparison.counts <- comparison.counts[rowSums(comparison.counts) > 20,]
dim(comparison.counts)

comparison.y <- DGEList(counts=comparison.counts, group=group)
comparison.keep <- rowSums(cpm(comparison.y)>1) >= 5
comparison.y <- comparison.y[comparison.keep, , keep.lib.sizes=FALSE]


dim(comparison.y)


head(comparison.y$samples)
head(comparison.y$counts)[,1:3]

comparison.y <- edgeR::calcNormFactors(comparison.y)

comparison.y <- estimateDisp(comparison.y, group.design)

plotBCV(comparison.y)
plotMD(comparison.y)

comparison.logcpm <- cpm(comparison.y, prior.count=1, log=TRUE)
comparison.cpm <- cpm(comparison.y, prior.count=1, log=F)

comparison.fit <- glmFit(comparison.y, group.design)

#plotQLDisp(comparison.fit)

## -------------------------------------------
## --------- Conduct pairwise statistical tests for a given coefficient or contrast
## -------------------------------------------
# baseline = 1
# coef=2; compare 2 vs 1
# coef=3; compare 3 vs 1
# contrast=c(0,-1,1); compare 3 vs 2
# coef=2:4; to find variables different between any of the 4 groups
#
# In this example, where we have control, treatment2, treatment3, treatment4, then;
# coef=2 is control vs treatment2
# coef=3 is control vs treatment3
# coef=4 is control vs treatment4
# contrast=c(0,-1,1) is treatment3 vs treatment2
#
# Contrast group WW vs WM ( with WW as the reference of comparison )
contrast_AC_v_AH <- glmLRT( comparison.fit, contrast=makeContrasts( control-treatment2, levels=design ) )

# Contrast group WW vs MM( with WW as the reference of comparison )
contrast_AC_v_LC <- glmLRT( comparison.fit, contrast=makeContrasts( control-treatment3, levels=design ) )

# Contrast group WM vs mm ( with WM as the reference of comparison )
contrast_AC_v_LH <- glmLRT( comparison.fit, contrast=makeContrasts( control-treatment4, levels=design ) )

results <- glmLRT(comparison.fit, coef=4)
comparison.results <- topTags(results,n=nrow(comparison.qlf))


## --------- Filter results for FDR >0.05 AND LogCPM > 0
## ---------
## --------- Positive logFC indicates higher expression/abundance in treatment group and negative logFC indicates higher in baseline
## --------- e.g. For CONTROL vs DIARR, a +ve logFC indicates higher abundance in DIARR, while a -ve logFC indicates higher in CONTROL
## --------- Fold Change (FC) = 0.585 is equivalent to a FC of 1.5.  So below is filtering on FDR of < 0.05 & FC of > 1.5

comparison.results <- comparison.results[comparison.results$table$FDR < 0.05 & abs(comparison.results$table$logFC) > 0.585,]
comparison.results$table
head(comparison.results$table)
rownames(head(comparison.results$table))

write.table(comparison.qlf.results, file = "_SEED_ncbi_L4_comparisons_aovp_Coeff_4_glmLRT.txt", sep = "\t", row.names = FALSE, quote = FALSE)


#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[1],]~group, main=rownames(head(comparison.qlf.results$table))[1])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[2],]~group, main=rownames(head(comparison.qlf.results$table))[2])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[3],]~group, main=rownames(head(comparison.qlf.results$table))[3])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[4],]~group, main=rownames(head(comparison.qlf.results$table))[4])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[5],]~group, main=rownames(head(comparison.qlf.results$table))[5])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[6],]~group, main=rownames(head(comparison.qlf.results$table))[6])


for(row_num in 1:nrow(comparison.results)){
  row_name<-(rownames(comparison.results)[row_num])
  for(i in 1:nrow(comparison.logcpm)){
    row_comparison<-(rownames(comparison.logcpm)[i])
    #print(row_name)
    if (row_name == row_comparison){
      boxplot(comparison.logcpm[i,]~group, main=row_comparison)
    }
  }  
}


###############
###############  Boxplot
###############



for(row_num in 1:nrow(comparison.qlf.results)){
  row_name<-(rownames(comparison.qlf.results)[row_num])
  for(i in 1:nrow(comparison.logcpm)){
    row_comparison<-(rownames(comparison.logcpm)[i])
    #print(row_name)
    mypath <- file.path(paste("_SEED_ncbi_L4_comparisons_", row_comparison, ".png", sep = ""))
    if (row_name == row_comparison){
      png(filename=mypath,height=5000, width=5000, pointsize=50)   
      par(mar=c(12,5,5,4))
      boxplot(as.numeric(comparison.logcpm[i,])*100~group,frame=F,xaxt="n",ylab="Comparison",xlab="",outline=F,boxwex=0.5,main=row_comparison,las=1)
      stripchart(as.numeric(comparison.logcpm[i,])*100~group, vertical=T,frame=F, pch=1,xaxt="n", ylab="Comparison",lwd=5,las=1, xlim=c(0.5,4.1),
                 method = "jitter",jitter=0.05, add=T)
      axis(side=1, at=c(1:4),labels = levels(group),cex.axis=1.2, las=2)
      box(bty="l",lwd=3)
      dev.off()
    }
  }  
}

################
############## Heatmaps SEEDL4.   low abundance >20, proportions
###############

Heat_SEEDL4 <- comparison.prop
dim(Heat_SEEDL4)

Heat_SEEDL4 <- Heat_SEEDL4[,order(mapping$Description)]
head(Heat_SEEDL4)
Heat_SEEDL4.mapping <- mapping[order(mapping$Description),]
head(Heat_SEEDL4.mapping)
table(mapping$Description)
typeof(Heat_SEEDL4)
typeof(Heat_SEEDL4.mapping$Seq_label)
dim(Heat_SEEDL4)
dim(Heat_SEEDL4.mapping)


colnames(Heat_SEEDL4) <- Heat_SEEDL4.mapping$Description[match(colnames(Heat_SEEDL4),Heat_SEEDL4.mapping$Seq_label)]
head(Heat_SEEDL4)
dim(Heat_SEEDL4)
colnames(Heat_SEEDL4)

col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(as.matrix(Heat_SEEDL4), Rowv = NA, Colv = NA, col = col)
data.dist <- vegdist(Heat_SEEDL4, method = "bray")
row.clus <- hclust(data.dist, "aver")
heatmap(as.matrix(Heat_SEEDL4[,order(mapping$Description)]), Rowv = as.dendrogram(row.clus), Colv = NA, col = col, margins = c(10, 3))
########################
#########################


################
################   Choose level for analyses and rename taxa into manageable bites
#################

####################################
######################################   KEGG
######################################


comparison.counts <- KEGG.L4
dim(comparison.counts)


################
################  Permutation ANOVA
################

##  Filter to remove very low abundance taxa

comparison.counts <- comparison.counts[rowSums(comparison.counts) > 20,] # 20 is a chosen threshold when there  are lower numbers use >50 for larger number of genes  
dim(comparison.counts)

i <- 1
filter.rare <- 0
for(i in 1:nrow(comparison.counts))  {
  filter.rare[i] <- length(comparison.counts[i,][comparison.counts[i,] > 1])  }
filter.rare

comparison.counts <- comparison.counts[filter.rare > 5,]

dim(comparison.counts)


##  Convert to proportions

comparison.prop <- prop.table(as.matrix(comparison.counts), 2)

colSums(comparison.prop)

##########

fact1 <- as.factor(mapping$treat1)
fact2 <- as.factor(mapping$treat2)

groups <- as.factor(mapping$Description)

group.size <- table(groups)
group.size

means.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(means.table) <- paste(levels(groups),".mean",sep="")

sem.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(sem.table) <- paste(levels(groups),".sem",sep="")

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  means.table[i,] <- tapply(as.numeric(comparison.prop[i,]),groups,mean)*100  }


i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  sem.table[i,] <- tapply(as.numeric(comparison.prop[i,]),groups,sd)*100 /sqrt(group.size) }


stats.table <- as.data.frame(matrix(1, ncol = 6, nrow = nrow(comparison.prop)))
colnames(stats.table) <- c("p.value.treat1","p.value.treat2","p.value.interaction","fdr.treat1","fdr.treat2","fdr.interaction")

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  
  reptest <- as.numeric(comparison.prop[i,])
  stat.test <- unlist(summary(aovp(reptest ~ fact1 * fact2, perm="Prob", maxIter=1000000)))
  stats.table$p.value.treat1[i] <- stat.test["Pr(Prob)1"] 
  stats.table$p.value.treat2[i] <- stat.test["Pr(Prob)2"]
  stats.table$p.value.interaction[i] <- stat.test["Pr(Prob)3"]
}

stats.table$fdr.treat1 <- p.adjust(stats.table$p.value.treat1, method = "BH", n=nrow(stats.table))
stats.table$fdr.treat2 <- p.adjust(stats.table$p.value.treat2, method = "BH", n=nrow(stats.table))
stats.table$fdr.interaction <- p.adjust(stats.table$p.value.interaction, method = "BH", n=nrow(stats.table))


lsd.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(lsd.table) <- levels(groups)

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  
  reptest <- as.numeric(comparison.prop[i,])
  ANOVA <- aov(reptest~groups)
  
  comparison <- LSD.test(ANOVA,"groups",group=T)
  comparison$groups$groups <- as.character(comparison$groups$groups)
  comparison$groups <- comparison$groups[order(rownames(comparison$groups)),]
  
  lsd.table[i,] <- as.character(comparison$groups$groups)   }

stats.table <- cbind(Classification=rownames(comparison.prop),means.table,sem.table,stats.table, lsd.table)

stats.table[stats.table$p.value.treat1 < 0.05,]
stats.table[stats.table$p.value.treat2 < 0.05,]
stats.table[stats.table$fdr.treat1 < 0.05,]
stats.table[stats.table$fdr.treat2 < 0.05,]


write.table(stats.table, file = "_KEGG_ncbi_L4_stats_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)


P_sig_treat1 <- stats.table[(stats.table[,'p.value.treat1']< 0.05),]
P_sig_treat2 <- stats.table[(stats.table[,'p.value.treat2']< 0.05),]
P_sig_interaction <- stats.table[(stats.table[,'p.value.interaction']< 0.05),]

FDR_treat1 <- stats.table[(stats.table[,'fdr.treat1']< 0.05),]
FDR_treat2 <- stats.table[(stats.table[,'fdr.treat2']< 0.05),]
FDR_interaction <- stats.table[(stats.table[,'fdr.interaction']< 0.05),]


write.table(FDR_treat1, file = "_KEGG_ncbi_L4_stats_FDR_treat1_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)

###############
###############  Boxplot
###############

KEGG_plots <- stats.table[stats.table$fdr.treat1 < 0.05,]
dim(KEGG_plots)
KEGG_plots

for (i in c(1:length(comparison.prop[,1]))) {
  boxplot(as.numeric(comparison.prop[i,])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main=comparison.prop[i],las=1)
  stripchart(as.numeric(comparison.prop[i,])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
             method = "jitter",jitter=0.05, add=T)
  axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
  box(bty="l",lwd=3)
}


i <- KEGG_plots[,1]
for (i in c(1:length(KEGG_plots[,'fdr.treat1']))) {
  boxplot(as.numeric(comparison.prop[i,])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main=KEGG_plots$Classification[i],las=1)
  stripchart(as.numeric(comparison.prop[i,])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
             method = "jitter",jitter=0.05, add=T)
  axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
  box(bty="l",lwd=3)
}


png(filename="_K00008_L_iditol_2_dehydrogenase_boxplot.png",height=5000, width=5000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["KEGG;Metabolism;Carbohydrate metabolism;ko00040 Pentose and glucuronate interconversions;K00008 L-iditol 2-dehydrogenase [EC:1.1.1.14];",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="K00008 L-iditol 2-dehydrogenase",las=1)
stripchart(as.numeric(comparison.prop["KEGG;Metabolism;Carbohydrate metabolism;ko00040 Pentose and glucuronate interconversions;K00008 L-iditol 2-dehydrogenase [EC:1.1.1.14];",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()

########################
#########################

#######
#############
##################  PCA
############################################
#############################################################


pca.res <- prcomp(t(comparison.prop),scale=T)

eigs <- pca.res$sdev^2
eigs / sum(eigs)
var.explained <- eigs / sum(eigs)
var.explained <- var.explained*100


cols <- c("darkorange3","darkorchid2","cornflowerblue","chartreuse4")[as.factor(mapping$Description)]


plot3d(pca.res$x[,1:3],col=cols,type="s",size=1)



png("_KEGG_ncbi_L4_PCA.png",height=4000,width=4000,pointsize=80)

scatter3D(pca.res$x[,3],pca.res$x[,1],pca.res$x[,2], col = cols, colvar = NA, colkey = F, pch=16,theta=30,phi=30,bty="g",cex=1.5,lwd=8,
          xlab=paste("\nPC1 (",round(as.numeric(var.explained[1]),1),"%)",sep=""),
          ylab=paste("\nPC2 (",round(as.numeric(var.explained[2]),1),"%)",sep=""),
          zlab=paste("\nPC3 (",round(as.numeric(var.explained[3]),1),"%)",sep=""))
legend(x=0.27,y=-0.39, levels(as.factor(mapping$Description)),bty="n", pch = 19, col = c("darkorange3","darkorchid2","cornflowerblue","chartreuse4"), cex=1.1)


dev.off()

################

################
################
################  EdgeR alternate analysis method
################
################


group <- as.factor(mapping$Description) 
group.design <- model.matrix(~group)

comparison.counts <- KEGG.L4

comparison.counts <- comparison.counts[rowSums(comparison.counts) > 20,]
dim(comparison.counts)

comparison.y <- DGEList(counts=comparison.counts, group=group)
comparison.keep <- rowSums(cpm(comparison.y)>1) >= 5
comparison.y <- comparison.y[comparison.keep, , keep.lib.sizes=FALSE]


dim(comparison.y)


head(comparison.y$samples)
head(comparison.y$counts)[,1:3]

comparison.y <- edgeR::calcNormFactors(comparison.y)

comparison.y <- estimateDisp(comparison.y, group.design)

# plotBCV(comparison.y)
# plotMD(comparison.y)

comparison.logcpm <- cpm(comparison.y, prior.count=1, log=TRUE)
comparison.cpm <- cpm(comparison.y, prior.count=1, log=F)

comparison.fit <- glmQLFit(comparison.y, group.design)

#plotQLDisp(comparison.fit)

## -------------------------------------------
## --------- Conduct pairwise statistical tests for a given coefficient or contrast
## -------------------------------------------
# baseline = 1
# coef=2; compare 2 vs 1
# coef=3; compare 3 vs 1
# contrast=c(0,-1,1); compare 3 vs 2
# coef=2:4; to find variables different between any of the 4 groups
#
# In this example, where whe have control, treatment2, treatment3, treatment4, then;
# coef=2 is control vs treatment2
# coef=3 is control vs treatment3
# coef=4 is control vs treatment4
# contrast=c(0,-1,1) is treatment3 vs treatment2
#

comparison.qlf <- glmQLFTest(comparison.fit, coef=3)   # coef indicates which coefficients (comparisons) to be tested. 

comparison.qlf.results <- topTags(comparison.qlf,n=nrow(comparison.qlf))

## --------- Filter results for FDR >0.05 AND LogCPM > 0
## ---------
## --------- Positive logFC indicates higher expression/abundance in treatment group and negative logFC indicates higher in baseline
## --------- e.g. For CONTROL vs DIARR, a +ve logFC indicates higher abundance in DIARR, while a -ve logFC indicates higher in CONTROL
## --------- 

comparison.qlf.results <- comparison.qlf.results[comparison.qlf.results$table$FDR < 0.05 & abs(comparison.qlf.results$table$logFC) > 0.585,]
comparison.qlf.results$table
head(comparison.qlf.results$table)
rownames(head(comparison.qlf.results$table))

write.table(comparison.qlf.results, file = "_KEGG_ncbi_L4_comparisons_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)

#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[1],]~group, main=rownames(head(comparison.qlf.results$table))[1])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[2],]~group, main=rownames(head(comparison.qlf.results$table))[2])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[3],]~group, main=rownames(head(comparison.qlf.results$table))[3])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[4],]~group, main=rownames(head(comparison.qlf.results$table))[4])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[5],]~group, main=rownames(head(comparison.qlf.results$table))[5])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[6],]~group, main=rownames(head(comparison.qlf.results$table))[6])
 
for(row_num in 1:nrow(comparison.results)){
  row_name<-(rownames(comparison.results)[row_num])
  for(i in 1:nrow(comparison.logcpm)){
    row_comparison<-(rownames(comparison.logcpm)[i])
    #print(row_name)
    if (row_name == row_comparison){
      boxplot(comparison.logcpm[i,]~group, main=row_comparison)
    }
  }  
}


###############
###############  Boxplot
###############



for(row_num in 1:nrow(comparison.qlf.results)){
  row_name<-(rownames(comparison.qlf.results)[row_num])
  for(i in 1:nrow(comparison.logcpm)){
    row_comparison<-(rownames(comparison.logcpm)[i])
    #print(row_name)
    mypath <- file.path(paste("_KEGG_ncbi_L4_comparisons_", row_comparison, ".png", sep = ""))
    if (row_name == row_comparison){
      png(filename=mypath,height=5000, width=5000, pointsize=50)   
      par(mar=c(12,5,5,4))
      boxplot(as.numeric(comparison.logcpm[i,])*100~group,frame=F,xaxt="n",ylab="Comparison",xlab="",outline=F,boxwex=0.5,main=row_comparison,las=1)
      stripchart(as.numeric(comparison.logcpm[i,])*100~group, vertical=T,frame=F, pch=1,xaxt="n", ylab="Comparison",lwd=5,las=1, xlim=c(0.5,4.1),
                 method = "jitter",jitter=0.05, add=T)
      axis(side=1, at=c(1:4),labels = levels(group),cex.axis=1.2, las=2)
      box(bty="l",lwd=3)
      dev.off()
    }
  }  
}

################
############## Heatmaps SEEDL4.   low abundance >20, proportions
###############

Heat_SEEDL4 <- comparison.prop
dim(Heat_SEEDL4)

Heat_SEEDL4 <- Heat_SEEDL4[,order(mapping$Description)]
head(Heat_SEEDL4)
Heat_SEEDL4.mapping <- mapping[order(mapping$Description),]
head(Heat_SEEDL4.mapping)
table(mapping$Description)
typeof(Heat_SEEDL4)
typeof(Heat_SEEDL4.mapping$Seq_label)
dim(Heat_SEEDL4)
dim(Heat_SEEDL4.mapping)


colnames(Heat_SEEDL4) <- Heat_SEEDL4.mapping$Description[match(colnames(Heat_SEEDL4),Heat_SEEDL4.mapping$Seq_label)]
head(Heat_SEEDL4)
dim(Heat_SEEDL4)
colnames(Heat_SEEDL4)

col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(as.matrix(Heat_SEEDL4), Rowv = NA, Colv = NA, col = col)
data.dist <- vegdist(Heat_SEEDL4, method = "bray")
row.clus <- hclust(data.dist, "aver")
heatmap(as.matrix(Heat_SEEDL4[,order(mapping$Description)]), Rowv = as.dendrogram(row.clus), Colv = NA, col = col, margins = c(10, 3))
########################
#########################


######################################   BRITE
######################################


comparison.counts <- BRITE.L4
dim(comparison.counts)


################
################  Permutation ANOVA
################

##  Filter to remove very low abundance taxa

comparison.counts <- comparison.counts[rowSums(comparison.counts) > 20,] # 20 is a chosen threshold when there  are lower numbers use >50 for larger number of genes  
dim(comparison.counts)

i <- 1
filter.rare <- 0
for(i in 1:nrow(comparison.counts))  {
  filter.rare[i] <- length(comparison.counts[i,][comparison.counts[i,] > 1])  }
filter.rare

comparison.counts <- comparison.counts[filter.rare > 5,]

dim(comparison.counts)


##  Convert to proportions

comparison.prop <- prop.table(as.matrix(comparison.counts), 2)

colSums(comparison.prop)

##########

fact1 <- as.factor(mapping$treat1)
fact2 <- as.factor(mapping$treat2)

groups <- as.factor(mapping$Description)

group.size <- table(groups)
group.size

means.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(means.table) <- paste(levels(groups),".mean",sep="")

sem.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(sem.table) <- paste(levels(groups),".sem",sep="")

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  means.table[i,] <- tapply(as.numeric(comparison.prop[i,]),groups,mean)*100  }


i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  sem.table[i,] <- tapply(as.numeric(comparison.prop[i,]),groups,sd)*100 /sqrt(group.size) }


stats.table <- as.data.frame(matrix(1, ncol = 6, nrow = nrow(comparison.prop)))
colnames(stats.table) <- c("p.value.treat1","p.value.treat2","p.value.interaction","fdr.treat1","fdr.treat2","fdr.interaction")

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  
  reptest <- as.numeric(comparison.prop[i,])
  stat.test <- unlist(summary(aovp(reptest ~ fact1 * fact2, perm="Prob", maxIter=1000000)))
  stats.table$p.value.treat1[i] <- stat.test["Pr(Prob)1"] 
  stats.table$p.value.treat2[i] <- stat.test["Pr(Prob)2"]
  stats.table$p.value.interaction[i] <- stat.test["Pr(Prob)3"]
}

stats.table$fdr.treat1 <- p.adjust(stats.table$p.value.treat1, method = "BH", n=nrow(stats.table))
stats.table$fdr.treat2 <- p.adjust(stats.table$p.value.treat2, method = "BH", n=nrow(stats.table))
stats.table$fdr.interaction <- p.adjust(stats.table$p.value.interaction, method = "BH", n=nrow(stats.table))


lsd.table <- as.data.frame(matrix(1, ncol = length(levels(groups)), nrow = nrow(comparison.prop)))
colnames(lsd.table) <- levels(groups)

i <- 1
for(i in c(1:length(comparison.prop[,1])))  {
  
  reptest <- as.numeric(comparison.prop[i,])
  ANOVA <- aov(reptest~groups)
  
  comparison <- LSD.test(ANOVA,"groups",group=T)
  comparison$groups$groups <- as.character(comparison$groups$groups)
  comparison$groups <- comparison$groups[order(rownames(comparison$groups)),]
  
  lsd.table[i,] <- as.character(comparison$groups$groups)   }

stats.table <- cbind(Classification=rownames(comparison.prop),means.table,sem.table,stats.table, lsd.table)

stats.table[stats.table$p.value.treat1 < 0.05,]
stats.table[stats.table$p.value.treat2 < 0.05,]
stats.table[stats.table$fdr.treat1 < 0.05,]
stats.table[stats.table$fdr.treat2 < 0.05,]


write.table(stats.table, file = "_BRITE_ncbi_L4_stats_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)


P_sig_treat1 <- stats.table[(stats.table[,'p.value.treat1']< 0.05),]
P_sig_treat2 <- stats.table[(stats.table[,'p.value.treat2']< 0.05),]
P_sig_interaction <- stats.table[(stats.table[,'p.value.interaction']< 0.05),]

FDR_treat1 <- stats.table[(stats.table[,'fdr.treat1']< 0.05),]
FDR_treat2 <- stats.table[(stats.table[,'fdr.treat2']< 0.05),]
FDR_interaction <- stats.table[(stats.table[,'fdr.interaction']< 0.05),]


write.table(FDR_treat1, file = "_BRITE_ncbi_L4_stats_FDR_treat1_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)

###############
###############  Boxplot
###############

BRITE_plots <- stats.table[stats.table$fdr.treat1 < 0.05,]
dim(BRITE_plots)
BRITE_plots

i <- BRITE_plots[,1]
for (i in c(1:length(BRITE_plots[,'fdr.treat1']))) {
  boxplot(as.numeric(comparison.prop[i,])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main=BRITE_plots$Classification[i],las=1)
  stripchart(as.numeric(comparison.prop[i,])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
             method = "jitter",jitter=0.05, add=T)
  axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
  box(bty="l",lwd=3)
}

########################
#########################

#######
#############
##################  PCA
############################################
#############################################################


pca.res <- prcomp(t(comparison.prop),scale=T)

eigs <- pca.res$sdev^2
eigs / sum(eigs)
var.explained <- eigs / sum(eigs)
var.explained <- var.explained*100


cols <- c("darkorange3","darkorchid2","cornflowerblue","chartreuse4")[as.factor(mapping$Description)]


plot3d(pca.res$x[,1:3],col=cols,type="s",size=1)



png("_BRITE_ncbi_L4_PCA.png",height=4000,width=4000,pointsize=80)

scatter3D(pca.res$x[,3],pca.res$x[,1],pca.res$x[,2], col = cols, colvar = NA, colkey = F, pch=16,theta=30,phi=30,bty="g",cex=1.5,lwd=8,
          xlab=paste("\nPC1 (",round(as.numeric(var.explained[1]),1),"%)",sep=""),
          ylab=paste("\nPC2 (",round(as.numeric(var.explained[2]),1),"%)",sep=""),
          zlab=paste("\nPC3 (",round(as.numeric(var.explained[3]),1),"%)",sep=""))
legend(x=0.27,y=-0.39, levels(as.factor(mapping$Description)),bty="n", pch = 19, col = c("darkorange3","darkorchid2","cornflowerblue","chartreuse4"), cex=1.1)


dev.off()

################

################
################
################  EdgeR alternate analysis method
################
################


group <- as.factor(mapping$Description) 
group.design <- model.matrix(~group)

comparison.counts <- BRITE.L4

comparison.counts <- comparison.counts[rowSums(comparison.counts) > 20,]
dim(comparison.counts)

comparison.y <- DGEList(counts=comparison.counts, group=group)
comparison.keep <- rowSums(cpm(comparison.y)>1) >= 5
comparison.y <- comparison.y[comparison.keep, , keep.lib.sizes=FALSE]


dim(comparison.y)


head(comparison.y$samples)
head(comparison.y$counts)[,1:3]

comparison.y <- edgeR::calcNormFactors(comparison.y)

comparison.y <- estimateDisp(comparison.y, group.design)

# plotBCV(comparison.y)
# plotMD(comparison.y)

comparison.logcpm <- cpm(comparison.y, prior.count=1, log=TRUE)
comparison.cpm <- cpm(comparison.y, prior.count=1, log=F)

comparison.fit <- glmQLFit(comparison.y, group.design)

#plotQLDisp(comparison.fit)

## -------------------------------------------
## --------- Conduct pairwise statistical tests for a given coefficient or contrast
## -------------------------------------------
# baseline = 1
# coef=2; compare 2 vs 1
# coef=3; compare 3 vs 1
# contrast=c(0,-1,1); compare 3 vs 2
# coef=2:4; to find variables different between any of the 4 groups
#
# In this example, where whe have control, treatment2, treatment3, treatment4, then;
# coef=2 is control vs treatment2
# coef=3 is control vs treatment3
# coef=4 is control vs treatment4
# contrast=c(0,-1,1) is treatment3 vs treatment2
#

comparison.qlf <- glmQLFTest(comparison.fit, coef=3)   # coef indicates which coefficients (comparisons) to be tested. 

comparison.qlf.results <- topTags(comparison.qlf,n=nrow(comparison.qlf))

## --------- Filter results for FDR >0.05 AND LogCPM > 0
## ---------
## --------- Positive logFC indicates higher expression/abundance in treatment group and negative logFC indicates higher in baseline
## --------- e.g. For CONTROL vs DIARR, a +ve logFC indicates higher abundance in DIARR, while a -ve logFC indicates higher in CONTROL
## --------- 

comparison.qlf.results <- comparison.qlf.results[comparison.qlf.results$table$FDR < 0.05 & abs(comparison.qlf.results$table$logFC) > 0.585,]
comparison.qlf.results$table
head(comparison.qlf.results$table)
rownames(head(comparison.qlf.results$table))

write.table(comparison.qlf.results, file = "_BRITE_ncbi_L4_comparisons_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)

#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[1],]~group, main=rownames(head(comparison.qlf.results$table))[1])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[2],]~group, main=rownames(head(comparison.qlf.results$table))[2])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[3],]~group, main=rownames(head(comparison.qlf.results$table))[3])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[4],]~group, main=rownames(head(comparison.qlf.results$table))[4])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[5],]~group, main=rownames(head(comparison.qlf.results$table))[5])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[6],]~group, main=rownames(head(comparison.qlf.results$table))[6])

for(row_num in 1:nrow(comparison.results)){
  row_name<-(rownames(comparison.results)[row_num])
  for(i in 1:nrow(comparison.logcpm)){
    row_comparison<-(rownames(comparison.logcpm)[i])
    #print(row_name)
    if (row_name == row_comparison){
      boxplot(comparison.logcpm[i,]~group, main=row_comparison)
    }
  }  
}


###############
###############  Boxplot
###############



for(row_num in 1:nrow(comparison.qlf.results)){
  row_name<-(rownames(comparison.qlf.results)[row_num])
  for(i in 1:nrow(comparison.logcpm)){
    row_comparison<-(rownames(comparison.logcpm)[i])
    #print(row_name)
    mypath <- file.path(paste("_KEGG_ncbi_L4_comparisons_", row_comparison, ".png", sep = ""))
    if (row_name == row_comparison){
      png(filename=mypath,height=5000, width=5000, pointsize=50)   
      par(mar=c(12,5,5,4))
      boxplot(as.numeric(comparison.logcpm[i,])*100~group,frame=F,xaxt="n",ylab="Comparison",xlab="",outline=F,boxwex=0.5,main=row_comparison,las=1)
      stripchart(as.numeric(comparison.logcpm[i,])*100~group, vertical=T,frame=F, pch=1,xaxt="n", ylab="Comparison",lwd=5,las=1, xlim=c(0.5,4.1),
                 method = "jitter",jitter=0.05, add=T)
      axis(side=1, at=c(1:4),labels = levels(group),cex.axis=1.2, las=2)
      box(bty="l",lwd=3)
      dev.off()
    }
  }  
}

################
############## Heatmaps SEEDL4.   low abundance >20, proportions
###############

Heat_SEEDL4 <- comparison.prop
dim(Heat_SEEDL4)

Heat_SEEDL4 <- Heat_SEEDL4[,order(mapping$Description)]
head(Heat_SEEDL4)
Heat_SEEDL4.mapping <- mapping[order(mapping$Description),]
head(Heat_SEEDL4.mapping)
table(mapping$Description)
typeof(Heat_SEEDL4)
typeof(Heat_SEEDL4.mapping$Seq_label)
dim(Heat_SEEDL4)
dim(Heat_SEEDL4.mapping)


colnames(Heat_SEEDL4) <- Heat_SEEDL4.mapping$Description[match(colnames(Heat_SEEDL4),Heat_SEEDL4.mapping$Seq_label)]
head(Heat_SEEDL4)
dim(Heat_SEEDL4)
colnames(Heat_SEEDL4)

col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(as.matrix(Heat_SEEDL4), Rowv = NA, Colv = NA, col = col)
data.dist <- vegdist(Heat_SEEDL4, method = "bray")
row.clus <- hclust(data.dist, "aver")
heatmap(as.matrix(Heat_SEEDL4[,order(mapping$Description)]), Rowv = as.dendrogram(row.clus), Colv = NA, col = col, margins = c(10, 3))
########################
#########################










