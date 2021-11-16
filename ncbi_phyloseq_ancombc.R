getwd()
setwd("C:/Users/")

dir()
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("phyloseq")
BiocManager::install("ANCOMBC")
install.packages("tidyverse")

library(phyloseq)
library(tidyverse)
library(ANCOMBC)
library(limma)
library(dplyr)
library(caret)
library(e1071)

browseVignettes("ANCOMBC")

###############
############### ANCOM:Analysis of Compositions. ANCOM anlayses log-ratios between all taxa to overcome compositional data issue
###############

##  Read in data table fromn megan taxa output at the species level


L7 <- data.frame(read.delim(file="ncbi_species.txt", sep="\t"))
rownames(L7) <- L7[,1]
L7 <- L7[,2:ncol(L7)]
colnames(L7) <- strsplit2(colnames(L7),"_P")[,1]


###  Create taxonomy matrix.  The trouble with the megan output is that not all the taxa have the same hierarchy.
###  E.g. some taxa include superphylum, others do not.  This means that simply splitting the taxonomy names by ";" is not enough.


tax_mat  = matrix(1, nrow = nrow(L7), ncol = 8)
colnames(tax_mat) <- c("Domain","Superphylum" , "Phylum", "Class", "Order", "Family", "Genus", "Species")
  
taxa.names <- strsplit2(rownames(L7),";")

dim(taxa.names)

taxa.names[,4]

tax_mat[,"Domain"] <- taxa.names[,3]
tax_mat[,"Superphylum"] <- ""
tax_mat[,"Phylum"] <- taxa.names[,5]
tax_mat[,"Class"] <- taxa.names[,6]
tax_mat[,"Order"] <- taxa.names[,7]
tax_mat[,"Family"] <- taxa.names[,8]
tax_mat[,"Genus"] <- taxa.names[,9]
tax_mat[,"Species"] <- taxa.names[,10]


tax_mat[1,"Domain"] <- "NCBI"
tax_mat[2,"Domain"] <- "Cellular organisms"
tax_mat[nrow(tax_mat),"Domain"] <- "Not assigned"

tax_mat[grep("FCB group",taxa.names[,4]),"Superphylum"] <- taxa.names[grep("FCB group",taxa.names[,4]),4]
tax_mat[grep("Terrabacteria",taxa.names[,4]),"Superphylum"] <- taxa.names[grep("Terrabacteria",taxa.names[,4]),4]

tax_mat[grep("Proteobacteria",taxa.names[,4]),"Phylum"] <- taxa.names[grep("Proteobacteria",taxa.names[,4]),4]
tax_mat[grep("FCB group",taxa.names[,4]),"Phylum"] <- taxa.names[grep("FCB group",taxa.names[,4]),6]

tax_mat[grep("Proteobacteria",taxa.names[,4]),"Class"] <- taxa.names[grep("Proteobacteria",taxa.names[,4]),5]
tax_mat[grep("FCB group",taxa.names[,4]),"Class"] <- taxa.names[grep("FCB group",taxa.names[,4]),7]

tax_mat[grep("Proteobacteria",taxa.names[,4]),"Order"] <- taxa.names[grep("Proteobacteria",taxa.names[,4]),6]
tax_mat[grep("FCB group",taxa.names[,4]),"Order"] <- taxa.names[grep("FCB group",taxa.names[,4]),8]

tax_mat[grep("Proteobacteria",taxa.names[,4]),"Family"] <- taxa.names[grep("Proteobacteria",taxa.names[,4]),7]
tax_mat[grep("FCB group",taxa.names[,4]),"Family"] <- taxa.names[grep("FCB group",taxa.names[,4]),9]

tax_mat[grep("Proteobacteria",taxa.names[,4]),"Genus"] <- taxa.names[grep("Proteobacteria",taxa.names[,4]),8]
tax_mat[grep("FCB group",taxa.names[,4]),"Genus"] <- taxa.names[grep("FCB group",taxa.names[,4]),10]

tax_mat[grep("Proteobacteria",taxa.names[,4]),"Species"] <- taxa.names[grep("Proteobacteria",taxa.names[,4]),9]
tax_mat[grep("FCB group",taxa.names[,4]),"Species"] <- taxa.names[grep("FCB group",taxa.names[,4]),11]

###  Fill the empty taxonomic ranks with the one before it and paste "unclassified"

tax_mat[,"Superphylum"] <- ifelse(tax_mat[,"Superphylum"] == "", paste("unclassified ", tax_mat[,"Domain"], sep=""),tax_mat[,"Superphylum"])
tax_mat[,"Phylum"] <- ifelse(tax_mat[,"Phylum"] == "", paste("unclassified ", tax_mat[,"Superphylum"], sep=""),tax_mat[,"Phylum"])
tax_mat[,"Class"] <- ifelse(tax_mat[,"Class"] == "", paste("unclassified ", tax_mat[,"Phylum"], sep=""),tax_mat[,"Class"])
tax_mat[,"Order"] <- ifelse(tax_mat[,"Order"] == "", paste("unclassified ", tax_mat[,"Class"], sep=""),tax_mat[,"Order"])
tax_mat[,"Family"] <- ifelse(tax_mat[,"Family"] == "", paste("unclassified ", tax_mat[,"Order"], sep=""),tax_mat[,"Family"])
tax_mat[,"Genus"] <- ifelse(tax_mat[,"Genus"] == "", paste("unclassified ", tax_mat[,"Family"], sep=""),tax_mat[,"Genus"])
tax_mat[,"Species"] <- ifelse(tax_mat[,"Species"] == "", paste("unclassified ", tax_mat[,"Genus"], sep=""),tax_mat[,"Species"])

###  Get rid of all the "unclassified unclassified" strings until only singular "unclassified" strings are present.

tax_mat <- gsub("unclassified unclassified","unclassified",tax_mat)
tax_mat <- gsub("unclassified unclassified","unclassified",tax_mat)
tax_mat <- gsub("unclassified unclassified","unclassified",tax_mat)
tax_mat <- gsub("unclassified unclassified","unclassified",tax_mat)
tax_mat <- gsub("unclassified unclassified","unclassified",tax_mat)
tax_mat <- gsub("unclassified unclassified","unclassified",tax_mat)
tax_mat <- gsub("unclassified unclassified","unclassified",tax_mat)
tax_mat <- gsub("unclassified Not assigned","Not assigned",tax_mat)
tax_mat <- gsub("unclassified","Unclassified",tax_mat)

rownames(tax_mat) <- paste0("taxon", 1:nrow(tax_mat))


###  Reading in metadata

mapping <- data.frame(read.delim(file="microbiome_mapping.txt", sep="\t",stringsAsFactors=F))
mapping <- mapping[order(mapping[,1]),]
head(mapping)
rownames(mapping) <- mapping$Seq_label

###  Create phyloseq object
OTU <- otu_table(L7, taxa_are_rows = TRUE)
rownames(OTU) <- paste0("taxon", 1:nrow(OTU))
META <- sample_data(mapping)
TAX <- tax_table(tax_mat)
physeq <- phyloseq(OTU, META, TAX)

######  Made a table for exporting dataset for machine learning
write.table(tax_mat, file = paste("taxa_matrix_",tax_level,".txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE) ## t see what tax_max looked like
rownames(OTU)
rownames(TAX)
merge <- merge(as.data.frame(TAX), as.data.frame(OTU), by='row.names', all=TRUE)
write.table(merge, file = paste("TAX_OTU_matrix_",tax_level,".txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)
###############
###############  Aggregate to a particular taxonomic rank for analysis
###############



tax_level <- "Genus"  
tax_level <- "Species"

physeq.t <- tax_glom(physeq, tax_level)

dim(physeq.t@otu_table)
physeq.t@tax_table



###############
###############  The ANCOM test itself.  Firstly looking at Group1 (normal vs alternative)
###############

compare <- "Group1"

out <- ancombc(phyloseq = physeq.t, formula = compare ,
              p_adj_method = "BH", zero_cut = 0.90, lib_cut = 20,
              group = compare , struc_zero = TRUE, neg_lb = FALSE,
              tol = 1e-5, max_iter = 5000, conserve = TRUE,
              alpha = 0.05, global = FALSE)  #global = TRUE for more than two groups

# out$res_global  # for more than two groups
head(out$feature_table)
head(out$res$p_val)
head(out$res$q_val)
head(out$res$diff_abn)

head(out$res$beta)

percents.table <- prop.table(as.matrix(out$feature_table),2)*100

means.table <- as.data.frame(matrix(1, ncol = length(levels(as.factor(unlist(physeq.t@sam_data[,compare])))), nrow = nrow(percents.table)))
colnames(means.table) <- paste(levels(as.factor(unlist(physeq.t@sam_data[,compare]))),".mean",sep="")

sem.table <- as.data.frame(matrix(1, ncol = length(levels(as.factor(unlist(physeq.t@sam_data[,compare])))), nrow = nrow(percents.table)))
colnames(sem.table) <- paste(levels(as.factor(unlist(physeq.t@sam_data[,compare]))),".sem",sep="")

i <- 1
for(i in c(1:nrow(percents.table)))  {
  means.table[i,] <- tapply(percents.table[i,],as.factor(unlist(physeq.t@sam_data[,compare])),mean) }


i <- 1
for(i in c(1:nrow(percents.table)))  {
  sem.table[i,] <- tapply(percents.table[i,],as.factor(unlist(physeq.t@sam_data[,compare])),sd) /sqrt(table(unlist(physeq.t@sam_data[,compare]))) }

stats.table <-cbind(Taxon=physeq.t@tax_table[rownames(physeq.t@tax_table) %in% rownames(out$feature_table),tax_level],
                    means.table,sem.table,p=out$res$p_val[,1],fdr=out$res$q_val[,1],diff_abn=out$res$diff_abn[,1])


Group1_stats <- select(stats.table, alternative_p_value = 6, alternative_FDR_value = 7)
head(Group1_stats)

write.table(Group1_stats, file = paste("taxa_stats_ancom_",compare,"_",tax_level,".txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)

Group1_p <- Group1_stats[Group1_stats$alternative_p_value < 0.05,]
Group1_fdr <- Group1_stats[Group1_stats$alternative_FDR_value < 0.05,]
Group1_diff <- Group1_stats[Group1_stats$diff_abn == TRUE,]

###########
##########   Boxplots of Group1_FDR < 0.05
##########

par(mfrow = c(2,5))
for(row_num in 1:nrow(Group1_fdr)){
  row_name<-(rownames(Group1_fdr)[row_num])
  for(i in 1:nrow(percents.table)){
    row_percent<-(rownames(percents.table)[i])
    #print(row_name)
    if (row_name == row_percent){
       boxplot(percents.table[i,]~mapping$Group1,main=stats.table$Species[i], xlab = " ", ylab = "Percent")  
       stripchart(percents.table[i,]~mapping$Group1, vertical = TRUE, method = "jitter",
                 pch = 19, add = TRUE, col = 3:length(unique(mapping$Group1)))
       
    }
  }  
}


for(row_num in 1:nrow(Group1_fdr)){
  row_name<-(rownames(Group1_fdr)[row_num])
  for(i in 1:nrow(percents.table)){
    row_percent<-(rownames(percents.table)[i])
    #print(row_name)
    mypath <- file.path(paste("ANCOMBC_Group1_Species_", stats.table$Species[i], ".png", sep = ""))
    if (row_name == row_percent){
      png(filename=mypath,height=5000, width=5000, pointsize=50)   
      par(mar=c(12,5,5,4))
      boxplot(as.numeric(percents.table[i,])*100~mapping$Group1,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main=stats.table$Species[i],las=1)
      stripchart(as.numeric(percents.table[i,])*100~mapping$Group1, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
                 method = "jitter",jitter=0.05, add=T)
      axis(side=1, at=c(1:2),labels=levels(as.factor(mapping$Group1)),cex.axis=1.2, las=2)
      box(bty="l",lwd=3)
      dev.off()
    }
  }  
}

##########
##########  Then looking the impact of the Group2 
##########

compare <- "Group2"

out <- ancombc(phyloseq = physeq.t, formula = compare ,
               p_adj_method = "BH", zero_cut = 0.90, lib_cut = 20,
               group = compare , struc_zero = TRUE, neg_lb = FALSE,
               tol = 1e-5, max_iter = 5000, conserve = TRUE,
               alpha = 0.05, global = FALSE)  #global = TRUE for more than two groups

# out$res_global  # for more than two groups
head(out$feature_table)
head(out$res$p_val)
head(out$res$q_val)
head(out$res$diff_abn)
head(out$res$beta) #  Coefficients 

percents.table <- prop.table(as.matrix(out$feature_table),2)*100

means.table <- as.data.frame(matrix(1, ncol = length(levels(as.factor(unlist(physeq.t@sam_data[,compare])))), nrow = nrow(percents.table)))
colnames(means.table) <- paste(levels(as.factor(unlist(physeq.t@sam_data[,compare]))),".mean",sep="")

sem.table <- as.data.frame(matrix(1, ncol = length(levels(as.factor(unlist(physeq.t@sam_data[,compare])))), nrow = nrow(percents.table)))
colnames(sem.table) <- paste(levels(as.factor(unlist(physeq.t@sam_data[,compare]))),".sem",sep="")


i <- 1
for(i in c(1:nrow(percents.table)))  {
  means.table[i,] <- tapply(percents.table[i,],as.factor(unlist(physeq.t@sam_data[,compare])),mean) }


i <- 1
for(i in c(1:nrow(percents.table)))  {
  sem.table[i,] <- tapply(percents.table[i,],as.factor(unlist(physeq.t@sam_data[,compare])),sd) /sqrt(table(unlist(physeq.t@sam_data[,compare]))) }

stats.table <-cbind(Taxon=physeq.t@tax_table[rownames(physeq.t@tax_table) %in% rownames(out$feature_table),tax_level],
                    means.table,sem.table,p=out$res$p_val[,1],fdr=out$res$q_val[,1],diff_abn=out$res$diff_abn[,1])

Group2_stats <- select(stats.table, Gr2_p_value = 6, Gr2_FDR_value = 7)
head(Group2_stats)

write.table(Group2_stats, file = paste("taxa_stats_ancom_",compare,"_",tax_level,".txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)

Group2_p <- Group2_stats[Group2_stats$Gr2_p_value < 0.05,]
Group2_fdr <- Group2_stats[Group2_stats$Gr2_FDR_value < 0.05,]
Group2_diff <- Group2_stats[Group2_stats$diff_abn == TRUE,]

###########
##########   Group2_FDR < 0.05
##########
par(mfrow = c(1,2))
for(row_num in 1:nrow(Group2_fdr)){
  row_name<-(rownames(Group2_fdr)[row_num])
  for(i in 1:nrow(percents.table)){
    row_percent<-(rownames(percents.table)[i])
    #print(row_name)
    if (row_name == row_percent){
      boxplot(percents.table[i,]~mapping$Group2,main=stats.table$Species[i], xlab = " ", ylab = "Percent")
      stripchart(percents.table[i,]~mapping$Group2, vertical = TRUE, method = "jitter",
                 pch = 19, add = TRUE, col = 3:length(levels(percents.table[i,]~mapping$Group2)))
    }
  }  
}

for(row_num in 1:nrow(Group2_fdr)){
  row_name<-(rownames(Group2_fdr)[row_num])
  for(i in 1:nrow(percents.table)){
    row_percent<-(rownames(percents.table)[i])
    #print(row_name)
    mypath <- file.path(paste("ANCOMBC_Group2_", stats.table$Genus[i], ".png", sep = ""))
    if (row_name == row_percent){
      png(filename=mypath,height=5000, width=5000, pointsize=50)   
      par(mar=c(12,5,5,4))
      boxplot(as.numeric(percents.table[i,])*100~mapping$Group2,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main=stats.table$Genus[i],las=1)
      stripchart(as.numeric(percents.table[i,])*100~mapping$Group2, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
                 method = "jitter",jitter=0.05, add=T)
      axis(side=1, at=c(1:2),labels=levels(as.factor(mapping$Group2)),cex.axis=1.2, las=2)
      box(bty="l",lwd=3)
      dev.off()
    }
  }  
}


##########
##########  ANCOM cannot do 2x2 factor analysis, so here we just treat the different groups as different levels of a single factor
##########  
##########



compare <- "Group1_Group2"

out <- ancombc(phyloseq = physeq.t, formula = compare ,
               p_adj_method = "BH", zero_cut = 0.90, lib_cut = 20,
               group = compare , struc_zero = TRUE, neg_lb = FALSE,
               tol = 1e-5, max_iter = 5000, conserve = TRUE,
               alpha = 0.05, global = TRUE)  #global = TRUE for more than two groups


out$res_global  # for more than two groups
head(out$feature_table)
head(out$res$p_val)
head(out$res$q_val)
head(out$res$diff_abn)
#head(out$res$beta)


percents.table <- prop.table(as.matrix(out$feature_table),2)*100

means.table <- as.data.frame(matrix(1, ncol = length(levels(as.factor(unlist(physeq.t@sam_data[,compare])))), nrow = nrow(percents.table)))
colnames(means.table) <- paste(levels(as.factor(unlist(physeq.t@sam_data[,compare]))),".mean",sep="")

sem.table <- as.data.frame(matrix(1, ncol = length(levels(as.factor(unlist(physeq.t@sam_data[,compare])))), nrow = nrow(percents.table)))
colnames(sem.table) <- paste(levels(as.factor(unlist(physeq.t@sam_data[,compare]))),".sem",sep="")


i <- 1
for(i in c(1:nrow(percents.table)))  {
  means.table[i,] <- tapply(percents.table[i,],as.factor(unlist(physeq.t@sam_data[,compare])),mean) }


i <- 1
for(i in c(1:nrow(percents.table)))  {
  sem.table[i,] <- tapply(percents.table[i,],as.factor(unlist(physeq.t@sam_data[,compare])),sd) /sqrt(table(unlist(physeq.t@sam_data[,compare]))) }

stats.table <-cbind(Taxon=physeq.t@tax_table[rownames(physeq.t@tax_table) %in% rownames(out$feature_table),tax_level],
                    means.table,sem.table,p=out$res_global$p_val,fdr=out$res_global$q_val)

Gr1_Gr2_stats <- select(stats.table, PD_p_value = 10, PD_FDR_value = 11)
head(Gr1_Gr2__stats)

write.table(Gr1_Gr2_stats, file = paste("taxa_stats_ancom_",compare,"_",tax_level,".txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)

Group1_Group2_p <- Gr1_Gr2_stats[Gr1_Gr2_stats$PD_p_value < 0.05,]
Group1_Group2_fdr <- Gr1_Gr2_stats[Gr1_Gr2_stats$PD_FDR_value < 0.05,]
write.table(Gr1_Gr2_fdr, file = paste("ANCOMBC_Gr1_Gr2_Species.txt",sep=""), sep = "\t", row.names = FALSE, quote = FALSE)
###########
##########   Boxplots of Group1_Group2_FDR < 0.05
##########

par(mfrow = c(2,4))
for(row_num in 1:nrow(Group1_Group2_fdr)){
  row_name<-(rownames(Group1_Group2_fdr)[row_num])
  for(i in 1:nrow(percents.table)){
    row_percent<-(rownames(percents.table)[i])
    #print(row_name)
    if (row_name == row_percent){
      boxplot(percents.table[i,]~mapping$Group1_Group2,main=stats.table$Species[i], xlab = " ", ylab = "Percent")
      stripchart(percents.table[i,]~mapping$Group1_Group2, vertical = TRUE, method = "jitter",
                 pch = 19, add = TRUE, col = 3:length(levels(percents.table[i,]~mapping$Group1_Group2)))
    }
  }  
}


for(row_num in 1:nrow(Group1_Group2_fdr)){
  row_name<-(rownames(Group1_Group2_fdr)[row_num])
  for(i in 1:nrow(percents.table)){
    row_percent<-(rownames(percents.table)[i])
    #print(row_name)
    mypath <- file.path(paste("ANCOMBC_Gr1_Gr2_Species_", stats.table$Species[i], ".png", sep = ""))
    if (row_name == row_percent){
      png(filename=mypath,height=5000, width=5000, pointsize=50)   
      par(mar=c(12,5,5,4))
      boxplot(as.numeric(percents.table[i,])*100~mapping$Group1_Group2,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main=stats.table$Species[i],las=1)
      stripchart(as.numeric(percents.table[i,])*100~mapping$Group1_Group2, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
                 method = "jitter",jitter=0.05, add=T)
      axis(side=1, at=c(1:2),labels=levels(as.factor(mapping$Group1_Group2)),cex.axis=1.2, las=2)
      box(bty="l",lwd=3)
      dev.off()
    }
  }  
}


