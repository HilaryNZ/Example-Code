setwd("C:/Users")

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

mapping <- data.frame(read.delim(file="_microbiome_mapping.txt", sep="\t",stringsAsFactors=F))
mapping <- mapping[order(mapping[,1]),]
head(mapping)


L2 <- data.frame(read.delim(file="_ncbi_phylum.txt", sep="\t"))
rownames(L2) <- L2[,1]
L2 <- L2[,2:ncol(L2)]
colnames(L2) <- strsplit2(colnames(L2),"_P")[,1]

L5 <- data.frame(read.delim(file="_ncbi_family.txt", sep="\t"))
rownames(L5) <- L5[,1]
L5 <- L5[,2:ncol(L5)]
colnames(L5) <- strsplit2(colnames(L5),"_P")[,1]

L6 <- data.frame(read.delim(file="_ncbi_genus.txt", sep="\t"))
rownames(L6) <- L6[,1]
L6 <- L6[,2:ncol(L6)]
colnames(L6) <- strsplit2(colnames(L6),"_P")[,1]

L7 <- data.frame(read.delim(file="_ncbi_species.txt", sep="\t"))
rownames(L7) <- L7[,1]
L7 <- L7[,2:ncol(L7)]
colnames(L7) <- strsplit2(colnames(L7),"_P")[,1]

mapping$Sequence_ID <- mapping$Seq_label
mapping <- mapping[mapping$Sequence_ID %in% colnames(L7),]

mapping$Sequence_ID
colnames(L7)
colnames(L6)
colnames(L5)
colnames(L2)

table(mapping$Description)

############

plot.micro <- L2
plot.names <- strsplit2(rownames(plot.micro),";")

i <- 1
for(i in 1:ncol(plot.names)) {
  
  plot.names[,ncol(plot.names)] <- ifelse(plot.names[,ncol(plot.names)] == "", plot.names[,ncol(plot.names)-i],plot.names[,ncol(plot.names)]) 
}
head(plot.names)

plot.micro <- plot.micro[,order(mapping$Description)]
plot.mapping <- mapping[order(mapping$Description),]
table(mapping$Description)

rownames(plot.micro) <- paste(plot.names[,4], plot.names[,ncol(plot.names)], sep="; ")
plot.micro <- plot.micro[rownames(plot.micro) != "; NCBI",]
plot.micro <- plot.micro[rownames(plot.micro) != "; cellular organisms",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Bacteria",]
plot.micro <- plot.micro[rownames(plot.micro) != "; Not assigned",]
plot.micro <- plot.micro[rownames(plot.micro) != "environmental samples <bacteria,superkingdom Bacteria>; environmental samples <bacteria,superkingdom Bacteria>",]

plot.micro <- prop.table(as.matrix(plot.micro),2)


qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro[,1]))  ## Pick some colours for each taxa you want to plot

png("_ncbi_L2_barplot.png", height=4000, width=8000, pointsize=50)

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


png("_ncbi_L2_barplot_mean.png", height=4000, width=4000, pointsize=50)

par(mar=c(5,6,5,27),xpd=T) 
barplot(as.matrix(plot.micro.mean)*100,horiz=F,las=1,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.mean),cex.names=1.2,main="")
axis(2, at = c(0,20,40,60,80,100) ,las=1, cex.axis = 1,lwd=1)
mtext("Percent", side=2, line=1.5, cex=1, at=50)
legend(x=ncol(plot.micro.mean)*1.23,y=100,legend=rev(rownames(plot.micro)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()


#######################
##################

plot.micro.firmicutes <- L5
plot.micro.firmicutes  <- prop.table(as.matrix(plot.micro.firmicutes ),2)

plot.micro.firmicutes <- plot.micro.firmicutes[grep("Firmicutes", rownames(plot.micro.firmicutes)),]
plot.names.firmicutes <- strsplit2(rownames(plot.micro.firmicutes),";")



plot.names.firmicutes[,2] <- ifelse(plot.names.firmicutes[,2] == "", paste("Unclassified ", plot.names.firmicutes[,1], sep=""),plot.names.firmicutes[,2])
plot.names.firmicutes[,3] <- ifelse(plot.names.firmicutes[,3] == "", paste("Unclassified ", plot.names.firmicutes[,2], sep=""),plot.names.firmicutes[,1])
plot.names.firmicutes[,4] <- ifelse(plot.names.firmicutes[,4] == "", paste("Unclassified ", plot.names.firmicutes[,3], sep=""),plot.names.firmicutes[,4])
plot.names.firmicutes[,5] <- ifelse(plot.names.firmicutes[,5] == "", paste("Unclassified ", plot.names.firmicutes[,4], sep=""),plot.names.firmicutes[,5])
plot.names.firmicutes[,6] <- ifelse(plot.names.firmicutes[,6] == "", paste("Unclassified ", plot.names.firmicutes[,5], sep=""),plot.names.firmicutes[,6])
plot.names.firmicutes[,7] <- ifelse(plot.names.firmicutes[,7] == "", paste("Unclassified ", plot.names.firmicutes[,6], sep=""),plot.names.firmicutes[,7])
plot.names.firmicutes[,8] <- ifelse(plot.names.firmicutes[,8] == "", paste("Unclassified ", plot.names.firmicutes[,7], sep=""),plot.names.firmicutes[,8])
plot.names.firmicutes[,9] <- ifelse(plot.names.firmicutes[,9] == "", paste("Unclassified ", plot.names.firmicutes[,8], sep=""),plot.names.firmicutes[,9])
plot.names.firmicutes[,10] <- ifelse(plot.names.firmicutes[,10] == "", paste("Unclassified ", plot.names.firmicutes[,9], sep=""),plot.names.firmicutes[,10])
plot.names.firmicutes[,11] <- ifelse(plot.names.firmicutes[,11] == "", paste("Unclassified ", plot.names.firmicutes[,10], sep=""),plot.names.firmicutes[,11])

plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] <- gsub("Unclassified Unclassified Unclassified Unclassified Unclassified Unclassified", "Unclassified", plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] )
plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] <- gsub("Unclassified Unclassified Unclassified Unclassified Unclassified", "Unclassified", plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] )
plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] <- gsub("Unclassified Unclassified Unclassified Unclassified", "Unclassified", plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] )
plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] <- gsub("Unclassified Unclassified Unclassified", "Unclassified", plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] )
plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] <- gsub("Unclassified Unclassified", "Unclassified", plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] )
plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] <- gsub("Unclassified", "Unclassified", plot.names.firmicutes[,1:ncol(plot.names.firmicutes)] )

head(plot.names.firmicutes)

plot.micro.firmicutes <- plot.micro.firmicutes[,order(mapping$Description)]
plot.mapping.firmicutes <- mapping[order(mapping$Description),]
table(plot.mapping.firmicutes$Description)

### needed if you need to change row names of gene function from one column to another
plot.names.firmicutes[13,8] <- plot.names.firmicutes[13,9]  
plot.names.firmicutes[14,8] <- plot.names.firmicutes[14,9]
plot.names.firmicutes[15,8] <- plot.names.firmicutes[15,9]
plot.names.firmicutes[16,8] <- plot.names.firmicutes[16,9]

rownames(plot.micro.firmicutes) <- plot.names.firmicutes[,8]

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.firmicutes[,1]))  ## Pick some colours for each taxa you want to plot


plot.micro.firmicutes.mean <- aggregate(t(plot.micro.firmicutes),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.firmicutes.mean) <- plot.micro.firmicutes.mean$Group.1
plot.micro.firmicutes.mean <- subset(plot.micro.firmicutes.mean, select =-Group.1)
plot.micro.firmicutes.mean <- t(plot.micro.firmicutes.mean)

png("_ncbi_L5_firmicutes_barplot_mean.png", height=4000, width=4000, pointsize=50)

par(mar=c(5,6,5,30),xpd=T) 
barplot(as.matrix(plot.micro.firmicutes.mean)*100,horiz=F,las=1,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.firmicutes.mean),cex.names=1.2,main="")
axis(2, at = c(0,10,20,30,40) ,las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=1.5, cex=1, at=20)
legend(x=ncol(plot.micro.firmicutes.mean)*1.23,y=40,legend=rev(rownames(plot.micro.firmicutes.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()



#######################
##################

plot.micro.bacteroidetes <- L5
plot.micro.bacteroidetes  <- prop.table(as.matrix(plot.micro.bacteroidetes ),2)

plot.micro.bacteroidetes <- plot.micro.bacteroidetes[grep("Bacteroidetes", rownames(plot.micro.bacteroidetes)),]
plot.names.bacteroidetes <- strsplit2(rownames(plot.micro.bacteroidetes),";")



plot.names.bacteroidetes[,2] <- ifelse(plot.names.bacteroidetes[,2] == "", paste("Unclassified ", plot.names.bacteroidetes[,1], sep=""),plot.names.bacteroidetes[,2])
plot.names.bacteroidetes[,3] <- ifelse(plot.names.bacteroidetes[,3] == "", paste("Unclassified ", plot.names.bacteroidetes[,2], sep=""),plot.names.bacteroidetes[,1])
plot.names.bacteroidetes[,4] <- ifelse(plot.names.bacteroidetes[,4] == "", paste("Unclassified ", plot.names.bacteroidetes[,3], sep=""),plot.names.bacteroidetes[,4])
plot.names.bacteroidetes[,5] <- ifelse(plot.names.bacteroidetes[,5] == "", paste("Unclassified ", plot.names.bacteroidetes[,4], sep=""),plot.names.bacteroidetes[,5])
plot.names.bacteroidetes[,6] <- ifelse(plot.names.bacteroidetes[,6] == "", paste("Unclassified ", plot.names.bacteroidetes[,5], sep=""),plot.names.bacteroidetes[,6])
plot.names.bacteroidetes[,7] <- ifelse(plot.names.bacteroidetes[,7] == "", paste("Unclassified ", plot.names.bacteroidetes[,6], sep=""),plot.names.bacteroidetes[,7])
plot.names.bacteroidetes[,8] <- ifelse(plot.names.bacteroidetes[,8] == "", paste("Unclassified ", plot.names.bacteroidetes[,7], sep=""),plot.names.bacteroidetes[,8])
plot.names.bacteroidetes[,9] <- ifelse(plot.names.bacteroidetes[,9] == "", paste("Unclassified ", plot.names.bacteroidetes[,8], sep=""),plot.names.bacteroidetes[,9])
plot.names.bacteroidetes[,10] <- ifelse(plot.names.bacteroidetes[,10] == "", paste("Unclassified ", plot.names.bacteroidetes[,9], sep=""),plot.names.bacteroidetes[,10])
plot.names.bacteroidetes[,11] <- ifelse(plot.names.bacteroidetes[,11] == "", paste("Unclassified ", plot.names.bacteroidetes[,10], sep=""),plot.names.bacteroidetes[,11])

plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] <- gsub("Unclassified Unclassified Unclassified Unclassified Unclassified Unclassified", "Unclassified", plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] )
plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] <- gsub("Unclassified Unclassified Unclassified Unclassified Unclassified", "Unclassified", plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] )
plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] <- gsub("Unclassified Unclassified Unclassified Unclassified", "Unclassified", plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] )
plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] <- gsub("Unclassified Unclassified Unclassified", "Unclassified", plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] )
plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] <- gsub("Unclassified Unclassified", "Unclassified", plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] )
plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] <- gsub("Unclassified", "Unclassified", plot.names.bacteroidetes[,1:ncol(plot.names.bacteroidetes)] )

head(plot.names.bacteroidetes)

plot.micro.bacteroidetes <- plot.micro.bacteroidetes[,order(mapping$Description)]
plot.mapping.bacteroidetes <- mapping[order(mapping$Description),]
table(plot.mapping.bacteroidetes$Description)

rownames(plot.micro.bacteroidetes) <- plot.names.bacteroidetes[,9]

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]   ##  Set up colour libary
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
plot.micro.cols <- sample(col_vector, length(plot.micro.bacteroidetes[,1]))  ## Pick some colours for each taxa you want to plot


plot.micro.bacteroidetes.mean <- aggregate(t(plot.micro.bacteroidetes),by=list(plot.mapping$Description),FUN=mean)
rownames(plot.micro.bacteroidetes.mean) <- plot.micro.bacteroidetes.mean$Group.1
plot.micro.bacteroidetes.mean <- subset(plot.micro.bacteroidetes.mean, select =-Group.1)
plot.micro.bacteroidetes.mean <- t(plot.micro.bacteroidetes.mean)

png("_ncbi_L5_bacteroidetes_barplot_mean.png", height=4000, width=4000, pointsize=50)

par(mar=c(5,6,5,30),xpd=T) 
barplot(as.matrix(plot.micro.bacteroidetes.mean)*100,horiz=F,las=1,axes=FALSE,xlab="",col=plot.micro.cols ,border=NA,names.arg=colnames(plot.micro.bacteroidetes.mean),cex.names=1.2,main="")
axis(2, at = c(0,5,10,15) ,las=1, cex.axis = 1,lwd=3)
mtext("Percent\n", side=2, line=1.5, cex=1, at=7.5)
legend(x=ncol(plot.micro.bacteroidetes.mean)*1.23,y=15,legend=rev(rownames(plot.micro.bacteroidetes.mean)), bty="n", cex=1.1,col=rev(plot.micro.cols), pch=15)

dev.off()




##################
#################   Choose level for analyses and rename taxa into manageable bites
#################

comparison.counts <- L6
dim(comparison.counts)


################
################  Permutation ANOVA
################

##  Filter to remove very low abundance taxa

comparison.prop <- comparison.prop[rowSums(comparison.counts) > 50,]
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


write.table(stats.table, file = "_microbiome_ncbi_L6_stats_aovp.txt", sep = "\t", row.names = FALSE, quote = FALSE)

boxplot(comparison.prop["Lactobacillus" ,]~group, main="Lactobacillus")
boxplot(comparison.prop["Coprococcus 1" ,]~group, main="Coprococcus 1")


###############
###############  Boxplot
###############


stats.table[stats.table$fdr.treat1 < 0.05,]

png(filename="_Alistipes_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;FCB group;Bacteroidetes/Chlorobi group;Bacteroidetes;Bacteroidia;Bacteroidales;Rikenellaceae;Alistipes;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Alistipes",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;FCB group;Bacteroidetes/Chlorobi group;Bacteroidetes;Bacteroidia;Bacteroidales;Rikenellaceae;Alistipes;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()


png(filename="_Lactobacillus_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Bacilli;Lactobacillales;Lactobacillaceae;Lactobacillus;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Lactobacillus",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Bacilli;Lactobacillales;Lactobacillaceae;Lactobacillus;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()



png(filename="_Streptococcaceae_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Bacilli;Lactobacillales;Streptococcaceae;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Unclassified Streptococcaceae",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Bacilli;Lactobacillales;Streptococcaceae;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()


png(filename="_Lactococcus_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Bacilli;Lactobacillales;Streptococcaceae;Lactococcus;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Lactococcus",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Bacilli;Lactobacillales;Streptococcaceae;Lactococcus;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()


png(filename="_Lachnospiraceae_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Lachnospiraceae;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Unclassified Lachnospiraceae",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Lachnospiraceae;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()


png(filename="_Oscillibacter_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Oscillospiraceae;Oscillibacter;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Oscillibacter",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Oscillospiraceae;Oscillibacter;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()



png(filename="_Faecalibaculum_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Erysipelotrichia;Erysipelotrichales;Erysipelotrichaceae;Faecalibaculum;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Faecalibaculum",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Erysipelotrichia;Erysipelotrichales;Erysipelotrichaceae;Faecalibaculum;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()




png(filename="_Butyrivibrio_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Lachnospiraceae;Butyrivibrio;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Butyrivibrio",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Lachnospiraceae;Butyrivibrio;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
           method = "jitter",jitter=0.05, add=T)

axis(side=1, at=c(1:4),labels=levels(as.factor(mapping$Description)),cex.axis=1.2, las=2)
box(bty="l",lwd=3)
dev.off()


png(filename="_Dorea_boxplot.png",height=3000, width=2000, pointsize=50)    
par(mar=c(12,5,5,4))
boxplot(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Lachnospiraceae;Dorea;",])*100~mapping$Description,frame=F,xaxt="n",ylab="Percent",xlab="",outline=F,boxwex=0.5,main="Dorea",las=1)
stripchart(as.numeric(comparison.prop["NCBI;cellular organisms;Bacteria;Terrabacteria group;Firmicutes;Clostridia;Clostridiales;Lachnospiraceae;Dorea;",])*100~mapping$Description, vertical=T,frame=F, pch=1,xaxt="n", ylab="Percent",lwd=5,las=1, xlim=c(0.5,4.1),
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

shapes <- c(19,18)[as.factor(mapping.T288$Sample_type)]

png("_ncbi_L5_PCA.png",height=4000,width=4000,pointsize=80)

scatter3D(pca.res$x[,1],pca.res$x[,2],pca.res$x[,3], col = cols, colvar = NA, colkey = F, pch=16,theta=30,phi=30,bty="g",cex=1.5,lwd=8,
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

comparison.counts <- L7

comparison.counts <- comparison.counts[rowSums(comparison.counts) > 20,]
dim(comparison.counts)

###### csv file of L7 counts with rowsums > 20 
tax <- as.data.frame(t(comparison.counts))
#write.table(comparison.counts, file = "_microbiome_ncbi_L7_comparison_analysis.txt", sep = "\t", row.names = FALSE, quote = FALSE)
write.csv(tax,"_microbiome_ncbi_L7_comparison_analysis.csv", row.names = TRUE)

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

# plotQLDisp(comparison.fit)

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

#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[1],]~group, main=rownames(head(comparison.qlf.results$table))[1])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[2],]~group, main=rownames(head(comparison.qlf.results$table))[2])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[3],]~group, main=rownames(head(comparison.qlf.results$table))[3])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[4],]~group, main=rownames(head(comparison.qlf.results$table))[4])
#boxplot(comparison.logcpm[rownames(head(comparison.qlf.results$table))[7],]~group, main=rownames(head(comparison.qlf.results$table))[7])


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
    mypath <- file.path(paste("_microbiome_ncbi_L7_comparisons_", row_comparison, ".png", sep = ""))
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