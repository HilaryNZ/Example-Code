setwd('C:/Users/dewhursth/Documents/T331/NanoString')
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("NanoStringDiff")
install.packages('NanoStringNorm')
install.packages('NACHO')

library(NanoStringDiff)
library(NanoStringNorm)
library(NACHO)

df <- read.csv("NS_normalised.csv", header = TRUE)
head(df)
colnames(df)
rownames(df)
dim(df)

mapping<-read.csv("T331_NSmapping.csv", header = TRUE)
head(mapping)
colnames(mapping)
dim(mapping)
