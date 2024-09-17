title: "Tximport_Normalization"
author: "Emily Van Buren"
date: "2023-11-5"
output: html_document
used and practiced by: "Mariah Noelle"

# Load packages and setup R - do this for symbionts A, B, C, and D for all 7 species
library(tximportData)
library(tximport)
library(DESeq2)
library(readr)
library(dplyr)
library(ggrepel)
library(PCAtools)  
setwd("/Users/marielle/Documents/Mydlarz Lab/Symbiont Study/TXImport/ofav/B")

Ofav_B_samples <- read.table("sampleInfo_ofav.csv", header = TRUE)
Ofav_B_samples
Ofav_B_files <- file.path(Ofav_B_samples$sample, "quant.sf")
names(Ofav_B_files) <- paste0("sample", 1:8)
all(file.exists(Ofav_B_files))

Ofav_B_tx2gene <- read.csv("symbB_txt2gene.csv")
Ofav_B_tx2gene <- Ofav_B_tx2gene[,c(1,2)]
head(Ofav_B_tx2gene)
Ofav_B_txi <- tximport(Ofav_B_files, type = 'salmon', tx2gene=Ofav_B_tx2gene)

# reading in files with read_tsv
# 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
# removing duplicated transcript rows from tx2gene
# transcripts missing from tx2gene: 37765
# summarizing abundance
# summarizing counts
# summarizing length

names(Ofav_B_txi)
tail(Ofav_B_txi$counts)
write.csv(Ofav_B_txi, file = "Ofav_B_counts.csv") # manually add Entry for row names in excel 

# Step 1: Filter for significant gene annotations 
quants <- read.csv("Ofav_B_counts.csv") 
head(quants)
tail(quants)

annot <- read.csv("symbB_annotation_filtered.csv")
names(annot)
head(annot)
tail(annot)

master <- merge(quants,annot,by="Entry")
tail(master)

#The merge function will sometimes create duplicates. Remove these duplicates and only keep annotations with a sufficient evalue (<1e-5).
master <- master[order(master$Entry, abs(master$E.value) ), ] ### sort first
master
master <- master[ !duplicated(master$Entry), ]  ### Keep lowest evalue

write.csv(master, file="Ofav_B_master_no_filtering.csv")
master <- master %>% filter(E.value < 0.00001)
write.csv(master, file="Ofav_B_master_evalue_filtered.csv")
