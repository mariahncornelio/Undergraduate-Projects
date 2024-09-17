title: "Merge Counts"
author: "Mariah Noelle"
date: "2023-11-9"

library(readr)
library(tidyr)
library(dplyr)
library(mixOmics)
library(ggrepel)
library(PCAtools)
library("ggalt")
library(DESeq2)
library(sva)

setwd("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts")

# These are the counts for the diseased samples only
# After identifying which samples were diseased, they were split into WP and SCTLD
# This pipeline creates gene counts for ALL and SHARED
# For this run, tehre are no Symbiont D's but check with Emily to make sure

SymbA_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbA_SCTLD.csv"))
SymbB_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbB_SCTLD.csv"))
SymbC_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbC_SCTLD.csv"))
SymbD_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbD_SCTLD.csv"))
SymbA_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbA_WP.csv"))
SymbB_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbB_WP.csv"))
SymbC_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbC_WP.csv"))
SymbD_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/SymbD_WP.csv"))


### All count matrices ###

gene_counts <- merge(SymbA_WP,SymbB_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbC_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbD_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbA_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbB_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbC_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbD_SCTLD, by="Entry", all=TRUE)
rownames(gene_counts) <- gene_counts$Entry
gene_counts[is.na(gene_counts)] = 0

write.csv(gene_counts, file = "/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/gene_counts_diseased_all_raw.csv")
# ^ there may be excess column??? double check and take it out if there is

### Shared count matrices ### 

gene_counts <- merge(SymbA_WP,SymbB_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbC_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbD_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbA_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbB_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbC_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,SymbD_SCTLD, by="Entry", all=TRUE)
rownames(gene_counts) <- gene_counts$Entry
gene_counts_noNA <- na.omit(gene_counts)

write.csv(gene_counts_noNA, file = "/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/gene_counts_diseased_shared_raw.csv") 
# ^ there also may be an excess column. check to take out
