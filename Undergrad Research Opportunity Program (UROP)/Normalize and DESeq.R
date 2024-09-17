title: "Normalize and DESeq"
author: "Mariah Noelle"
date: "2023-11-7"

library(readr)
library(tidyr)
library(dplyr)
library(mixOmics)
library(ggrepel)
library(PCAtools)
library("ggalt")
library(DESeq2)
library(sva)

# Run this pipeline twice for ALL and SHARED

setwd("/Users/marielle/Documents/Mydlarz Lab/UROP/Normalization and DESeq/All")

colData <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/metadata35.csv", row.names = "Sample_ID"))
colData$Disease
countData <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/gene_counts_diseased_all_raw_35.csv", row.names = "Entry"))
batch <- c(rep(1,16),rep(2,18))
print(batch)

countData <- as.matrix(countData)

adjusted <- ComBat_seq(countData,batch=batch,group = NULL)
adjusted <- as.data.frame(adjusted)

### write.csv(adjusted, "gene_counts_batch_diseased_all_50.csv") 

# If vst does not work, use this to filter out samples (need to use the overall file to pick out counts):
# countData <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/gene_counts_diseased_all_raw_50.csv", row.names = "Entry"))
# countData <- as.matrix(countData)
# adjusted <- ComBat_seq(countData,batch=batch,group = NULL)
# adjusted <- as.data.frame(adjusted)
# sums <- as.data.frame(colSums(adjusted))
# print(max(sums, na.rm=TRUE)-min(sums, na.rm=TRUE))
# colnames(sums) <- c("gene_counts")

# sum_10k <- sums %>% filter(sums$gene_counts > 10000)
# sum_10k <- sums %>% filter(sums%gene_counts > 15000)
# sum_20k <- sums %>% filter(sums$gene_counts > 20000) 
# sum_30k <- sums %>% filter(sums$gene_counts > 30000) 
# sum_35k <- sums %>% filter(sums$gene_counts > 35000) # this worked with all 
# sum_40k <- sums %>% filter(sums$gene_counts > 40000) 
# sum_50k <- sums %>% filter(sums$gene_counts>50000) # works with 50k but this is too large

# ...re make new data with just these genes...

# normalization 
row.names(colData) == colnames(countData)

dds <- DESeqDataSetFromMatrix(countData = round(countData),
                              colData = colData,
                              design = ~ Species_host + Clade + Disease)
dds <- dds[ rowMeans(counts(dds)) > 10, ]
vst <- vst(dds, blind=FALSE) 
vstCounts <- assay(vst)
write.csv(vstCounts, file = "normalized_counts_vstCounts_Diseased_all_35.csv")
# Manually add in "Entry" column again ^^^

# DESeq model 

dds <- DESeq(dds)
resultsNames(dds)

res_fate <- results(dds, contrast = c("Disease","WP","SCTLD"))
resordered_fate <- as.data.frame(res_fate[order(res_fate$padj),])
fate_DEGs <- resordered_fate %>% filter(padj < 0.05)
write.csv(fate_DEGs, file = "fate_sig_DEGs_all_35.csv")

uniprot <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/uniprot-filtered-reviewed_yes.csv")
fate_DEGs$Entry <- rownames(fate_DEGs)
sig_master <- merge(fate_DEGs,uniprot, by="Entry")
logs <- read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Normalization and DESeq/All/normalized_counts_vstCounts_Diseased_all_35.csv")
sig_master <- merge(sig_master,logs, by="Entry")
write.csv(sig_master, file = "/Users/marielle/Documents/Mydlarz Lab/UROP/Normalization and DESeq/All/fate_annotated_sig_DEGs_all_35.csv")

DEGs <- sig_master[,c(1,3,7,10)]
DEGs$Protein.names <- gsub("\\s*\\([^\\)]+\\)","",as.character(DEGs$Protein.names))

upReg <- subset(sig_master, log2FoldChange > 0)
downReg <- subset(sig_master, log2FoldChange < 0)

summary(res_fate)

ls()
save(colData, countData, dds, downReg, fate_DEGs, metadata, 
     metadata_diseased, res_fate, resordered_fate, upReg, 
     vst, vstCounts, file = "DESeq_WPvsSCTLD_all_35.RData")



