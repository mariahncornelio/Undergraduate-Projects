title: "Normalize Counts & DEGs"
author: "Emily Van Buren"
date: "2023-04-06"
output: html_document
used and practiced by: "Mariah Noelle"

# Load Packages
library(readr)
library(tidyr)
library(dplyr)
library(mixOmics)
library(ggrepel)
library(PCAtools)
library("ggalt")
library(DESeq2)
library(sva)
library(pheatmap)
library(openxlsx)

setwd("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression")

# RUN THIS PIPELINE 3 TIMES (THIS MEANS MAKING 3
# SEPARATE FOLDERS FOR ALL, SHARED, AND ORTHOLOGS)

# Merge Datasets and Import Count Matrices
# Categorize by species first (clade A, B, C, D) and then merge

cladeA_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/SCTLD Counts/cladeA_counts_SCTLD.csv"))
cladeC_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/SCTLD Counts/cladeC_counts_SCTLD.csv"))
cladeA_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/WP Counts/cladeA_counts_WP.csv"))
cladeB_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/WP Counts/cladeB_counts_WP.csv"))
cladeC_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/WP Counts/cladeC_counts_WP.csv"))

# Slim down to entry and control counts only, which I feel that I have already done? Then merge.
# I was just told that it won't matter what host that it comes from because the batch 
# affect only focuses on the disease study, not the host like cnat, oann, mcav, or past. 

gene_counts <- merge(cladeA_WP,cladeB_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cladeC_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cladeA_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cladeC_SCTLD, by="Entry", all=TRUE)
rownames(gene_counts) <- gene_counts$Entry
gene_counts[is.na(gene_counts)] = 0

# Individual ALL count matrices

#cladeC_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/SCTLD Counts/cladeC_counts_SCTLD.csv"))
#cladeC_WP <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/WP Counts/cladeC_counts_WP.csv"))

#gene_counts <- merge(cladeC_WP, cladeC_SCTLD, by="Entry", all=TRUE)
#rownames(gene_counts) <- gene_counts$Entry 
#gene_counts[is.na(gene_counts)] = 0


# Save file of gene counts mixed together - save as an Excel first to take out excess column
#write.xlsx(gene_counts, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Disease Fate/Normalized Counts/All/gene_counts_all_DISEASED.xlsx")
#write.csv(gene_counts, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/Normalized Counts/All/gene_counts_all_DISEASED_all.csv")

#Shared count matrices 

cladeA_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/SCTLD Counts/cladeA_counts_SCTLD.csv"))
cladeC_SCTLD <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/SCTLD Counts/cladeC_counts_SCTLD.csv"))
cladeA_WP <- as.data.frame (read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/WP Counts/cladeA_counts_WP.csv"))
cladeB_WP <- as.data.frame (read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/WP Counts/cladeB_counts_WP.csv"))
cladeC_WP <- as.data.frame (read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/WP Counts/cladeC_counts_WP.csv"))

gene_counts <- merge(cladeA_WP,cladeB_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cladeC_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cladeA_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cladeC_SCTLD, by="Entry", all=TRUE)
rownames(gene_counts) <- gene_counts$Entry
gene_counts_noNA <- na.omit(gene_counts)

# Also save as xlsx (Excel) file first to take out the excess column
#write.xlsx(gene_counts_noNA, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Disease Fate/Normalized Counts/Shared/gene_counts_shared_DISEASED.xlsx")
#write.csv(gene_counts_noNA, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/Normalized Counts/Shared/gene_counts_DISEASED_shared.csv") 

# Remove batch effect for both _all and _shared
# Check if case-sensitive correct by doing rownames(metadata) == colnames(countdata)
# Start new pipeline from HERE!!!

colData <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/Normalized Counts/metadata.csv", row.names = "Sample_ID"))
colData$Disease
countData <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/Normalized Counts/Shared/gene_counts_DISEASED_shared.csv", row.names = "Entry"))
batch <- c(rep(1,7),rep(2,15))
print(batch)

countData <- as.matrix(countData)
adjusted <- ComBat_seq(countData,batch=batch,group = NULL)

# Rlog Normalization - don't use rlog for rld, use vsm

dds <- DESeqDataSetFromMatrix(countData = round(adjusted),
                              colData = colData,
                              design = ~ Clade + Disease)
dds <- dds[ rowMeans(counts(dds)) > 10, ]
rld <- vst(dds, blind=FALSE)
rlogCounts <- assay(rld)

# Save in normalized counts as normalized_counts_rlog_DISEASED_cladeC.csv (MAKE SURE TO ADD "ENTRY" COLUMN)
#write.xlsx(rlogCounts, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE Individual/Normalized Counts/All/normalized_counts_rlog_DISEASED_cladeC.xlsx")
#write.csv(rlogCounts, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/Normalized Counts/Shared/normalized_counts_rlog_DISEASED_shared.csv")
# Then save RData Normalized Data
#save(adjusted, batch, countData, gene_counts, gene_counts_noNA, dds, rlogCounts, rld, cladeA_SCTLD, cladeA_WP, cladeB_WP, cladeC_SCTLD, cladeC_WP, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/Normalized Counts/Shared/Normalized_data_shared.RData")
#save as RData for SVM 
#save(rld, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/rld_shared.RData")

#For the individual clade
#save(adjusted, batch, countData, gene_counts, dds, rlogCounts, rld, cladeC_SCTLD, cladeC_WP, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE Individual/Normalized Counts/All/Normalized_data_cladeC.RData")

# Onto the Differential Expression Study! Run DESeq with ~Clade + Disease???
# DEG Study for all 

dds <- DESeq(dds)
resultsNames(dds)

# DEG's of WP vs SCTLD, obtain DEGs for Disease_WP_vs_SCTLD differential expressed

res_fate <- results(dds, contrast = c("Disease", "WP", "SCTLD"))
resordered_fate <- as.data.frame(res_fate[order(res_fate$padj),])
fate_DEGs <- resordered_fate %>% filter(padj < 0.05) 
#write.csv(fate_DEGs, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/DEGs/Shared/fate_sig_DEGs_shared.csv")

# Annotate WP vs SCTLD DEGs???

uniprot <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/uniprot-filtered-reviewed_yes.csv")
fate_DEGs$Entry <- rownames(fate_DEGs)
sig_master <- merge(fate_DEGs, uniprot, by="Entry")
logs <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/Normalized Counts/Shared/normalized_counts_rlog_DISEASED_shared.csv")
sig_master <- merge(sig_master,logs, by="Entry")
#write.csv(sig_master, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/DEGs/Shared/fate_annotated_sig_DEGs_shared.csv")

# Output DEGs
DEGs <- sig_master[,c(1,3,7,10)]
DEGs$Protein.names <- gsub("\\s*\\([^\\)]+\\)", "", as.character(DEGs$Protein.names))

# Since there are 1929 DEGs for all, you can do upReg > 10 instead of 0 to play around with in heatmaps
# 1320 are upReg and 601 are downReg
# For shared DEGs, there are 185; 84 are upReg and 100 are downReg
upReg <- subset(sig_master, log2FoldChange > 0)
downReg <- subset(sig_master, log2FoldChange < 0)

summary(res_fate)

# Write a heatmap - up DEGs
library(pheatmap)
testing_genes <- upReg
testing_genes <- testing_genes$Entry 
mat <- assay(rld)[ testing_genes, ]
mat <- mat - rowMeans(mat)
anno <- as.data.frame(colData(rld)[, c("Species_host", "Disease", "Clade")])
colnames(anno) <- c("Species Host", "Disease", "Clade")
row.names(anno) <- colnames(rld)

pheatmap(mat,
         scale = "row",
         annotation_col = anno,
         color = colorRampPalette(c("#2166ac", "white", "#b2182b"))(200),
         legend = TRUE,
         legend_labels = anno,
         cluster_cols = TRUE,
         cluster_rows = TRUE, 
         fontsize_row = 7, 
         fontsize_col = 8, 
         cellwidth = 8, 
         cellheight = 10, 
         treeheight_row = 3,
         treeheight_col = 40
)

# Save upReg heatmap as a PDF file or just export it 
#pdf(file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Disease Fate/DEGs/Shared/heatmap_sig_DEGs_UP_fate_shared.pdf", height = 20, width = 10)
#pheatmap(mat,
        #scale = "row",
         #annotation_col = anno,
         #color = colorRampPalette(c("#2166ac", "white", "#b2182b"))(200),
         #legend = TRUE, 
         #legend_labels = anno, 
         #cluster_cols = TRUE,
         #cluster_rows = TRUE,
         #fontsize_row = 7,
         #fontsize_col = 8,
         #cellwidth = 8,
         #cellheight = 10, 
         #treeheight_row = 3,
         #treeheight_col = 40
#)
#dev.off()

# Write a heatmap - down DEGs
library(pheatmap)
testing_genes <- downReg
testing_genes <- testing_genes$Entry
mat <- assay(rld)[ testing_genes, ]
mat <- mat - rowMeans(mat)
anno <- as.data.frame(colData(rld)[, c("Species_host", "Disease", "Clade")])
colnames(anno) <- c("Species Host", "Disease", "Clade")
row.names(anno) <- colnames(rld)

pheatmap(mat,
         scale = "row",
         annotation_col = anno,
         color = colorRampPalette(c("#2166ac", "white", "#b2182b"))(200),
         legend = TRUE,
         legend_labels = anno,
         cluster_cols = TRUE,
         cluster_rows = TRUE,
         fontsize_row = 7,
         fontsize_col = 8,
         cellwidth = 8,
         cellheight = 10,
         treeheight_row = 3,
         treeheight_col = 40
)

# Save downReg heatmap as a PDF file or just export it 
#pdf(file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Disease Fate/DEGs/Shared/heatmap_sig_DEGs_DOWN_fate_shared.pdf", height = 8, width = 10)
#pheatmap(mat,
         #scale = "row",
         #annotation_col = anno,
         #color = colorRampPalette(c("#2166ac", "white", "#b2182b"))(200),
         #legend = TRUE,
         #legend_labels = anno,
         #cluster_cols = TRUE, 
         #cluster_rows = TRUE,
         #fontsize_row = 7,
         #fontsize_col = 8,
         #cellwidth = 8,
         #cellheight = 10,
         #treeheight_row = 3,
         #treeheight_col = 40
#)
#dev.off()

# Save RData DESeq2
ls()
save(dds, rlogCounts, rld, anno, colData, countData, DEGs, downReg, fate_DEGs, logs, mat, res_fate, resordered_fate, rld, rlogCounts, sig_master, testing_genes, uniprot, upReg, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE All and Shared/DEGs//DESeq_data_shared.RData")

# For individual 
#save(dds, rlogCounts, rld, anno, colData, countData, DEGs, downReg, fate_DEGs, logs, mat, res_fate, resordered_fate, rld, rlogCounts, sig_master, testing_genes ,uniprot, upReg, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE Individual/DEGs//DESEq_data_cladeC.RData")

# Pipeline finished. 

