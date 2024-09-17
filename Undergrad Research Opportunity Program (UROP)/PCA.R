title: "PCA"
author: "Emily Van Buren"
used and practiced by: "Mariah Noelle"

library(readr)
library(tidyr)
library(dplyr)
library(mixOmics)
library(ggrepel)
library(PCAtools)
library("ggalt")
library(DESeq2)
library(sva)

# set working directory 
setwd("/Users/marielle/Documents/Mydlarz Lab/UROP/PCA and IPCA")

X <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Normalization and DESeq/All/normalized_counts_vstCounts_Diseased_all_35.csv"))
row.names(X) <- X$Entry
X <- X[,-c(1)]

metadata_diseased <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/metadata35.csv", row.names = "Sample_ID"))
Y <- metadata_diseased
rownames(Y) == colnames(X)

# PCA
p <- pca(X,metadata = Y,center = TRUE, removeVar = 0.1)

# Scree Plot
pdf(file = "screeplot.pdf", height = 6, width = 8)
screeplot(p)
dev.off()

# Biplot
pdf(file = "biplot.pdf", height = 8, width = 8)
biplot(p,
       lab = p$metadata$Host,
       colby = "Disease", colkey = c('SCTLD'='#5ab4ac','WP'='#d8b365'),
       colLegendTitle = "Disease Condition", 
       hline = 0, vline = 0, 
       shape = "Disease", 
       shapekey = c('SCTLD'= 19, 'WP'= 17),
       shapeLegendTitle = "Disease Study", 
       title = 'Diseased Samples Gene Expression PCA',
       legendPosition = 'right')
dev.off()

## Eigen Correlation plots
horn <- parallelPCA(X)
horn$n

pdf(file = "eigencorplot.pdf", height = 6, width = 8)
eigencorplot(p,
             components = getComponents(p, 1:horn$n),
             metavars = c('Disease','Host'),
             col = c('white', 'cornsilk1', 'gold', 'forestgreen', 'darkgreen'),
             cexCorval = 1.2,
             fontCorval = 2,
             posLab = 'all',
             rotLabX = 45,
             scale = TRUE,
             main = bquote(Principal ~ component ~ Pearson ~ r^2 ~ clinical ~ correlates),
             plotRsquared = TRUE,
             corFUN = 'pearson',
             corUSE = 'pairwise.complete.obs',
             corMultipleTestCorrection = 'BH',
             signifSymbols = c('****', '***', '**', '*', ''),
             signifCutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1))

dev.off()

# Plot loadings 
plotloadings(p,
             components = getComponents(p, c(1:7)),
             rangeRetain = 0.2,
             labSize = 4.0,
             absolute = FALSE,
             title = 'Loadings plot',
             subtitle = 'Misc PCs',
             caption = 'Top 10% variables',
             shape = 23, shapeSizeRange = c(1, 8),
             col = c('white', 'pink'),
             drawConnectors = FALSE)

plotloadings(p,
             components = getComponents(p, c(2)),
             rangeRetain = 0.1, absolute = TRUE,
             col = c('black', 'pink', 'red4'),
             drawConnectors = TRUE, labSize = 4) + coord_flip()

# what is this list for? 
x <- list(
  PC2 = c("P53568", "Q90705", "Q58DT1", "Q01833", "Q962Q6", 
          "Q8T5Z4", "Q6Q420", "P30050", "O93477", "Q27HV0",
          "Q95WA0", "P11833", "Q5RKI1", "P85118", "P04844", 
          "Q9VXX8", "Q7QH73", "P62755", "O57592", "Q4R4D3", 
          "Q76I25", "Q3SZ90", "Q4GXG7", "B8UU51", "O08700")
  





