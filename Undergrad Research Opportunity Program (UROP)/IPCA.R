title: "IPCA"
author: "Emily Van Buren"
used and practiced by: "Mariah Noelle"

library(mixOmics)

# set working directory 
setwd("/Users/marielle/Documents/Mydlarz Lab/UROP/PCA and IPCA")

X <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Normalization and DESeq/All/normalized_counts_vstCounts_Diseased_all_35.csv"))
row.names(X) <- X$Entry
X <- X[,-c(1)]
X <- t(X)
dim(X)

metadata_diseased <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/UROP/Counts/metadata35.csv", row.names = "Sample_ID"))
Y <- metadata_diseased
rownames(Y) == colnames(X)

# IPCA
coral.ipca <- ipca(X, ncomp = 10, mode="deflation", scale = FALSE)
plot(coral.ipca)

# Biplot
pdf(file = "ipca_biplot.pdf", height = 8, width = 8)
plotIndiv(coral.ipca, 
          ind.names = Y$Species, 
          group= Y$Disease, 
          legend = TRUE, 
          title = 'IPCA of Coral Control Transcripts ALL', 
          col = c("#5ab4ac", "#d8b365")
          # pch = c('SCTLD'= 16,'WP'=17)[as.character(Y$Disease)],
)
dev.off()

head(selectVar(coral.ipca, comp = 1)$value)
# Kurtosis 
coral.ipca$kurtosis

barplot(coral.ipca$kurtosis, ylim = c(0, 15),
        #names.arg = seq(1, 10, 1), 
        xlab = "Independent Principal Components", 
        ylab = "Kurtosis value")

head(selectVar(coral.ipca, comp = 1)$value)



