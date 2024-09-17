title:"SVM Group"
author:"Emily Van Buren"
output: html_document
used and practiced by: "Mariah Noelle"

# Set up R and load packages
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("sigFeature")
install.packages("magrittr")
install.packages("dplyr")

library(magrittr)
library(sigFeature)
library(dplyr)

# Load and organize data
setwd("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression")
gene_counts <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE Individual/Normalized Counts/normalized_counts_rlog_DISEASED_cladeC.csv", row.names = "Entry"))
X <- as.data.frame(t(gene_counts))
dim(X)

metadata_diseased <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Differential Expression/DE Individual/Normalized Counts/metadata_cladeC.csv", row.names = "Sample_ID"))
Y <- as.factor(metadata_diseased$Disease)

# Obtain P-values for whole dataset through t-test of genes
pvals <- sigFeaturePvalue(X,Y)

hist(unlist(pvals),
     # breaks=seq(0,0.4,0.04),
     col="skyblue",
     xlab="p value",ylab="Frequency",main="")

system.time(sigfeatureRankedList <- sigFeature(X, Y))
sigfeatureRankedList <- sigFeature(X, Y) #roughly 5-ish hours with 19k
print(sigfeatureRankedList[1:10])

# Run initial SVM models 
library(e1071)
sigFeature.model=svm(X[ ,sigfeatureRankedList[1:1000]], Y,
                     type="C-classification", kernel="linear")
summary(sigFeature.model)

pred <- predict(sigFeature.model, X[ ,sigfeatureRankedList[1:1000]])
table(pred,Y)

featureRankedList <- svmrfeFeatureRanking(X, Y)
print("Top 10 features are printed below:")
print(featureRankedList[1:10])

RFE.model=svm(X[ ,featureRankedList[1:1000]], Y,
              type="C-classification", kernel="linear")
summary(RFE.model)

pred <- predict(RFE.model, X[ ,sigfeatureRankedList[1:1000]])
table(pred,Y)

# Identify P-values for the models developed 
pvalsigFe <- sigFeaturePvalue(X, Y, 100, sigfeatureRankedList)
pvalRFE   <- sigFeaturePvalue(X, Y, 100, featureRankedList)
par(mfrow=c(1,2))

hist(unlist(pvalsigFe),breaks=50, col="skyblue", main=paste("sigFeature"),
     xlab="p value")
hist(unlist(pvalRFE),breaks=50, col="skyblue",
     main=paste("SVM-RFE"), xlab="p value")

mytitle <- 'Box Plot'
boxplot(unlist(pvalsigFe), unlist(pvalRFE), main=mytitle,
        names=c("sigFeature", "SVM-RFE"),
        ylab="p value", ylim=c(min(unlist(pvalsigFe)), max(unlist(pvalRFE))))
stripchart(unlist(pvalsigFe), vertical=TRUE, method="jitter", add=TRUE, pch=16,
           col=c('green'))
stripchart(unlist(pvalRFE), vertical=TRUE, at=2, method="jitter", add=TRUE,
           pch=16, col=c('blue'))
grid(nx=NULL, ny=NULL, col="black", lty="dotted")

# Create heatmap of the top 20 significant genes
library("pheatmap")
library("RColorBrewer")
pheatmap(X[ ,sigfeatureRankedList[1:20]], scale="row",
         clustering_distance_rows="correlation")

pheatmap(X[ ,featureRankedList[1:20]], scale="row",
         clustering_distance_rows="correlation")

testing_genes <- sigfeatureRankedList[1:25]
#testing_genes <- testing_genes$Entry
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

testing_genes <- featureRankedList[1:25]
#testing_genes <- testing_genes$Entry
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

# Save everything as an .RData file as a checkpoint 
ls()
save(featureRankedList, gene_counts, metadata_diseased, mytitle, pred, pvalRFE, pvals, pvalsigFe, RFE.model, sigFeature.model, sigfeatureRankedList, X, Y, file="/Users/marielle/Documents/Mydlarz Lab/Disease Study/SVM/SVM Clade C/SVM_cladeC_rough.RData")

# Obtain the statistical results for  models - DO THIS FOR CLADE C
set.seed(1234)
results = sigFeature.enfold(X, Y, "kfold", 10)

str(results[1])

FeatureBasedonFrequency <- sigFeatureFrequency(X, results, 400, 400, pf=FALSE)
str(FeatureBasedonFrequency[1])

inputdata <- data.frame(Y=as.factor(Y), X=X)
featsweepSigFe = lapply(1:400, sigCVError, FeatureBasedonFrequency, inputdata)

str(featsweepSigFe[1])

PlotErrors(featsweepSigFe, 0, 0.4)

# Save everything as an RData file again
ls()
save(featsweepSigFe, FeatureBasedonFrequency, featureRankedList, gene_counts, inputdata, metadata_diseased, mytitle, pred, pvalRFE, pvals, pvalsigFe, results, RFE.model, sigFeature.model, sigfeatureRankedList, X, Y, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/SVM/SVM Shared/SVM_shared.RData")

