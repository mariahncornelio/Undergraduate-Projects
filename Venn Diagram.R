title: "Venn Diagrams" 
author: "Emily Van Buren"
date: "2023-06-22"
output: html_document
used and practiced by: "Mariah Noelle"

# Converting SVM outputs into files to load into this pipeline
# write out sigFeatures
library(dplyr)
uniprot <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/uniprot-filtered-reviewed_yes.csv")

# SVM only (do for all and shared) - DOING THE ALL ONLY LOADED 1:99, SO MAY HAVE TO RE-DO
sigFeature_top100_shared <-as.data.frame(X[ ,sigfeatureRankedList[1:100]])
sigFeature_top100_shared <- as.list.data.frame(colnames(sigFeature_top100_shared))
sigFeature_top100_shared <- filter(uniprot, Entry %in% sigFeature_top100_shared)
write.csv(sigFeature_top100_shared, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigFeature_top100_shared.csv")

#SVM-RFE (do for all and shared)
sigRFE_top_100_shared <- as.data.frame(X[ ,featureRankedList[1:100]])
sigRFE_top_100_shared <- as.list.data.frame(colnames(sigRFE_top_100_shared))
sigRFE_top100_shared <- filter(uniprot, Entry %in% sigRFE_top_100_shared)
write.csv(sigRFE_top100_shared, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigRFE_top100_shared.csv")

# now add p-value information in

#svm-rfe (do for all and shared)
pvalRFE_df_shared <- as.data.frame(pvalRFE)
colnames(pvalRFE_df_shared) <- colnames(X[ ,featureRankedList[1:100]])
pvalRFE_df_shared <- as.data.frame(t(pvalRFE_df_shared))
colnames(pvalRFE_df_shared) <- "pvalue"
write.csv(pvalRFE_df_shared, "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/SVM-RFE_pvalues_shared.csv")

pvalsigFE_df_shared <- as.data.frame(pvalsigFe)
colnames(pvalsigFE_df_shared) <- colnames(X[ ,sigfeatureRankedList[1:100]])
pvalsigFE_df_shared <- as.data.frame(t(pvalsigFE_df_shared))
colnames(pvalsigFE_df_shared) <- "pvalue"
write.csv(pvalsigFE_df_shared, "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigFE_pvalues_shared.csv")

# merge the uniprot information with pvalues
pvalRFE_df_shared$Entry <- rownames(pvalRFE_df_shared)
sigRFE_top100_pval_shared <- inner_join(sigRFE_top100_shared, pvalRFE_df_shared, by='Entry')
write.csv(sigRFE_top100_pval_shared, "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigRFE_top100_w_pval_shared.csv")

pvalsigFE_df_shared$Entry <- rownames(pvalsigFE_df_shared)
sigFeature_top100_pval_shared <- inner_join(sigFeature_top100_shared, pvalsigFE_df_shared, by='Entry')
write.csv(sigFeature_top100_pval_shared, "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigFeature_top100_w_pval_shared.csv")

# Identifying proportions of transcriptomes to algorithms
# Proportion of overlap between transcriptomes

library(tidyverse)
library(ggvenn)
library(ggplot2)
library(dplyr)

svm_all <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigFeature_top100_w_pval_all.csv"))
svmrfe_all <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigRFE_top100_w_pval_all.csv"))
svm_shared <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigFeature_top100_w_pval_shared.csv"))
svmrfe_shared <- as.data.frame(read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/sigRFE_top100_w_pval_shared.csv"))
  
x <- list(
  # PCA = PCA$Entry, 
  svm_All = svm_all$Entry, 
  svmRFE_All = svmrfe_all$Entry,
  svm_Shared = svm_shared$Entry,
  svmRFE_Shared = svmrfe_shared$Entry
)

ggvenn(
  x, 
  fill_color = c("#FFF7BC", "#A1DAB4", "#41B6C4", "#225EA8"),
  stroke_size = 0.5, set_name_size = 5
)
# Save this pdf and call it "transcriptome_comparison"

# Isolation of intersect/unique genes in venn diagram 
Intersect <- function (x) {  
  # Multiple set version of intersect
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

Union <- function (x) {  
  # Multiple set version of union
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}

Setdiff <- function (x, y) {
  # Remove the union of the y's from the common x's. 
  # x and y are lists of characters.
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}
# all genes 
all_genes <- Intersect(x)

# sa = svm_all
# sra = svmrfe_all
# ss = svm_shared
# srs = svmrfe_shared

# 3 species overlap
sa_sra_srs <- Setdiff(x[c("svm_All", "svmRFE_All", "svmRFE_Shared")], x[c("svm_Shared")])
sa_sra_ss <- Setdiff(x[c("svm_All", "svmRFE_All", "svm_Shared")], x[c("svmRFE_Shared")])
sa_srs_ss <- Setdiff(x[c("svm_All", "svm_Shared", "svmRFE_Shared")], x[c("svmRFE_All")])
ss_sra_srs <- Setdiff(x[c("svm_Shared", "svmRFE_All", "svmRFE_Shared")], x[c("svm_All")])

# 2 species overlap 
sa_sra <- Setdiff(x[c("svm_All", "svmRFE_All")], x[c("svm_Shared", "svmRFE_Shared")])
sa_ss <- Setdiff(x[c("svm_All", "svm_Shared")], x[c("svmRFE_All", "svmRFE_Shared")])
sa_srs <- Setdiff(x[c("svm_All", "svmRFE_Shared")], x[c("svm_Shared", "svmRFE_All")])
ss_sra <- Setdiff(x[c("svm_Shared", "svmRFE_All")], x[c("svm_All", "svmRFE_Shared")])
ss_srs <- Setdiff(x[c("svm_Shared", "svmRFE_Shared")], x[c("svm_All", "svmRFE_All")])
srs_sra <- Setdiff(x[c("svmRFE_All", "svmRFE_Shared")], x[c("svm_All", "svm_Shared")])

# 1 species UNIQUE 
svm_All_unique <- Setdiff(x[c("svm_All")], x[c("svm_Shared", "svmRFE_Shared", "svmRFE_All")])
svmRFE_Shared_unique <- Setdiff(x[c("svmRFE_Shared")], x[c("svm_Shared", "svm_All", "svmRFE_All")])
svm_Shared_unique <- Setdiff(x[c("svm_Shared")], x[c("svmRFE_Shared", "svm_All", "svmRFE_All")])
svmRFE_All_unique <- Setdiff(x[c("svmRFE_All")], x[c("svm_Shared", "svm_All", "svmRFE_Shared")])

ls()
save(all_genes, Intersect, sa_sra, sa_sra_srs, sa_sra_ss, sa_srs, sa_srs_ss, sa_ss, Setdiff, srs_sra, ss_sra, ss_sra_srs, ss_srs, svm_all, svm_All_unique, svm_shared, svm_Shared_unique, svmrfe_all, svmRFE_All_unique, svmrfe_shared, svmRFE_Shared_unique, Union, uniprot, x, file = "/Users/marielle/Documents/Mydlarz Lab/Disease Study/Venn Diagrams/SVM_SVMRFE_VennDiagram.RData")
