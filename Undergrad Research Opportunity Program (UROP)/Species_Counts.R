title: "Merge Counts by Species"
author: "Mariah Noelle"
date: "2023-11-9"

# load packages 
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
setwd("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/counts/")
# load counts 

cnat <- as.data.frame(read.csv("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/salmon_outputs/cnat/cnat_raw_counts.csv"))
mcav <- as.data.frame(read.csv("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/salmon_outputs/mcav/mcav_raw_counts.csv"))
past <- as.data.frame(read.csv("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/salmon_outputs/past/past_raw_count.csv"))
oann <- as.data.frame(read.csv("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/salmon_outputs/oann/oann_raw_counts.csv"))
pstrig <- as.data.frame(read.csv("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/salmon_outputs/pstr/pstr_raw_counts.csv"))
ofav <- as.data.frame(read.csv("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/salmon_outputs/ofav/ofav_raw_counts.csv"))
ssid <- as.data.frame(read.csv("~/Documents/Documents/UTA/RESEARCH/Machine_Learning/salmon_outputs/ssid/ssid_raw_counts.csv"))

metadata <- as.data.frame(read.csv("metadata_ALL.csv"))

metadata_diseased <- metadata %>% filter(metadata$Colony_status == c("Diseased"))
metadata_diseased$Sample

# Diseased Only  
# files need to be slimmed down to entry and diseased count numbers only 
# separate into WP & SCTLD for batch effect order
colnames(cnat)
cnat <- cnat[,-c(2:6,12,14,15,17,19,20)] # all cnats got sick in SCTLD, UVI_80 remained healthy
colnames(cnat)
cnat_WP <- cnat[,c(1,7:9)]
cnat_SCTLD <- cnat[,c(1:6)]

colnames(mcav) 
mcav_SCTLD <- mcav[,c(1,8,10,13)] # d1,3,6 get sick, no WPs get sick 

colnames(past)
past <- past[,c(1,6:8,11:12,18)] #USVI_118 gets WP, all SCTLD get sick
colnames(past)
past_SCTLD <- past[,c(1:6)]
past_WP <- past[,c(1,7)]

colnames(oann)
oann <- oann[, c(1,3,5,7,16:20)] #WP all except 098 get sick, all SCTLD get sick
colnames(oann)
oann_WP <- oann[,c(1:4)]
oann_SCTLD <- oann[,c(1,5:9)]

colnames(pstrig)
pstrig <- pstrig[, c(1,8)] # only pstr_d7 got sick

colnames(ofav)
ofav <- ofav[, c(1,3,4,6,8,10)] #all ofav get sick 

colnames(ssid)
ssid <- ssid[, c(1,4,8,10)] # UVI_134 does not get sick


# files then can be merged together by entry ID 

gene_counts <- merge(cnat_SCTLD,mcav_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,oann_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,past_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,pstrig, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cnat_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,oann_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,ofav, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,past_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,ssid, by="Entry", all=TRUE)
rownames(gene_counts) <- gene_counts$Entry
gene_counts[is.na(gene_counts)] = 0

# save file of gene counts mixed together 
write.csv(gene_counts, file = "gene_counts_diseased_raw.csv")
# a total of 41,307 contigs are expressed in 7 species 

# shared counts 
gene_counts <- merge(cnat_SCTLD,mcav_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,oann_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,past_SCTLD, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,pstrig, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,cnat_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,oann_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,ofav, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,past_WP, by="Entry", all=TRUE)
gene_counts <- merge(gene_counts,ssid, by="Entry", all=TRUE)
rownames(gene_counts) <- gene_counts$Entry
gene_counts_noNA <- na.omit(gene_counts)


write.csv(gene_counts, file = "gene_counts_diseased_shared_raw.csv") 
# a total of 1897 contigs are shared between 7 species 
