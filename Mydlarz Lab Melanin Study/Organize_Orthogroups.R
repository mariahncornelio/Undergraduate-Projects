
library(tidyverse)
setwd("/Users/marielle/Documents/Mydlarz Lab/Melanin")

# List out all the mcav proteomes 

mcav_list <- read.csv("mcav_protein_to_ortho_list.csv")

mcav_ordered_list <- mcav_list %>%
  separate_rows(mcav_gene_name, sep=", ") %>% 
  filter(mcav_gene_name != " ")

write.csv(mcav_ordered_list, file = "mcav_ordered_list.csv", row.names = FALSE)

# Match and merge orthogroups

geneInfo_melanin <- read.csv("geneInfo_melanin.csv")

mcav_orthogroups <- merge(mcav_ordered_list, geneInfo_melanin, by="mcav_gene_name", all.x=TRUE)
mcav_orthogroups_cleaned <- merge(mcav_ordered_list, geneInfo_melanin, by="mcav_gene_name", all.x=FALSE)

write.csv(mcav_orthogroups, file = "mcav_orthogroups.csv", row.names = FALSE)
write.csv(mcav_orthogroups_cleaned, file = "mcav_orthogroups_cleaned.csv", row.names = FALSE)
# Manually rename the mcav_gene_name column to Mcav_protein_sequence :)