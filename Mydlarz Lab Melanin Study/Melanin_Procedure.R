Title: "Melanin Procedure"
Author: "Mars"
Date: "2024-1-16"

# Load in libraries and set working directory
library("reshape2")
library(dplyr)
library(tidyverse)

setwd("/Users/marielle/Documents/Mydlarz Lab/Melanin/phylogenetic tree")

### Melt by PFAM (accession)

data_wide <- read.csv("tyrosinase_protein_update_4_8_2024_refpfam_domain.csv") # import the cleaned up excel sheet
melted_data <- melt(data_wide, id.vars = "accession", measure.vars = "target_name") # melts by accession

result <- melted_data %>% # groups same variables together and separates it by. comma 
  group_by(accession, variable) %>%
  summarize(value = paste(value, collapse = ", "))

write.csv(result, "tyrosinase_updated_melted.csv", row.names = FALSE) # save it 


### Assigning and organizing by orthogroups

setwd("/Users/marielle/Documents/Mydlarz Lab/Melanin/phylogenetic tree")
tyrosinase_list <- read.csv("tyrosinase_protein_to_ortho.csv")
geneInfo_melanin <- read.csv("geneInfo_melanin.csv")

tyrosinase_ordered_list <- tyrosinase_list %>% # takes apart groups so it is not separated by commas anymore
  separate_rows(tyrosinase_updated_gene_name, sep=", ") %>% 
  filter(tyrosinase_updated_gene_name != " ")

write.csv(tyrosinase_ordered_list, file = "tyrosinase_updated_ordered_list.csv", row.names = FALSE) # save it

# rename tyrosinase_updated_gene_name to mcav_gene_name? 
tyrosinase_ordered_list <- read.csv("tyrosinase_updated_ordered_list.csv")
tyrosinase_updated_orthogroups <- merge(tyrosinase_ordered_list, geneInfo_melanin, by="mcav_gene_name", all.x=TRUE) # merges by orthogroup
tyrosinase_updated_orthogroups_cleaned <- merge(tyrosinase_ordered_list, geneInfo_melanin, by="mcav_gene_name", all.x=FALSE) # takes out empty rows

write.csv(tyrosinase_updated_orthogroups, file = "tyrosinase_updated_orthogroups.csv", row.names = FALSE) # save file
write.csv(tyrosinase_updated_orthogroups_cleaned, file = "tyrosinase_updated_orthogroups_cleaned.csv", row.names = FALSE) # save file

# Note: manually rename the "mcav_gene_name" column to "Tyrosinase_protein_sequence" :)





