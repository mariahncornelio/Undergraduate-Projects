title: "HOW TO MELT"
author: "Mariah Noelle Cornelio"
date: "2024-1-26"
output: csv_file

# Load in libraries and packages used and set your working directory
library("reshape2")
library(dplyr)
setwd("/Users/marielle/Documents/Mydlarz Lab/Melanin/tabular ref species")

# Load in your dataset
data_wide <- read.csv("pstr_ref_pfam_domain.csv")
  
# Melt your data
melted_data <- melt(data_wide, id.vars = "accession", measure.vars = "target_name")

result <- melted_data %>%
  group_by(accession, variable) %>%
  summarize(value = paste(value, collapse = ", "))
  
# Save file
write.csv(result, "pstr_melted.csv", row.names = FALSE)