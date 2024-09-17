title: "Presence Absence Study Playground"
author: "Mariah Noelle"
date: "2023-07-11"
output: html_document

# Set up R and load in presence absence data set 
library(dplyr) #filters
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggvenn)
library(vegan)

# Isolation of intersect/unique genes in venn diagram - functions: what do all 3 of them do? 
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

# Gameplan
Figure out how to filter and list out the genes in the Entry column that corresponds to the total
number. For example, if the total = 2, then list out those gene entries that have a total of 2
species. Then find out what genes are present in each group, that another group may not have using
the intersect(), union(), and setdiff() functions. After, create a barplot somehow. 

# Principal Component Analysis based on Presence-Absence 
PA_Symbionts_pca <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/PA_symbionts.csv", row.names=1)
PA_Symbionts_pca <- na.omit(PA_Symbionts_pca)

PA_Symbionts.pca <- rda(PA_Symbionts_pca, scale = TRUE)
PA_Symbionts.pca 
summary(PA_Symbionts.pca)

# Broken stick model 
(inventory <- PA_Symbionts_pca$CA$eig)
screeplot(PA_Symbionts.pca, bstick=TRUE, npcs=length(PA_Symbionts.pca$CA$eig))

# PCA Biplot 
par(mfrow=c(1,1))
biplot(PA_Symbionts.pca, scaling=1, type="text", xlab= "PC1 (30.1%)", ylab = "PC2 (21.6%)")

# Playground
# Filtered based on unique total number (includes clade D) 
PA_Symbionts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/PA_symbionts.csv")

Total4 <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(Total == 4)

Total3 <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(Total == 3)

Total2 <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(Total == 2)

Total1 <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(Total == 1)

x <- list(
  Total1 = Total1$Entry,
  Total2= Total2$Entry, 
  Total3 = Total3$Entry, 
  Total4 = Total4$Entry
)

ggvenn(
  x, 
  fill_color = c("#FEF0E4", "#F5A8B4", "#C18EA6", "#347284"),
  stroke_size = 0.5, set_name_size = 5
)

# 3 species overlap 
Total_1_2_4 <- Setdiff(x[c("Total1", "Total2", "Total4")], x[c("Total3")])
Total_1_2_3 <- Setdiff(x[c("Total1", "Total2", "Total3")], x[c("Total4")])
Total_1_4_3 <- Setdiff(x[c("Total1", "Total3", "Total4")], x[c("Total2")])
Total_3_2_4 <- Setdiff(x[c("Total3", "Total2", "Total4")], x[c("Total1")])

# 2 species overlap 
Total_1_2 <- Setdiff(x[c("Total1", "Total2")], x[c("Total3", "Total4")])
Total_1_3 <- Setdiff(x[c("Total1", "Total3")], x[c("Total2", "Total4")])
Total_1_4 <- Setdiff(x[c("Total1", "Total4")], x[c("Total3", "Total2")])
Total_3_2 <- Setdiff(x[c("Total3", "Total2")], x[c("Total1", "Total4")])
Total_3_4 <- Setdiff(x[c("Total3", "Total4")], x[c("Total1", "Total2")])
Total_4_2 <- Setdiff(x[c("Total2", "Total4")], x[c("Total1", "Total3")])

# 1 species UNIQUE 
Total1_unique <- Setdiff(x[c("Total1")], x[c("Total3", "Total4", "Total2")])
Total4_unique <- Setdiff(x[c("Total4")], x[c("Total3", "Total1", "Total2")])
Total3_unique <- Setdiff(x[c("Total3")], x[c("Total4", "Total1", "Total2")])
Total2_unique <- Setdiff(x[c("Total2")], x[c("Total3", "Total1", "Total4")])

# Filtered based on clade and presence (includes clade D)
PA_Symbionts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/PA_symbionts.csv")

cladeA <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(A == 1) 

cladeB <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(B == 1) 

cladeC <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(C == 1)

cladeD <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(D == 1)

y <- list(
  cladeA = cladeA$Entry,
  cladeB = cladeB$Entry,
  cladeC = cladeC$Entry,
  cladeD = cladeD$Entry
)

ggvenn(
  y, 
  fill_color = c("#AFC5EE", "#4E4492", "#461856", "#FC4909"),
  stroke_size = 0.5, set_name_size = 5
)

# Output shows up as NULL???
# 3 species overlap 
clade_A_B_D <- Setdiff(x[c("cladeA", "cladeB", "cladeD")], x[c("cladeC")])
clade_A_B_C <- Setdiff(x[c("cladeA", "cladeB", "cladeC")], x[c("cladeD")])
clade_A_D_C <- Setdiff(x[c("cladeA", "cladeC", "cladeD")], x[c("cladeB")])
clade_C_B_D <- Setdiff(x[c("cladeC", "cladeB", "cladeD")], x[c("cladeA")])

# 2 species overlap 
clade_A_B <- Setdiff(x[c("cladeA", "cladeB")], x[c("cladeC", "cladeD")])
clade_A_C <- Setdiff(x[c("cladeA", "cladeC")], x[c("cladeB", "cladeD")])
clade_A_D <- Setdiff(x[c("cladeA", "cladeD")], x[c("cladeC", "cladeB")])
clade_C_B <- Setdiff(x[c("cladeC", "cladeB")], x[c("cladeA", "cladeD")])
clade_C_D <- Setdiff(x[c("cladeC", "cladeD")], x[c("cladeA", "cladeB")])
clade_D_B <- Setdiff(x[c("cladeB", "cladeD")], x[c("cladeA", "cladeC")])

# 1 species UNIQUE 
cladeA_unique <- Setdiff(x[c("cladeA")], x[c("cladeC", "cladeD", "cladeB")])
cladeD_unique <- Setdiff(x[c("cladeD")], x[c("cladeC", "cladeA", "cladeB")])
cladeC_unique <- Setdiff(x[c("cladeC")], x[c("cladeD", "cladeA", "cladeB")])
cladeB_unique <- Setdiff(x[c("cladeB")], x[c("cladeC", "cladeA", "cladeD")])

# Filtered based on clade and absence (includes clade D)
PA_Symbionts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/PA_symbionts.csv")

cladeAabs <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(A == 0)

cladeBabs <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(B == 0)

cladeCabs <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(C == 0)

cladeDabs <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(D == 0)

z <- list(
  cladeAabs = cladeAabs$Entry, 
  cladeBabs = cladeBabs$Entry, 
  cladeCabs = cladeCabs$Entry,
  cladeDabs = cladeDabs$Entry
)

ggvenn(
  z, 
  fill_color = c("#D8EFD0", "#F7F7F7", "#F6DCD7", "#E9B8AC"),
  stroke_size = 0.5, set_name_size = 5
)

# Output shows up as NULL???
# 3 species overlap 
cladeabs_A_B_D <- Setdiff(x[c("cladeAabs", "cladeBabs", "cladeDabs")], x[c("cladeCabs")])
cladeabs_A_B_C <- Setdiff(x[c("cladeAabs", "cladeBabs", "cladeCabs")], x[c("cladeDabs")])
cladeabs_A_C_D <- Setdiff(x[c("cladeAabs", "cladeCabs", "cladeDabs")], x[c("cladeBabs")])
cladeabs_C_B_D <- Setdiff(x[c("cladeCabs", "cladeBabs", "cladeDabs")], x[c("cladeAabs")])

# 2 species overlap 
cladeabs_A_B <- Setdiff(x[c("cladeAabs", "cladeBabs")], x[c("cladeCabs", "cladeDabs")])
cladeabs_A_C <- Setdiff(x[c("cladeAabs", "cladeCabs")], x[c("cladeBabs", "cladeDabs")])
cladeabs_A_D <- Setdiff(x[c("cladeAabs", "cladeDabs")], x[c("cladeCabs", "cladeBabs")])
cladeabs_C_B <- Setdiff(x[c("cladeCabs", "cladeBabs")], x[c("cladeAabs", "cladeDabs")])
cladeabs_C_D <- Setdiff(x[c("cladeCabs", "cladeDabs")], x[c("cladeAabs", "cladeBabs")])
cladeabs_B_D <- Setdiff(x[c("cladeBabs", "cladeDabs")], x[c("cladeAabs", "cladeCabs")])

# 1 species UNIQUE 
cladeAabs_unique <- Setdiff(x[c("cladeAabs")], x[c("cladeCabs", "cladeDabs", "cladeBabs")])
cladeDabs_unique <- Setdiff(x[c("cladeDabs")], x[c("cladeCabs", "cladeAabs", "cladeBabs")])
cladeCabs_unique <- Setdiff(x[c("cladeCabs")], x[c("cladeDabs", "cladeAabs", "cladeBabs")])
cladeBabs_unique <- Setdiff(x[c("cladeBabs")], x[c("cladeCabs", "cladeAabs", "cladeDabs")])

# Bar Graph Playground
# Combo Playground - Individual Clade Presence and Absence 
A_Presence <- PA_Symbionts %>%
  select(Entry, A) %>%
  filter(A == 1) 

A_Absence <- PA_Symbionts %>%
  select(Entry, A) %>%
  filter(A == 0)

A_PA <- PA_Symbionts %>%
  select(Entry, A)

B_Presence <- PA_Symbionts %>%
  select(Entry, B) %>%
  filter(B == 1)
  
B_Absence <- PA_Symbionts %>%
  select(Entry, B) %>%
  filter(B == 0)

B_PA <- PA_Symbionts %>%
  select(Entry, B)

C_Presence <- PA_Symbionts %>%
  select(Entry, C) %>%
  filter(C == 1)

C_Absence <- PA_Symbionts %>%
  select(Entry, C) %>%
  filter(C == 0)

C_PA <- PA_Symbionts %>%
  select(Entry, C)

D_Presence <- PA_Symbionts %>%
  select(Entry, D) %>%
  filter(D == 1)

D_Absence <- PA_Symbionts %>%
  select(Entry, D) %>%
  filter(D == 0)

D_PA <- PA_Symbionts %>%
  select(Entry, D)

#so confused fart poop n butt!!! coding is so difficult i am learning all of this from scratch :<
  
PA_Symbiont_counts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/PA_Symbiont_counts.csv")
Symbiont_PA <- pivot_longer(PA_Symbiont_counts, cols = c("Presence", "Absence"), 
                            names_to = "PA", 
                            values_to = "Number")

PA_colors <- c(Presence = "#AABF67", Absence = "#AD2E45")
Symbiont_PA_barchart <- ggplot()
Symbiont_PA_barchart <- Symbiont_PA_barchart + geom_col(data = Symbiont_PA, 
                                                        aes(x = Clade, y = Number, fill = PA),
                                                        position = "dodge")
Symbiont_PA_barchart <- Symbiont_PA_barchart + labs(title = "Presence Absence Study Counts of Genes in Clades A-D", 
                                                    y = "Counts")
Symbiont_PA_barchart <- Symbiont_PA_barchart + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                                                     panel.background = element_blank(),
                                                     panel.grid.major.y = element_line(color = "grey"),
                                                     legend.position = "top")
Symbiont_PA_barchart <- Symbiont_PA_barchart + scale_fill_manual(values = PA_colors)
# How do I add the count numbers at the top? I'll just add it on using an editor LOL. 
Symbiont_PA_barchart

# Combo Playground - Total & Clades (Presence & Absence)
PA_Symbionts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/PA_symbionts.csv")

One_P_A <- PA_Symbionts %>%
  select(Entry, A, Total) %>%
  filter(Total == 1 & A == 1)

One_P_B <- PA_Symbionts %>%
  select(Entry, B, Total) %>%
  filter(Total == 1 & B == 1)

One_P_C <- PA_Symbionts %>%
  select(Entry, C, Total) %>%
  filter(Total == 1 & C == 1)

One_P_D <- PA_Symbionts %>%
  select(Entry, D, Total) %>%
  filter(Total == 1 & D == 1)

One_A_A <- PA_Symbionts %>%
  select(Entry, A, Total) %>%
  filter(Total == 1 & A == 0)

One_A_B <- PA_Symbionts %>%
  select(Entry, B, Total) %>%
  filter(Total == 1 & B == 0)

One_A_C <- PA_Symbionts %>%
  select(Entry, C, Total) %>%
  filter(Total == 1 & C == 0)

One_A_D <- PA_Symbionts %>%
  select(Entry, D, Total) %>%
  filter(Total == 1 & D == 0)

TotalOne_PA_Symbiont_counts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/TotalOne_PA_Symbiont_counts.csv")
TotalOne_Symbiont_PA <- pivot_longer(TotalOne_PA_Symbiont_counts, cols = c("Presence", "Absence"), 
                            names_to = "PA", 
                            values_to = "Number")

PA_colors <- c(Presence = "#AABF67", Absence = "#AD2E45")
TotalOne_Symbiont_PA_barchart <- ggplot()
TotalOne_Symbiont_PA_barchart <- TotalOne_Symbiont_PA_barchart + geom_col(data = TotalOne_Symbiont_PA, 
                                                        aes(x = Symbiont_Combo, y = Number, fill = PA),
                                                        position = "dodge")
TotalOne_Symbiont_PA_barchart <- TotalOne_Symbiont_PA_barchart + labs(title = "PA Counts of Symbionts in Listed Combos When Total = 1", 
                                                    y = "Counts", 
                                                    x = "Symbiont Combo")
TotalOne_Symbiont_PA_barchart <- TotalOne_Symbiont_PA_barchart + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                                                     panel.background = element_blank(),
                                                     panel.grid.major.y = element_line(color = "grey"),
                                                     legend.position = "top")
TotalOne_Symbiont_PA_barchart <- TotalOne_Symbiont_PA_barchart + scale_fill_manual(values = PA_colors)
TotalOne_Symbiont_PA_barchart

##########

Two_P_AB <- PA_Symbionts %>%
  select(Entry, A, B, Total) %>%
  filter(Total == 2 & A == 1 & B == 1)

Two_P_AC <- PA_Symbionts %>%
  select(Entry, A, C, Total) %>%
  filter(Total == 2 & A == 1 & C == 1)

Two_P_AD <- PA_Symbionts %>%
  select(Entry, A, D, Total) %>%
  filter(Total == 2 & A == 1 & D == 1)

Two_P_BC <- PA_Symbionts %>%
  select(Entry, B, C, Total) %>%
  filter(Total == 2 & B == 1 & C == 1)

Two_P_BD <- PA_Symbionts %>%
  select(Entry, B, D, Total) %>%
  filter(Total == 2 & B == 1 & D == 1)

Two_P_CD <- PA_Symbionts %>%
  select(Entry, C, D, Total) %>%
  filter(Total == 2 & C == 1 & D == 1)

Two_A_AB <- PA_Symbionts %>%
  select(Entry, A, B, Total) %>%
  filter(Total == 2 & A == 0 & B == 0)

Two_A_AC <- PA_Symbionts %>%
  select(Entry, A, C, Total) %>%
  filter(Total == 2 & A == 0 & C == 0)

Two_A_AD <- PA_Symbionts %>%
  select(Entry, A, D, Total) %>%
  filter(Total == 2 & A == 0 & D == 0)

Two_A_BC <- PA_Symbionts %>%
  select(Entry, B, C, Total) %>%
  filter(Total == 2 & B == 0 & C == 0)

Two_A_BD <- PA_Symbionts %>%
  select(Entry, B, D, Total) %>%
  filter(Total == 2 & B == 0 & D == 0)

Two_A_CD <- PA_Symbionts %>%
  select(Entry, C, D, Total) %>%
  filter(Total == 2 & C == 0 & D == 0)

TotalTwo_PA_Symbiont_counts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/TotalTwo_PA_Symbiont_counts.csv")
TotalTwo_Symbiont_PA <- pivot_longer(TotalTwo_PA_Symbiont_counts, cols = c("Presence", "Absence"), 
                                     names_to = "PA", 
                                     values_to = "Number")

PA_colors <- c(Presence = "#AABF67", Absence = "#AD2E45")
TotalTwo_Symbiont_PA_barchart <- ggplot()
TotalTwo_Symbiont_PA_barchart <- TotalTwo_Symbiont_PA_barchart + geom_col(data = TotalTwo_Symbiont_PA, 
                                                                          aes(x = Symbiont_Combo, y = Number, fill = PA),
                                                                          position = "dodge")
TotalTwo_Symbiont_PA_barchart <- TotalTwo_Symbiont_PA_barchart + labs(title = "PA Counts of Symbionts in Listed Combos When Total = 2", 
                                                                      y = "Counts", 
                                                                      x = "Symbiont Combo")
TotalTwo_Symbiont_PA_barchart <- TotalTwo_Symbiont_PA_barchart + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                                                                       panel.background = element_blank(),
                                                                       panel.grid.major.y = element_line(color = "grey"),
                                                                       legend.position = "top")
TotalTwo_Symbiont_PA_barchart <- TotalTwo_Symbiont_PA_barchart + scale_fill_manual(values = PA_colors)
TotalTwo_Symbiont_PA_barchart

##########

Three_P_ABC <- PA_Symbionts %>%
  select(Entry, A, B, C, Total) %>%
  filter(Total == 3 & A == 1 & B == 1 & C == 1) 

Three_P_ABD <- PA_Symbionts %>%
  select(Entry, A, B, D, Total) %>%
  filter(Total == 3 & A == 1 & B == 1 & D == 1) 

Three_P_ACD <- PA_Symbionts %>%
  select(Entry, A, C, D, Total) %>%
  filter(Total == 3 & A == 1 & C == 1 & D == 1) 

Three_P_BCD <- PA_Symbionts %>%
  select(Entry, B, C, D, Total) %>%
  filter(Total == 3 & B == 1 & C == 1 & D == 1) 

Three_A_ABC <- PA_Symbionts %>%
  select(Entry, A, B, C, Total) %>%
  filter(Total == 3 & A == 0 & B == 0 & C == 0) 

Three_A_ABD <- PA_Symbionts %>%
  select(Entry, A, B, D, Total) %>%
  filter(Total == 3 & A == 0 & B == 0 & D == 0) 

Three_A_ACD <- PA_Symbionts %>%
  select(Entry, A, C, D, Total) %>%
  filter(Total == 3 & A == 0 & C == 0 & D == 0) 

Three_A_BCD <- PA_Symbionts %>%
  select(Entry, B, C, D, Total) %>%
  filter(Total == 3 & B == 0 & C == 0 & D == 0) 

TotalThree_PA_Symbiont_counts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/TotalThree_PA_Symbiont_counts.csv")
TotalThree_Symbiont_PA <- pivot_longer(TotalThree_PA_Symbiont_counts, cols = c("Presence", "Absence"), 
                                     names_to = "PA", 
                                     values_to = "Number")

PA_colors <- c(Presence = "#AABF67", Absence = "#AD2E45")
TotalThree_Symbiont_PA_barchart <- ggplot()
TotalThree_Symbiont_PA_barchart <- TotalThree_Symbiont_PA_barchart + geom_col(data = TotalThree_Symbiont_PA, 
                                                                          aes(x = Symbiont_Combo, y = Number, fill = PA),
                                                                          position = "dodge")
TotalThree_Symbiont_PA_barchart <- TotalThree_Symbiont_PA_barchart + labs(title = "PA Counts of Symbionts in Listed Combos When Total = 3", 
                                                                      y = "Counts", 
                                                                      x = "Symbiont Combo")
TotalThree_Symbiont_PA_barchart <- TotalThree_Symbiont_PA_barchart + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                                                                       panel.background = element_blank(),
                                                                       panel.grid.major.y = element_line(color = "grey"),
                                                                       legend.position = "top")
TotalThree_Symbiont_PA_barchart <- TotalThree_Symbiont_PA_barchart + scale_fill_manual(values = PA_colors)
TotalThree_Symbiont_PA_barchart

##########

Four_P_ABCD <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(Total == 4 & A == 1 & B == 1 & C == 1 & D == 1)

Four_A_ABCD <- PA_Symbionts %>%
  select(Entry, A, B, C, D, Total) %>%
  filter(Total == 4 & A == 0 & B == 0 & C == 0 & D == 0)

TotalFour_PA_Symbiont_counts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/TotalFour_PA_Symbiont_counts.csv")
TotalFour_Symbiont_PA <- pivot_longer(TotalFour_PA_Symbiont_counts, cols = c("Presence", "Absence"), 
                                       names_to = "PA", 
                                       values_to = "Number")

PA_colors <- c(Presence = "#AABF67", Absence = "#AD2E45")
TotalFour_Symbiont_PA_barchart <- ggplot()
TotalFour_Symbiont_PA_barchart <- TotalFour_Symbiont_PA_barchart + geom_col(data = TotalFour_Symbiont_PA, 
                                                                              aes(x = Symbiont_Combo, y = Number, fill = PA),
                                                                              position = "dodge")
TotalFour_Symbiont_PA_barchart <- TotalFour_Symbiont_PA_barchart + labs(title = "PA Counts of Symbionts in Listed Combos When Total = 4", 
                                                                          y = "Counts", 
                                                                          x = "Symbiont Combo")
TotalFour_Symbiont_PA_barchart <- TotalFour_Symbiont_PA_barchart + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                                                                           panel.background = element_blank(),
                                                                           panel.grid.major.y = element_line(color = "grey"),
                                                                           legend.position = "top")
TotalFour_Symbiont_PA_barchart <- TotalFour_Symbiont_PA_barchart + scale_fill_manual(values = PA_colors)
TotalFour_Symbiont_PA_barchart

# Combo Playground - Totals
One_PA <- PA_Symbionts %>%
  select(Entry, Total) %>%
  filter(Total == 1)

Two_PA <- PA_Symbionts %>%
  select(Entry, Total) %>%
  filter(Total == 2)

Three_PA <- PA_Symbionts %>%
  select(Entry, Total) %>%
  filter(Total == 3)

Four_PA <- PA_Symbionts %>%
  select(Entry, Total) %>%
  filter(Total == 4)

Total_PA_Symbiont_counts <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/Presence Absence/Total_PA_Symbiont_counts.csv")

Total_Symbiont_PA_barchart <- ggplot()
Total_Symbiont_PA_barchart <- Total_Symbiont_PA_barchart + geom_col(data = Total_PA_Symbiont_counts, 
                                                                            aes(x = Total, y = Number))
Total_Symbiont_PA_barchart <- Total_Symbiont_PA_barchart + labs(title = "PA Counts of Symbionts Grouped by Total", 
                                                                        y = "Counts", 
                                                                        x = "Total Group")
Total_Symbiont_PA_barchart <- Total_Symbiont_PA_barchart + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                                                                         panel.background = element_blank(),
                                                                         panel.grid.major.y = element_line(color = "grey"),
                                                                         legend.position = "top")
Total_Symbiont_PA_barchart


# What I learned
I learned how to filter() using dplyr and other dplyr functions. I learned how to use ggplot2
and ggvenn and how to manipulate data using excel and creating a .csv file. I also learned how to
create, manipulate, and filter out my own data frames. This was such a challenge but also fun! A
lot of frustration and headaches but it's okay!'






