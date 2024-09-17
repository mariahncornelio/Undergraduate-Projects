Title: "WALSH LAB - DATA RESULTS"
Author: "Mariah Noelle Cornelio"
Date: "27-Mar-2024"
Output: "html"

## NOTES: focus on population and river
## DOWNLOAD PACKAGES AND DATASETS
library(RColorBrewer)
library(ggplot2)
library(ggrepel)
library(readr)
library(dplyr)

setwd("/Users/marielle/Desktop/Walsh Trials")
NP_metadata <- read.csv("NP_water_consolidated.csv")
P_metadata <- read.csv("P_water_consolidated.csv")
  
### The Non-Predator Samples
# AMHP - NP
setwd("/Users/marielle/Desktop/Walsh Trials/Non Predator/AMHP")
AMHP5_NP_G4_2 <- read.csv("AMHP5_NP_G4_2.csv")
AMHP11_NP_G2 <- read.csv("AMHP11_NP_G2.csv")
AMHP11_NP_G5 <- read.csv("AMHP11_NP_G5.csv")
AMHP5_NP_G3 <- read.csv("AMHP5_NP_G3.csv")
AMHP5_NP_G4 <- read.csv("AMHP5_NP_G4.csv")

# AMKO - NP
setwd("/Users/marielle/Desktop/Walsh Trials/Non Predator/AMKO")
AMKO2_NP_G6 <- read.csv("AMKO2_NP_G6.csv")
AMKO11_NP_G2 <- read.csv("AMKO11_NP_G2.csv")
AMKO13_NP_G2 <- read.csv("AMKO13_NP_G2.csv")
AMKO19_NP_G3 <- read.csv("AMKO19_NP_G3.csv")
AMKO3_NP_G5 <- read.csv("AMKO3_NP_G5.csv")

# APHP - NP
setwd("/Users/marielle/Desktop/Walsh Trials/Non Predator/APHP")
APHP7_NP_G2 <- read.csv("APHP7_NP_G2.csv")
APHP16_NP_G4 <- read.csv("APHP16_NP_G4.csv")
APHP12_NP_G5 <- read.csv("APHP12_NP_G5.csv")
APHP1_NP_G3 <- read.csv("APHP1_NP_G3.csv")
APHP16_NP_G6 <- read.csv("APHP16_NP_G6.csv")

# APKO - NP
setwd("/Users/marielle/Desktop/Walsh Trials/Non Predator/APKO")
APKO9_NP_G4 <- read.csv("APKO9_NP_G4.csv")
APKO8_NP_G4 <- read.csv("APKO8_NP_G4.csv")
APKO8_NP_G3 <- read.csv("APKO8_NP_G3.csv")
APKO18_NP_G5 <- read.csv("APKO18_NP_G5.csv")
APKO9_NP_G3 <- read.csv("APKO9_NP_G3.csv")

### The Predator Samples
# AMHP - P
setwd("/Users/marielle/Desktop/Walsh Trials/Predator/AMHP")
AMHP5_P_G4_2 <- read.csv("AMHP5_P_G4_2.csv")
AMHP5_P_G4 <- read.csv("AMHP5_P_G4.csv")
AMHP5_P_G3 <- read.csv("AMHP5_P_G3.csv")
AMHP11_P_G2 <- read.csv("AMHP11_P_G2.csv")
AMHP11_P_G5 <- read.csv("AMHP11_P_G5.csv")

# AMKO - P 
setwd("/Users/marielle/Desktop/Walsh Trials/Predator/AMKO")
AMKO3_P_G5 <- read.csv("AMKO3_P_G5.csv")
AMKO2_P_G6 <- read.csv("AMKO2_P_G6.csv")
AMKO19_P_G3 <- read.csv("AMKO19_P_G3.csv")
AMKO11_P_G2 <- read.csv("AMKO11_P_G2.csv")
AMKO13_P_G2 <- read.csv("AMKO13_P_G2.csv")

# APHP - P
setwd("/Users/marielle/Desktop/Walsh Trials/Predator/APHP")
APHP7_P_G2 <- read.csv("APHP7_P_G2.csv")
APHP16_P_G6 <- read.csv("APHP16_P_G6.csv")
APHP16_P_G4 <- read.csv("APHP16_P_G4.csv")
APHP1_P_G3 <- read.csv("APHP1_P_G3.csv")
APHP12_P_G5 <- read.csv("APHP12_P_G5.csv")

# APKO - P
setwd("/Users/marielle/Desktop/Walsh Trials/Predator/APKO")
APKO9_P_G4 <- read.csv("APKO9_P_G4.csv")
APKO9_P_G3 <- read.csv("APKO9_P_G3.csv")
APKO8_P_G4 <- read.csv("APKO8_P_G4.csv")
APKO18_P_G5 <- read.csv("APKO18_P_G5.csv")
APKO8_P_G3 <- read.csv("APKO8_P_G3.csv")

## START VISUALIZATION
### Counts of how many grouped/individualistic behaviors based on river and population (bar graph)
# Non predator
setwd("/Users/marielle/Desktop/Walsh Trials")
NP_metadata <- read.csv("NP_water_consolidated.csv")

classify_data <- function(NP_metadata, river, population) {
  subset_data <- subset(NP_metadata, River == river & Population == population)
  counts <- table(subset_data$Grouping)
  return(counts)
}

count_AMHP <- classify_data(NP_metadata, "AM", "HP")
count_AMKO <- classify_data(NP_metadata, "AM", "KO")
count_APHP <- classify_data(NP_metadata, "AP", "HP")
count_APKO <- classify_data(NP_metadata, "AP", "KO")

counts_matrix <- rbind(AMHP = count_AMHP, AMKO = count_AMKO, APHP = count_APHP, APKO = count_APKO)
counts_matrix <- t(counts_matrix)

barplot(counts_matrix,
        beside = TRUE,  # Plot bars side by side
        main = "Grouping Behavior by River and Population for Non-Predator Trials",
        xlab = "River and Population",
        ylab = "Count",
        col = c("#85C1E9", "#E74C3C"),
        legend.text = c("Individualistic (N)", "Grouping (Y)"),
        args.legend = list(x = "topleft", bty = "n", inset = c(0.05, 0.05)),
        ylim = c(0, max(counts_matrix) * 1.1))

# Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
P_metadata <- read.csv("P_water_consolidated.csv")

classify_data <- function(P_metadata, river, population) {
  subset_data <- subset(P_metadata, River == river & Population == population)
  counts <- table(subset_data$Grouping)
  return(counts)
}

count_AMHP <- classify_data(P_metadata, "AM", "HP")
count_AMKO <- classify_data(P_metadata, "AM", "KO")
count_APHP <- classify_data(P_metadata, "AP", "HP")
count_APKO <- classify_data(P_metadata, "AP", "KO")

counts_matrix <- rbind(AMHP = count_AMHP, AMKO = count_AMKO, APHP = count_APHP, APKO = count_APKO)
counts_matrix <- t(counts_matrix)

barplot(counts_matrix,
        beside = TRUE,  # Plot bars side by side
        main = "Grouping Behavior by River and Population for Predator Trials",
        xlab = "River and Population",
        ylab = "Count",
        col = c("#85C1E9", "#E74C3C"),
        legend.text = c("Individualistic (N)", "Grouping (Y)"),
        args.legend = list(x = "topleft", bty = "n", inset = c(0.05, 0.05)),
        ylim = c(0, max(counts_matrix) * 1.1))

### Counts of how many grouped/individualistic behaviors on big brain vs small brain (KO vs HP) (bar graph)  
# Non predator
classify_data <- function(NP_metadata, population) {
  subset_data <- subset(NP_metadata, Population == population)
  counts <- table(subset_data$Grouping)
  return(counts)
}

count_HP <- classify_data(NP_metadata, "HP")
count_KO <- classify_data(NP_metadata, "KO")
counts_matrix <- rbind(HP = count_HP, KO = count_KO)
counts_matrix <- t(counts_matrix)

barplot(counts_matrix,
        beside = TRUE,
        main = "Big-Brain vs Small-Brain Grouping Behavior in Non-Predator Trials",
        xlab = "Population",
        ylab = "Count",
        col = c("#85C1E9", "#E74C3C"),
        legend.text = c("Individualistic (N)", "Grouping (Y)"),
        args.legend = list(x = "topleft", bty = "n", inset = c(0.05, 0.05)),
        ylim = c(0, max(counts_matrix) * 1.1))

bar_positions <- barplot(counts_matrix, plot = FALSE)
text_labels <- c("HP", "KO")
text_x <- c()
for (i in 1:length(text_labels)) {
  text_x <- c(text_x, mean(bar_positions[, i]))
}
text(x = text_x, y = counts_matrix + 0.5, labels = rep(c("Y", "N"), each = length(text_labels)), pos = 3, cex = 0.8, col = "black")

# Predator
classify_data <- function(P_metadata, population) {
  subset_data <- subset(P_metadata, Population == population)
  counts <- table(subset_data$Grouping)
  return(counts)
}

count_HP <- classify_data(P_metadata, "HP")
count_KO <- classify_data(P_metadata, "KO")
counts_matrix <- rbind(HP = count_HP, KO = count_KO)
counts_matrix <- t(counts_matrix)

barplot(counts_matrix,
        beside = TRUE,
        main = "Big-Brain vs Small-Brain Grouping Behavior in Predator Trials",
        xlab = "Population",
        ylab = "Count",
        col = c("#85C1E9", "#E74C3C"),
        legend.text = c("Individualistic (N)", "Grouping (Y)"),
        args.legend = list(x = "topright", bty = "n", inset = c(0.05, 0.05)),
        ylim = c(0, max(counts_matrix) * 1.1))

bar_positions <- barplot(counts_matrix, plot = FALSE)
text_labels <- c("HP", "KO")
text_x <- c()
for (i in 1:length(text_labels)) {
  text_x <- c(text_x, mean(bar_positions[, i]))
}
text(x = text_x, y = counts_matrix + 0.5, labels = rep(c("Y", "N"), each = length(text_labels)), pos = 3, cex = 0.8, col = "black")

### Counts of how many grouped/individualistic behaviors based on rivers (bar graph)
# Non predator
classify_data <- function(NP_metadata, river) {
  subset_data <- subset(NP_metadata, River == river)
  counts <- table(subset_data$Grouping)
  return(counts)
}

count_AM <- classify_data(NP_metadata, "AM")
count_AP <- classify_data(NP_metadata, "AP")
counts_matrix <- rbind(AM = count_AM, AP = count_AP)
counts_matrix <- t(counts_matrix)

barplot(counts_matrix,
        beside = TRUE,
        main = "Grouping Behavior Based on Rivers in Non-Predator Trials",
        xlab = "River",
        ylab = "Count",
        col = c("#85C1E9", "#E74C3C"),
        legend.text = c("Individualistic (N)", "Grouping (Y)"),
        args.legend = list(x = "topleft", bty = "n", inset = c(0.05, 0.05)),
        ylim = c(0, max(counts_matrix) * 1.1))

bar_positions <- barplot(counts_matrix, plot = FALSE)
text_labels <- c("AM", "AP")
text_x <- c()
for (i in 1:length(text_labels)) {
  text_x <- c(text_x, mean(bar_positions[, i]))
}
text(x = text_x, y = counts_matrix + 0.5, labels = rep(c("Y", "N"), each = length(text_labels)), pos = 3, cex = 0.8, col = "black")

# Predator
classify_data <- function(P_metadata, river) {
  subset_data <- subset(P_metadata, River == river)
  counts <- table(subset_data$Grouping)
  return(counts)
}

count_AM <- classify_data(P_metadata, "AM")
count_AP <- classify_data(P_metadata, "AP")
counts_matrix <- rbind(AM = count_AM, AP = count_AP)
counts_matrix <- t(counts_matrix)

barplot(counts_matrix,
        beside = TRUE,
        main = "Grouping Behavior Based on Rivers in Predator Trials",
        xlab = "River",
        ylab = "Count",
        col = c("#85C1E9", "#E74C3C"),
        legend.text = c("Individualistic (N)", "Grouping (Y)"),
        args.legend = list(x = "topleft", bty = "n", inset = c(0.05, 0.05)),
        ylim = c(0, max(counts_matrix) * 1.1))

bar_positions <- barplot(counts_matrix, plot = FALSE)
text_labels <- c("AM", "AP")
text_x <- c()
for (i in 1:length(text_labels)) {
  text_x <- c(text_x, mean(bar_positions[, i]))
}
text(x = text_x, y = counts_matrix + 0.5, labels = rep(c("Y", "N"), each = length(text_labels)), pos = 3, cex = 0.8, col = "black")


### Counts of where the fish liked to hide in final time regardless of river and population (10 minutes) 
# Non Predator
setwd("/Users/marielle/Desktop/Walsh Trials/Non Predator")
NP_files <- read.csv("NP_samples.csv")

behavior_counts <- list()

for (filename in NP_files$Filename) {
  if (exists(filename)) {
    counts <- subset(eval(parse(text = filename)), Time == 10, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
    behavior_counts[[filename]] <- counts
  }
}

total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)

sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
sorted_labels <- names(sorted_counts)

palette <- brewer.pal(5, "PuBuGn")
barplot(sorted_counts, 
        main = "Final Shelter Preference for Overall Non-Predator Trials",
        xlab = "Shelter",
        ylab = "Count",
        col = palette,
        ylim = c(0, max(sorted_counts) * 1.1),
        names.arg = sorted_labels)  

mtext("AMHP, AMKO, APHP, and APKO considered", side = 3, line = 0.5, cex = 0.8)

# Predator 
setwd("/Users/marielle/Desktop/Walsh Trials/Predator")
P_files <- read.csv("P_samples.csv")

behavior_counts <- list()

for (filename in P_files$Filename) {
  if (exists(filename)) {
    counts <- subset(eval(parse(text = filename)), Time == 10, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
    behavior_counts[[filename]] <- counts
  }
}

total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)

sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
sorted_labels <- names(sorted_counts)

palette <- brewer.pal(5, "Reds")
barplot(sorted_counts, 
        main = "Final Shelter Preference for Overall Predator Trials",
        xlab = "Shelter",
        ylab = "Count",
        col = palette,
        ylim = c(0, max(sorted_counts) * 1.1),
        names.arg = sorted_labels)  

mtext("AMHP, AMKO, APHP, and APKO considered", side = 3, line = 0.5, cex = 0.8)

### Counts of where the fish liked to hide in initial time regardless of river and population (10 minutes) 
# Non Predator
setwd("/Users/marielle/Desktop/Walsh Trials/Non Predator")
NP_files <- read.csv("NP_samples.csv")

behavior_counts <- list()

for (filename in NP_files$Filename) {
  if (exists(filename)) {
    counts <- subset(eval(parse(text = filename)), Time == 2, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
    behavior_counts[[filename]] <- counts
  }
}

total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)

sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
sorted_labels <- names(sorted_counts)

palette <- brewer.pal(5, "PuBuGn")
barplot(sorted_counts, 
        main = "Initial Shelter Preference for Overall Non-Predator Trials",
        xlab = "Shelter",
        ylab = "Count",
        col = palette,
        ylim = c(0, max(sorted_counts) * 1.1),
        names.arg = sorted_labels)  

mtext("AMHP, AMKO, APHP, and APKO considered", side = 3, line = 0.5, cex = 0.8)

# Predator
setwd("/Users/marielle/Desktop/Walsh Trials/Predator")
P_files <- read.csv("P_samples.csv")

behavior_counts <- list()

for (filename in P_files$Filename) {
  if (exists(filename)) {
    counts <- subset(eval(parse(text = filename)), Time == 2, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
    behavior_counts[[filename]] <- counts
  }
}

total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)

sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
sorted_labels <- names(sorted_counts)

palette <- brewer.pal(5, "Reds")
barplot(sorted_counts, 
        main = "Initial Shelter Preference for Overall Predator Trials",
        xlab = "Shelter",
        ylab = "Count",
        col = palette,
        ylim = c(0, max(sorted_counts) * 1.1),
        names.arg = sorted_labels)  

mtext("AMHP, AMKO, APHP, and APKO considered", side = 3, line = 0.5, cex = 0.8)

### Counts of where the fish liked to hide in final time big brain vs small brain (HP vs KO) (10 minutes)
# Non predator
setwd("/Users/marielle/Desktop/Walsh Trials")
HP_files <- read.csv("HP_NP_samples.csv")
KO_files <- read.csv("KO_NP_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 10, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_HP <- process_behavior_counts(HP_files)
sorted_counts_KO <- process_behavior_counts(KO_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "HP")
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "KO")
combined_data <- rbind(HP_data, KO_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Final Shelter Preference for HP vs KO Fish in Non-Predator Trials",
       subtitle = "HP vs KO",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#E74C3C", "#85C1E9")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
HP_files <- read.csv("HP_P_samples.csv")
KO_files <- read.csv("KO_P_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 10, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_HP <- process_behavior_counts(HP_files)
sorted_counts_KO <- process_behavior_counts(KO_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "HP")
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "KO")
combined_data <- rbind(HP_data, KO_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Final Shelter Preference for HP vs KO Fish in Predator Trials",
       subtitle = "HP vs KO",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#E74C3C", "#85C1E9")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

### Counts of where the fish liked to hide in intial time big brain vs small brain (HP vs KO) (10 minutes)
# Non Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
HP_files <- read.csv("HP_NP_samples.csv")
KO_files <- read.csv("KO_NP_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_HP <- process_behavior_counts(HP_files)
sorted_counts_KO <- process_behavior_counts(KO_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "HP")
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "KO")
combined_data <- rbind(HP_data, KO_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for HP vs KO Fish in Non-Predator Trials",
       subtitle = "HP vs KO",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#E74C3C", "#85C1E9")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
HP_files <- read.csv("HP_P_samples.csv")
KO_files <- read.csv("KO_P_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_HP <- process_behavior_counts(HP_files)
sorted_counts_KO <- process_behavior_counts(KO_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "HP")
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "KO")
combined_data <- rbind(HP_data, KO_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for HP vs KO Fish in Predator Trials",
       subtitle = "HP vs KO",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#E74C3C", "#85C1E9")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

### Counts of where the fish liked to hide in final time in rivers (AM vs HP) (10 minutes)
# Non Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
AM_files <- read.csv("AM_NP_samples.csv")
AP_files <- read.csv("AP_NP_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 10, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_AM <- process_behavior_counts(AM_files)
sorted_counts_AP <- process_behavior_counts(AP_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
AM_data <- data.frame(Shelter = shelters, Count = sorted_counts_AM, Sample_Type = "AM")
AP_data <- data.frame(Shelter = shelters, Count = sorted_counts_AP, Sample_Type = "AP")
combined_data <- rbind(AM_data, AP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Final Shelter Preference for River Populations in Non-Predator Trials",
       subtitle = "AM vs AP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
AM_files <- read.csv("AM_P_samples.csv")
AP_files <- read.csv("AP_P_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 10, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_AM <- process_behavior_counts(AM_files)
sorted_counts_AP <- process_behavior_counts(AP_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
AM_data <- data.frame(Shelter = shelters, Count = sorted_counts_AM, Sample_Type = "AM")
AP_data <- data.frame(Shelter = shelters, Count = sorted_counts_AP, Sample_Type = "AP")
combined_data <- rbind(AM_data, AP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Final Shelter Preference for River Populations in Predator Trials",
       subtitle = "AM vs AP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

### Counts of where the fish liked to hide in initial time in rivers (AM vs HP) (2 minutes)
# Non Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
AM_files <- read.csv("AM_NP_samples.csv")
AP_files <- read.csv("AP_NP_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_AM <- process_behavior_counts(AM_files)
sorted_counts_AP <- process_behavior_counts(AP_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
AM_data <- data.frame(Shelter = shelters, Count = sorted_counts_AM, Sample_Type = "AM")
AP_data <- data.frame(Shelter = shelters, Count = sorted_counts_AP, Sample_Type = "AP")
combined_data <- rbind(AM_data, AP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for River Populations in Non-Predator Trials",
       subtitle = "AM vs AP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
AM_files <- read.csv("AM_P_samples.csv")
AP_files <- read.csv("AP_P_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2, select = c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water"))
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_AM <- process_behavior_counts(AM_files)
sorted_counts_AP <- process_behavior_counts(AP_files)

shelters <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
AM_data <- data.frame(Shelter = shelters, Count = sorted_counts_AM, Sample_Type = "AM")
AP_data <- data.frame(Shelter = shelters, Count = sorted_counts_AP, Sample_Type = "AP")
combined_data <- rbind(AM_data, AP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for River Populations in Predator Trials",
       subtitle = "AM vs AP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Compare overall non-predator vs predator social behavior (counts) 
# Non Predator
setwd("/Users/marielle/Desktop/Walsh Trials")
df <- read.csv("NP_water_consolidated.csv")

grouping_counts <- table(df$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie <- ggplot(grouping_df, aes(x = "", y = Count, fill = Grouping, label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Grouping Behavior in Non-Predator Samples",
       subtitle="Do the Non Predator Samples Show Grouping Behavior?") +
  scale_fill_manual(values = c("N" = "#F1948A", "Y" = "#A9DFBF"), labels = legend_labels) +
  theme_void()
print(pie)

# Predator
df <- read.csv("P_water_consolidated.csv")

grouping_counts <- table(df$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie <- ggplot(grouping_df, aes(x = "", y = Count, fill = Grouping, label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Grouping Behavior in Predator Samples",
       subtitle="Do the Predator Samples Show Grouping Behavior?") +
  scale_fill_manual(values = c("N" = "#F1948A", "Y" = "#A9DFBF"), labels = legend_labels) +
  theme_void()
print(pie)

# Compare big brain and small brain social behavior (these 2 graphs go side by side together)
# Non Predator
KO_samples <- read_csv("KO_NP_samples.csv")
NP_water_consolidated <- read_csv("NP_water_consolidated.csv")

ko_filenames <- unique(KO_samples$Filename)
filtered_data <- NP_water_consolidated %>%
  filter(Trial_ID %in% ko_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of KO Population in Non-Predator Samples",
       subtitle = "Do the KO Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

HP_samples <- read_csv("HP_NP_samples.csv")
NP_water_consolidated <- read_csv("NP_water_consolidated.csv")

hp_filenames <- unique(HP_samples$Filename)
filtered_data <- NP_water_consolidated %>%
  filter(Trial_ID %in% hp_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of HP Population in Non-Predator Samples",
       subtitle = "Do the HP Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

# Predator
KO_samples <- read_csv("KO_P_samples.csv")
P_water_consolidated <- read_csv("P_water_consolidated.csv")

ko_filenames <- unique(KO_samples$Filename)
filtered_data <- P_water_consolidated %>%
  filter(Trial_ID %in% ko_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of KO Population in Predator Samples",
       subtitle = "Do the KO Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

HP_samples <- read_csv("HP_P_samples.csv")
P_water_consolidated <- read_csv("P_water_consolidated.csv")

hp_filenames <- unique(HP_samples$Filename)
filtered_data <- P_water_consolidated %>%
  filter(Trial_ID %in% hp_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of HP Population in Predator Samples",
       subtitle = "Do the HP Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

# Compare rivers brain social behavior (these 2 graphs go side by side together)
# Non Predator
AP_samples <- read_csv("AP_NP_samples.csv")
NP_water_consolidated <- read_csv("NP_water_consolidated.csv")

ap_filenames <- unique(AP_samples$Filename)
filtered_data <- NP_water_consolidated %>%
  filter(Trial_ID %in% ap_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of AP Population in Non-Predator Samples",
       subtitle = "Do the AP Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

AM_samples <- read_csv("AM_NP_samples.csv")
NP_water_consolidated <- read_csv("NP_water_consolidated.csv")

am_filenames <- unique(AM_samples$Filename)
filtered_data <- NP_water_consolidated %>%
  filter(Trial_ID %in% am_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of AM Population in Non-Predator Samples",
       subtitle = "Do the AM Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

# Predator
AP_samples <- read_csv("AP_P_samples.csv")
P_water_consolidated <- read_csv("P_water_consolidated.csv")

ap_filenames <- unique(AP_samples$Filename)
filtered_data <- P_water_consolidated %>%
  filter(Trial_ID %in% ap_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of AP Population in Predator Samples",
       subtitle = "Do the AP Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

AM_samples <- read_csv("AM_P_samples.csv")
P_water_consolidated <- read_csv("P_water_consolidated.csv")

am_filenames <- unique(AM_samples$Filename)
filtered_data <- P_water_consolidated %>%
  filter(Trial_ID %in% am_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of AM Population in Predator Samples",
       subtitle = "Do the AM Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

# Compare non-predator big brain (KO) vs predator big brain (KO) social behavior 
# Non Predator KO
KO_samples <- read_csv("KO_NP_samples.csv")
NP_water_consolidated <- read_csv("NP_water_consolidated.csv")

ko_filenames <- unique(KO_samples$Filename)
filtered_data <- NP_water_consolidated %>%
  filter(Trial_ID %in% ko_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of KO Population in Non-Predator Samples",
       subtitle = "Do the KO Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

# Predator KO
KO_samples <- read_csv("KO_P_samples.csv")
P_water_consolidated <- read_csv("P_water_consolidated.csv")

ko_filenames <- unique(KO_samples$Filename)
filtered_data <- P_water_consolidated %>%
  filter(Trial_ID %in% ko_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of KO Population in Predator Samples",
       subtitle = "Do the KO Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)












## Initial Hiding Spot for KO vs HP 
# Non Predator
KO_files <- read.csv("KO_NP_samples.csv")
HP_files <- read.csv("HP_NP_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2)
      required_columns <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
      available_columns <- intersect(required_columns, colnames(counts))
      counts <- counts[, available_columns, drop = FALSE]
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_KO <- process_behavior_counts(KO_files)
sorted_counts_HP <- process_behavior_counts(HP_files)

shelters <- names(sorted_counts_KO) 
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "AM")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "AP")

combined_data <- rbind(KO_data, HP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for KO vs HP in Non-Predator Trials",
       subtitle = "KO vs HP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Predator 
KO_files <- read.csv("KO_P_samples.csv")
HP_files <- read.csv("HP_P_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2)
      required_columns <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
      available_columns <- intersect(required_columns, colnames(counts))
      counts <- counts[, available_columns, drop = FALSE]
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_KO <- process_behavior_counts(KO_files)
sorted_counts_HP <- process_behavior_counts(HP_files)

shelters <- names(sorted_counts_KO) 
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "AM")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "AP")
combined_data <- rbind(KO_data, HP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for KO vs HP in Predator Trials",
       subtitle = "KO vs HP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

## Initial Hiding Spot for AM vs AP 
# Non Predator
AM_files <- read.csv("AM_NP_samples.csv")
AP_files <- read.csv("AP_NP_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2)
      required_columns <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
      available_columns <- intersect(required_columns, colnames(counts))
      counts <- counts[, available_columns, drop = FALSE]
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_AM <- process_behavior_counts(AM_files)
sorted_counts_AP <- process_behavior_counts(AP_files)

shelters <- names(sorted_counts_AM) 
AM_data <- data.frame(Shelter = shelters, Count = sorted_counts_AM, Sample_Type = "AM")
AP_data <- data.frame(Shelter = shelters, Count = sorted_counts_AP, Sample_Type = "AP")

combined_data <- rbind(AM_data, AP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for AM vs AP in Non-Predator Trials",
       subtitle = "AM vs AP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Predator 
AM_files <- read.csv("AM_P_samples.csv")
AP_files <- read.csv("AP_P_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2)
      required_columns <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
      available_columns <- intersect(required_columns, colnames(counts))
      counts <- counts[, available_columns, drop = FALSE]
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_AM <- process_behavior_counts(AM_files)
sorted_counts_AP <- process_behavior_counts(AP_files)

shelters <- names(sorted_counts_AM) 
AM_data <- data.frame(Shelter = shelters, Count = sorted_counts_AM, Sample_Type = "AM")
AP_data <- data.frame(Shelter = shelters, Count = sorted_counts_AP, Sample_Type = "AP")
combined_data <- rbind(AM_data, AP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for AM vs AP in Predator Trials",
       subtitle = "AM vs AP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))











## Initial Hiding Spot for KO vs HP 
# Non Predator
KO_files <- read.csv("KO_NP_samples.csv")
HP_files <- read.csv("HP_NP_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2)
      required_columns <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
      available_columns <- intersect(required_columns, colnames(counts))
      counts <- counts[, available_columns, drop = FALSE]
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_KO <- process_behavior_counts(KO_files)
sorted_counts_HP <- process_behavior_counts(HP_files)

shelters <- names(sorted_counts_KO) 
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "AM")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "AP")

combined_data <- rbind(KO_data, HP_data)

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for KO vs HP in Non-Predator Trials",
       subtitle = "KO vs HP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5"),
                    labels=c("KO", "HP")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Predator 
KO_files <- read.csv("KO_P_samples.csv")
HP_files <- read.csv("HP_P_samples.csv")

process_behavior_counts <- function(files) {
  behavior_counts <- list()
  for (filename in files$Filename) {
    if (exists(filename)) {
      counts <- subset(eval(parse(text = filename)), Time == 2)
      required_columns <- c("Tall.Plant", "Short.Plant", "Cave", "Substrate", "Open.Water")
      available_columns <- intersect(required_columns, colnames(counts))
      counts <- counts[, available_columns, drop = FALSE]
      behavior_counts[[filename]] <- counts
    }
  }
  total_counts <- colSums(do.call(rbind, behavior_counts), na.rm = TRUE)
  sorted_counts <- total_counts[order(total_counts, decreasing = TRUE)]
  return(sorted_counts)
}

sorted_counts_KO <- process_behavior_counts(KO_files)
sorted_counts_HP <- process_behavior_counts(HP_files)

shelters <- names(sorted_counts_KO) 
KO_data <- data.frame(Shelter = shelters, Count = sorted_counts_KO, Sample_Type = "AM")
HP_data <- data.frame(Shelter = shelters, Count = sorted_counts_HP, Sample_Type = "AP")
combined_data <- rbind(KO_data, HP_data)s

ggplot(data = combined_data, aes(x = Shelter, y = Count, fill = Sample_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Initial Shelter Preference for KO vs HP in Predator Trials",
       subtitle = "KO vs HP",
       x = "Shelter",
       y = "Count",
       fill = "Population") +
  scale_fill_manual(values = c("#48C9B0", "#AF7AC5"),
                    labels=c("KO", "HP")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))









# Compare AMKO, AMHP, APKO, and APHP social behavior under stress of predation

# AMKO
KO_samples <- read_csv("KO_P_samples.csv")
P_water_consolidated <- read_csv("P_water_consolidated.csv")

ko_filenames <- unique(KO_samples$Filename)
filtered_data <- P_water_consolidated %>%
  filter(Trial_ID %in% ko_filenames)

grouping_counts <- table(filtered_data$Grouping)
grouping_df <- as.data.frame(grouping_counts)
colnames(grouping_df) <- c("Grouping", "Count")
legend_labels <- c("Individualistic", "Grouping")

pie_chart <- ggplot(grouping_df, aes(x = "", y = Count, fill = factor(Grouping, levels = c("N", "Y")), label = paste0(Count, " (", Grouping, ")"))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#F1948A", "#A9DFBF"), labels = legend_labels) +  # Specify the legend labels here
  labs(title = "Distribution of Grouping Behavior of KO Population in Predator Samples",
       subtitle = "Do the KO Samples Show Grouping Behavior?",
       fill = "Grouping") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")
print(pie_chart)

#AMHP

#APKO

#APHP




