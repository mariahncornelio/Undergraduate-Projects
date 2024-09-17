title: "Average Plots BBSplit Visualization for Supplements"
author: "Mariah Noelle"
date: "2023-08-23"
output: html_document

setwd("/Users/marielle/Documents/Mydlarz Lab/Disease Study/BBSplit")
library(tidyverse)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)
library(plyr)
library(nationalparkcolors)

# Averages For Species WP
mapping <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/BBSplit/bbsplit_average_wp.csv")
pal <- park_palette("MtMckinley")

pdf(file = "Average_WP_ALL.pdf", height = 6, width = 7)
p <- ggplot(mapping, aes(fill=Clade, y=Average_unmbiguous, x=Species)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Averages Based On Treatment", x="Species", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Treatment) + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

# mapping <- subset(mapping, Transcriptome!="Unmatched")
# pdf(file = "Average_WP.pdf", height = 6, width = 7)
# p <- ggplot(mapping, aes(fill=Transcriptome, y=Average_unmbiguous, x=Species)) + geom_bar(position="fill", stat="identity") 
# p + labs(title="Summary of Clade Averages Based On Treatment", x="Species", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Treatment) + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
# dev.off()


# Proportions For individual samples WP - you can use these to play around with it and ***change facet wrap by what you want to do
# Play with symbionts; control, exposed, disease (disease status); play with metadata; moderate, high, or low (resistance)
mapping <- read.csv("/Users/marielle/Documents/Mydlarz Lab/Disease Study/BBSplit/bbsplit_individual_wp.csv")
Cnat_mapping <- mapping %>% filter(Species == "CNAT")
Mcav_mapping <- mapping %>% filter(Species == "MCAV")
Ofav_mapping <- mapping %>% filter(Species == "OFAV")
Ssid_mapping <- mapping %>% filter(Species == "SSID") 
pal <- park_palette("MtMckinley")

# Cnat
pdf(file = "Cnat_indiv_WP_ALL.pdf", height = 6, width = 7)
p <- ggplot(Cnat_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Cnat Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

Cnat_mapping <- subset(Cnat_mapping, Clade!="UNMATCHED")
pdf(file = "Cnat_indiv_WP.pdf", height = 6, width = 7)
p <- ggplot(Cnat_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Cnat Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

Cnat_mapping <- subset(Cnat_mapping, Clade!="HOST")
pdf(file = "Cnat_indiv_symbiont.pdf", height = 6, width = 7)
p <- ggplot(Cnat_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Cnat Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

# Mcav
pdf(file = "Mcav_indiv_WP_ALL.pdf", height = 6, width = 7)
p <- ggplot(Mcav_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Mcav Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

Mcav_mapping <- subset(Mcav_mapping, Clade!="UNMATCHED")
pdf(file = "Mcav_indiv_WP.pdf", height = 6, width = 7)
p <- ggplot(Mcav_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Mcav Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

#SAME AS LAST WHAT IS GOING ON
Mcav_mapping <- subset(Mcav_mapping, Clade!="Host")
pdf(file = "Mcav_indiv_WP_symbionts.pdf", height = 6, width = 7)
p <- ggplot(Mcav_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Mcav Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

# Ofav
pdf(file = "Ofav_indiv_WP_ALL.pdf", height = 6, width = 7)
p <- ggplot(Ofav_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Ofav Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

Ofav_mapping <- subset(Ofav_mapping, Clade!="UNMATCHED")
pdf(file = "Ofav_indiv_WP.pdf", height = 6, width = 7)
p <- ggplot(Ofav_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Ofav Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

# same output graph as previous one or slight difference???
Ofav_mapping <- subset(Ofav_mapping, Clade!="Host")
pdf(file = "Ofav_indiv_WP_symbionts.pdf", height = 6, width = 7)
p <- ggplot(Ofav_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Ofav Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

# Ssid
pdf(file = "Ssid_indiv_WP_ALL.pdf", height = 6, width = 7)
p <- ggplot(Ssid_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Ssid Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

Ssid_mapping <- subset(Ssid_mapping, Clade!="UNMATCHED")
pdf(file = "Ssid_indiv_WP.pdf", height = 6, width = 7)
p <- ggplot(Ssid_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Ssid Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()

# Also the same output graph as the previous one???
Ssid_mapping <- subset(Ssid_mapping, Clade!="Host")
pdf(file = "Ssid_indiv_WP_symbiont.pdf", height = 6, width = 7)
p <- ggplot(Ssid_mapping, aes(fill=Clade, y=unmbiguous, x=Sample_ID)) + geom_bar(position="fill", stat="identity") 
p + labs(title="Summary of Clade Proportion for Ssid Samples Based On Top Symbiont", x="Sample", y = "Mapping Rate Proportion")+ theme_classic() + scale_fill_manual(values = pal) + facet_wrap(~Top_symbiont, scales='free_x') + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=10))
dev.off()