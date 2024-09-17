Title: "WALSH LAB - DATA RESULTS"
Author: "Mariah Noelle Cornelio Emily Hong An Khuu & Mariah Noelle Cornelio"
Date: "2-Apr-2024"
Output: "Statistical Results"


# NOTE: H0 will be rejected if the p-value is less than a = 0.05!
# Download packages and datasets 
setwd("/Users/marielle/Desktop/Walsh Trials")
data <- read.csv("cleaned_kilifish_data.csv")
install.packages("knitr")
library(knitr)
install.packages("gridExtra")
library(gridExtra)

## Chi squared test to test for relationship between Population and Grouping
# Null Hypothesis (H0): There is no relationship between Population and Grouping.
# Alternative Hypothesis (HA): There is a relationship between Population and Grouping.
str(data)
chisq_result <- chisq.test(data$Population, data$Grouping)
print(chisq_result)

result_df <- data.frame(
  Test = "Chi-Square Test",
  df=chisq_result$parameter,
  Statistic = chisq_result$statistic,
  p_value = chisq_result$p.value
)
table_grob <- tableGrob(result_df, rows = NULL)
plot(table_grob)

## Chi squared test to test for relationship between River and Grouping
# Null Hypothesis (H0): There is no relationship between Experiment Water and Grouping.
# Alternative Hypothesis (HA): There is a relationship between Experiment Water and Grouping.
cross_table <- table(data$Experiment_Water, data$Grouping)
chi_square_result <- chisq.test(cross_table)
print(chi_square_result)

result_df2 <- data.frame(
  Test="Chi-Square Test",
  df=chi_square_result$parameter,
  Statistic=chi_square_result$statistic,
  p_value=chi_square_result$p.value
)

table_grob2 <- tableGrob(result_df2, rows=NULL)
plot(table_grob2)


## ANOVA Tests
# Using '+' focuses on the main effects of variables independently.
# Using '*' considers the combined effects and potential interactions between variables.
# In ANOVA models, choosing between '+' and '' depends on the research question and whether you are interested in examining interaction effects between variables. If you suspect that variables may interact with each other, including terms with '' is appropriate. Otherwise, using '+' may suffice to examine the main effects of variables independently.

### Random combinations - look for p-values that are less than alpha ( p < 0.05 ) which rejects the H0 that says that there is a difference between variables 
# Perform ANOVA for Location and Grouping
anova_location_grouping <- aov(Total ~ Location * Grouping, data = data)
print(summary(anova_location_grouping))

# Perform ANOVA for Location and Population
anova_location_population <- aov(Total ~ Location * Population, data = data)
print(summary(anova_location_population))

# Perform ANOVA for Location and River
anova_location_river <- aov(Total ~ Location * River, data = data)
print(summary(anova_location_river))

# Perform ANOVA for Grouping and Population - THIS ONE 
anova_result <- aov(Grouping ~ Population, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and River
anova_result <- aov(Grouping ~ River, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and Experiment Water - THIS ONE
anova_result <- aov(Grouping ~ Experiment_Water, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and Location
anova_result <- aov(Grouping ~ Location, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and Totals
anova_result <- aov(Grouping ~ Total, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and N - THIS ONE
anova_result <- aov(Grouping ~ N, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and Population with Total into Account 
anova_result <- aov(Grouping ~ Population * Total, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and River with Total into Account
anova_result <- aov(Grouping ~ River * Total, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and Experiment Water with Total into Account
anova_result <- aov(Grouping ~ Experiment_Water * Total, data = data)
print(summary(anova_result))

### WITH + SIGNS
anova_location_grouping <- aov(Total ~ Location + Grouping, data = data)
print(summary(anova_location_grouping))

# Perform ANOVA for Location and Population
anova_location_population <- aov(Total ~ Location + Population, data = data)
print(summary(anova_location_population))

# Perform ANOVA for Location and River
anova_location_river <- aov(Total ~ Location + River, data = data)
print(summary(anova_location_river))
# This does not affect where they hide 

# Perform ANOVA for Grouping and Population with Total into Account 
anova_result <- aov(Grouping ~ Population + Total, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and River with Total into Account
anova_result <- aov(Grouping ~ River + Total, data = data)
print(summary(anova_result))

# Perform ANOVA for Grouping and Experiment Water with Total into Account
anova_result <- aov(Grouping ~ Experiment_Water + Total, data = data)
print(summary(anova_result))

### Answering the Research Question "the population and/or river of these killifish affect their grouping behavior"
# Perform ANOVA for Grouping and Population
# Null Hypothesis (H0): There is no significant difference in the mean 'Grouping' behavior among different populations ('Population' variable).
# Alternative Hypothesis (Ha): There is a significant difference in the mean 'Grouping' behavior among different populations ('Population' variable).
anova_population <- aov(Grouping ~ Population, data = data)
print("ANOVA for Grouping and Population:")
print(summary(anova_population))

# Perform ANOVA for Grouping and River
anova_river <- aov(Grouping ~ River, data = data)
print("ANOVA for Grouping and River:")
print(summary(anova_river))

# Perform ANOVA for Grouping and the interaction between Population and River
# Null Hypothesis (H0): The effect of 'Population' on 'Grouping' behavior is the same across all levels of 'River' (no interaction effect).
# Alternative Hypothesis (Ha): The effect of 'Population' on 'Grouping' behavior varies across different levels of 'River' (interaction effect).
anova_df_interaction <- aov(Grouping ~ Population * River * Total * Experiment_Water * Location, data=data)
print("ANOVA for All Columns")
print(summary(anova_df_interaction))

anova_df_all <- as.data.frame(summary(anova_df_interaction))
write.csv(anova_df_all, file="anova_df_all.csv")


# Extract relevant components from the ANOVA summary
anova_summary <- summary(anova_df_interaction)
anova_table <- anova_summary[[1]]

# Convert the ANOVA table to a data frame
anova_df_all <- as.data.frame(anova_table)

# Write the data frame to a CSV file
write.csv(anova_df_all, file="anova_df_all.csv")










# Perform ANOVA analysis
anova_df_interaction <- aov(Grouping ~ Population * River * Total * Experiment_Water * Location, data=data)

# Create summary table
summary_table <- summary(anova_df_interaction)

# Extract relevant information from the summary
anova_summary <- data.frame(
  Source = rownames(summary_table[[1]]),
  Df = summary_table[[1]][, "Df"],
  SumSq = summary_table[[1]][, "Sum Sq"],
  MeanSq = summary_table[[1]][, "Mean Sq"],
  Fvalue = summary_table[[1]][, "F value"],
  p_value = summary_table[[1]][, "Pr(>F)"]
)

# Highlight rows with p-value < 0.05
significant_rows <- anova_summary$p_value < 0.05
anova_summary$Significance <- ifelse(significant_rows, "Yes", "No")

# Print summary table
print(anova_summary)

# Plot the table with highlighting
library(gridExtra)
library(grid)

# Create a table grob
table_grob <- tableGrob(anova_summary, rows = NULL)

# Plot the table
plot(table_grob)
