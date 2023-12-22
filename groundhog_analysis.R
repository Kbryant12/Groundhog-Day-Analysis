# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

# Import dataset
dataset <- read_csv("archive.csv")

# View first few rows of the dataset
head(dataset)

# Summary statistics of the dataset
summary(dataset)

# Rename variables (if needed)
dataset <- dataset %>%
  rename(
    Shadow_Presence = 'Punxsutawney Phil',
    Feb_Temp = `February Average Temperature`
  )

# Remove rows with "Partial Shadow" from shadow_presence column
dataset <- dataset[dataset$Shadow_Presence != "Partial Shadow", ]

# Remove rows with "No Record" from shadow_presence column
dataset <- dataset[dataset$Shadow_Presence != "No Record", ]

# Check for missing values
sum(is.na(dataset$`Feb_Temp`))
sum(is.na(dataset$Shadow_Presence))

# Remove missing values
dataset <- na.omit(dataset)

# Create a new binary variable for shadow presence differentiation
# "Full Shadow" is coded as 1, "No Shadow" is coded as 0, and "Partial Shadow" is excluded 
dataset$Shadow_Presence_Differentiated <- ifelse(dataset$Shadow_Presence == "Full Shadow", 1,
                                                 ifelse(dataset$Shadow_Presence == "No Shadow", 0, NA))

# Calculate average temperature
avg_temp <- dataset %>%
  group_by(Shadow_Presence_Differentiated) %>%
  summarise(Feb_Temp = mean(Feb_Temp, na.rm = TRUE))

# Create a bar plot
ggplot(dataset, aes(x = factor(Shadow_Presence_Differentiated), y = Feb_Temp)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black") +
  labs(title = "Average February Temperature by Shadow Presence",
       x = "Shadow Presence",
       y = "Average February Temperature") +
  scale_x_discrete(labels = c("No Shadow", "Full Shadow"))

# This visual shows that there is very little difference between the temperature and shadow presence.

# Scatter plot
ggplot(dataset, aes(x = Feb_Temp, y = Shadow_Presence)) +
  geom_point() +
  labs(title = "Relationship between Groundhog Shadow Presence and February Temperature",
       x = "February Temperature",
       y = "Shadow Presence")


# Box plot for February Average Temperature
ggplot(dataset, aes(x = Shadow_Presence, y = Feb_Temp, fill = Shadow_Presence)) +
  geom_boxplot() +
  labs(title = "Boxplot of February Temperature by Shadow Presence",
       x = "Shadow Presence",
       y = "February Average Temp")


# Calculate correlation coefficient
correlation_coefficient <- cor(dataset$`Feb_Temp`, dataset$Shadow_Presence_Differentiated, method = "pearson")

# Print the correlation coefficient
print(correlation_coefficient)

# Conclusion
# Summarize key findings
cat("Summary of Findings:\n")
cat("1. Relationship between Groundhog Shadow Presence and February Temperature:\n")
cat("- [Brief summary of the relationship, e.g., No significant correlation observed between shadow presence and temperature.]\n")

# Interpretation of Results
cat("\nInterpretation of Results:\n")
cat("- [The correlation coeffiecient is -0.198 and shows a weak negative relationship between the average February temperature and shadow presence]\n")

# Closing Remarks
cat("\nClosing Remarks:\n")
cat("In conclusion, this analysis provided valuable insights into the relationship between Groundhog Day shadow presence and February temperatures. While no significant correlation was observed in this dataset, the exploration and interpretation of the results contribute to the broader understanding of the factors influencing this annual event. Future research could consider incorporating additional variables or exploring different datasets to further investigate this phenomenon. Overall, this analysis highlights the importance of data-driven approaches in understanding and interpreting complex phenomena.\n")
