# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(effsize)
library(apa)

# Load the data
data <- read_excel("Aufbereitete_Aggregierte_Daten_MM7_JKH.xlsx")

# Inspect the data
head(data)
str(data)

# Subset the data for analysis
analysis_data <- data %>% select(erlebnisbasiert, i1_ue)

# Perform a T-Test
# Assuming erlebnisbasiert is a binary variable (0 and 1)
t_test_results <- t.test(i1_ue ~ erlebnisbasiert, data = data)

# Print T-Test results
print("T-Test Results")
print(t_test_results)

cd <- cohens_d(i1_ue ~ erlebnisbasiert, data = data); # apa
print(cd)
cd <- cohen.d(i1_ue ~ erlebnisbasiert, data = data); # effsize
print(cd)

# Non-Parametric Test - Kruskal-Wallis Test
kruskal_test_results <- kruskal.test(i1_ue ~ erlebnisbasiert, data = analysis_data)

# Print Kruskal-Wallis Test results
print("Kruskal-Wallis Test Results")
print(kruskal_test_results)

# Optional: Visualizing the data
ggplot(analysis_data, aes(x = as.factor(erlebnisbasiert), y = i1_ue)) +
  geom_boxplot() +
  labs(title = "i1_ue by erlebnisbasiert Level", x = "erlebnisbasiert Level", y = "i1_ue")

