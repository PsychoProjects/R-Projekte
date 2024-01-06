# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(effsize)
library(apa)

# Load the data
data <- read_excel("Beispiel3.xlsx")

# Inspect the data
head(data)
str(data)

# Subset the data for analysis
analysis_data <- data %>% select(`False Memory`, Surprise)

# Perform a T-Test
# Assuming `False Memory` is a binary variable (0 and 1)
t_test_results <- t.test(Surprise ~ `False Memory`, data = data)

# Print T-Test results
print("T-Test Results")
print(t_test_results)

cd <- cohens_d(Surprise ~ `False Memory`, data = data); # apa
print(cd)
cd <- cohen.d(Surprise ~ `False Memory`, data = data); # effsize
print(cd)

# Non-Parametric Test - Kruskal-Wallis Test
kruskal_test_results <- kruskal.test(Surprise ~ `False Memory`, data = analysis_data)

# Print Kruskal-Wallis Test results
print("Kruskal-Wallis Test Results")
print(kruskal_test_results)

# Optional: Visualizing the data
ggplot(analysis_data, aes(x = as.factor(`False Memory`), y = Surprise)) +
  geom_boxplot() +
  labs(title = "Surprise by `False Memory` Level", x = "`False Memory` Level", y = "Surprise")

