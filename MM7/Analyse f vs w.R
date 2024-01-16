# Laden der benötigten Bibliotheken
library(readxl)
library(effsize)

# Einlesen der Daten
data <- read_excel("Mittelwerte_MM7_korrigiert.xlsx")

f_data <- data$i1_dd_f_MW + (data$i1_ue_f_MW - 1) / 2
w_data <- data$i1_dd_w_MW + (data$i1_ue_w_MW - 1) / 2
differences <- f_data - w_data

n_data <- data.frame(w_data, f_data, differences)
View(n_data)

shapiro_test <- shapiro.test(differences)
print(shapiro_test)

t_test <- t.test(f_data, w_data, paired = TRUE )
print(t_test)

c <- cohen.d(f_data, w_data, paired = TRUE)
print(c)

boxplot(differences)
hist(differences)

par(mfrow = c(1, 2))
hist(w_data)
hist(f_data)
par(mfrow = c(1, 1))
