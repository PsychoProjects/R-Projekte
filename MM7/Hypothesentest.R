# Laden der benötigten Bibliotheken
library(readxl)
library(ggplot2)

# Einlesen der Daten
data <- read_excel("Mittelwerte_MM7_korrigiert.xlsx")

# Berechnung der Differenzen
differences <- data$i1_dd_w_MW - data$i1_dd_f_MW

# Shapiro-Wilk-Test zur Überprüfung der Normalverteilung
shapiro_test <- shapiro.test(differences)
print(shapiro_test)

# Wilcoxon-Vorzeichen-Rang-Test
wilcoxon_test <- wilcox.test(data$i1_dd_w_MW, data$i1_dd_f_MW, paired = TRUE)
print(wilcoxon_test)

# Erstellung eines Histogramms und eines Boxplots für die Differenzen
par(mfrow = c(1, 2))

# Histogramm
hist(differences, main = "Histogramm der Differenzen", xlab = "Differenzen", col = "blue", border = "black")

# Boxplot
boxplot(differences, main = "Boxplot der Differenzen", ylab = "Differenzen", col = "lightblue")

par(mfrow=c(1,1))

