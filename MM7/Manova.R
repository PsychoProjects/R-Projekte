# Laden der benötigten Bibliotheken
library(car)
library(readxl)

rm(list=ls(all=TRUE))

# Daten einlesen
data <- read_excel("Aggregierte_Daten_Studierende MM7_JKH.xlsx")

# Angenommen, Sie haben Ihre Daten in einem DataFrame namens 'data'
# mit den Spalten 'i1_dd_w_MW', 'i1_dd_f_MW', 'i1_ue_w_MW', 'i1_ue_f_MW'

# Vorbereiten der Daten für MANOVA
# Erstellen eines neuen DataFrames, das 'w' und 'f' Werte als separate Fälle behandelt
manova_data <- rbind(
  data.frame(dd = data$i1_dd_w_MW, ue = data$i1_ue_w_MW, Truth = 'w'),
  data.frame(dd = data$i1_dd_f_MW, ue = data$i1_ue_f_MW, Truth = 'f')
)

# Durchführen der MANOVA
manova_results <- manova(cbind(dd, ue) ~ Truth, data = manova_data)

# Anzeigen der Ergebnisse
print(manova_results)
summary(manova_results, test = "Wilks")
summary.aov(manova_results)

