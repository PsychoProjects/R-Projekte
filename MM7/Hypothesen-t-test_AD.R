# Pakete laden
library(readxl)
library(stats)

# Daten einlesen
daten <- read_excel("Aggregierte_Daten_Studierende MM7_JKH.xlsx")

par(mfrow=c(1,2))

# ------------------------------------------------------------
# Analyse der Diskrepanzerkennung
print("Analyse der Diskrepanzerkennung")

describe(daten$i1_dd_w_MW)

# T-Test für dd durchführen
ergebnis <- t.test(daten$i1_dd_f_MW, daten$i1_dd_w_MW, paired = TRUE)
cd <- cohen.d(daten$i1_dd_f_MW, daten$i1_dd_w_MW)

# Durchführung des Wilcoxon-Vorzeichen-Rang-Tests
result <- wilcox.test(daten$i1_dd_w_MW, daten$i1_dd_f_MW, paired = TRUE)

# Anzeigen der Testergebnisse
print(result)

# Ergebnis ausgeben
print(ergebnis)
print(cd)

boxplot(daten$i1_dd_w_MW, daten$i1_dd_f_MW, names = c("wahr", "falsch"), ylab = "Diskrepanzerkennung", xlab = "erlebnisbasisert", main = "Diskrepanzerkennung")

# ------------------------------------------------------------
# Analyse der initialen Überraschung
print("Analyse der initialen Überraschung")

describe(daten$i1_ue_w_MW)

# T-Test für ue durchführen
ergebnis <- t.test(daten$i1_ue_f_MW, daten$i1_ue_w_MW, paired = TRUE)
cd <- cohen.d(daten$i1_ue_f_MW, daten$i1_ue_w_MW)

# Ergebnis ausgeben
print(ergebnis)
print(cd)

boxplot(daten$i1_ue_w_MW, daten$i1_ue_f_MW, names = c("wahr", "falsch"), ylab = "Initiale Überraschung", xlab = "erlebnisbasisert", main = "Initiale Überraschung")

par(mfrow=c(1,1))
