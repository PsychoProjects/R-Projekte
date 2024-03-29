# Pakete laden
library(BSDA)
library(coin)
library(datasets)
library(dplyr)
library(effsize)
library(ggplot2)
library(psych)
library(readxl)

# Daten einlesen
daten <- read_excel("Mittelwerte_MM7_korrigiert.xlsx")


# Funktion für die deskriptive Analyse
fn_descriptive_analysis <- function(i1_w_MW, i1_f_MW){
  cat("summary(i1_w_MW, digits = 3)\n")
  summary(i1_w_MW, digits = 3) %>% print()
  cat("describe(i1_w_MW, skew = FALSE)\n")
  describe(i1_w_MW, skew = FALSE)  %>% print()
  cat("\n\n")
  
  cat("summary(i1_f_MW, digits = 3)\n")
  summary(i1_f_MW, digits = 3) %>% print()
  cat("describe(i1_f_MW, skew = FALSE)\n")
  describe(i1_f_MW, skew = FALSE) %>% print()
  cat("\n\n")
  
  cat("difference <- (i1_f_MW - i1_w_MW)\n\n")
  differences <- (i1_f_MW - i1_w_MW)
  cat("summary(differences, digits = 3)\n")
  summary(differences, digits = 3) %>% print()
  cat("describe(differences, skew = FALSE)\n")
  describe(differences, skew = FALSE) %>% print()
}


# Funktion für die grafische Analyse der Grunddaten
fn_graphical_analysis <- function(i1_w_MW, i1_f_MW, title = "") {
  boxplot(i1_w_MW, i1_f_MW, names = c("wahr", "falsch"), 
          ylab = "Rating", xlab = "erlebnisbasisert", main = title)

  par(mfrow = c(1, 2))
  hist(i1_w_MW, breaks = seq(min(i1_w_MW) - 0.1, max(i1_w_MW) + 0.1, by = 0.1), 
       main = "erlebnisbasiert", xlab = "Rating", col = "lightblue", border = "black")
  hist(i1_f_MW, breaks = seq(min(i1_f_MW) - 0.1, max(i1_f_MW) + 0.1, by = 0.1), 
       main = "nicht erlebnisbasiert", xlab = "Rating", col = "lightblue", border = "black")
  par(mfrow = c(1, 1))
}


# Funktion für die Prüfung und Durchführung des t-Tests
fn_tTest <- function (i1_w_MW, i1_f_MW) {
  cat(">>> Vorbedingungen prüfen\n")

  cat("difference <- (i1_f_MW - i1_w_MW)\n\n")
  differences <- (i1_f_MW - i1_w_MW)
  
  cat(">>> Grafische Beurteilung der Normalverteilung der Differenzen\n")
  hist(differences, breaks = seq(min(differences) - 0.5, max(differences) + 0.5, by = 0.1), 
       main = "Histogramm der Differenzen", xlab = "Differenzen", 
       ylab = "Häufigkeit", col = "lightblue", border = "black")
  
  cat("stats::shapiro.test(differences)\n")
  stats::shapiro.test(differences) %>% print() 

  cat("stats::t.test(i1_f_MW, i1_w_MW, paired = TRUE, alternative = two.sided)\n")
  result <- stats::t.test(i1_f_MW, i1_w_MW, paired = TRUE, 
                          alternative = "two.sided") %>% print() 
  
  z <- fn_zValue_from_pValue(result$p.value)
  cat("z-value: ", z, "\n\n")
  
  cat("effsize::cohen.d(i1_f_MW, i1_w_MW, paired = TRUE, alternative = two.sided)\n")
  effsize::cohen.d(i1_f_MW, i1_w_MW, paired = TRUE, alternative = "two.sided") %>% print() 
}


# Hilfsfunktion für die Errechnung des z-Wertes aus dem p-Wert
fn_zValue_from_pValue <- function(pValue) {
  return(qnorm(pValue))
}

# Hilfsfunktion für die Errechnung der Effektstärke (Pearson)
fn_pearson_r <- function (N, zValue) {
  return(abs(zValue) / sqrt(N))
}


# Funktion für die Prüfung und Durchführung des Wilcoxon-Tests
fn_wilcoxon <- function (i1_w_MW, i1_f_MW) {
  cat(">>> Vorbedingungen prüfen\n")

  cat("difference <- (i1_f_MW - i1_w_MW)\n")
  differences <- (i1_f_MW - i1_w_MW)
  
  boxplot(differences, main = "Boxplot der Differenzen", ylab = "Differenzen", col = "lightblue")
  
  cat("wilcox.test(i1_f_MW, i1_w_MW, paired = TRUE, alternative = two.sided)")
  result <- stats::wilcox.test(i1_f_MW, i1_w_MW, paired = TRUE, 
                               alternative = "two.sided", conf.int = TRUE) %>% print() 
  
  z <- fn_zValue_from_pValue(result$p.value)
  cat("z-value: ", z, "\n\n")
  
  r <- fn_pearson_r(N = length(differences), zValue = z)
  cat("Effektstärke (r) für den Wilcoxon-Test: ", r, "\n\n")
}


# Funktion für die Prüfung und Durchführung des Vorzeichen-Tests
fn_sign <- function (i1_w_MW, i1_f_MW) {
  cat("difference <- (i1_f_MW - i1_w_MW)\n\n")
  differences <- (i1_f_MW - i1_w_MW)

  cat("SIGN.test(i1_f_MW, i1_w_MW, md=0, paired = TRUE, alternative = two.sided")
  result <- BSDA::SIGN.test(i1_f_MW, i1_w_MW, md=0, paired = TRUE, 
                            alternative = "two.sided", conf.int = TRUE) %>% print() 
  
  z <- fn_zValue_from_pValue(result$p.value)
  cat("z-value: ", z, "\n\n")
  
  r <- fn_pearson_r(N = length(differences),  zValue = z)
  cat("Effektstärke (r) für den Vorzeichen-Test: ", r, "\n\n")
}


# Funktion für die Analyse der Summen aus Diskrepanz und Überraschung
fn_sum_analysis <- function() {
  # Bildung der Summen bei Normierung der ue-Daten auf Werte in [0;1]
  cat("i1_f_MW <- daten$i1_dd_f_MW + (daten$i1_ue_f_MW - 1) / 2\n")
  i1_f_MW <- daten$i1_dd_f_MW + (daten$i1_ue_f_MW - 1) / 2
  cat("i1_w_MW <- daten$i1_dd_w_MW + (daten$i1_ue_w_MW - 1) / 2\n")
  i1_w_MW <- daten$i1_dd_w_MW + (daten$i1_ue_w_MW - 1) / 2

  cat("\n")
  fn_descriptive_analysis(i1_w_MW = i1_w_MW, i1_f_MW = i1_f_MW)

  fn_graphical_analysis(i1_w_MW = i1_w_MW, i1_f_MW = i1_f_MW, title = "Summen der Werte")
  
  cat("\n\n")
  fn_tTest(i1_w_MW = i1_w_MW, i1_f_MW = i1_f_MW)  
}

# Funktion zur Analyse von Korrelationen
fn_correlation_analysis <- function() {
  cat("i1_ue <- c(daten$i1_ue_w_MW, daten$i1_ue_f_MW)\n")
  i1_ue <- c(daten$i1_ue_w_MW, daten$i1_ue_f_MW)
  cat("i1_dd <- c(daten$i1_dd_w_MW, daten$i1_dd_f_MW)\n")
  i1_dd <- c(daten$i1_dd_w_MW, daten$i1_dd_f_MW)
  
  scatter.smooth(i1_dd, i1_ue, main = "Diskrepanzerkennungswert vs. Initialer Überraschungswert", 
                 xlab = "Diskrepanzerkennungswert", ylab = "Initialer Überraschungswert", 
                 pch = 19, col = "lightblue")
  
  cat("cor.test(i1_dd, i1_ue, method = pearson)\n")
  cor.test(i1_dd, i1_ue, method = "pearson") %>% print()
  cat("cor.test(i1_dd, i1_ue, method = kendall)\n")
  cor.test(i1_dd, i1_ue, method = "kendall") %>% print()
  cat("cor.test(i1_dd, i1_ue, method = spearman)\n")
  cor.test(i1_dd, i1_ue, method = "spearman", exact = FALSE) %>% print()
}

# Hauptfunktion für die Durchführung der gesamten Analyse
fn_main <- function() {
  # Analyse der initialen Überraschung
  fn_descriptive_analysis(i1_w_MW = daten$i1_ue_w_MW, i1_f_MW = daten$i1_ue_f_MW)
  
  fn_graphical_analysis(i1_w_MW = daten$i1_ue_w_MW, i1_f_MW = daten$i1_ue_f_MW, title = "Initiale Überraschung")
  
  fn_tTest(i1_w_MW = daten$i1_ue_w_MW, i1_f_MW = daten$i1_ue_f_MW)
  
  fn_wilcoxon(i1_w_MW = daten$i1_ue_w_MW, i1_f_MW = daten$i1_ue_f_MW)
  
  fn_sign(i1_w_MW = daten$i1_ue_w_MW, i1_f_MW = daten$i1_ue_f_MW)
  
  # Analyse der Diskrepanzerkennung
  fn_descriptive_analysis(i1_w_MW = daten$i1_dd_w_MW, i1_f_MW = daten$i1_dd_f_MW)

  fn_graphical_analysis(i1_w_MW = daten$i1_dd_w_MW, i1_f_MW = daten$i1_dd_f_MW, title = "Diskrepanzerkennung")
  
  fn_tTest(i1_w_MW = daten$i1_dd_w_MW, i1_f_MW = daten$i1_dd_f_MW)
  
  fn_wilcoxon(i1_w_MW = daten$i1_dd_w_MW, i1_f_MW = daten$i1_dd_f_MW)
  
  fn_sign(i1_w_MW = daten$i1_dd_w_MW, i1_f_MW = daten$i1_dd_f_MW)
  
  # Analyse der Differenzen zwischen f- und w-Werten
  fn_sum_analysis()
  
  # Untersuchung auf Korrelation zwischen Diskrepanzerkennung und initialer Überraschung
  fn_correlation_analysis()
}

