# Pakete laden
library(readxl)
library(dplyr)
library(psych)
library(datasets)
library(effsize)
library(BSDA)
library(ggplot2)
library(exactRankTests)

# Daten einlesen
daten <- read_excel("Mittelwerte_MM7_korrigiert.xlsx")

fn_descriptive_analysis <- function(i1_w_MW, i1_f_MW){
  cat(">>> i1_w_MW\n")
  summary(i1_w_MW, digits = 3) %>% print()
  cat("\n")
  describe(i1_w_MW)  %>% print()
  cat("\n\n")
  
  cat(">>> i1_f_MW\n")
  summary(i1_f_MW, digits = 3) %>% print()
  cat("\n")
  describe(i1_f_MW) %>% print()
}

fn_graphical_analysis <- function(i1_w_MW, i1_f_MW, title = "") {
  boxplot(i1_w_MW, i1_f_MW, names = c("wahr", "falsch"), ylab = "Rating", xlab = "erlebnisbasisert", main = title)
  
  par(mfrow = c(1, 2), pty = "s")
  hist(i1_w_MW, breaks = seq(min(i1_w_MW) - 0.1, max(i1_w_MW) + 0.1, by = 0.1), main = "erlebnisbasiert", xlab = "Rating", col = "lightblue", border = "black")
  hist(i1_f_MW, breaks = seq(min(i1_f_MW) - 0.1, max(i1_f_MW) + 0.1, by = 0.1), main = "nicht erlebnisbasiert", xlab = "Rating", col = "lightblue", border = "black")
  par(mfrow = c(1, 1))
}

fn_tTest <- function (i1_w_MW, i1_f_MW, differences) {
  cat(">>> Vorbedingungen prüfen\n")

  cat(">>> Grafische Beurteilung der Normalverteilung der Differenzen\n")
  hist(differences, breaks = seq(min(differences) - 0.5, max(differences) + 0.5, by = 0.1), main = "Histogramm der Differenzen", xlab = "Differenzen", ylab = "Häufigkeit", col = "lightblue", border = "black")
  
  # Shapiro-Wilk-Test zur Überprüfung der Normalverteilung
  shapiro_test <- shapiro.test(differences) %>% print() 

  # t-Test durchführen
  cat(">>> t-Test\n")
  t_test <- t.test(i1_f_MW, i1_w_MW, paired = TRUE) %>% print() 
  
  # Effektstärke mittels Cohens d berechnen
  cat(">>> Effektstärke gemäß Cohen's d\n")
  cohensd <- effsize::cohen.d(i1_f_MW, i1_w_MW, paired = TRUE) %>% print() 
}

fn_wilcoxon <- function (i1_w_MW, i1_f_MW, differences) {
  cat(">>> Vorbedingungen prüfen\n")
  
  # Boxplot
  boxplot(differences, main = "Boxplot der Differenzen", ylab = "Differenzen", col = "lightblue")
  
  cat(">>> Wilcoxon-Vorzeichen-Rang-Test\n")
  result <- wilcox.exact(i1_f_MW, i1_w_MW, paired = TRUE) %>% print() 

  # Effektstärke für den Wilcoxon-Test berechnen
  cat(">>> Effektstärke gemäß rangbasiertem Effektgrößenindex\n")
  n <- length(differences)
  z <- qnorm(result$p.value / 2)
  r <- abs(z) / sqrt(n)
  cat("Effektstärke (r) für den Wilcoxon-Test: ", r, "\n\n")
}

fn_sign <- function (i1_w_MW, i1_f_MW, differences) {
  cat(">>> Vorzeichen-Test\n")
  
  # Test durchführen (library BSDA)
  result <- SIGN.test(differences, md=0, alternative="two.sided", conf.level=0.95) %>% print() 

  # Effektstärke für den Vorzeichen-Test beurteilen
  cat(">>> Effektstärke gemäß rangbasiertem Effektgrößenindex\n")
  n <- length(differences) # Anzahl der Beobachtungen
  z <- qnorm(result$p.value / 2) # Berechnung der Z-Statistik
  r <- abs(z) / sqrt(n)
  
  cat("Effektstärke (r) für den Vorzeichen-Test: ", r, "\n\n")
}

fn_sum_analysis <- function() {
  # Bildung der Summen bei Normierung der ue-Daten auf Werte in [0;1]
  f_daten <- daten$i1_dd_f_MW + (daten$i1_ue_f_MW - 1) / 2
  w_daten <- daten$i1_dd_w_MW + (daten$i1_ue_w_MW - 1) / 2

  fn_tTest(i1_w_MW = w_daten, i1_f_MW = f_daten, differences = (f_daten - w_daten))  
}

fn_main <- function() {
  # Analyse der Diskrepanzerkennung
  ## Deskriptive Statistik
  fn_descriptive_analysis(i1_w_MW = daten$i1_dd_w_MW, i1_f_MW = daten$i1_dd_f_MW)
  
  
  ## Grafische Analyse
  fn_graphical_analysis(i1_w_MW = daten$i1_dd_w_MW, i1_f_MW = daten$i1_dd_f_MW, title = "Diskrepanzerkennung")
  
  
  ## t-Test
  fn_tTest(i1_w_MW = daten$i1_dd_w_MW, 
           i1_f_MW = daten$i1_dd_f_MW, 
           differences = (daten$i1_dd_f_MW - daten$i1_dd_w_MW))
  
  
  ## Nicht parametrischer Test mittels Wilcoxon-Vorzeichen-Rang-Test
  fn_wilcoxon(i1_w_MW = daten$i1_dd_w_MW, 
              i1_f_MW = daten$i1_dd_f_MW, 
              differences = (daten$i1_dd_f_MW - daten$i1_dd_w_MW))
  
  
  ## Nicht parametrischer Test mittels Vorzeichen-Test
  fn_sign(i1_w_MW = daten$i1_dd_w_MW, 
          i1_f_MW = daten$i1_dd_f_MW, 
          differences = (daten$i1_dd_f_MW - daten$i1_dd_w_MW))
  
  
  # Analyse der initialen Überraschung
  ## Deskriptive Statistik
  fn_descriptive_analysis(i1_w_MW = daten$i1_ue_w_MW, i1_f_MW = daten$i1_ue_f_MW)
  
  ## Grafische Analyse
  fn_graphical_analysis(i1_w_MW = daten$i1_ue_w_MW, i1_f_MW = daten$i1_ue_f_MW, title = "Initiale Überraschung")
  
  ## t-Test
  fn_tTest(i1_w_MW = daten$i1_ue_w_MW, 
           i1_f_MW = daten$i1_ue_f_MW, 
           differences = (daten$i1_ue_f_MW - daten$i1_ue_w_MW))
  
  
  ## Nicht parametrischer Test mittels Wilcoxon-Vorzeichen-Rang-Test
  fn_wilcoxon(i1_w_MW = daten$i1_ue_w_MW, 
              i1_f_MW = daten$i1_ue_f_MW, 
              differences = (daten$i1_ue_f_MW - daten$i1_ue_w_MW))
  
  
  ## Nicht parametrischer Test mittels Vorzeichen-Test
  fn_sign(i1_w_MW = daten$i1_ue_w_MW, 
          i1_f_MW = daten$i1_ue_f_MW, 
          differences = (daten$i1_ue_f_MW - daten$i1_ue_w_MW))
  
  
  # Analyse der Differenzen zwischen f- und w-Werten
  fn_sum_analysis()
}

