# Pakete laden
library(readxl)

# Daten einlesen
daten <- read_excel("Aggregierte_Daten_Studierende MM7_JKH.xlsx")

ausreisser <- subset(daten, (daten$i1_ue_f_MW > 3) 
                     | (daten$i1_ue_w_MW > 3) 
                     | (daten$i1_dd_f_MW > 1) 
                     | (daten$i1_dd_w_MW > 1)
                     | (daten$i1_ue_f_MW < 1)
                     | (daten$i1_ue_w_MW < 1))


