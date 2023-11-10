library(readxl)
Beispiel <- read_excel("Beispiel.xlsx")
View(Beispiel)

attach(Beispiel)
tapply(X=Discrepancy, INDEX=Rater, FUN = summary)
tapply(X=Surprise, INDEX=Rater, FUN = summary)

kruskal.test(Rater ~ Surprise)
kruskal.test(Rater ~ Discrepancy)

chisq.test(Rater,Discrepancy)
chisq.test(Rater,Surprise)

fisher.test(Rater,Discrepancy)
fisher.test(Rater,Surprise)

