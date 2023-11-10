library(readxl)
Beispiel <- read_excel("Beispiel.xlsx")
View(Beispiel)

attach(Beispiel)
tapply(X=Discrepancy, INDEX=Rater, FUN = summary)
tapply(X=Surprise, INDEX=Rater, FUN = summary)

#disc_aov <- aov(Rater ~ Discrepancy, data = Beispiel)
#surp_aov <- aov(Rater ~ Surprise)
#summary(disc_aov)
#summary(surp_aov)

kruskal.test(Rater ~ Surprise)
kruskal.test(Rater ~ Discrepancy)

chisq.test(Rater,Discrepancy)
chisq.test(Rater,Surprise)

fisher.test(Rater,Discrepancy)
fisher.test(Rater,Surprise)

