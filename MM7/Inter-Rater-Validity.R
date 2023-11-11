library(readxl)
library(psych)

remove(Beispiel)
Beispiel <- read_excel("Beispiel.xlsx")
View(Beispiel)

attach(Beispiel)
describeBy(Discrepancy, Rater)
describeBy(Surprise, Rater)

tapply(Surprise, Rater, median)
tapply(Surprise, Rater, mean)

kruskal.test(Rater ~ Surprise)
kruskal.test(Rater ~ Discrepancy)

chisq.test(Rater,Discrepancy)
chisq.test(Rater,Surprise)

fisher.test(Rater,Discrepancy)
fisher.test(Rater,Surprise)

dr <- length(levels(factor(Rater)))
par(mfrow=c(1,dr))
intervals <- seq(-0.5, 3.5)

for (i in 1:dr) {
  hist(Surprise[Rater == i], breaks=intervals)
}

par(mfrow=c(1,1))


