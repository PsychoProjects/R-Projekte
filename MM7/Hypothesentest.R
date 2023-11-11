install.packages("psych")
library(psych)
library(readxl)

remove(Beispiel)
Beispiel <- read_excel("Beispiel.xlsx")

attach(Beispiel)

describeBy(Discrepancy, `False Memory`)
describeBy(Surprise, `False Memory`)

boxplot(Discrepancy ~ `False Memory`, Beispiel)
boxplot(Surprise ~ `False Memory`, Beispiel)

wilcox.test(Discrepancy ~ `False Memory`, Beispiel, exact=FALSE, conf.int=TRUE)
wilcox.test(Surprise ~ `False Memory`, Beispiel, exact=FALSE, conf.int=TRUE)

par(mfrow=c(1,2))
intervals <- seq(-0.5, 3.5)
hist(Surprise[`False Memory` == 0], breaks=intervals)
hist(Surprise[`False Memory` == 1], breaks=intervals)

intervals <- seq(-0.5, 1.5)
hist(Discrepancy[`False Memory` == 0], breaks=intervals)
hist(Discrepancy[`False Memory` == 1], breaks=intervals)

par(mfrow=c(1,1))
