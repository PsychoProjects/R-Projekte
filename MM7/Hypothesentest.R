install.packages("psych")
library(psych)
library(readxl)

remove(Data)
Data <- read_excel("Beispiel.xlsx")

attach(Data)

describeBy(Discrepancy, `False Memory`)
describeBy(Surprise, `False Memory`)

boxplot(Discrepancy ~ `False Memory`, Data)
boxplot(Surprise ~ `False Memory`, Data)

wilcox.test(Discrepancy ~ `False Memory`, Data, exact=FALSE, conf.int=TRUE)
wilcox.test(Surprise ~ `False Memory`, Data, exact=FALSE, conf.int=TRUE)

par(mfrow=c(1,2))
intervals <- seq(-0.5, 3.5)
hist(Surprise[`False Memory` == 0], breaks=intervals)
hist(Surprise[`False Memory` == 1], breaks=intervals)

intervals <- seq(-0.5, 1.5)
hist(Discrepancy[`False Memory` == 0], breaks=intervals)
hist(Discrepancy[`False Memory` == 1], breaks=intervals)

par(mfrow=c(1,1))
