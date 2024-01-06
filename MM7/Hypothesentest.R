install.packages("psych")
library(psych)
library(readxl)

remove(Data)
Data <- read_excel("Aufbereitete_Aggregierte_Daten_MM7_JKH.xlsx")

attach(Data)

describeBy(i1_dd, `erlebnisbasiert`)
describeBy(i1_dd, `erlebnisbasiert`)

boxplot(i1_dd ~ `erlebnisbasiert`, Data)
boxplot(i1_dd ~ `erlebnisbasiert`, Data)

wilcox.test(i1_dd ~ `erlebnisbasiert`, Data, exact=FALSE, conf.int=TRUE)
wilcox.test(i1_dd ~ `erlebnisbasiert`, Data, exact=FALSE, conf.int=TRUE)

par(mfrow=c(1,2))
intervals <- seq(-0.5, 3.5)
hist(i1_dd[`erlebnisbasiert` == 0], breaks=intervals)
hist(i1_dd[`erlebnisbasiert` == 1], breaks=intervals)

intervals <- seq(-0.5, 1.5)
hist(i1_dd[`erlebnisbasiert` == 0], breaks=intervals)
hist(i1_dd[`erlebnisbasiert` == 1], breaks=intervals)

par(mfrow=c(1,1))

