install.packages("psych")
library(psych)

library(readxl)
Beispiel <- read_excel("Beispiel.xlsx")
View(Beispiel)

attach(Beispiel)
tapply(X=Discrepancy, INDEX = `False Memory`, FUN = summary)
tapply(X=Surprise, INDEX=`False Memory`, FUN = summary)

describeBy(Discrepancy, `False Memory`)
describeBy(Surprise, `False Memory`)

boxplot(Discrepancy ~ `False Memory`, Beispiel)
boxplot(Surprise ~ `False Memory`, Beispiel)

wilcox.test(Discrepancy ~ `False Memory`, Beispiel, exact=FALSE, conf.int=TRUE)
wilcox.test(Surprise ~ `False Memory`, Beispiel, exact=FALSE, conf.int=TRUE)
