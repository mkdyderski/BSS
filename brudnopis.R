###Brudnopis - kod użyty do stworzenia częsci obrazków i pomocy

###eksplotracja
eks<-read.csv('datasety/vege_1517_traits.csv',sep=';')
class(eks)



#regresja part 1
sosny<-read.csv('datasety/sosna.csv',sep=';')
cor(sosny$AB,sosny$Age)
library(corrplot)
corrplot(cor(sosny[,6:13]))

summary(sosny[,5:13])
