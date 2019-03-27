###Brudnopis - kod użyty do stworzenia częsci obrazków i pomocy
library(tidyverse)
###eksplotracja
eks<-read.csv('datasety/vege_1517_traits.csv',sep=';')
class(eks)

library(GGally)
ggpairs(eks[,c(2,4:24)])

ggpairs(eks[,c(2,4,11,14,19,21,24)])

ggpairs(eks[,c(2,4,11,19,21)])+theme_bw()


mm<-lm(SLA~N, data=eks)

summary(mm)

summary(lm(predict(mm)~eks$SLA))


ggplot(eks, aes(x=SLA, y=seed_mass,col=strategy))+geom_point()+theme_bw()+scale_y_log10()

ggplot(eks, aes(x=SLA, y=seed_mass,col=strategy))+geom_point()+theme_bw()+scale_y_log10()+geom_smooth(method='lm')

ggplot(eks, aes(x=SLA, y=seed_mass,col=strategy))+geom_point()+theme_bw()+scale_y_log10()+geom_smooth(method='lm')+facet_wrap(~strategy, scales = 'free')

ggplot(eks, aes(x=canopy_height))+geom_histogram()

#regresja part 1
sosny<-read.csv('datasety/sosna.csv',sep=';')
cor(sosny$AB,sosny$Age)
library(corrplot)
corrplot(cor(sosny[,6:13]))

summary(sosny[,5:13])

mm<-lm(AB~V, data=sosny)


library(MuMIn)
global.model<-lm(AB~V+G+Hg+dens+Age, data=sosny,na.action = na.fail)
dred<-dredge(global.model)
dred


summary(lm(AB~type,data=sosny))

an<-aov(lm(AB~type,data=sosny))
summary(an)
library(agricolae)
HSD.test(an, 'type',console = T)

library(multcomp)
summary(glht(an,mcp(type='Tukey')))
cld(glht(an,mcp(type='Tukey')))


an2<-aov(lm(AB~type*Soil.type,data=sosny))
summary(an2)


summary(lm(AB~Age+type,data=sosny))
ggplot(sosny, aes(x=Age,y=AB,col=type))+geom_point()+geom_smooth(method='lm')

an3<-aov(lm(AB~Age+type,data=sosny))
summary(an3)

cld(glht(an3, mcp(type='Tukey')))


model<-lm(BR~Age, data=sosny)
summary(model)

ggplot(sosny[-c(33,36),], aes(x=Age,y=BR))+geom_point()+geom_smooth(method = 'lm')

plot(model)
as.data.frame(influence(model))

model<-lm(BR~Age, data=sosny)
model2<-lm(BR~Age, data=sosny[-c(33,36),])
summary(model2)

library(car)
outlierTest(model)

cechy<-sosny[,c(8:11)]
mm<-manova(cbind(AB,V,Hg,G)~type,data=sosny)
summary(mm)

model<-lm(AB~poly(V,2),data=sosny)
nowedane<-data.frame(V=c(1,2,3,10))
predict(model,nowedane)

mm

summary(lm(predict(mm)~sosny$AB))

(sum((predict(mm)-mean(sosny$AB))^2)/sum((sosny$AB-mean(sosny$AB))^2))

dupa<-read.csv('https://raw.githubusercontent.com/mkdyderski/BSS/BSS2019/datasety/vege_1517_traits.csv',sep=';')




#czeremcha
prunus<-read.csv('datasety/prunus.csv',sep=';')
summary(prunus)
