###Brudnopis - kod użyty do stworzenia częsci obrazków i pomocy
library(tidyverse)

summary(glm(richness~a,data=prunus, family=poisson()))
ggplot(prunus, aes(x=a,y=richness))+geom_point()+geom_smooth(method = 'glm', method.args=list(family='poisson'),col='darkgreen')+theme_bw()+geom_smooth(method='lm',col='red')

ggplot(prunus, aes(x=shannon,y=richness))+geom_point()+geom_smooth(method = 'glm', method.args=list(family='poisson'),col='darkgreen')+theme_bw()+geom_smooth(method='lm',col='red')

mod1<-lm(prunusc~richness,prunus)
mod2<-glm(prunusc~richness, prunus, family=poisson())
AIC(mod1,mod2)

ggplot(prunus, aes(x=richness,y=prunusc))+geom_point()+geom_smooth(method = 'glm', method.args=list(family='poisson'),col='darkgreen')+theme_bw()+geom_smooth(method='lm',col='red')

###eksplotracja
eks<-read.csv('datasety/vege_1517_traits.csv',sep=';')
class(eks)


#lichenes
lichenes<-read.csv('datasety/lichenes1.csv',sep=';')

model<-glm(Rich~EIV_N+habitat+time, data=lichenes)
summary(model)

model2<-glm(Rich~EIV_N+habitat+time, data=lichenes, family=poisson)
summary(model2)


model<-glm(prunusc~richness,data=prunus)
summary(model)

model2<-glm(prunusc~richness,data=prunus, family=poisson)
summary(model2)

library(pscl)
model3<-zeroinfl(prunusc~richness|richness, data=prunus) #kreska jest ważna
summary(model3)

AIC(model, model2, model3)

plot(hist(prunus$richness))


afis<-read.csv('datasety/afis.csv',sep=';')

mm<-glm(Ficavern~OLDFR,afis, family=binomial(link='logit'))
library(multcomp)
cld(glht(mm, mcp=(OLDFR='Tukey')))

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


library(lmerTest);library(tidyverse)

hotspots<-read.csv('datasety/hotspots.csv',sep=';')

mod<-lm(plants~mammals+mainl,data=hotspots)
summary(mod)

ggplot(hotspots, aes(x=mammals,y=plants,col=mainl))+geom_point()+theme_bw()+geom_smooth(method='lm')

ggplot(hotspots, aes(x=mammals,y=plants,col=mainl,shape=continent))+geom_point()+theme_bw()+geom_smooth(method='lm')
confint(mod2)

mod2<-lmer(plants~mammals+(1|continent),hotspots)
summary(mod2)

mod3<-lmer(plants~mammals+(mammals|continent),hotspots)
summary(mod3)

coef(mod2)

coef(mod3)
r.squaredGLMM(mod2)
r.squaredGLMM(mod3)


AIC(mod, mod2, mod3)

#czeremcha
prunus<-read.csv('datasety/prunus.csv',sep=';')
summary(prunus)



survi<-read.csv('datasety/survi.csv',sep=';')


mod.lm<-glm(surv~light,family=binomial(link='logit'),survi)
mod<-glmer(surv~light+(1|plot:blok)+(1|rok),family=binomial(link='logit'),survi)

ggplot(survi, aes(x=light, y=surv, col=factor(rok)))+geom_point()+geom_smooth(method='glm',method.args=list(family='binomial'))


mod2<-glmer(surv~light+(light|plot:blok)+(1|rok),family=binomial(link='logit'),survi)
mod3<-glmer(surv~light+(light|plot:blok)+(light|rok),family=binomial(link='logit'),survi)
mod4<-glmer(surv~light+(1|plot:blok)+(light|rok),family=binomial(link='logit'),survi)

AIC(mod.lm, mod,mod2,mod3,mod4)


confint(mod)

summary(mod4)

summary(mod)
summary(mod.lm)
AIC(mod.lm, mod)
r.squaredGLMM(mod4)
