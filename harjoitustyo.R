# Useamman selittäjän regressiomalli

library(foreign)
ht1.dat<-read.spss("elinolo2020.sav", to.data.frame=TRUE)
attach(ht1.dat)

set.seed(523834)
# 800 kokoinen otos
oma.otos1<-ht1.dat[sample(nrow(ht1.dat), 800), ]
attach(oma.otos1)

# kuluttajayksiköiden lukumäärä: rkyks
# asumismenot yhteensä: asmenot
# alueella asumisaika: alaika
# asunnon pinta-ala: pala

# Sirontakuviomatriisi

pairs(oma.otos1[,c(2,1,3,8)], pch = 19, lower.panel = NULL)

# Korrelaatiokertoimet

cor.test(rkyks, pala, method="pearson")
cor.test(asmenot, pala, method="pearson")
cor.test(alaika, pala, method="pearson")
cor.test(rkyks, pala, method="spearman")
cor.test(asmenot, pala, method="spearman")
cor.test(alaika, pala, method="spearman")

# Yksinkertainen regressiomalli
lm.ala <- lm(pala~rkyks)
summary(lm.ala)

# Jäännöstarkastelu

plot(fitted(lm.ala),resid(lm.ala))
hist(resid(lm.ala))

# Toistomittausmalli

# Alustus

library(foreign)
ht2.dat<-read.spss("Toistomittausaineisto2020.sav", to.data.frame=TRUE)
attach(ht2.dat)
# Suluissa olevan 1:n tilalle oma opiskelijanumero
set.seed(523834)
# 500 kokoinen otos
oma.otos2<-ht2.dat[sample(nrow(ht2.dat), 500), ]
attach(oma.otos2)

install.packages('dplyr')
library(dplyr)
naiset <- select(filter(oma.otos2, D2=='female'), c(Functional_M1, Functional_M2))
miehet <- select(filter(oma.otos2, D2=='male'), c(Functional_M1, Functional_M2))

# Tehdään normaalijakaumatestit

shapiro.test(Functional_M1)
shapiro.test(Functional_M2)

attach(naiset)
shapiro.test(Functional_M1)
shapiro.test(Functional_M2)

attach(miehet)
shapiro.test(Functional_M1)
shapiro.test(Functional_M2)




# Toistettujen mittausten varianssianalyysi

attach(oma.otos2)
fit1 = aov(Functional_M1 ~ Functional_M2, data=oma.otos2)
summary(fit1)

attach(naiset)
fit1 = aov(Functional_M1 ~ Functional_M2, data=oma.otos2)
summary(fit1)

attach(miehet)
fit1 = aov(Functional_M1 ~ Functional_M2, data=oma.otos2)
summary(fit1)





# Kategoristen vastemuuttujien mallitus

library(foreign)
ht3.dat<-read.spss("EK2011.sav", to.data.frame=TRUE)
attach(ht3.dat)
# Suluissa olevan 1:n tilalle oma opiskelijanumero
set.seed(523834)
# 800 kokoinen otos
oma.otos3<-ht3.dat[sample(nrow(ht3.dat), 800), ]
attach(oma.otos3)


# logistinen binäärinen regressio
logr_tyotilan <- glm(d32 ~ d2+ika, data=oma.otos3, family=binomial)
logr_tyotilan

summary(logr_tyotilan)

exp(cbind(OR=coef(logr_tyotilan), confint(logr_tyotilan)))

install.packages("fmsb")
library(fmsb)
data.nagel<-NagelkerkeR2(logr_tyotilan)
data.nagel





 # Monimuuttujamenetelmät

library(foreign)
ht4.dat<-read.spss("pankkiotos2020.sav", to.data.frame=TRUE)
attach(ht4.dat)
# Suluissa olevan 1:n tilalle oma opiskelijanumero
set.seed(523834)
# 1600 kokoinen otos
oma.otos4<-ht4.dat[sample(nrow(ht4.dat), 1600), ]
attach(oma.otos4)


# korrelaatiokertoimet

data.kor2<-cor(oma.otos4, method = "pearson", use = "complete.obs")
data.kor2

# pääkomponenttianalyysi

pca <- prcomp(data.kor2, center = T, scale = T)
pca
summary(pca)

#  Valitaan kolme pääkomponenttia, promax-rotaatio

pca.chosen <- pca$rotation[,1:3]
pca.promax <- promax(pca.chosen)
pca.promax
