dados=read.csv2("copaibagrandeibt.csv");

names(dados)

library(dplyr)
data2<-select(dados, germ, plant)
data2

hist(data2$germ) #nao tem distribui??o normal
hist(data2$plant) #nao tem distribuicao normal

#teste de shapiro wilk
shapiro.test(data2$germ)
shapiro.test(data2$plant)
#O p-valor foi de 0.45. Isto quer dizer que os dados s?o normais, 
#pois n?o diferem de uma curva normal. O p-valor for < 0.05 indica que os dados n?o apresentam normalidade.

#install.packages("dgof")
library(dgof)

#teste kolmogorov smirnov (normalidade tambem)
ks.test(data2$germ,"pnorm",mean(data2$germ),sd(data2$germ))
ks.test(data2$plant,"pnorm",mean(data2$plant),sd(data2$plant))
#os dois testes n?o foram normais...

# https://medium.com/bio-data-blog/testes-de-normalidade-no-r-fccefe7ae0fd

#teste lilliefors
#install.packages("nortest")
library(nortest)
lillie.test(data2$germ)
lillie.test(data2$plant)
#nao deu normal, de novo

#teste anderson-darling
ad.test(data2$germ)
ad.test(data2$plant)
#nao deu normal, ele ? mais poderoso pra mais de 50 amostras, por isso a gente usa Anderson-Darling

#testes n?o parametricos

#Kruskal Wallis
kruskal.test(germ ~ trat, data=dados)
kruskal.test(plant ~ trat, data=dados)

#"tukey" do kruskal wallis
#install.packages("pgirmess")
#library(pgirmess)
#resp<-data2$germ
#categ<-as.factor(c(1:210))
#kruskalmc(resp, categ, cont="two-tailed")

library(agricolae)
ktg<-kruskal(dados$germ, dados$trat); ktg
ktp<-kruskal(dados$plant, dados$trat); ktp

#teste de friedman
frgerm<-friedman(dados$rep,dados$trat,dados$germ,alpha=0.05, group=TRUE,main="Teste de Friedman"); frgerm
frplant<-friedman(dados$rep,dados$trat,dados$plant,alpha=0.05, group=TRUE,main="Teste de Friedman"); frplant

#analise glm
#install.packages("hnp")
#install.packages("DescTools")

#glm para germinacao  = criei os modelos gaussianos
glm1<-glm(germ ~ temp, data=dados, family = gaussian (link="identity")); glm1
glm2<-glm(germ ~ temp*sub, data=dados, family = gaussian (link="identity")); glm2
glm3<-glm(germ ~ temp*sub*luz, data=dados, family = gaussian (link="identity")); glm3
glm4<-glm(germ ~ temp*sub*luz*umidade, data=dados, family = gaussian (link="identity")); glm4
glm5<-glm(germ ~ temp*luz, data=dados, family = gaussian (link="identity")); glm5
glm6<-glm(germ ~ temp*luz*umidade, data=dados, family = gaussian (link="identity")); glm6
glm7<-glm(germ ~ temp*umidade, data=dados, family = gaussian (link="identity")); glm7
glm8<-glm(germ ~ sub, data=dados, family = gaussian (link="identity")); glm8
glm9<-glm(germ ~ sub*luz, data=dados, family = gaussian (link="identity")); glm9
glm10<-glm(germ ~ sub*luz*umidade, data=dados, family = gaussian (link="identity")); glm10
glm11<-glm(germ ~ sub*umidade, data=dados, family = gaussian (link="identity")); glm11
glm12<-glm(germ ~ luz, data=dados, family = gaussian (link="identity")); glm12
glm13<-glm(germ ~ luz*umidade, data=dados, family = gaussian (link="identity")); glm13
glm14<-glm(germ ~ umidade, data=dados, family = gaussian (link="identity")); glm14

#avaliacao dos dados em geral dos modelos

summary(glm1)
summary(glm2)
summary(glm3)
summary(glm4)
summary(glm5)
summary(glm5)
summary(glm6)
summary(glm7)
summary(glm8)
summary(glm9)
summary(glm10)
summary(glm11)
summary(glm12)
summary(glm13)
summary(glm14) #tem que fazer igual pra cada um dos modelos... 

#glm para plantula = mesma coisa da germinacao
glm15<-glm(plant ~ temp, data=dados, family = gaussian (link="identity")); glm15
glm16<-glm(plant ~ temp*sub, data=dados, family = gaussian (link="identity")); glm16
glm17<-glm(plant ~ temp*sub*luz, data=dados, family = gaussian (link="identity")); glm17
glm18<-glm(plant ~ temp*sub*luz*umidade, data=dados, family = gaussian (link="identity")); glm18
glm19<-glm(plant ~ temp*luz, data=dados, family = gaussian (link="identity")); glm19
glm20<-glm(plant ~ temp*luz*umidade, data=dados, family = gaussian (link="identity")); glm20
glm21<-glm(plant ~ temp*umidade, data=dados, family = gaussian (link="identity")); glm21
glm22<-glm(plant ~ sub, data=dados, family = gaussian (link="identity")); glm22
glm23<-glm(plant ~ sub*luz, data=dados, family = gaussian (link="identity")); glm23
glm24<-glm(plant ~ sub*luz*umidade, data=dados, family = gaussian (link="identity")); glm24
glm25<-glm(plant ~ sub*umidade, data=dados, family = gaussian (link="identity")); glm25
glm26<-glm(plant ~ luz, data=dados, family = gaussian (link="identity")); glm26
glm27<-glm(plant ~ luz*umidade, data=dados, family = gaussian (link="identity")); glm27
glm28<-glm(plant ~ umidade, data=dados, family = gaussian (link="identity")); glm28

summary(glm15)
summary(glm16)
summary(glm17)
summary(glm18)
summary(glm19)
summary(glm20)
summary(glm21)
summary(glm22)
summary(glm23)
summary(glm24)
summary(glm25)
summary(glm26)
summary(glm27)
summary(glm28)

#o ajuste segundo o site do RPubs pra poder calcular a deviance depois
ajustegerm<-c('glm1','glm2','glm3','glm4','glm5','glm6','glm7','glm8','glm9','glm10','glm11','glm12','glm13','glm14')
ajusteplant<-c('glm15','glm16','glm17','glm18','glm19','glm20','glm21','glm22','glm23','glm24','glm25','glm26','glm27','glm28')

#arrumar o AIC, menor AIC, melhor modelo
aicgerm<-c(AIC(glm1),AIC(glm2),AIC(glm3),AIC(glm4), AIC(glm5),AIC(glm6),AIC(glm7),AIC(glm8),
           AIC(glm9),AIC(glm10),AIC(glm11),AIC(glm12),AIC(glm13),AIC(glm14))
aicplant<-c(AIC(glm15),AIC(glm16),AIC(glm17),AIC(glm18), AIC(glm19),AIC(glm20),AIC(glm21),
           AIC(glm22),AIC(glm23),AIC(glm24),AIC(glm25), AIC(glm26),AIC(glm27),AIC(glm28))

#arrumar deviance; menor deviance, melhor modelo
devgerm<-c(deviance(glm1),deviance(glm2),deviance(glm3),deviance(glm4),
           deviance(glm5),deviance(glm6),deviance(glm7),deviance(glm8),
           deviance(glm9),deviance(glm10),deviance(glm11),deviance(glm12),
           deviance(glm13),deviance(glm14))
devplant<-c(deviance(glm15),deviance(glm16),deviance(glm17),deviance(glm18),
           deviance(glm19),deviance(glm20),deviance(glm21),deviance(glm22),
           deviance(glm23),deviance(glm24),deviance(glm25),deviance(glm26),
           deviance(glm27),deviance(glm28))

#arrumar verossimilhança
vergerm<-c(logLik(glm1), logLik(glm2),logLik(glm3), logLik(glm4),
           logLik(glm5),logLik(glm6),logLik(glm7),logLik(glm8),
           logLik(glm9),logLik(glm10),logLik(glm11),logLik(glm12),
           logLik(glm13),logLik(glm14))
verplant<-c(logLik(glm15), logLik(glm16),logLik(glm17), logLik(glm18),
           logLik(glm19),logLik(glm20),logLik(glm21),logLik(glm22),
           logLik(glm23),logLik(glm24),logLik(glm25),logLik(glm26),
           logLik(glm27),logLik(glm28))

#cria a tabela com os ajustes, aic, verossimilhança e deviance
data.frame(ajustegerm,aicgerm,vergerm,devgerm)
data.frame(ajusteplant,aicplant,verplant,devplant)

library(DescTools)

#maior pseudoR2, melhor modelo
PseudoR2(glm1) 
PseudoR2(glm2)
PseudoR2(glm3)
PseudoR2(glm4)
PseudoR2(glm5)
PseudoR2(glm6)
PseudoR2(glm7)
PseudoR2(glm8)
PseudoR2(glm9)
PseudoR2(glm10)
PseudoR2(glm11)
PseudoR2(glm12)
PseudoR2(glm13)
PseudoR2(glm14)
PseudoR2(glm15)
PseudoR2(glm16)
PseudoR2(glm17)
PseudoR2(glm18)
PseudoR2(glm19)
PseudoR2(glm20)
PseudoR2(glm21)
PseudoR2(glm22)
PseudoR2(glm23)
PseudoR2(glm24)
PseudoR2(glm25)
PseudoR2(glm26)
PseudoR2(glm27)
PseudoR2(glm28)

#menor deviance, melhor modelo
deviance(glm1)
deviance(glm2)
deviance(glm3)
deviance(glm4)
deviance(glm5)
deviance(glm6)
deviance(glm7)
deviance(glm8)
deviance(glm9)
deviance(glm10)
deviance(glm11)
deviance(glm12)
deviance(glm13)
deviance(glm14)
deviance(glm15)
deviance(glm16)
deviance(glm17)
deviance(glm18)
deviance(glm19)
deviance(glm20)
deviance(glm21)
deviance(glm22)
deviance(glm23)
deviance(glm24)
deviance(glm25)
deviance(glm26)
deviance(glm27)
deviance(glm28)


#https://www.rpubs.com/TiagoCosta/502155

#grafico do melhor modelo escolhido
library(hnp)

#grafico residuos melhor modelo germinacao
hnp(glm4$residuals, sim=99, resid.type='deviance',how.many.out=T,
    conf=0.95, scale=T)

#grafico residuos melhor modelo plantulas
hnp(glm18$residuals, sim=99, resid.type='deviance',how.many.out=T,
    conf=0.95, scale=T)

#interpretacao dos coeficientes do melhor modelo
#germinacao
glm4$coefficients

#plantulas
glm18$coefficients
