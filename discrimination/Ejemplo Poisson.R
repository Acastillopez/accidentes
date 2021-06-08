
####################################
####
####  EJEMPLO REGRESIÓN POISSON
####
####################################

# Lectura de los datos
setwd("E:/Documentos Irantzu Copia/Docencia/EHU/Master Modelizacion/2019/R MarkDown Documentacion")
especies <- read.table("especies.txt",header=TRUE)
names(especies)
summary(especies)
attach(especies)

# Análisis exploratorio
plot(Biomasa,Especies,type="n")
points(Biomasa[pH==0],Especies[pH==0])
points(Biomasa[pH==1],Especies[pH==1],pch=2,col=2)
points(Biomasa[pH==2],Especies[pH==2],pch=3,col=3)
legend(4,45,legend=c("Bajo","Medio","Alto"),pch=c(1,2,3),col=c(1,2,3))

abline(lm(Especies[pH==0]~Biomasa[pH==0]), col=1)
abline(lm(Especies[pH==1]~Biomasa[pH==1]), col=2)
abline(lm(Especies[pH==2]~Biomasa[pH==2]), col=3)


# Ajuste del modelo de Poisson

# Modelos univariantes
m0=glm(Especies~Biomasa,family=poisson)
m1=glm(Especies~pH,family=poisson)

# Significación estadística de las variables
anova(m0,test="Chi")
anova(m1,test="Chi")

# Modelos multivariable aditivo
especies0=glm(Especies~Biomasa+pH,family=poisson)
summary(especies0)

# Modelos multivariable con interacción
especies1=glm(Especies~Biomasa*pH,family=poisson)
summary(especies1)

# Significación estadística de la interacción
anova(especies1,especies0,test="Chisq")

# Gráfico del modelo estimado
plot(Biomasa,Especies,type="n")
points(Biomasa[pH==0],Especies[pH==0])
points(Biomasa[pH==1],Especies[pH==1],pch=2,col=2)
points(Biomasa[pH==2],Especies[pH==2],pch=3,col=3)
legend(4,45,legend=c("Bajo","Medio","Alto"),pch=c(1,2,3),col=c(1,2,3))


ajustados1=fitted(especies1)
x=Biomasa[pH==0]
or=order(x)
y=ajustados1[pH==0]
lines(x[or],y[or])

x=Biomasa[pH==1]
or=order(x)
y=ajustados1[pH==1]
lines(x[or],y[or])


x=Biomasa[pH==2]
or=order(x)
y=ajustados1[pH==2]
lines(x[or],y[or])



############################################################
####
####  EJEMPLO REGRESIÓN POISSON PARA TASAS DE INCIDENCIA
####
############################################################

setwd("E:/Documentos Irantzu Copia/Docencia/EHU/Master Modelizacion/2019/R MarkDown Documentacion")
diabetes <- read.table("diabetes.txt",header=TRUE)
names(diabetes)
diabetes$edad=factor(diabetes$edad)
diabetes$periodo=factor(diabetes$periodo)
diabetes$estacion=factor(diabetes$estacion)
diabetes$sexo=factor(diabetes$sexo)
diabetes$mes=factor(diabetes$mes)
diabetes[1:10,]

attach(diabetes)

tasa=casos/poblacion
tasa
tasa2=tasa*100000
tasa2

tasa2[edad==1 & mes==12 & sexo==2 & periodo==1997]


## Ajuste del modelo

logpobla <- log(poblacion)
diabetes1 <- glm(casos~periodo+offset(logpobla),family=poisson) # Con la opcion offset le estoy indicando que es un modelo para tasas.

diabetes2 <- glm(casos~sexo+offset(logpobla),family=poisson)
summary(diabetes2)$coefficients

diabetes3 <- glm(casos~edad+offset(logpobla),family=poisson)
summary(diabetes3)$coefficients

diabetes4 <- glm(casos~edad+ sexo + offset(logpobla),family=poisson)

anova(diabetes3, diabetes4, test="Chisq") #la variable sexo no es significativa


### Los valores ajustados de este modelo son el conteo.

exp(diabetes3$coeff) ## Riesgo relativo de la variable edad
exp(confint(diabetes3)) ## Intervalos de confianza para RR
diabetes.resid=rstandard(diabetes3) ## errores estándar
summary(diabetes.resid)
diabetes.ajustados=predict(diabetes3,type="response") ## Numero medio de niños con diabetes diferente por cada grupo de edad.
tasa.ajustada=diabetes.ajustados/poblacion ## Tasa de diabetes 
tapply(tasa.ajustada,edad,mean) ## tasa de diabetes diferente por cada grupo de edad.

