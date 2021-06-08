library(MASS)
library(foreign)
library(VGAM)
library(effects)


###################################
#LECTURA DE LOS DATOS
###################################

postgrado=read.dta(file="postgrado.dta")
postgrado$pared=factor(postgrado$pared)
postgrado$public=factor(postgrado$public)
summary(postgrado)


####################################
#AJUSTE DEL MODELO UNIVARIANTE
###################################

ord1=polr(apply~pared,postgrado)
summary(ord1)

ord2=polr(apply~public,postgrado)
summary(ord2)

ord3=polr(apply~gpa,postgrado)
summary(ord3)

# Significación estadística . Test razón de verosimilitud
ord0=polr(apply~1,postgrado)

anova(ord0,ord1)
anova(ord0,ord2)
anova(ord0,ord3)


####################################
#AJUSTE DEL MODELO MULTIPLE
###################################

ord4=polr(apply~pared+gpa,postgrado) 
summary(ord4)

# Intervalos de confianza
ci=confint(ord4)
exp(cbind(OR=coef(ord4),ci))

# Grafico de las probabilidades estimadas

plot(effect("pared", ord4, style='stack'))
plot(effect("gpa", ord4, style='stack'))


# Comprobación Odds Proporcionales
apply2 = ordered(postgrado$apply)

m0 <- vglm(apply2~pared+gpa,family=cumulative(parallel=T),
           data=postgrado)

m1 <- vglm(apply2~pared+gpa,family=cumulative(parallel=F),
           data=postgrado)

test.po <- 2*logLik(m1)-2*logLik(m0)
df.po <- length(coef(m1))-length(coef(m0))
test.po
df.po
1-pchisq(test.po,df=df.po)


