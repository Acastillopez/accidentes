
####################################
####
####  EJEMPLO REGRESIÓN MULTINOMIAL
####
####################################

# Lectura de los datos
setwd("E:/Documentos Irantzu Copia/Docencia/EHU/Master Modelizacion/2019/R MarkDown Documentacion")
mamexp <- read.table("mamexp.txt",header=TRUE)
names(mamexp)

# Convertimos variables categóricas en factor
mamexp$symp <- factor(mamexp$symp)
mamexp$hist <- factor(mamexp$hist)
mamexp$bse <- factor(mamexp$bse)
mamexp$dect <- factor(mamexp$dect)
attach(mamexp)
table(me)

# Cargamos la librería para usar la función multinom()
library("nnet")

# Ajustamos modelos multinomiales univariantes
multi1_hist <- multinom(me~hist)
multi1_dect <- multinom(me~dect)

# Test de la razón de verosimilitud. Significación de variables
multi0 <- multinom(me~1)
anova(multi1_hist,multi0)
anova(multi1_dect,multi0)

# Estimación de OR
exp(coef(multi1_hist))
exp(coef(multi1_dect))

# Estimación intervalo de confianza para los parámetros
IC_dect <- confint(multi1_dect, level=0.95)
IC_dect[,,1] # estimaciones de los par?metros del logit Ultimo a?o vs Nunca
IC_dect[,,2] # estimaciones de los par?metros del logit M?s de un a?o vs Nunca


# En base al test de la razón de la verosimilitud, todos los modelos univariantes son significativos.
multi1_symp <- multinom(me~symp)
multi1_bse <-  multinom(me~bse)
multi1_pb <- multinom(me~pb)
anova(multi1_symp,multi0)
anova(multi1_bse,multi0)
anova(multi1_pb,multi0)

# Ajuste del modelo múltiple con todas las variables explicativas
multi2 <- multinom(me ~ hist+dect+symp+bse+pb)
summary(multi2)
confint(multi2)

# Selección de variables
step(multi2)

multi3 <- multinom(me ~hist+symp+bse+pb)

# Test de la razón de verosimilitud (comparación de modelos)
anova(multi2,multi3)

