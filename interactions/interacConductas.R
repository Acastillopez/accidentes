## INTERACCIÓN ALCOHOL- CINTURON 
mod_alcoCintuInteraccion = glm(acturismonum~conductaalcohol+conductacinturon+conductaalcohol:conductacinturon+offset(loganyos), family=quasipoisson, data=accidentes)
mod_alcoCintu = glm(acturismonum~conductaalcohol+conductacinturon+offset(loganyos), family=quasipoisson, data=accidentes)
anova(mod_alcoCintuInteraccion,test="Chisq"); anova(mod_alcoCintu,test="Chisq")
summary(mod_alcoCintuInteraccion)$coefficients; summary(mod_alcoCintu)$coefficients
anova(mod_alcoCintu, mod_alcoCintuInteraccion,test="Chisq")


## INTERACCIÓN ALCOHOL- MOVIL
mod_alcoMovilInteraccion = glm(acturismonum~conductaalcohol+conductamovil+conductaalcohol:conductamovil+offset(loganyos), family=quasipoisson, data=accidentes)
mod_alcoMovil = glm(acturismonum~conductaalcohol+conductamovil+offset(loganyos), family=quasipoisson, data=accidentes)
anova(mod_alcoMovilInteraccion,test="Chisq"); anova(mod_alcoMovil,test="Chisq")
summary(mod_alcoMovilInteraccion)$coefficients; summary(mod_alcoMovil)$coefficients
anova(mod_alcoMovil, mod_alcoMovilInteraccion,test="Chisq")


## INTERACCIÓN MOVIL - CINTU 
mod_cintuMovilInteraccion = glm(acturismonum~conductacinturon+conductamovil+conductacinturon:conductamovil+offset(loganyos), family=quasipoisson, data=accidentes)
mod_cintuMovil = glm(acturismonum~conductacinturon+conductamovil+offset(loganyos), family=quasipoisson, data=accidentes)
anova(mod_cintuMovilInteraccion,test="Chisq"); anova(mod_cintuMovil,test="Chisq")
summary(mod_cintuMovilInteraccion)$coefficients; summary(mod_cintuMovil)$coefficients
anova(mod_cintuMovil, mod_cintuMovilInteraccion,test="Chisq")

## MODELO TRIVARIANTE
mod_triConducta = glm(acturismonum~conductacinturon+conductamovil+conductaalcohol+offset(loganyos), family=quasipoisson, data=accidentes)
mod_triConductaINt = glm(acturismonum~conductacinturon+conductamovil+conductaalcohol+
                           #conductacinturon:conductamovil+
                           conductamovil:conductaalcohol+
                           #conductaalcohol:conductacinturon+
                           offset(loganyos), family=quasipoisson, data=accidentes)
anova(mod_triConducta,test = "Chisq")
anova(mod_triConductaINt,test = "Chisq")

summary(mod_triConducta)
summary(mod_triConductaINt)

