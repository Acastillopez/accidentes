## INTERACCIÓN PERIODO - SEXO 
mod_periSexoInteraccion = glm(acturismonum~periodo+sexo+periodo:sexo+offset(loganyos), family=quasipoisson, data=accidentes)
mod_periSexo = glm(acturismonum~periodo+sexo+offset(loganyos), family=quasipoisson, data=accidentes)
anova(mod_periSexoInteraccion,test="Chisq")
summary(mod_periSexoInteraccion)$coefficients
anova(mod_periSexo, mod_periSexoInteraccion,test="Chisq")
AIC(mod_periSexoInteraccion)
AIC(mod_periSexo)

## INTERACCIÓN PERIODO - EDAD  
mod_periEdadInteraccion = glm(acturismonum~periodo+edad+periodo:edad+offset(loganyos), family=quasipoisson, data=accidentes)
mod_periEdad = glm(acturismonum~periodo+edad+offset(loganyos), family=quasipoisson, data=accidentes)
anova(mod_periEdadInteraccion,test="Chisq")
summary(mod_periEdadInteraccion)$coefficients
anova(mod_periEdad, mod_periEdadInteraccion,test="Chisq")

## INTERACCIÓN EDAD - SEXO 
mod_edadSexoInteraccion = glm(acturismonum~edad+sexo+edad:sexo+offset(loganyos), family=quasipoisson, data=accidentes)
mod_edadSexo = glm(acturismonum~edad+sexo+offset(loganyos), family=quasipoisson, data=accidentes)
anova(mod_edadSexoInteraccion,test="Chisq")
summary(mod_edadSexoInteraccion)$coefficients
summary(mod_edadSexo)$coefficients
anova(mod_edadSexo, mod_edadSexoInteraccion,test="Chisq")
