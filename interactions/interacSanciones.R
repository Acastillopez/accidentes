## INTERACCIÓN SANCION - SANCION VELOCIDAD  
mod_sancionVelInterac = glm(acturismonum~sancion+sancion_velocidad+ sancion:sancion_velocidad+offset(loganyos), family=quasipoisson, data=train)
mod_sancionVel = glm(acturismonum~sancion+sancion_velocidad+offset(loganyos), family=quasipoisson, data=train)
anova(mod_sancionVelInterac,test="Chisq")
summary(mod_sancionVelInterac)$coefficients
anova(mod_sancionVel, mod_sancionVelInterac,test="Chisq")
summary(mod_sancionVel)
