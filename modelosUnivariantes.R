###################################################
###
### MODELOS UNIVARIANTES
###
###################################################
# ELECCIÓN DE QUASIPOISSON
Phi = var(accidentes$acturismonum)/mean(accidentes$acturismonum)
# La variable acturismo_anyo es la tasa a predecir
# tasa = acturisnum_anyo , E = carnetanyos
# loganyos = log(train$carnetanyos)
# los exponentes de los betas -> numero de veces de más de media  

# modelo para test estadísticos
mod0 = glm(acturismonum~carnet+offset(loganyos), family=quasipoisson, data=train)
Deviance = deviance(mod0) # 0.6*300 = 180 approx? -> Sí si se eliminan 213 y 215 

## SEXO EDAD TERRITORIO...
mod_sexo = glm(acturismonum~sexo+offset(loganyos), family=quasipoisson, data=train)
anova(mod_sexo,test="Chisq")
summary(mod_sexo)$coefficients

mod_edad = glm(acturismonum~edad+offset(loganyos), family=quasipoisson, data=train)
anova(mod_edad,test="Chisq")
summary(mod_edad)$coefficients

mod_periodo = glm(acturismonum~periodo+offset(loganyos), family=quasipoisson, data=train)
anova(mod_periodo,test="Chisq")
summary(mod_periodo)$coefficients

mod_provi = glm(acturismonum~th+offset(loganyos), family=quasipoisson, data=train)
anova(mod_provi,test="Chisq")
summary(mod_provi)$coefficients

## EXPOSICIÓN EN CARRETERA
mod_km_tur = glm(acturismonum~kmturismo+offset(loganyos), family=quasipoisson, data=train)
anova(mod_km_tur,test="Chisq")
summary(mod_km_tur)$coefficients

#SANCIONES
mod_sancn = glm(acturismonum~sancion+offset(loganyos), family=quasipoisson, data=train)
anova(mod_sancn,test="Chisq")
summary(mod_sancn)$coefficients

mod_s_vel = glm(acturismonum~sancion_velocidad+offset(loganyos), family=quasipoisson, data=train)
anova(mod_s_vel,test="Chisq")
summary(mod_s_vel)$coefficients

mod_puntos = glm(acturismonum~carnetpuntosperdidos+offset(loganyos), family=quasipoisson, data=train)
anova(mod_puntos,test="Chisq")
summary(mod_puntos)$coefficients

#OPINIONES Y CONDUCTAS
mod_radar = glm(acturismonum~opinionradares+offset(loganyos), family=quasipoisson, data=train)
anova(mod_radar,test="Chisq")
summary(mod_radar)$coefficients

mod_limts = glm(acturismonum~opinionlimites+offset(loganyos), family=quasipoisson, data=train)
anova(mod_limts,test="Chisq")
summary(mod_limts)$coefficients

mod_movil = glm(acturismonum~conductamovil+offset(loganyos), family=quasipoisson, data=train)
anova(mod_movil,test="Chisq")
summary(mod_movil)$coefficients

mod_alchl = glm(acturismonum~conductaalcohol+offset(loganyos), family=quasipoisson, data=train)
anova(mod_alchl,test="Chisq")
summary(mod_alchl)$coefficients

mod_cintu = glm(acturismonum~conductacinturon+offset(loganyos), family=quasipoisson, data=train)
anova(mod_cintu,test="Chisq")
summary(mod_cintu)$coefficients

#TIPO VEHICULO
mod_bici = glm(acturismonum~conducebicicleta+offset(loganyos), family=quasipoisson, data=train)
anova(mod_bici,test="Chisq")
summary(mod_bici)$coefficients

mod_moto = glm(acturismonum~conducemoto+offset(loganyos), family=quasipoisson, data=train)
anova(mod_moto,test="Chisq")
summary(mod_moto)$coefficients

mod_camion = glm(acturismonum~conducecamion+offset(loganyos), family=quasipoisson, data=train)
anova(mod_camion,test="Chisq")
summary(mod_camion)$coefficients

mod_ligero = glm(acturismonum~conducevehiculoligero+offset(loganyos), family=quasipoisson, data=train)
anova(mod_ligero,test="Chisq")
summary(mod_ligero)$coefficients
