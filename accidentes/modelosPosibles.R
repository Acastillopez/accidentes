

mod_best =  glm(acturismonum~conductacinturon+conductamovil+conductaalcohol+
                  conductamovil:conductaalcohol+
                  edad+
                  
                  offset(loganyos), family=quasipoisson, data=train)

mod_candi = glm(acturismonum~conductacinturon+
                  #conductamovil:conductaalcohol+
                  edad+sexo+
                  sancion_velocidad+
                  offset(loganyos), family=quasipoisson, data=train)

mod_candi2 = glm(acturismonum~conductacinturon+
                  #conductamovil:conductaalcohol+
                  edad+
                  sancion_velocidad+
                  offset(loganyos), family=quasipoisson, data=train)

mod_signiMax = glm(acturismonum~conductacinturon+conductamovil+conductaalcohol+
                     conductamovil:conductaalcohol+
                     edad+sexo+
                     sancion_velocidad+
                     offset(loganyos), family=quasipoisson, data=train)

anova(mod_candi,mod_candi2,test = "Chisq")

summary(mod_best)
summary(mod_candi)

## EVALUACIÓN EN TEST

mod_fin = glm(acturismonum ~ conductacinturon +
               edad + sancion_velocidad +
               offset(loganyos), family=quasipoisson, data=train)
test_predicted = predict(mod_fin, test, type="response")
val_predicted = predict(mod_fin, val, type="response")
ajustados_test = test_predicted/test$carnetanyos
ajustados_val = val_predicted/val$carnetanyos


plot(test$acturismonum - test_predicted)


summary(ajustados_val)
tapply(ajustados_val, val$conductacinturon, mean)
tapply(ajustados_val, val$conductamovil, mean)
tapply(ajustados_val, val$conducebicicleta, mean)
tapply(ajustados_val, val$sancion_velocidad, mean)
plot(val$edad)
plot(tapply(ajustados_val, val$edad, mean))
tapply(ajustados_val, val$sexo, mean)
tapply(ajustados_val, val$edad, mean)


