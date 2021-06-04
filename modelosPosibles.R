

mod_best =  glm(acturismonum~conductacinturon+conductamovil+conductaalcohol+
                  conductamovil:conductaalcohol+
                  edad+
                  
                  offset(loganyos), family=quasipoisson, data=train)

mod_candi = glm(acturismonum~conductacinturon+
                  conductamovil:conductaalcohol+
                  edad+sexo+
                  sancion_velocidad+
                  offset(loganyos), family=quasipoisson, data=train)

mod_signiMax = glm(acturismonum~conductacinturon+conductamovil+conductaalcohol+
                     conductamovil:conductaalcohol+
                     edad+sexo+
                     sancion_velocidad+
                     offset(loganyos), family=quasipoisson, data=train)

anova(mod_candi,mod_signiMax,test = "Chisq")
summary(mod_signiMax)
summary(mod_candi)

#There is a potential problem in using glm fits with a variable scale, 
#as in that case the deviance is not simply related to the maximized 
#log-likelihood. The glm method for extractAIC makes the appropriate 
#adjustment for a gaussian family, but may need to be amended for other cases. 
#(The binomial and poisson families have fixed scale by default and 
#do not correspond to a particular maximum-likelihood problem for variable scale.) 


stepAIC
