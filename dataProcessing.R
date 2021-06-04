accidentes <- read.delim("/cloud/project/accidentes_extraordinaria.txt", row.names=1)
accidentes <- accidentes[-c(215,213),] 

accidentes$periodo = "Adulto"; accidentes$periodo[accidentes$edad < 30] = "Joven"
accidentes$periodo[accidentes$edad >= 60] = "Tercera"
multival = c("th","sexo","opinionlimites","conductavelocidad","conductaalcohol",
             "conductamovil","conductacinturon","opinionradares","periodo")
accidentes[multival] <- lapply(accidentes[multival], factor) 

bools = c("conducebicicleta","conducecamion","conducemoto",
          "conducevehiculoligero", "acturismo",
          "sancion","carnetpuntosperdidos","sancion_velocidad")
accidentes[bools] <- lapply(accidentes[bools], factor,labels = c("No", "Sí"))

###################################################
###
### CREAR VARIABLES DE FUTURO INTERES
###
###################################################

# ACCIDENTES X KMS CONDUCIDOS (PROBLEMA ZEROS)
#accidentes$acturismonum_km = acturismonum / kmturismo

# ACCIDENTES POR AÑO DE CARNET
accidentes["acturismo_anyo"] = acturismonum / carnetanyos


summary(accidentes)
attach(accidentes)


###################################################
###
### CREAR TRAIN, TEST Y VAL
###
###################################################
# train test y val transportables? X PROVINCIAS
# bizkai = subset(accidentes,th=="Bizkaia")
# araba  = subset(accidentes,th=="Araba")
# gipuzk = subset(accidentes,th=="Gipuzkoa")  
# train: araba+gipuzk, test: 108 de bizk, val: 60 de bizk 

# EL DE TODA LA VIDA (60, 30, 10)%
spec = c(train = .6, test = .3, validate = .1)
g = sample(cut(seq(nrow(accidentes)),nrow(accidentes)*cumsum(c(0,spec)),labels = names(spec)))
full = split(accidentes, g)
train = full$train; test = full$test; val = full$validate

# CROSS VALIDATION
# set.seed(125)
# train_control <- trainControl(method = "cv", number = 10) 

