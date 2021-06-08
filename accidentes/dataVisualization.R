######################################
##  ANALISIS VARIABLES
######################################
install.packages('Amelia')
install.packages('corrplot')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('caret')
install.packages('plotly')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')
install.packages('polycor')

library(Amelia)
library(corrplot)
library(readr)
library(ggplot2)
library(mlbench)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)
library(polycor)

## COMPROBAR SI HAY DATOS FALLIDOS
missmap(accidentes,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)

########################
## ANALISIS EXPLORATORIO
summary(acturismo) # VARIABLE A ESTIMAR?
summary(acturismonum) # VARIABLE A ESTIMAR?
summary(acturismo_anyo) # VARIABLE A ESTIMAR?

# VARIAS GRAFICAS ENTRE VARIABLES
#relacion barrio/precio
smoothScatter(acturismo_anyo,sancion, nbin = 128,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, #xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"))

# CORRELACIONES ENTRE VARIABLES
cormat = hetcor(select(accidentes,
                       -carnet,#-conductaalcohol,-conductacinturon,
                       -conductamovil,#-conductavelocidad,-opinionlimites,
                       -opinionradares,-acturismonum,-carnetpuntosperdidos
                       #-conducebicicleta,-conducecamion
                       ))
#cormat[is.nan(cormat)] = 0
corrplot(cormat$correlations) # -variable_no interes

# DENSIDAD DE LA VARIABLE PRECIO
accidentes %>% 
  ggplot(aes(acturismo_anyo)) +
  stat_density() + 
  theme_bw()

# VISUALIZACIÓN DE LAS VARIABLES SOBRE LA TASA
acturismo_anyo %>%
  select(c(sancion, sancion_velocidad, edad, carnetpuntosperdidos,sexo)) %>%
  melt(id.vars = "acturismo_anyo") %>%
  ggplot(aes(x = value, y = PRECIO, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = " ")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Valor de la Variable", y = "Accidentes/Años_carnet") +
  theme_minimal()

