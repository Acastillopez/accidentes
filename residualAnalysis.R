########################
## RESIDUALES
########################
residuales = residuals(mod0)
residuales = as.data.frame(residuales)
ggplot(residuales,aes(residuales)) +  geom_histogram(fill='blue',alpha=0.5)
plot(mod0)

