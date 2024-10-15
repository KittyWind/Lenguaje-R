#ejercicio 1 del 8/10/2024
calidadaire <- airquality
calidadaire

#tipos de variables
# ozone continua
# solar.r continua
# wind continua
# temp continua
# month no continua
# dia no continua

calidadaire <- calidadaire %>% 
  dplyr::select(Ozone,Solar.R,Wind,Temp) %>% 
  na.omit()

pairs(calidadaire, #base
      col="blue", #color de la dispersion
      main = "Matriz de gráficos de dispersión" #título
)

calidadaire_cor <- cor(calidadaire)
corrplot(calidadaire_cor, 
         method = "circle",     # Representa las correlaciones con círculos
         type = "full",         # Muestra la matriz completa (ambos lados simétricos)
         title = "Matriz de Correlación", # Título
         cex.main = 1, #Tamaño del título
         tl.col = "black",      # Color de las etiquetas de las variables
         addCoef.col = "black", # Mostrar los valores numéricos de las correlaciones
         col = colorRampPalette(c("orange", "white", "green"))(200),# Colores 
         mar = c(0,0,2,0))  #Ajuste de márgenes

modelo1 <- lm(Ozone ~ Solar.R + Wind, calidadaire)
tidy(modelo1)

valores1 <- data.frame(Solar.R = 22,Wind = 12.2)

#15/10/2024

datos_personas <- read.csv("datos_personas.csv")
head(datos_personas)

datos_personas <- datos_personas %>%
  mutate(Sexo_dummy = case_when( #la nueva columna o variable se llamará Sexo_dummy
    Sexo == "Hombre" ~ 1, #cuando Sexo tome el valor hombre se reemplazará por 1
    Sexo == "Mujer" ~ 0 #cuando Sexo tome el valor mujer se reemplazará por 0
  ))

modelo <- lm(Peso ~ Altura + Sexo_dummy, data = datos_personas)
tidy(modelo)

modelo <- lm(Peso ~ Altura + Ancho + Altura * Ancho, data = datos_personas)
tidy(modelo)