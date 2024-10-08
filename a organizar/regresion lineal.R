autos <- mtcars

autos <- autos %>%
  select(mpg, qsec,disp)
pairs(autos, #base
      col="blue", #color de la dispersion
      main = "Matriz de gráficos de dispersión" #título
)

matriz_cor <- cor(autos)
corrplot(matriz_cor, 
         method = "circle",     # Representa las correlaciones con círculos
         type = "full",         # Muestra la matriz completa (ambos lados simétricos)
         title = "Matriz de Correlación", # Título
         cex.main = 1, #Tamaño del título
         tl.col = "black",      # Color de las etiquetas de las variables
         addCoef.col = "black", # Mostrar los valores numéricos de las correlaciones
         col = colorRampPalette(c("orange", "white", "green"))(200),# Colores 
         mar = c(0,0,2,0))  #Ajuste de márgenes
boston <- Boston
# crim = continua
# zn = continua
# indus = continua
# chas = no continua
# nox = continua
# rm = continua
# age = continua
# dis = continua
# rad = no continua
# tax = no continua
# ptratio = continua
# black = continua
# lstat = continua
# medv = continua

boston <- boston %>% 
  dplyr::select(crim,zn,indus,nox,rm,age,dis,ptratio,black,lstat,medv)
pairs(boston, #base
      col="blue", #color de la dispersion
      main = "Matriz de gráficos de dispersión" #título
)

matrizboston_cor <- cor(boston)
corrplot(matrizboston_cor, 
         method = "circle",     # Representa las correlaciones con círculos
         type = "full",         # Muestra la matriz completa (ambos lados simétricos)
         title = "Matriz de Correlación", # Título
         cex.main = 1, #Tamaño del título
         tl.col = "black",      # Color de las etiquetas de las variables
         addCoef.col = "black", # Mostrar los valores numéricos de las correlaciones
         col = colorRampPalette(c("orange", "white", "green"))(200),# Colores 
         mar = c(0,0,2,0))  #Ajuste de márgenes

modelo_caso_1 <- lm(indus ~ nox, data = boston)
res_mod_1 <- tidy(modelo_caso_1)
res_mod_1

