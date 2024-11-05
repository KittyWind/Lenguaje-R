# Segundo Parcial Probabilidad Aplicada
# 1) Analizar si las variables continuas del dataset quakes se distribuyen
# normalmente. Justificar.
# 2) Estimar puntualmente la media y el desvío de la variable depth. Explicar
# qué reprsentan.
# 3) Construir un intervalo de confianza de nivel de 90% para la media de la
# variable depth. Interpretar.
# 4) Realizar un análisis de regresión lineal completo (modelo, bondad del
# modelo y alguna predicción) para la situación depth en funcion de mag y
# stations.

quakes <- quakes

#ej1
latdist <- ggplot(quakes, aes(sample = lat)) + geom_qq() + geom_qq_line() +
  geom_hline(yintercept = 8) + geom_vline(xintercept = 0)
latdist

longdist <- ggplot(quakes, aes(sample = long)) + geom_qq() + geom_qq_line() +
  geom_hline(yintercept = 8) + geom_vline(xintercept = 0)
longdist

depthdist <- ggplot(quakes, aes(sample = depth)) + geom_qq() + geom_qq_line() +
  geom_hline(yintercept = 8) + geom_vline(xintercept = 0)
depthdist

magdist <- ggplot(quakes, aes(sample = mag)) + geom_qq() + geom_qq_line() +
  geom_hline(yintercept = 8) + geom_vline(xintercept = 0)
magdist

stationdist <- ggplot(quakes, aes(sample = stations)) + geom_qq() + geom_qq_line() +
  geom_hline(yintercept = 8) + geom_vline(xintercept = 0)
stationdist
#ninguna de las variables continuas se distribuye completamente de manera normal,pero algunas se distribuyen mejor que otras, debido a que tienen una recta mas uniforme

#ej 2
meantotal <- mean(quakes$depth)
desviototal <- sd(quakes$depth)
#la media es el promedio de la poblacion, y el desvio es cuanto puede ir para cada lado


#ej3
quakesminimizada <- quakes %>% 
  slice_sample(n=100,replace=F)
quakesminimizada

n = 100
meanmuestra <- mean(quakesminimizada$depth)
desviomuestra <- sd(quakesminimizada$depth)
z_90 <- qnorm(0.95)
z_90

L_i <- meanmuestra - z_90 * (desviomuestra/sqrt(n))
L_i

L_s <- meanmuestra + z_90 * (desviomuestra/sqrt(n))
L_s

# Número de muestras y tamaño de cada muestra
num_muestras <- 100
n2 <- 10

# Inicializamos un dataframe vacío para almacenar los resultados
resultados <- data.frame(Muestra = 1:num_muestras, Limite_Inferior = numeric(num_muestras),
              Limite_Superior = numeric(num_muestras), Medias_muestrales = numeric(num_muestras))

# Fijamos el Nivel de confianza (por ejemplo, 90%)
confianza <- 0.9

# Realizamos el proceso de muestreo y cálculo del intervalo de confianza para
# cada muestra
for (i in 1:num_muestras) {
  # Tomamos una muestra aleatoria de tamaño 'tamano_muestra' n de la variable
  muestra <- sample(quakes$depth, size = n2)
  
  # Calculamos la media y el desvío de la muestra
  x <- mean(muestra)
  s <- sd(muestra)
  
  # Valor crítico para la distribución normal estándar
  z <- qnorm(1 - (1 - confianza)/2)
  
  # Calculamos el error estándar de la media
  error_estandar_media <- s/sqrt(n2)
  
  # Calculamos el intervalo de confianza
  limite_inferior <- x - z * error_estandar_media
  limite_superior <- x + z * error_estandar_media
  
  # Almacenamos los resultados en el dataframe
  resultados[i, ] <- c(i, limite_inferior, limite_superior, x)
}

# Creamos una función para verificar si la media poblacional está contenida en
# el intervalo

verificar_inclusion <- function(row) {
  if (row["Limite_Inferior"] <= meantotal && row["Limite_Superior"] >= meantotal) {
    return("si")
  } else {
    return("no")
  }
}

# esta función inspeccionará cada fila (row) de nuestro dataframe 'resultados'.
# Y agregaremos a nuestro dataframe una columna 'Inclusion' al dataframe
# resultados.
resultados$Inclusion <- apply(resultados, 1, verificar_inclusion)

tabla <- resultados %>%
  dplyr::group_by(Inclusion) %>%
  dplyr::summarise(cantidad = n())
tabla


intconfianza  <- ggplot(resultados, aes(x = (Limite_Superior + Limite_Inferior)/2, y = Muestra,
                color = Inclusion)) + geom_segment(aes(xend = Limite_Inferior, yend = Muestra),
                linetype = "solid", linewidth = 0.5) + geom_segment(aes(xend = Limite_Superior,
                yend = Muestra), linetype = "solid", linewidth = 0.5) + geom_point(aes(x = Medias_muestrales,
                y = Muestra), color = "blue", size = 2) + geom_vline(xintercept = meantotal, linetype = "dashed",                                                                                                                                                                                                                                                    color = "purple") + scale_color_manual(values = c(si = "green", no = "red")) +
                labs(title = "Intervalos de Confianza de nivel 90%", x = "Medias", y = "Muestra") +
                theme_minimal() + theme(legend.position = "top")
intconfianza 
#es un buen intervalo de confianza ya que la mayoria de las lineas estan en verde

#ej4

pairs(quakes, #base
      col="blue", #color de la dispersion
      main = "Matriz de gráficos de dispersión" #título
)

matriz_cor <- cor(quakes)
corrplot(matriz_cor, 
         method = "circle",     # Representa las correlaciones con círculos
         type = "full",         # Muestra la matriz completa (ambos lados simétricos)
         title = "Matriz de Correlación", # Título
         cex.main = 1, #Tamaño del título
         tl.col = "black",      # Color de las etiquetas de las variables
         addCoef.col = "black", # Mostrar los valores numéricos de las correlaciones
         col = colorRampPalette(c("orange", "white", "green"))(200),# Colores 
         mar = c(0,0,2,0))  #Ajuste de márgenes

#entre mag y stations hay una correlacion muy positiva, pero ninguna de las dos posee correlacion fuerte o debil con depth

modelo1 <- lm(depth ~ stations, data = quakes)
resumen <- tidy(modelo1)
resumen
sum_mod <- summary(modelo1)
R_cuad1 <- sum_mod$r.squared
R_cuad1

modelo2 <- lm(depth ~ mag, data = quakes)
resumen <- tidy(modelo2)
resumen
sum_mod <- summary(modelo2)
R_cuad2 <- sum_mod$r.squared
R_cuad2

modelo3 <- lm(depth ~ mag + stations, data = quakes)
resumen <- tidy(modelo3)
resumen
sum_mod <- summary(modelo3)
R_cuad3 <- sum_mod$r.squared
R_cuad3

#ninguno de los coeficientes son buenos, por lo que ninguno de los modelso son muy buenos
