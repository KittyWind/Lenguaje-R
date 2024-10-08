rnorm(10, 0, 1)
n <- 10  #tamaño de la muestra
muestra <- rnorm(n, 0, 1)
muestra
# Convertimos la muestra en un dataframe
muestra <- data.frame(muestra)
muestra <- muestra %>%
  arrange(muestra)
muestra
data <- muestra %>%
  mutate(Probabilidad = pnorm(muestra, 0, 1))
data
data <- data %>%
  mutate(Densidad = dnorm(muestra, 0, 1))
data
data %>%
  kbl() %>%
  kable_styling(full_width = F)
x_raya <- mean(data$muestra)
s_d <- sd(data$muestra)

resumen_data <- data.frame(Tamaño_muestral = n, Media_muestral = x_raya, Desvio_muestral = s_d)
resumen_data <- resumen_data %>%
  mutate(Numero_muestra = nrow(resumen_data))
# reordeno
resumen_data <- resumen_data %>%
  select(Numero_muestra, Tamaño_muestral, Media_muestral, Desvio_muestral)
resumen_data %>%
  kbl() %>%
  kable_styling(full_width = F)

g1 <- ggplot(data = data, mapping = aes(x = muestra, y = Probabilidad)) + geom_point() +
  geom_hline(yintercept = 1)
g1

g2 <- ggplot(data = data, mapping = aes(x = muestra, y = Densidad)) + geom_point() +
  geom_vline(xintercept = x_raya)
g2

resumen = function(n, mu, sigma) {
  muestra = rnorm(n, mu, sigma)
  media_muestral = mean(muestra)
  desvio_muestral = sd(muestra)
  return(salida = data.frame(media_muestral = media_muestral, desvio_muestral = desvio_muestral))
}

m1 <- resumen(100, 0, 1)
m2 <- resumen(100, 0, 1)
m3 <- resumen(100, 0, 1)
m4 <- resumen(100, 0, 1)
m5 <- resumen(100, 0, 1)
m6 <- resumen(100, 0, 1)
m7 <- resumen(100, 0, 1)
m8 <- resumen(100, 0, 1)
m9 <- resumen(100, 0, 1)
m10 <- resumen(100, 0, 1)
res <- rbind(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)
res <- res %>%
  mutate(muestra_numero = 1:nrow(res)) %>%
  dplyr::arrange(media_muestral)

res %>%
  kbl() %>%
  kable_styling(full_width = F)

g <- ggplot(data = res, mapping = aes(x = media_muestral)) + geom_histogram(aes(y = ..density..),
    colour = 1, fill = "lightblue") + geom_density(lwd = 1, colour = 4)
g

# Definimos la función que realiza las iteraciones
simulacion <- function(I, n, mu, sigma) {
  # En primer lugar, definimos la función resumen, dentro de la funcion
  # generalizadora
  
  resumen = function(n, mu, sigma) {
    muestra = rnorm(n, mu, sigma)
    media_muestral = mean(muestra)
    desvio_muestral = sd(muestra)
    return(salida = data.frame(media_muestral = media_muestral, desvio_muestral = desvio_muestral))
  }
  
  # Creaamos un Dataframe vacío para almacenar los resultados
  resultado <- data.frame()
  # Creamos un ciclo for que en cada iteracción, genere una muestra y calcule
  for (i in 1:I) {
    # Llamamos a la función resumen y obtenemos el dataframe en cada
    # iteracion
    df <- resumen(n, mu, sigma)
    # Unificamos el Dataframe actual con el dataframe vacío llamado
    # resultado
    resultado <- rbind(resultado, df)
    
  }
  
  # Reiniciamos los índices del Dataframe resultado
  rownames(resultado) <- NULL
  # Agregamos una columna con el tamaño muestral
  resultado <- resultado %>%
    mutate(Muestreo_numero = 1:nrow(resultado))
  # Reordenamos las columnas del dataframe
  resultado <- resultado %>%
    select(Muestreo_numero, media_muestral, desvio_muestral)
  
  return(resultado)
}

I <- 1000  #cantidad de iteraciones
n <- 10  #tamaño de muestra
mu <- 0
sigma <- 1
m <- simulacion(I, n, mu, sigma)
head(m)


I <- 1000  #cantidad de iteraciones
n <- 100  #tamaño de muestra
mu <- 0
sigma <- 1
m <- simulacion(I, n, mu, sigma)
head(m)

g <- ggplot(data = m, mapping = aes(x = media_muestral)) + geom_histogram(aes(y = ..density..),
    colour = 1, fill = "lightblue") + geom_density(lwd = 1, colour = 4)
g