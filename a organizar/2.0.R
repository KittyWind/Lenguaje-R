P <- pnorm(q = 0, mean = 0, sd = 1, lower.tail = TRUE)
P
P <- pnorm(q = 1.78, mean = 0, sd = 1, lower.tail = TRUE)
P
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

