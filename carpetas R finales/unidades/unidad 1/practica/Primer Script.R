# Hola mundo
getwd()

1+2
available.packages()

install.packages()

estadistica <- "Esto es un texto"

analisis <- "hola"

estadistica==analisis

estadistica!=analisis

a <- 5

a

class(a)

class(analisis)

library(readr)
insurance <- read_csv("insurance.csv")
View(insurance)

colnames(insurance)
ncol(insurance)
nrow(insurance)

dim(insurance)

class(insurance)

head(insurance,5)

tail(insurance,3)

summary(insurance)

str(insurance)

library(dplyr)
ejemplo_1 <- insurance %>%
  summarise(min(bmi), mean(bmi), max(charges))
#en el sig seleccionames columnas
subconjunto_1 <- insurance %>%
  dplyr::select(sex, age, smoker)
#sleccionamos registros bajo alguna condición
subconjunto_2 <- insurance %>%
  filter(sex == "male")

subconjunto_3 <- insurance %>%
  filter(region == "southwest",sex == "male")

ejemplo_11 <- insurance %>%
  filter(age >= 30 & age <= 60)









