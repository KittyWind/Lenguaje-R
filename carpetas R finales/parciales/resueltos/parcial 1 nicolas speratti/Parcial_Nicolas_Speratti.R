heroes <- read_csv("avengers_Nicolas_Speratti.csv")
#ejercicio 2
heroes <- rename(heroes, Genero  = Gender)
#ejercicio 1
heroes$Genero <- replace(heroes$Genero, heroes$Genero == "MALE", "MASCULINO")
heroes$Genero <- replace(heroes$Genero, heroes$Genero == "FEMALE", "FEMENINO")
#ejercicio 3
heroes %>% 
  group_by(Genero) %>%
  summarise(frecuencia = n())
#ejercicio 4
el_mejor_anio <- heroes %>% 
  filter(Year == 1984)
#ejercicio 5:
#exito: atajar el penal, probabilidad: 85% o 0.85
#fracaso: no atajar el penal, probabilidad: 15% o 0.15
#a
dbinom(4,5,0.85)
#0.3915047
#b
dbinom(2,5,0.85)
#0.02438438
#c 
dbinom(0,5,0.85)