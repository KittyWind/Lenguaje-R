# Trabajo práctico 01: Probabilidades

#Cargar las librerias necesarias
library(tidyverse) #Esta librería es como dyplr (más completa)
library(expss) #para tablas de contingencia


#1) Considerando la base insurance, ¿Cuál es la probabilidad de seleccionar
# una persona del suroeste?


suroeste <- insurance %>% 
  filter(region=="southwest") %>% 
  summarise(n())


#2) ¿Cuál es la probabilidad de seleccionar una mujer del noreste?

mujernoroeste <- insurance %>% 
  filter(sex=="female" & region=="northeast") %>% 
  summarise(n())


#3) ¿Cuál es la probabilidad de seleccionar un hombre sin hijos?

hombresinhijo <- insurance %>% 
  filter(sex=="male" & children=="0")%>% 
  summarise(n())


#4) Según la OMS, para adultos de 20 años o más, el Índice de Masa Corporal 
#(IMC) se interpreta usando categorías de estado de peso estándar. 
# Estas categorías son iguales para hombres y mujeres de todos los tipos 
# de cuerpo y edades. Visitar: https://www.cdc.gov/healthyweight/spanish/assessing/bmi/adult_bmi/index.html#calcula-el-IMC

# La base insurance, contiene el IMC de los asegurados. 
#a) ¿Cuál es la probabilidad de seleccionar una persona con bajo peso?

bajopeso <- insurance %>%
  filter(bmi < "18.5")%>% 
  summarise(n())

#b) ¿Y cuál con sobrepeso? 

sobrepeso <- insurance %>% 
  filter(bmi < 29.9 & bmi  > 25)%>% 
  summarise(n())

#c) ¿Qué es más probable seleccionar?

# a alguien con sobrepeso


#5) Una moneda se lanza dos veces. ¿Cuál es la probabilidad de que ocurra al menos
#una cara

#Puede salir CARA-CRUZ CRUZ-CARA o CARA-CARA o CRUZ-CRUZ. Por lo tanto
#la probabilidad es de 0.5

#6) A una clase de estadística para técnicos asisten 25 de informática,
# 10 de mecánica, 10 de electrónica y 8 de robótica. Un estudiante, no cursa más
# de una carrera. 
# Si el profesor elige al azar a un estudiante para que conteste una pregunta,
#a) ¿qué probabilidades hay de que el estudiante sea de informática?
#b) ¿Qué probabilidades hay de que el estudiante sea de robótica o electrónica?

#a) 25/(25+10+10+8)=~0.47

#b) 8/(25+10+10+8)+10/(25+10+10+8)=(8+10)/(25+10+10+8)=~0.33


#7) Al final del cuatrimestre, Juan se va a graduar de técnico en informática. 
#despues de tener entrevistas en dos empresas en donde quiere trabajar, 
#determina que la probabilidad de que lo llamen para trabajar en la empresa
#A es de  0.8 y de que lo llamen de la B es 0.6. Si además considera la 
# probabilidad de que lo llamen de ambas empresas es de 0.5 ¿Qué probabilidad
#tiene de que lo llamen de al menos una de las dos empresas?

#P(A): lo llaman de la empresa A
#P(B): lo llaman de la empresa B
#P(AUB): Lo llaman de A o de B

#P(AUB)=P(A)+P(B)-P(A int  B)

#P(AUB)=0.8+0.6-0.5=0.9


#8) Las probabilidades de que una persona que compra una heladera nueva elija 
#una de color negro, una de color blanco, una de color gris o una azul son
#0.09, 0.15, 0.21 y 0.23 respectivamente. Supongamos que hay otros colores 
#y que la persona solo puede optar por un único color.

# a) ¿Cuál es la probabilidad de que adquiera una de gis?
# 0.21


# b) ¿Cuál es la probabilidad de que adquiera una gris o una azul?
# 0.21 + 0.23

#9) Si las probabilidades de que un técnico reparador de celulares brinde 
#servicio a 3,4,5,6,7,8 o más equipos en un día de trabajo dado son: 0.12, 
#0.19, 0.28, 0.24, 0.10 y 0.07 respectivamente, ¿Cuál es la probabilidad
#de que de servicio al menos a 5 equipos el siguiente día de trabajo?

#la probabilidad es 0.28 + 0.24 + 0.10 + 0.07

#10) Teniendo en cuenta la base insurance. ¿Cuál es la probabilidad 
# de seleccionar al azar a un asegurado que se hombre y mujer? ¿Cómo
# son estos eventos?

#Suponiendo que sólo se tienen dos géneros disponibles y que cada persona 
#tiene sólo uno de esos géneros, los susesos son mutuamente excluyentes.
#Por lo tanto, la probabilidad de sleccionar un asegurado hombre y mujer es 0.


#11) Confeccionar una tabla de contingencia por género 
#y por región y determinar cuál es la probabilidad
# de seleccionar al azar una persona de la región
#suroeste sabiendo que es mujer.


Tabla_Contingencia <- cross_cases(insurance, sex, region)
Tabla_Contingencia 

Tabla_Contingencia_Totales <- Tabla_Contingencia %>%
  mutate(Total=rowSums(Tabla_Contingencia[,2:5]))


#12) Teniendo en cuenta la base growth
#confeccionar una tabla de contingencia por suplemento y dieta  y
#determinar cuál es la probabilidad de seleccionar al azar un 
#animal que haya recibido el suplemento "agrimore" sabiendo que su 
#dieta es de avena (oats).

TC01 <- cross_cases(growth, supplement, diet)
TC01
TC02 <- TC01 %>% mutate(Total=rowSums(TC01[,2:4]))
TC02

p <- 1/4

#13) La probabilidad de que un vuelo programado
#normalmente salga a tiempo es de 0.83.
#La probabilidad de que llegue a tiempo es 0.82 y la
#probabilidad de que salga y llegue a tiempo es 0.78.

#P(salga a t)=0.83
#P(Llegue a t)=0.82
#P(salga y llegue a t)=0.78

#a) ¿Cuál es la probabilidad de que un avión
#llegue a tiempo sabiendo que salió a tiempo?

#P(llegue a t| salio a t)=P(llegue y salga a t)/P(sale a t)
P1 <-0.78/ 0.83
P1

#b) ¿Cuál es la probabilidad de que un avión haya salido
#a tiempo, sabiendo que llegó a tiempo.

#P(salga a t|llego a t)=P(salga y llegue a t)/P(llegue a t)

P2 <-0.78/ 0.82
P2


#c) ¿Cuál es la probabilidad de que un avión llegue a tiempo
#sabiendo que no salió a tiempo?

#P(no sale a t)=1-P(sale a t)=1-0.83=0.17
#P(llegue a t|no salio a t)=P(llegue a t y no salga t)/P(no sale a t)
#P(llegue a t y no salga a t)=P(llegue a t)-P(salga y llegue a t)

#P(llegue a t|no salio a t)=(P(llegue a t)-P(salga y llegue a t))/P(no sale a t)

P3 <-(0.82-0.78) /0.17
P3

#d) Comente y analice las respuestas obtenidas en anteriormente
#Comentamos la próxima clase!
