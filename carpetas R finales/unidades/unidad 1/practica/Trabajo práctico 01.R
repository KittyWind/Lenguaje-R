# Trabajo práctico 01: Probabilidades

insurance <- read_csv("insurance.csv")

#1) Considerando la base insurance, ¿Cuál es la probabilidad de seleccionar
# una persona del suroeste?

#ejercicio 1
ejercicio1 <- insurance %>% 
  filter(region == "southwest")
(325/1338)

#2) ¿Cuál es la probabilidad de seleccionar una mujer del noreste?

#ejercicio 2
ejercicio2 <- insurance %>% 
  filter(region == "northeast", sex =="female")
(161/1338)

#3) ¿Cuál es la probabilidad de seleccionar un hombre sin hijos?

ejercicio3 <- insurance %>% 
  filter(sex == "male", children==0)
(283/1338)

#4) Según la OMS, para adultos de 20 años o más, el Índice de Masa Corporal 
#(IMC) se interpreta usando categorías de estado de peso estándar. 
# Estas categorías son iguales para hombres y mujeres de todos los tipos 
# de cuerpo y edades. Visitar: https://www.cdc.gov/healthyweight/spanish/assessing/bmi/adult_bmi/index.html#calcula-el-IMC

# La base insurance, contiene el IMC de los asegurados. 
#a) ¿Cuál es la probabilidad de seleccionar una persona con bajo peso?

#b) ¿Y cuál con sobrepeso? 

#c) ¿Qué es más probable seleccionar?

#ejercicio4
ejercicio4a <- insurance %>% 
  filter(bmi <= 18.5)
21/1338
ejercicio4b <- insurance %>% 
  filter(bmi > 25 & bmi < 29.9)
372/1338
#ejercicio4c sobre peso

#5) Una moneda se lanza dos veces. ¿Cuál es la probabilidad de que ocurra al menos
#una cara

#ejercicio5
1/2

#6) A una clase de estadística para técnicos asisten 25 de informática,
# 10 de mecánica, 10 de electrónica y 8 de robótica. Un estudiante, no cursa más
# de una carrera. 
# Si el profesor elige al azar a un estudiante para que conteste una pregunta,
#a) ¿qué probabilidades hay de que el estudiante sea de informática?
#b) ¿Qué probabilidades hay de que el estuadiante sea de robótica o electrónica?

#ejercicio6a
(25/(28+25))
#6b
((10 + 8) / (25 + 28))

#7) Al final del cuatrimestre, Juan se va a graduar de técnico en informática. 
#despues de tener entrevistas en dos empresas en donde quiere trabajar, 
#determina que la probabilidad de que lo llamen para trabajar en la empresa
#A es de  0.8 y de que lo llamen de la B es 0.6. Si además considera la 
# probabilidad de que lo llamen de ambas empresas es de 0.5 ¿Qué probabilidad
#tiene de que lo llamen de al menos una de las dos empresas?

#ejercicio7
0.6 + 0.8 - 0.5

#8) Las probabilidades de que una persona que compra una heladera nueva elija 
#una de color negro, una de color blanco, una de color gris o una azul son
#0.09, 0.15, 0.21 y 0.23 respectivamente. Supongamos que hay otros colores 
#y que la persona solo puede optar por un único color.
# a) ¿Cuál es la probabilidad de que adquiera una de gis?
# b) ¿Cuál es la probabilidad de que adquiera una gris o una azul?


#9) Si las probabilidades de que un técnico reparador de celulares brinde 
#servicio a 3,4,5,6,7,8 o más equipos en un día de trabajo dado son: 0.12, 
#0.19, 0.28, 0.24, 0.10 y 0.07 respectivamente, ¿Cuál es la probabilidad
#de que de servicio al menos a 5 equipos el siguiente día de trabajo?


#10) Teniendo en cuenta la base insurance. ¿Cuál es la probabilidad 
#de seleccionar al azar a un asegurado que se hombre y mujer? ¿Cómo
# son estos eventos?
#la probabilidad es cero por que es imposible que sea hombre y mujer a la vez

#11) Confeccionar una tabla de contingencia por género 
#y por región y determinar cuál es la probabilidad
# de seleccionar al azar una persona de la región
#suroeste sabiendo que es mujer.


#12) Teniendo en cuenta la base growth
#confeccionar una tabla de contingencia por suplemento y dieta  y
#determinar cuál es la probabilidad de seleccionar al azar un 
#animal que haya recibido el suplemento "agrimore" sabiendo que su 
#dieta es de avena (oats).

#13) La probabilidad de que un vuelo programado
#normalmente salga a tiempo es de 0.83.
#La probabilidad de que llegue a tiempo es 0.82 y la
#probabilidad de que salga y llegue a tiempo es 0.78.

#a) ¿Cuál es la probabilidad de que un avión
#llegue a tiempo sabiendo que salió a tiempo?

#b) ¿Cuál es la probabilidad de que un avión haya salido
#a tiempo, sabiendo que llegó a tiempo.

#c) ¿Cuál es la probabilidad de que un avión llegue a tiempo
#sabiendo que no salió a tiempo?

#d) Comente y analice las respuestas obtenidas en anteriormente

