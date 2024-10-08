library(readr)
insurance <- read_csv("insurance.csv")
View(insurance)
colnames(insurance)#el nombre de las columnas
ncol(insurance)#la cantidad de columnas
nrow(insurance)#la cantidad de filas
dim(insurance)#la dimension
class(insurance)
head(insurance,5)#desde el comienzo
tail(insurance,3)#desde el final
summary(insurance)#resumen
str(insurance)#informacion sobre cada columna
ejemplo_1 <- insurance %>% 
  summarise(min(bmi),mean(bmi),max(charges))
subconjunto_1 <- insurance %>% 
  select(sex,age,smoker)
subconjunto_2 <- insurance %>% 
  filter(sex == "male",region == "southwest")
ejemplo_11 <- insurance %>% 
  filter(age >= 30 & age <= 60,sex=="female")

#ejercicio 1
ejercicio1 <- insurance %>% 
  filter(region == "southwest")
(325/1338)
#ejercicio 2
ejercicio2 <- insurance %>% 
  filter(region == "northeast", sex =="female")
(161/1338)
#ejercicio 3
ejercicio3 <- insurance %>% 
  filter(sex == "male", children==0)
(283/1338)
#ejercicio4
ejercicio4a <- insurance %>% 
  filter(bmi <= 18.5)
21/1338
ejercicio4b <- insurance %>% 
  filter(bmi > 25 & bmi < 29.9)
372/1338
#ejercicio4c sobre peso
#ejercicio5
1/2
#ejercicio6a
(25/(28+25))
#6b
((10 + 8) / (25 + 28))
#7
0.6 + 0.8 - 0.5
#8a


grafica_bmi_con_promedio <- ggplot(data = insurance, aes(x = bmi))+
  geom_density(color="orange")+
  geom_vline(aes(xintercept=mean(bmi)),color="red")+
  theme(panel.background = element_rect(fill = "#4c2882"))
grafica_bmi_con_promedio