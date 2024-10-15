# realizar un estudio completo de regresion lineal de la base insurance ampliada
insurance <- read.csv("insurance.csv")

#analisis de variables

# age discreta
# sex nominal
# bmi continua
# children discreta
# smoker nominal
# region nominal
# charges continua

#matriz de dispersion


insurance1 <- insurance %>%
  mutate(Sex_dummy = case_when( #la nueva columna o variable se llamará Sexo_dummy
    sex == "male" ~ 1, #cuando Sexo tome el valor hombre se reemplazará por 1
    sex == "female" ~ 0 #cuando Sexo tome el valor mujer se reemplazará por 0
  ))
insurance1 <- insurance1 %>%
  mutate(Smoker_dummy = case_when( #la nueva columna o variable se llamará Sexo_dummy
    smoker == "yes" ~ 1, #cuando Sexo tome el valor hombre se reemplazará por 1
    smoker == "no" ~ 0 #cuando Sexo tome el valor mujer se reemplazará por 0
  ))
insurance1 <- insurance1 %>%
  mutate(haschildren_dummy = case_when( #la nueva columna o variable se llamará Sexo_dummy
    children == 0 ~ 0, #cuando Sexo tome el valor hombre se reemplazará por 1
    children >= 1 ~ 1 #cuando Sexo tome el valor mujer se reemplazará por 0
  ))
modelo <- lm(charges ~ Sex_dummy + haschildren_dummy + age +  age * Sex_dummy * haschildren_dummy, data = insurance1)
tidy(modelo)

R_cuad <- summary(modelo)$r.squared
R_cuad