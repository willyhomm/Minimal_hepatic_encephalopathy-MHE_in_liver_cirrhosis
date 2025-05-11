
#install.packages("dplyr")
#install.packages("coin")
#install.packages("ggplot2")
#install.packages("lmtest")
#install.packages("car")
#install.packages("GGally")
#install.packages("nortest")

colnames(data_ehm)
data_ehm$...1 <- NULL
library(dplyr)

#### 1) ANÁLISIS DESCRIPTIVO ####

### *** TABLA 1 *** ----

# 1.1. Sexo
table(data_ehm$sexo)
round(prop.table(table(data_ehm$sexo))*100, 2)


# 1.2. Edad
hist(data_ehm$edad) # Se observa NO normal

hist(data_ehm$edad, probability = TRUE) # Para crear la curva normal, el eje Y debe ser DENSIDAD
media_edad <- mean(data_ehm$edad, na.rm = TRUE) # Se crea la media de la edad
desv_edad <- sd(data_ehm$edad, na.rm = TRUE) # Se crea la desviación estándar de la edad
x <- seq(min(data_ehm$edad, na.rm = TRUE),
         max(data_ehm$edad, na.rm = TRUE),
         length = 100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_edad,
           sd = desv_edad) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2) # Se crea la línea a partir de los puntos
  # Se observa NO normal

shapiro.test(data_ehm$edad) # p=0.0007846 --> NO NORMAL
median(data_ehm$edad) # 56
min(data_ehm$edad) # 27
max(data_ehm$edad) # 65


# 1.3. Ocupación
table(data_ehm$ocupacion)
round(prop.table(table(data_ehm$ocupacion))*100, 2)
  # Se puede fusionar CONDUCTOR con CHOFER
  # Se puede fusionar ADMINISTRADOR con ADMINISTRATIVO
  # Las categorías que presenten pocos eventos se puedes clasificar como "OTROS"

data_ehm <- data_ehm %>% mutate(ocupa_cat = case_when(
  ocupacion == "CONDUCTOR" ~ "CHOFER",
  ocupacion == "ADMINISTRADOR" ~ "ADMINISTRATIVO",
  ocupacion == "OFICINA" ~ "ADMINISTRATIVO",
  ocupacion == "SECRETARIA" ~ "ADMINISTRATIVO",
  TRUE ~ ocupacion
))
table(data_ehm$ocupa_cat)
data_ehm$ocupa_cat <- ifelse(data_ehm$ocupa_cat != "AMA DE CASA" &
                               data_ehm$ocupa_cat != "CHOFER" &
                               data_ehm$ocupa_cat != "ADMINISTRATIVO" &
                               data_ehm$ocupa_cat != "COMERCIANTE",
                             "OTROS", data_ehm$ocupa_cat)
data_ehm <- data_ehm %>% dplyr::relocate(ocupa_cat, .after = ocupacion)
table(data_ehm$ocupa_cat)
round(prop.table(table(data_ehm$ocupa_cat))*100, 2)


# 1.4. Años de instrucción
hist(data_ehm$anios_instr) # Se observa NO normal

hist(data_ehm$anios_instr, probability = TRUE) # Para crear la curva normal, el eje Y debe ser DENSIDAD
media_anios_instr = mean(data_ehm$anios_instr, na.rm = TRUE) # Se crea la media de años de instrucción
desv_anios_instr = sd(data_ehm$anios_instr, na.rm = TRUE) # Se crea la desviación estándar de años de instrucción
x <- seq(min(data_ehm$anios_instr, na.rm = TRUE),
         max(data_ehm$anios_instr, na.rm = TRUE),
         length = 100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_anios_instr,
           sd = desv_anios_instr) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2) # Se crea la línea a partir de los puntos
  # Se observa NO normal

shapiro.test(data_ehm$anios_instr) # p=7.684e-05 --> NO NORMAL
median(data_ehm$anios_instr) # 13
min(data_ehm$anios_instr) # 11
max(data_ehm$anios_instr) # 22


# 1.5. Comorbilidad
table(data_ehm$comorb_1)
table(data_ehm$comorb_2)
  # Se decide crear una variable dicotómica

data_ehm$comorb_di <- ifelse(data_ehm$comorb_1 != "no reporta" | data_ehm$comorb_2 != "no reporta",
                             "Sí",
                             "No")
data_ehm <- data_ehm %>% dplyr::relocate(comorb_di, .after = comorb_2)
table(data_ehm$comorb_di)
round(prop.table(table(data_ehm$comorb_di))*100, 2)


# 1.x. Tipo de comorbilidad
#data_ehm$comorb_fusion <- ifelse(data_ehm$comorb_1 != "no reporta" & data_ehm$comorb_2 != "no reporta",
#                                 paste(data_ehm$comorb_1, data_ehm$comorb_2, sep =  ", "),
#                                 ifelse(data_ehm$comorb_1 != "no reporta" & data_ehm$comorb_2 == "no reporta",
#                                        data_ehm$comorb_1, NA)
#                                 )
#data_ehm <- data_ehm %>% dplyr::relocate(comorb_fusion, .after = comorb_di)
#table(data_ehm$comorb_fusion)
#round(prop.table(table(data_ehm$comorb_fusion))*100, 2)
#sum(table(data_ehm$comorb_fusion)) # 16


# 1.x. Año de evaluación
#table(data_ehm$anio)
#round(prop.table(table(data_ehm$anio))*100, 2)


# 1.6. Etiología de la cirrosis
table(data_ehm$etiologia)
round(prop.table(table(data_ehm$etiologia))*100, 2)
  # Es necesario realizar algunas fusiones según lo indicado por el clínico

data_ehm <- data_ehm %>% mutate(etio_cat = case_when(
  etiologia == "MAFLD + HVC" ~ "Doble etiología",
  etiologia == "MAFLD + OH" ~ "Doble etiología",
  etiologia == "NAFLD" ~ "MAFLD",
  etiologia == "PROB CBP" ~ "CBP",
  TRUE ~ etiologia
  ))
data_ehm <- data_ehm %>% dplyr::relocate(etio_cat, .after = etiologia)
table(data_ehm$etio_cat)
round(prop.table(table(data_ehm$etio_cat))*100, 2)


# 1.7. Tiempo de enfermedad cirrótica
hist(data_ehm$time_enf_cirrosis) # Se observa NO 

hist(data_ehm$time_enf_cirrosis, probability = TRUE) # Para crear la curva normal, el eje Y debe ser DENSIDAD
media_time_enf_cirrosis <- mean(data_ehm$time_enf_cirrosis, na.rm = TRUE)
desv_time_enf_cirrosis <- sd(data_ehm$time_enf_cirrosis, na.rm = TRUE)
x <- seq(min(data_ehm$time_enf_cirrosis, na.rm = TRUE),
         max(data_ehm$time_enf_cirrosis, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_time_enf_cirrosis,
           sd = desv_time_enf_cirrosis) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2) # Se crea la línea a partir de los puntos
  # Se observa NO normal

shapiro.test(data_ehm$time_enf_cirrosis) # p=1.713e-08 --> NO NORMAL
median(data_ehm$time_enf_cirrosis) # 12
min(data_ehm$time_enf_cirrosis) # 1
max(data_ehm$time_enf_cirrosis) # 96


# 1.8. Várices esofágicas
table(data_ehm$endosco_diges_vvee_label)
round(prop.table(table(data_ehm$endosco_diges_vvee_label))*100, 2)
  # Es necesario realizar algunas fusiones según lo indicado por el clínico

data_ehm$varices_tri <- ifelse(data_ehm$endosco_diges_vvee == 0, "Ausencia", 
                               ifelse(data_ehm$endosco_diges_vvee == 1, "Pequeñas", "Grandes"))
data_ehm <- data_ehm %>% dplyr::relocate(varices_tri, .after = vvee_cal)
table(data_ehm$varices_tri)
round(prop.table(table(data_ehm$varices_tri))*100, 2)


# 1.9. Ascitis
table(data_ehm$asci_label)
round(prop.table(table(data_ehm$asci_label))*100, 2)


# 1.10. CHILD PUGH
table(data_ehm$child_cat)
round(prop.table(table(data_ehm$child_cat))*100, 2)


# 1.11. Encefalopatía hepática mínima
table(data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$dx_final_cal))*100, 2)

data_ehm$dx_final_cal <- ifelse(data_ehm$dx_final_cal == "EHM", "SI EHM", data_ehm$dx_final_cal)
table(data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$dx_final_cal))*100, 2)


# Medicación habitual
#table(data_ehm$medica_1)
#table(data_ehm$medica_2)
#table(data_ehm$medica_3)
#table(data_ehm$medica_4)
#table(data_ehm$medica_5)
#table(data_ehm$medica_6)

# Weast Haven
#table(data_ehm$west_haven)
#round(prop.table(table(data_ehm$west_haven))*100, 2)

# Encefalopatía
#table(data_ehm$encefa_label)
#round(prop.table(table(data_ehm$encefa_label))*100, 2)



### *** TABLA 2 *** ----

# 2.1. INR
hist(data_ehm$inr) # Se observa NO normal

hist(data_ehm$inr, probability = TRUE)
media_inr <- mean(data_ehm$inr, na.rm = TRUE)
desv_inr <- sd(data_ehm$inr, na.rm = TRUE)
x <- seq(min(data_ehm$inr, na.rm = TRUE),
         max(data_ehm$inr, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_inr,
           sd = desv_inr) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2) # Se crea la línea a partir de los puntos
  # Se observa NO normal

shapiro.test(data_ehm$inr) # p=1.699e-05 # Se decide NO NORMAL
median(data_ehm$inr) # 1.2
min(data_ehm$inr) # 0.9
max(data_ehm$inr) # 2.2


# 2.2. Albúmina sérica
hist(data_ehm$alb) # Se observa NO normal

hist(data_ehm$alb, probability = TRUE)
media_alb <- mean(data_ehm$alb, na.rm = TRUE)
desv_alb <- sd(data_ehm$alb, na.rm = TRUE)
x <- seq(min(data_ehm$alb, na.rm = TRUE),
         max(data_ehm$alb, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_alb,
           sd = desv_alb) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2) # Se crea la línea a partir de los puntos
  # Se observa NO normal

shapiro.test(data_ehm$alb) # p=0.07912 # Se decide NORMAL
mean(data_ehm$alb) # 3.957869
sd(data_ehm$alb) # 0.4334248


# 2.3. Creatinina sérica
hist(data_ehm$crea) # Se observa NO normal

hist(data_ehm$crea, probability = TRUE)
media_crea <- mean(data_ehm$crea, na.rm = TRUE)
desv_crea <- sd(data_ehm$crea, na.rm = TRUE)
x <- seq(min(data_ehm$crea, na.rm = TRUE),
         max(data_ehm$crea, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_crea,
           sd = desv_crea) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2) # Se crea la línea a partir de los puntos
  # Se observa NO normal

shapiro.test(data_ehm$crea) # p=0.005019 --> Se decide NO NORMAL
median(data_ehm$crea) # 0.7
min(data_ehm$crea) # 0.4
max(data_ehm$crea) # 1.4


# 2.4. Billirubina total
hist(data_ehm$bt) # Se observa NO normal

hist(data_ehm$bt, probability = TRUE)
media_bt <- mean(data_ehm$bt, na.rm = TRUE)
desv_bt <- sd(data_ehm$bt, na.rm = TRUE)
x <- seq(min(data_ehm$bt, na.rm = TRUE),
         max(data_ehm$bt, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_bt,
           sd = desv_bt) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2)
  # Se observa NO normal

shapiro.test(data_ehm$bt) # p=1.728e-07 --> Se decide NO NORMAL
median(data_ehm$bt) # 1.19
min(data_ehm$bt) # 0.56
max(data_ehm$bt) # 5.01


# 2.5. Sodio sérico
hist(data_ehm$na) # Se observa Normal

hist(data_ehm$na, probability = TRUE)
media_na <- mean(data_ehm$na, na.rm = TRUE)
desv_na <- sd(data_ehm$na, na.rm = TRUE)
x <- seq(min(data_ehm$na, na.rm = TRUE),
         max(data_ehm$na, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_na,
           sd = desv_na) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2)
  # Se observa NO normal

shapiro.test(data_ehm$na) # p=0.04421 --> Se decide NO NORMAL
median(data_ehm$na) # 139
min(data_ehm$na) # 130
max(data_ehm$na) # 145


# 2.6. MELD score
hist(data_ehm$meld_cal_deci) # Se observa NO normal

hist(data_ehm$meld_cal_deci, probability = TRUE)
media_meld_cal_deci <- mean(data_ehm$meld_cal_deci, na.rm = TRUE)
desv_meld_cal_deci <- sd(data_ehm$meld_cal_deci, na.rm = TRUE)
x <- seq(min(data_ehm$meld_cal_deci, na.rm = TRUE),
         max(data_ehm$meld_cal_deci, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_meld_cal_deci,
           sd = desv_meld_cal_deci) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2)
  # Se observa NO normal

shapiro.test(data_ehm$meld_cal_deci) # p=0.0002602 --> NO NORMAL
median(data_ehm$meld_cal_deci) # 9.161177 -->  Se redondea a 9
min(data_ehm$meld_cal_deci) # 6.43 --> Se redondea a 6
max(data_ehm$meld_cal_deci) # 20.45337 --> Se redondea a 20

# 2.6.1. Valoración de trasplante hepático con MELD score
data_ehm$meld_cal_deci_cat <- ifelse(data_ehm$meld_cal_deci < 15,
                                     "Menor de 15 puntos",
                                     "Mayor o igual a 15 puntos")
data_ehm <- data_ehm %>% dplyr::relocate(meld_cal_deci_cat, .after = meld_cal_deci)
table(data_ehm$meld_cal_deci_cat)
round(prop.table(table(data_ehm$meld_cal_deci_cat))*100, 2)


# 2.7. MELD Na score
hist(data_ehm$meldNA_cal_deci) # Se observa NO normal

hist(data_ehm$meldNA_cal_deci, probability = TRUE)
media_meldNA_cal_deci <- mean(data_ehm$meldNA_cal_deci, na.rm = TRUE)
desv_meldNA_cal_deci <- sd(data_ehm$meldNA_cal_deci, na.rm = TRUE)
x <- seq(min(data_ehm$meldNA_cal_deci, na.rm = TRUE),
         max(data_ehm$meldNA_cal_deci, na.rm = TRUE),
         length=100) # Se crea los puntos de la línea en el eje X
y <- dnorm(x,
           mean = media_meldNA_cal_deci,
           sd = desv_meldNA_cal_deci) # Se crea los puntos de la línea en el eje Y
lines(x, y, col = "red", lwd = 2)
  # Se observa NO normal

shapiro.test(data_ehm$meldNA_cal_deci) # p=0.0528 # Se decide NO NORMAL
median(data_ehm$meldNA_cal_deci) # 10.89821 --> Se redondea a 11
min(data_ehm$meldNA_cal_deci) # 4.550362 --> Se redondea a 5
max(data_ehm$meldNA_cal_deci) # 22.40803 --> Se redondea a 22

# 2.7.1. Valoración de trasplante hepático con MELD Na score
data_ehm$meldNA_cal_deci_cat <- ifelse(data_ehm$meldNA_cal_deci < 15,
                                       "Menor de 15 puntos",
                                       "Mayor o igual a 15 puntos")
data_ehm <- data_ehm %>% dplyr::relocate(meldNA_cal_deci_cat, .after = meldNA_cal_deci)
table(data_ehm$meldNA_cal_deci_cat)
round(prop.table(table(data_ehm$meldNA_cal_deci_cat))*100, 2)



#### 2) ANÁLISIS BIVARIADO ####

### *** TABLA 3 *** ----

# 3.1. EHM VS child pugh
table(data_ehm$child_cat, data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$child_cat, data_ehm$dx_final_cal))*100, 2)

chisq.test(table(data_ehm$child_cat, data_ehm$dx_final_cal))$expected # frecuencias esperadas < 5
fisher.test(table(data_ehm$child_cat, data_ehm$dx_final_cal)) # p=0.1469


# 3.2. EHM VS sexo
table(data_ehm$sexo, data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$sexo, data_ehm$dx_final_cal), margin = 1)*100, 2)

chisq.test(table(data_ehm$sexo, data_ehm$dx_final_cal))$expected # frecuencias esperadas > 5
chisq.test(table(data_ehm$sexo, data_ehm$dx_final_cal), correct = T) # p=0.7488
  # La corrección continuidad de Yates está predeterminado, pero se especifica por motivos prácticos
  # Se usó corrección de continuidad de Yates porque es una tabla 2x2


# 3.3. EHM VS edad
data_ehm_NO_EHM <- subset(data_ehm, dx_final_cal == "NO EHM")
data_ehm_SI_EHM <- subset(data_ehm, dx_final_cal == "SI EHM")

hist(data_ehm_NO_EHM$edad) # Se ve No Normal
shapiro.test(data_ehm_NO_EHM$edad) # p=0.02111 --> NO NORMAL
hist(data_ehm_SI_EHM$edad) # Se ve normal
shapiro.test(data_ehm_SI_EHM$edad) # p=0.4036 --> NORMAL

median(data_ehm_NO_EHM$edad) # 54
min(data_ehm_NO_EHM$edad) # 27
max(data_ehm_NO_EHM$edad) # 65

median(data_ehm_SI_EHM$edad) # 57.5
min(data_ehm_SI_EHM$edad) # 47
max(data_ehm_SI_EHM$edad) # 65

#library(coin)
coin::wilcox_test(edad ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.09192
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.4. EHM VS años de instrucción
hist(data_ehm_NO_EHM$anios_instr) # Se ve No Normal
shapiro.test(data_ehm_NO_EHM$anios_instr) # p=0.004292 --> NO NORMAL
hist(data_ehm_SI_EHM$anios_instr) # Se ve No Normal
shapiro.test(data_ehm_SI_EHM$anios_instr) # p=0.001732 --> NO NORMAL

median(data_ehm_NO_EHM$anios_instr) # 14
min(data_ehm_NO_EHM$anios_instr) # 11
max(data_ehm_NO_EHM$anios_instr) # 22

median(data_ehm_SI_EHM$anios_instr) # 13
min(data_ehm_SI_EHM$anios_instr) # 11
max(data_ehm_SI_EHM$anios_instr) # 19

#library(coin)
coin::wilcox_test(anios_instr ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.5606
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.5. EHM VS comorbilidad
table(data_ehm$comorb_di, data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$comorb_di, data_ehm$dx_final_cal), margin = 1)*100, 2)

chisq.test(table(data_ehm$comorb_di, data_ehm$dx_final_cal))$expected # frecuencias esperadas > 5
chisq.test(table(data_ehm$comorb_di, data_ehm$dx_final_cal), correct = T) # p=1
  # La corrección continuidad de Yates está predeterminado, pero se especifica por motivos prácticos
  # Se usó corrección de continuidad de Yates porque es una tabla 2x2


# 3.6. EHM VS etiología
table(data_ehm$etiologia, data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$etiologia, data_ehm$dx_final_cal), margin = 1)*100, 2)
  # Hay que re-clasificar de acuerdo a lo propuesto por el clínico

data_ehm <- data_ehm %>% mutate(etio_cru = case_when(
  etiologia == "CBP" ~ "Enfermedades autoinmunes",
  etiologia == "CBP + HAI" ~ "Enfermedades autoinmunes",
  etiologia == "HAI" ~ "Enfermedades autoinmunes",
  etiologia == "PROB CBP" ~ "Enfermedades autoinmunes",
  etiologia == "HVB" ~ "Hepatitis viral",
  etiologia == "HVC" ~ "Hepatitis viral",
  etiologia == "MAFLD + HVC" ~ "Doble etiología", 
  etiologia == "MAFLD + OH" ~ "Doble etiología",
  etiologia == "MAFLD" ~ "Hígado graso",
  etiologia == "NAFLD" ~ "Hígado graso",
  TRUE ~ etiologia
  ))
data_ehm <- data_ehm %>% dplyr::relocate(etio_cru, .after = etio_cat)

table(data_ehm$etio_cru, data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$etio_cru, data_ehm$dx_final_cal), margin = 1)*100, 2)

chisq.test(table(data_ehm$etio_cru, data_ehm$dx_final_cal))$expected # frecuencias esperadas < 5
fisher.test(table(data_ehm$etio_cru, data_ehm$dx_final_cal)) # p=0.3459


# 3.7. EHM VS tiempo de enfermedad cirrótica
hist(data_ehm_NO_EHM$time_enf_cirrosis) # Se ve NO normal
shapiro.test(data_ehm_NO_EHM$time_enf_cirrosis) # p=4.123e-06 --> NO NORMAL
hist(data_ehm_SI_EHM$time_enf_cirrosis) # Se ve No normal
shapiro.test(data_ehm_SI_EHM$time_enf_cirrosis) # p=7.795e-05 --> NO NORMAL

median(data_ehm_NO_EHM$time_enf_cirrosis) # 11
min(data_ehm_NO_EHM$time_enf_cirrosis) # 1
max(data_ehm_NO_EHM$time_enf_cirrosis) # 96

median(data_ehm_SI_EHM$time_enf_cirrosis) # 12
min(data_ehm_SI_EHM$time_enf_cirrosis) # 1
max(data_ehm_SI_EHM$time_enf_cirrosis) # 96

#library(coin)
coin::wilcox_test(time_enf_cirrosis ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.946
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.8. EHM VS várices esofágicas
table(data_ehm$varices_tri, data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$varices_tri, data_ehm$dx_final_cal), margin = 1)*100, 2)

chisq.test(table(data_ehm$varices_tri, data_ehm$dx_final_cal))$expected # frecuencias esperadas > 5
chisq.test(table(data_ehm$varices_tri, data_ehm$dx_final_cal), correct = T) # p=0.03834
  # La corrección continuidad de Yates está predeterminado, pero se especifica por motivos prácticos
  # Se usó corrección de continuidad de Yates para el estándar igual que el resto. A parte sale el mismo valor p


# 3.9. EHM VS ascitis
table(data_ehm$asci_label, data_ehm$dx_final_cal)
round(prop.table(table(data_ehm$asci_label, data_ehm$dx_final_cal))*100, 2)

chisq.test(table(data_ehm$asci_label, data_ehm$dx_final_cal))$expected # frecuencias esperadas < 5
fisher.test(table(data_ehm$asci_label, data_ehm$dx_final_cal)) # p=0.1262


# 3.10. EHM VS MELD score
hist(data_ehm_NO_EHM$meld_cal_deci) # Se ve NO normal
shapiro.test(data_ehm_NO_EHM$meld_cal_deci) # p=0.0002652 --> NO NORMAL
hist(data_ehm_SI_EHM$meld_cal_deci) # Se ve NO normal
shapiro.test(data_ehm_SI_EHM$meld_cal_deci) # p=0.06608 --> NORMAL

median(data_ehm_NO_EHM$meld_cal_deci) # 9.026931 --> 9
min(data_ehm_NO_EHM$meld_cal_deci) # 6.43 --> 6
max(data_ehm_NO_EHM$meld_cal_deci) # 20.45337 --> 20

median(data_ehm_SI_EHM$meld_cal_deci) # 9.986998 --> 10
min(data_ehm_SI_EHM$meld_cal_deci) # 6.43 --> 6
max(data_ehm_SI_EHM$meld_cal_deci) # 15.50379 --> 16

#library(coin)
coin::wilcox_test(meld_cal_deci ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.4388
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.11. EHM VS MELD-Na score
hist(data_ehm_NO_EHM$meldNA_cal_deci) # Se ve NO normal
shapiro.test(data_ehm_NO_EHM$meldNA_cal_deci) # 0.07257--> NO NORMAL
hist(data_ehm_SI_EHM$meldNA_cal_deci) # Se ve Normal
shapiro.test(data_ehm_SI_EHM$meldNA_cal_deci) # 0.2015 --> NORMAL

median(data_ehm_NO_EHM$meldNA_cal_deci) # 10.8366 --> 11
min(data_ehm_NO_EHM$meldNA_cal_deci) # 4.790993 --> 5
max(data_ehm_NO_EHM$meldNA_cal_deci) # 22.40803 --> 22

median(data_ehm_SI_EHM$meldNA_cal_deci) # 10.90439 --> 11
min(data_ehm_SI_EHM$meldNA_cal_deci) # 4.550362 --> 5
max(data_ehm_SI_EHM$meldNA_cal_deci) # 18.36307 --> 18

#library(coin)
coin::wilcox_test(meldNA_cal_deci ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.8334
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.12. EHM VS INR
hist(data_ehm_NO_EHM$inr) # Se ve NO normal
shapiro.test(data_ehm_NO_EHM$inr) # 2.32e-05 --> NO NORMAL
hist(data_ehm_SI_EHM$inr) # Se ve NO NORMAL
shapiro.test(data_ehm_SI_EHM$inr) # 0.2242 --> NORMAL

median(data_ehm_NO_EHM$inr) # 1.2
min(data_ehm_NO_EHM$inr) # 0.9
max(data_ehm_NO_EHM$inr) # 2.2

median(data_ehm_SI_EHM$inr) # 1.2
min(data_ehm_SI_EHM$inr) # 0.9
max(data_ehm_SI_EHM$inr) # 1.6

#library(coin)
coin::wilcox_test(inr ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.7121
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.13. EHM VS albúmina sérica
hist(data_ehm_NO_EHM$alb) # Se ve NO NORMAL
shapiro.test(data_ehm_NO_EHM$alb) # p=0.06459 --> NORMAL
hist(data_ehm_SI_EHM$alb) # Se ve NORMAL
shapiro.test(data_ehm_SI_EHM$alb) # p=0.4684 --> NORMAL

mean(data_ehm_NO_EHM$alb) # 3.972821
sd(data_ehm_NO_EHM$alb) # 0.4628172

mean(data_ehm_SI_EHM$alb) # 3.931364
sd(data_ehm_SI_EHM$alb) # 0.3846866

var.test(alb ~ factor(dx_final_cal), data = data_ehm) # p=0.3696 --> varianzas iguales
t.test(alb ~ factor(dx_final_cal), data = data_ehm, var.equal = T) # p= 0.723


# 3.14. EHM VS creatinina
hist(data_ehm_NO_EHM$crea) # Se ve NO NORMAL
shapiro.test(data_ehm_NO_EHM$crea) # p=0.002284 --> NO NORMAL
hist(data_ehm_SI_EHM$crea) # # Se ve NORMAL
shapiro.test(data_ehm_SI_EHM$crea) # p=0.8186 --> NORMAL

median(data_ehm_NO_EHM$crea) # 0.72
min(data_ehm_NO_EHM$crea) # 0.50
max(data_ehm_NO_EHM$crea) # 1.40

median(data_ehm_SI_EHM$crea) # 0.70
min(data_ehm_SI_EHM$crea) # 0.40
max(data_ehm_SI_EHM$crea) # 1.04

#library(coin)
coin::wilcox_test(crea ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.4432
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.15. EHM VS Bilirrubina total
hist(data_ehm_NO_EHM$bt) # Se ve NO NORMAL
shapiro.test(data_ehm_NO_EHM$bt) # p=4.207e-06 --> NO NORMAL
hist(data_ehm_SI_EHM$bt) # Se ve NO NORMAL
shapiro.test(data_ehm_SI_EHM$bt) # p=0.002492 --> NO NORMAL

median(data_ehm_NO_EHM$bt) # 1.10
min(data_ehm_NO_EHM$bt) # 0.60
max(data_ehm_NO_EHM$bt) # 4.25

median(data_ehm_SI_EHM$bt) # 1.41
min(data_ehm_SI_EHM$bt) # 0.56
max(data_ehm_SI_EHM$bt) # 5.01

#library(coin)
coin::wilcox_test(bt ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.4044
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# 3.16. EHM VS sodio sérico
hist(data_ehm_NO_EHM$na) # Se ve NORMAL
shapiro.test(data_ehm_NO_EHM$na) # p=0.1203 --> NORMAL
hist(data_ehm_SI_EHM$na) # Se ve NORMAL
shapiro.test(data_ehm_SI_EHM$na) # p=0.006091 --> NO NORMAL

median(data_ehm_NO_EHM$na) # 138
min(data_ehm_NO_EHM$na) # 133
max(data_ehm_NO_EHM$na) # 143

median(data_ehm_SI_EHM$na) # 140
min(data_ehm_SI_EHM$na) # 130
max(data_ehm_SI_EHM$na) # 145

#library(coin)
coin::wilcox_test(na ~ factor(dx_final_cal), data = data_ehm, distribution = "asymptotic")
  # p=0.4905
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


### *** FIGURA 1 (GRÁFICO DE BARRAS AGRUPADO) *** ----

# Paso 1: Revisar la existencia variable varices_tri y transformarla a factor
table(data_ehm$varices_tri)
class(data_ehm$varices_tri)

data_ehm$varices_tri <- as.factor(data_ehm$varices_tri)
class(data_ehm$varices_tri)

# Paso 2: Crear una nueva columna con la cantidad total de personas en cada grupo de dx_final_cal y
# varices_tri
data_ehm <- data_ehm %>%
  group_by(varices_tri, dx_final_cal) %>%
  mutate(total_personas_2 = n()) %>%
  ungroup()

# Paso 3: Crear el gráfico DE BOX PLOT con etiquetas manuales
library(ggplot2)
ggplot(data_ehm, aes(x = varices_tri, y = phes_cal, fill = dx_final_cal)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Agrega el boxplot sin resaltar outliers +
  geom_jitter(aes(color = dx_final_cal), width = 0.2, size = 2, alpha = 0.5) +  # Agrega los puntos con jitter para visualizar mejor la distribución  
  scale_x_discrete(labels = c(
    "Ausencia" = "Absent", 
    "Grandes" = "Small",
    "Pequeñas" = "Large")) +
  scale_fill_manual(name = "Diagnosis of MHE",  # Cambia el nombre de la leyenda
                    labels = c("SI EHM" = "With MHE (n=22)", "NO EHM" = "Without MHE (n=39)"), # Cambia las categorías
                    values = c("SI EHM" = "grey50", "NO EHM" = "grey80")) + # Tonos de gris
  scale_color_manual(name = "Diagnosis of MHE",  
                     labels = c("SI EHM" = "With MHE (n=22)", "NO EHM" = "Without MHE (n=39)"),  
                     values = c("SI EHM" = "black", "NO EHM" = "darkgrey")) +
  labs(title = "PHES according to varicose vein size",
       x = "Esophageal varices",
       y = "PHES") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centra el título, lo pone en negritas y aumenta el tamaño
    axis.title.x = element_text(face = "bold", size = 16),  # Eje X en negritas
    axis.title.y = element_text(face = "bold", size = 16),  # Eje Y en negritas
    axis.text.x = element_text(size = 14, face = "bold"),  # Aumenta el tamaño de las categorías en el eje X
    axis.text.y = element_text(size = 14, face = "bold"),  # Aumenta el tamaño de los números en el eje Y
    legend.title = element_text(face = "bold"),  # Pone en negritas el título de la leyenda
    legend.position = c(0.1, 0.1),  # Ubica la leyenda en la esquina inferior izquierda
    legend.background = element_rect(fill = "white", color = "black")  # Fondo blanco y borde negro para mayor claridad
  )



#### 3) ANÁLISIS DE MÚLTIPLES VARIABLES ####

### PRE MULTIVARIABLE: Codificación numérica de variables para poder correr análisis de regresión ----

# Variable DEPENDIENTE: EHM --> dx_final_cal_cod
table(data_ehm$dx_final_cal)
class(data_ehm$dx_final_cal) # Es "character", debe ser "numeric" para poder correr la regresión

data_ehm$dx_final_cal_cod <- ifelse(data_ehm$dx_final_cal == "NO EHM", 0, 1)
class(data_ehm$dx_final_cal_cod) # Es "numeric", está bien

table(data_ehm$dx_final_cal_cod)
data_ehm <- data_ehm %>% dplyr::relocate(dx_final_cal_cod, .after = dx_final_cal)


# Varible INDEPENDIENTE PRINCIPAL: CHILD PUGH --> child_cat_cod
table(data_ehm$child_cat)
class(data_ehm$child_cat) # Es "character", por ser categórica debe ser "factor"

data_ehm$child_cat_cod <- ifelse(data_ehm$child_cat == "CHILD A", 1, 2)
class(data_ehm$child_cat_cod) # Es "numeric", debe ser "factor"
data_ehm$child_cat_cod <- as.factor(data_ehm$child_cat_cod)
class(data_ehm$child_cat_cod) # Es "factor", está bien

table(data_ehm$child_cat_cod)
data_ehm <- data_ehm %>% dplyr::relocate(child_cat_cod, .after = child_cat)


# OTRA VARIABLE que se usará como INDEPENDIENTE PRINCIPAL: várices --> varices_tri
table(data_ehm$varices_tri)
class(data_ehm$varices_tri) # Es "factor", está bien


# Variable sexo --> sexo_cod
table(data_ehm$sexo)
class(data_ehm$sexo) # Es "character", por ser categórica debe ser "factor"

data_ehm$sexo_cod <- ifelse(data_ehm$sexo == "F", 0, 1)
class(data_ehm$sexo_cod) # Es "numeric", debe ser "factor"
data_ehm$sexo_cod <- as.factor(data_ehm$sexo_cod)
class(data_ehm$sexo_cod) # Es "factor", está bien

table(data_ehm$sexo_cod)
data_ehm <- data_ehm %>% dplyr::relocate(sexo_cod, .after = sexo)


# Variable comorbilidad --> comorb_di_cod
table(data_ehm$comorb_di)
class(data_ehm$comorb_di) # Es "character", por ser categórica debe ser "factor"

data_ehm$comorb_di_cod <- ifelse(data_ehm$comorb_di == "Sí", 1, 0)
class(data_ehm$comorb_di_cod) # Es "numeric", debe ser "factor"
data_ehm$comorb_di_cod <- as.factor(data_ehm$comorb_di_cod)
class(data_ehm$comorb_di_cod) # Es "factor", está bien

table(data_ehm$comorb_di_cod)
data_ehm <- data_ehm %>% dplyr::relocate(comorb_di_cod, .after = comorb_di)


# Variable etiología --> etio_cru_cod
table(data_ehm$etio_cru)
class(data_ehm$etio_cru) # Es "character", por ser categórica debe ser "factor"

data_ehm$etio_cru_cod <- ifelse(data_ehm$etio_cru == "Hígado graso", 1,
                                ifelse(data_ehm$etio_cru == "Enfermedades autoinmunes", 2,
                                       ifelse(data_ehm$etio_cru == "Hepatitis viral", 3,
                                              ifelse(data_ehm$etio_cru == "Doble etiología", 4, 5))))
class(data_ehm$etio_cru_cod) # Es "numeric", debe ser "factor"
data_ehm$etio_cru_cod <- as.factor(data_ehm$etio_cru_cod)
class(data_ehm$etio_cru_cod) # Es "factor", está bien

table(data_ehm$etio_cru_cod)
data_ehm <- data_ehm %>% dplyr::relocate(etio_cru_cod, .after = etio_cru)


# Variable ascitis --> asci_label_cod
table(data_ehm$asci_label)
class(data_ehm$asci_label) # Es "character", por ser categórica debe ser "factor"

data_ehm$asci_label_cod <- ifelse(data_ehm$asci_label == "ascitis leve", 1, 2)
class(data_ehm$asci_label_cod) # Es "numeric", debe ser "factor"
data_ehm$asci_label_cod <- as.factor(data_ehm$asci_label_cod)
class(data_ehm$asci_label_cod) # Es "factor", está bien

table(data_ehm$asci_label_cod)
data_ehm <- data_ehm %>% dplyr::relocate(asci_label_cod, .after = asci_label)



### *** TABLA 4 *** ----

# MODELO 1: REGRESIÓN DE POISSON (ehm = child + tiempo_enf) ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Ajustar el modelo de regresión de Poisson ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_1 <- glm(dx_final_cal_cod ~ child_cat_cod + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)
modelo_1
summary(modelo_1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 2: Realiza la prueba de Heterocedasticidad de Breusch-Pagan ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # COMENTARIO: Este paso servirá para decidir que parámetro ingresar en "type" durante el cálculo de
  # los errores estandar robusto

lmtest::bptest(modelo_1) # p=0.8968 --> Homocedasticidad

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 3: Calcular errores estándar robustos (se usa HC2 porque es muestra mediana) ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_1_robusto <- lmtest::coeftest(modelo_1, vcov = sandwich::vcovHC(modelo_1, type = "HC2"))
modelo_1_robusto
  # p=0.06385
  # p=0.94627

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 4: Cálculo de RP e IC 95 % en el modelo robusto ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
exp(cbind(coef(modelo_1_robusto), confint(modelo_1_robusto)))
  # RP=1.91, IC 95 % (0.96 - 3.79)
  # RP=1.00, IC 95 % (0.98 - 1.02)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso  5: Evaluación de supuestos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # COMENTARIO: Se evalúa si el modelo robusto cumple con los supuestos correspondientes

# Primer supuesto: LINEALIDAD
  # COMENTARIO: La regresión de Poisson con enlace logarítmico (link = "log") asume relación lineal
  # entre las variables independientes y el valor esperado de la variable dependiente
  # en la escala logarítmica. Quier decir que la evaluación de linealidad NO debe ser con el valor de
  # la dependiente en su escala original, sino su valor esperado  en la escala del log.

# Forma 1: Método gráfico
modelo_1 <- glm(dx_final_cal_cod ~ child_cat_cod + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)
summary(modelo_1)

data_ehm$predichos_1 <- predict(modelo_1, type = "link")  # "link" calcula predichos en escala log

library(ggplot2)
ggplot(data_ehm, aes(x = time_enf_cirrosis, y = predichos_1)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Verificación de linealidad en escala log",
       x = "Tiempo de enfermedad cirrótica (meses)", 
       y = "Log(predicción de EHM)")
  # NO CUMPLE CON LA LINEALIDAD


# Forma 2: Método estadístico (uso de splines fraccionados y pruebas de Wald.)

# Cargar paquetes necesarios
library(splines)
library(car)

# Modelo original (asume linealidad)
modelo_1 <- glm(dx_final_cal_cod ~ child_cat_cod + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)

# Modelo con splines cúbicos restringidos (RCS)
modelo_1_spline_rcs <- glm(dx_final_cal_cod ~ child_cat_cod + ns(time_enf_cirrosis, df = 3),
                           family = poisson(link = "log"),
                           data = data_ehm)
summary(modelo_1_spline_rcs)

# Prueba de Wald para evaluar si hay no linealidad
linearHypothesis(modelo_1_spline_rcs, matchCoefs(modelo_1_spline_rcs, "ns\\(time_enf_cirrosis.*\\)"))
# p=0.9536 --> SÍ CUMPLE CON LA LINEALIDAD


# DECISIÓN: Se decide concluir que desde el punto de vista estadístico el modelo de Poisson robusto
# SÍ CUMPLE CON EL SUPUESTO DE LINEALIDAD.


# Segundo supuesto: INDEPENDENCIA
  # COMENTARIO: Se trata de un supuesto teórico. SI CUMPLE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 6: Resultados con el modelo final ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_1_robusto
  # Child Pugh --> p=0.06385
  # Tiempo de enfermedad cirrótica --> p=0.94627
exp(cbind(coef(modelo_1_robusto), confint(modelo_1_robusto)))
  # Child Pugh --> RP=1.91, IC 95 % (0.96 - 3.79)
  # Tiempo de enfermedad cirrótica --> RP=1.00, IC 95 % (0.98 - 1.02)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 7: Evaluación de multicolinealidad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
vif(modelo_1) # Todo es menor de 5. No hay multicolinealidad


# MODELO 2: REGRESIÓN DE POISSON (ehm = child + etiología) ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Ajustar el modelo de regresión de Poisson ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_2 <- glm(dx_final_cal_cod ~ child_cat_cod + etio_cru_cod,
                family = poisson(link = "log"),
                data = data_ehm)
modelo_2
summary(modelo_2)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 2: Realiza la prueba de Heterocedasticidad de Breusch-Pagan ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # COMENTARIO: Este paso servirá para decidir que parámetro ingresar en "type" durante el cálculo de
  # los errores estandar robusto
lmtest::bptest(modelo_2) # p=0.003369 --> HETEROCEDASTICIDAD 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 3: Calcular errores estándar robustos (se usa HC2 porque es muestra mediana y presenta hetero) ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_2_robusto <- lmtest::coeftest(modelo_2, vcov = sandwich::vcovHC(modelo_2, type = "HC2"))
modelo_2_robusto
  # p=0.01339
  # p=0.14435
  # p=0.87197
  # p=0.59125
  # p< 2.2e-16

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 4: Cálculo de RP e IC 95 % en el modelo robusto ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
exp(cbind(coef(modelo_2_robusto), confint(modelo_2_robusto)))
  # RP=2.34, IC 95 % (1.19 - 4.58)
  # RP=0.43, IC 95 % (0.14 - 1.34)
  # RP=0.88, IC 95 % (0.19 - 4.09)
  # RP=1.40, IC 95 % (0.41 - 4.72)
  # RP=2.57e-08, IC 95 % (7.87e-09 - 8.411525e-08)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso  5: Evaluación de supuestos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# COMENTARIO: Se evalúa si el modelo robusto cumple con los supuestos correspondientes

# Primer supuesto: LINEALIDAD
  # COMENTARIO: SE ASUME LINEALIDAD porque todas las variables son categóricas


# Segundo supuesto: INDEPENDENCIA
  # COMENTARIO: Se trata de un supuesto teórico. SI CUMPLE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 6: Resultados con el modelo final ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_2_robusto
  # Child Pugh --> p=0.01339
  # Etiología: enfermedad autoinmune --> p=0.14435
  # Etiología: hepatitis viral --> p=0.87197
  # Etiología: doble etiología --> p=0.59125
  # Etiología: alcohol --> p< 2.2e-16
exp(cbind(coef(modelo_2_robusto), confint(modelo_2_robusto)))
  # Child Pugh --> RP=2.34, IC 95 % (1.19 - 4.58)
  # Etiología: enfermedad autoinmune --> RP=0.43, IC 95 % (0.14 - 1.34)
  # Etiología: hepatitis viral --> RP=0.88, IC 95 % (0.19 - 4.09)
  # Etiología: doble etiología --> RP=1.40, IC 95 % (0.41 - 4.72)
  # Etiología: alcohol --> RP=2.57e-08, IC 95 % (7.87e-09 - 8.41-08)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 7: Evaluación de multicolinealidad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
vif(modelo_2) # Todo es menor de 5. No hay multicolinealidad


# MODELO 3: REGRESIÓN DE POISSON (ehm = meld + tiempo_enf) ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Ajustar el modelo de regresión de Poisson ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_3 <- glm(dx_final_cal_cod ~ meld_cal_deci + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)
modelo_3
summary(modelo_3)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 2: Realiza la prueba de Heterocedasticidad de Breusch-Pagan ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # COMENTARIO: Este paso servirá para decidir que parámetro ingresar en "type" durante el cálculo
  # de los errores estandar robusto
lmtest::bptest(modelo_3) # p=0.6871 --> Homocedasticidad

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 3: Calcular errores estándar robustos (se usa HC2 porque es muestra mediana) ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_3_robusto <- lmtest::coeftest(modelo_3, vcov = sandwich::vcovHC(modelo_3, type = "HC2"))
modelo_3_robusto
  # p=0.49429
  # p=0.90157

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 4: Cálculo de RP e IC 95 % en el modelo robusto ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
exp(cbind(coef(modelo_3_robusto), confint(modelo_3_robusto)))
  # RP=1.04, IC 95 % (0.93 - 1.16)
  # RP=1.00, IC 95 % (0.98 - 1.02)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso  5: Evaluación de supuestos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # COMENTARIO: Se evalúa si el modelo robusto cumple con los supuestos correspondientes

# Primer supuesto: LINEALIDAD
  # COMENTARIO: La regresión de Poisson con enlace logarítmico (link = "log") asume relación lineal
  # entre las variables independientes y el valor esperado de la variable dependiente
  # en la escala logarítmica. Quier decir que la evaluación de linealidad NO debe ser con el valor de
  # la dependiente en su escala original, sino su valor esperado  en la escala del log.

# Forma 1: Método gráfico
modelo_3 <- glm(dx_final_cal_cod ~ meld_cal_deci + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)
summary(modelo_3)

data_ehm$predichos_3 <- predict(modelo_3, type = "link")  # "link" calcula predichos en escala log

library(ggplot2)
ggplot(data_ehm, aes(x = time_enf_cirrosis, y = predichos_3)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Verificación de linealidad en escala log",
       x = "Tiempo de enfermedad cirrótica (meses)", 
       y = "Log(predicción de EHM)")
  # NO CUMPLE CON LA LINEALIDAD

ggplot(data_ehm, aes(x = meld_cal_deci, y = predichos_3)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Verificación de linealidad en escala log",
       x = "MELD score", 
       y = "Log(predicción de EHM)")
  # SÍ CUMPLE CON LA LINEALIDAD


# Forma 2: Método estadístico (uso de splines fraccionados y pruebas de Wald.)

# Cargar paquetes necesarios
library(splines)
library(car)

# Modelo original (asume linealidad)
modelo_3 <- glm(dx_final_cal_cod ~ meld_cal_deci + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)

# Modelo con splines cúbicos restringidos (RCS)
modelo_3.1_spline_rcs <- glm(dx_final_cal_cod ~ meld_cal_deci + ns(time_enf_cirrosis, df = 3),
                           family = poisson(link = "log"),
                           data = data_ehm)
summary(modelo_3.1_spline_rcs)

modelo_3.2_spline_rcs <- glm(dx_final_cal_cod ~ ns(meld_cal_deci, df = 3) + time_enf_cirrosis,
                             family = poisson(link = "log"),
                             data = data_ehm)
summary(modelo_3.2_spline_rcs)

# Prueba de Wald para evaluar si hay no linealidad
linearHypothesis(modelo_3.1_spline_rcs, matchCoefs(modelo_3.1_spline_rcs, "ns\\(time_enf_cirrosis.*\\)"))
  # p=0.9942 --> SÍ CUMPLE CON LA LINEALIDAD con variable TIEMPO DE ENFERMEDAD CIRRÓTICA

linearHypothesis(modelo_3.2_spline_rcs, matchCoefs(modelo_3.2_spline_rcs, "ns\\(meld_cal_deci.*\\)"))
  # p=0.541 --> SÍ CUMPLE CON LA LINEALIDAD con variable MELD SCORE

# DECISIÓN: Se decide concluir que desde el punto de vista estadístico el modelo de Poisson robusto
  # SÍ CUMPLE CON EL SUPUESTO DE LINEALIDAD.


# Segundo supuesto: INDEPENDENCIA
  # COMENTARIO: Se trata de un supuesto teórico. SI CUMPLE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 6: Resultados con el modelo final ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_3_robusto
  # MELD score --> p=0.49429
  # Tiempo de enfermedad cirrótica --> p=0.90157
exp(cbind(coef(modelo_3_robusto), confint(modelo_3_robusto)))
  # MELD score --> RP=1.04, IC 95 % (0.93 - 1.16)
  # Tiempo de enfermedad cirrótica --> RP=1.00, IC 95 % (0.98 - 1.02)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 7: Evaluación de multicolinealidad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
vif(modelo_3) # Todo es menor de 5. No hay multicolinealidad


# MODELO 4: REGRESIÓN DE POISSON (ehm = meld + etiología) ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Ajustar el modelo de regresión de Poisson ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_4 <- glm(dx_final_cal_cod ~ meld_cal_deci + etio_cru_cod,
                family = poisson(link = "log"),
                data = data_ehm)
modelo_4
summary(modelo_4)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 2: Realiza la prueba de Heterocedasticidad de Breusch-Pagan ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # COMENTARIO: Este paso servirá para decidir que parámetro ingresar en "type" durante el cálculo
  # de los errores estandar robusto
lmtest::bptest(modelo_4) # p=0.01046 --> HETEROCEDASTICIDAD

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 3: Calcular errores estándar robustos (se usa HC2 porque es muestra mediana) ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_4_robusto <- lmtest::coeftest(modelo_4, vcov = sandwich::vcovHC(modelo_4, type = "HC2"))
modelo_4_robusto
  # p=0.25319
  # p=0.28087
  # p=0.91510
  # p=0.59778
  # p< 2e-16

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 4: Cálculo de RP e IC 95 % en el modelo robusto ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
exp(cbind(coef(modelo_4_robusto), confint(modelo_4_robusto)))
  # RP=1.06, IC 95 % (0.96 - 1.18)
  # RP=0.47, IC 95 % (0.12 - 1.86)
  # RP=0.93; IC 95 % (0.23 - 3.75)
  # RP=1.40; IC 95 % (0.41 - 4.82)
  # RP=2.65e-08; IC 95 (9.22e-09 - 7.64e-08)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso  5: Evaluación de supuestos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # COMENTARIO: Se evalúa si el modelo robusto cumple con los supuestos correspondientes

# %%%%%%%%%%%%% LINEALIDAD %%%%%%%%%%%%% #
# COMENTARIO: La regresión de Poisson con enlace logarítmico (link = "log") asume relación lineal
# entre las variables independientes y el valor esperado de la variable dependiente
# en la escala logarítmica. Quier decir que la evaluación de linealidad NO debe ser con el valor de
# la dependiente en su escala original, sino su valor esperado  en la escala del log.

# Forma 1: Método gráfico
modelo_4 <- glm(dx_final_cal_cod ~ meld_cal_deci + etio_cru_cod,
                family = poisson(link = "log"),
                data = data_ehm)
summary(modelo_4)

data_ehm$predichos_4 <- predict(modelo_4, type = "link")  # "link" calcula predichos en escala log

library(ggplot2)
ggplot(data_ehm, aes(x = meld_cal_deci, y = predichos_4)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Verificación de linealidad en escala log",
       x = "MELD score", 
       y = "Log(predicción de EHM)")
  # NO CUMPLE CON LA LINEALIDAD


# Forma 2: Método estadístico (uso de splines fraccionados y pruebas de Wald.)

# Cargar paquetes necesarios
library(splines)
library(car)

# Modelo original (asume linealidad)
modelo_4 <- glm(dx_final_cal_cod ~ meld_cal_deci + etio_cru_cod,
                family = poisson(link = "log"),
                data = data_ehm)

# Modelo con splines cúbicos restringidos (RCS)
modelo_4_spline_rcs <- glm(dx_final_cal_cod ~ ns(meld_cal_deci, df = 3) + etio_cru_cod,
                             family = poisson(link = "log"),
                             data = data_ehm)
summary(modelo_4_spline_rcs)

# Prueba de Wald para evaluar si hay no linealidad
linearHypothesis(modelo_4_spline_rcs, matchCoefs(modelo_4_spline_rcs, "ns\\(meld_cal_deci.*\\)"))
  # p=0.4064 --> SÍ CUMPLE CON LA LINEALIDAD con variable TIEMPO DE ENFERMEDAD CIRRÓTICA


# DECISIÓN: Se decide concluir que desde el punto de vista estadístico el modelo de Poisson robusto
  # SÍ CUMPLE CON EL SUPUESTO DE LINEALIDAD.


# %%%%%%%%%%%%% INDEPENDENCIA %%%%%%%%%%%%% #
# COMENTARIO: Se trata de un supuesto teórico. SI CUMPLE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 6: Resultados con el modelo final ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_4_robusto
  # MELD score --> p=0.25319
  # Etiología: enfermedades autoinmunes --> p=0.28087
  # Etiología: hepatitis viral --> p=0.91510
  # Etiología: doble etiología --> p=0.59778
  # Etiología: alcohol --> p< 2e-16
exp(cbind(coef(modelo_4_robusto), confint(modelo_4_robusto)))
  # MELD score --> RP=1.06, IC 95 % (0.96 - 1.18)
  # Etiología: enfermedades autoinmunes --> RP=0.47, IC 95 % (0.12 - 1.86)
  # Etiología: hepatitis viral --> RP=0.93; IC 95 % (0.23 - 3.75)
  # Etiología: doble etiología --> RP=1.40; IC 95 % (0.41 - 4.82)
  # Etiología: alcohol --> RP=2.65e-08; IC 95 (9.22e-09 - 7.64e-08)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 7: Evaluación de multicolinealidad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
vif(modelo_4) # Todo es menor de 5. No hay multicolinealidad


# MODELO 5: REGRESIÓN DE POISSON (ehm = várices + tiempo_enf) ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Ajustar el modelo de regresión de Poisson ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_5 <- glm(dx_final_cal_cod ~ varices_tri + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)
modelo_5
summary(modelo_5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 2: Realiza la prueba de Heterocedasticidad de Breusch-Pagan ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # COMENTARIO: Este paso servirá para decidir que parámetro ingresar en "type" durante el cálculo de
  # los errores estandar robusto

lmtest::bptest(modelo_5) # p=0.4476 --> Homocedasticidad

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 3: Calcular errores estándar robustos (se usa HC2 porque es muestra mediana) ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_5_robusto <- lmtest::coeftest(modelo_5, vcov = sandwich::vcovHC(modelo_5, type = "HC2"))
modelo_5_robusto
  # Várices pequeñas --> p=0.5064752
  # Várices grandes --> p=0.0188949
  # Tiempo de enfermedad cirrótica --> p=0.5070535

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 4: Cálculo de RP e IC 95 % en el modelo robusto ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
exp(cbind(coef(modelo_5_robusto), confint(modelo_5_robusto)))
  # Várices pequeñas --> RP=1.54, IC 95 % (0.43 - 5.54)
  # Várices grandes --> RP=2.95, IC 95 % (1.20 - 7.30)
  # Tiempo de enfermedad cirrótica --> RP=0.99, IC 95 % (0.97 - 1.01)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso  5: Evaluación de supuestos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # COMENTARIO: Se evalúa si el modelo robusto cumple con los supuestos correspondientes

# %%%%%%%%%%%%% LINEALIDAD %%%%%%%%%%%%% #
  # COMENTARIO: La regresión de Poisson con enlace logarítmico (link = "log") asume relación lineal
  # entre las variables independientes y el valor esperado de la variable dependiente
  # en la escala logarítmica. Quier decir que la evaluación de linealidad NO debe ser con el valor de
  # la dependiente en su escala original, sino su valor esperado  en la escala del log.

# Forma 1: Método gráfico
modelo_5 <- glm(dx_final_cal_cod ~ data_ehm$varices_tri + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)
summary(modelo_5)

data_ehm$predichos_5 <- predict(modelo_5, type = "link")  # "link" calcula predichos en escala log

library(ggplot2)
ggplot(data_ehm, aes(x = time_enf_cirrosis, y = predichos_5)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Verificación de linealidad en escala log",
       x = "Tiempo de enfermedad cirrótica (meses)", 
       y = "Log(predicción de EHM)")
  # NO CUMPLE CON LA LINEALIDAD


# Forma 2: Método estadístico (uso de splines fraccionados y pruebas de Wald.)

# Cargar paquetes necesarios
library(splines)
library(car)

# Modelo original (asume linealidad)
modelo_5 <- glm(dx_final_cal_cod ~ data_ehm$varices_tri + time_enf_cirrosis,
                family = poisson(link = "log"),
                data = data_ehm)

# Modelo con splines cúbicos restringidos (RCS)
modelo_5_spline_rcs <- glm(dx_final_cal_cod ~ varices_tri + ns(time_enf_cirrosis, df = 3),
                           family = poisson(link = "log"),
                           data = data_ehm)
summary(modelo_5_spline_rcs)

# Prueba de Wald para evaluar si hay no linealidad
linearHypothesis(modelo_5_spline_rcs, matchCoefs(modelo_5_spline_rcs, "ns\\(time_enf_cirrosis.*\\)"))
  # p=0.9113 --> SÍ CUMPLE CON LA LINEALIDAD


# DECISIÓN: Se decide concluir que desde el punto de vista estadístico el modelo de Poisson robusto
  # SÍ CUMPLE CON EL SUPUESTO DE LINEALIDAD.


# %%%%%%%%%%%%% INDEPENDENCIA %%%%%%%%%%%%% #
  # COMENTARIO: Se trata de un supuesto teórico. SI CUMPLE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 6: Resultados con el modelo final ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_5_robusto
  # Várices pequeñas --> p=0.5064752
  # Várices grandes --> p=0.0188949
  # Tiempo de enfermedad cirrótica --> p=0.5070535
exp(cbind(coef(modelo_5_robusto), confint(modelo_5_robusto)))
  # Várices pequeñas --> RP=1.54, IC 95 % (0.43 - 5.54)
  # Várices grandes --> RP=2.95, IC 95 % (1.20 - 7.30)
  # Tiempo de enfermedad cirrótica --> RP=0.99, IC 95 % (0.97 - 1.01)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 7: Evaluación de multicolinealidad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
vif(modelo_5) # Todo es menor de 5. No hay multicolinealidad


# MODELO 6: REGRESIÓN DE POISSON (ehm = child + várices) ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Ajustar el modelo de regresión de Poisson ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_6 <- glm(dx_final_cal_cod ~ child_cat_cod + varices_tri,
                family = poisson(link = "log"),
                data = data_ehm)
modelo_6
summary(modelo_6)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 2: Realiza la prueba de Heterocedasticidad de Breusch-Pagan ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # COMENTARIO: Este paso servirá para decidir que parámetro ingresar en "type" durante el cálculo de
  # los errores estandar robusto

lmtest::bptest(modelo_6) # p=0.5364 --> Homocedasticidad

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 3: Calcular errores estándar robustos (se usa HC2 porque es muestra mediana) ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_6_robusto <- lmtest::coeftest(modelo_6, vcov = sandwich::vcovHC(modelo_6, type = "HC2"))
modelo_6_robusto
  # Child Pugh --> p=0.12522
  # Várices pequeñas --> p=0.53167
  # Várices grandes --> p=0.04081
  

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 4: Cálculo de RP e IC 95 % en el modelo robusto ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
exp(cbind(coef(modelo_6_robusto), confint(modelo_6_robusto)))
  # Child Pugh --> RP=1.64, IC 95 % (0.08 - 0.42)
  # Várices pequeñas --> RP=1.52, IC 95 % (0.41 - 5.62)
  # Várices grandes --> RP=2.53, IC 95 % (1.04 - 6.14)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso  5: Evaluación de supuestos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # COMENTARIO: Se evalúa si el modelo robusto cumple con los supuestos correspondientes

# Primer supuesto: LINEALIDAD
# COMENTARIO: SE ASUME LINEALIDAD porque todas las variables son categóricas


# Segundo supuesto: INDEPENDENCIA

  # COMENTARIO: Se trata de un supuesto teórico. SI CUMPLE

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 6: Resultados con el modelo final ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_6_robusto
  # Child Pugh --> p=0.12522
  # Várices pequeñas --> p=0.53167
  # Várices grandes --> p=0.04081
exp(cbind(coef(modelo_6_robusto), confint(modelo_6_robusto)))
  # Child Pugh --> RP=1.64, IC 95 % (0.08 - 0.42)
  # Várices pequeñas --> RP=1.52, IC 95 % (0.41 - 5.62)
  # Várices grandes --> RP=2.53, IC 95 % (1.04 - 6.14)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 7: Evaluación de multicolinealidad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
vif(modelo_6) # Todo es menor de 5. No hay multicolinealidad


#### 4) MATERIAL SUPLEMENTARIO ####

### *** SUPLEMENTO 1 (Pruebas psicométricas) *** ----

# DST
hist(data_ehm_NO_EHM$dst_1) # Se ve NO NORMAL
shapiro.test(data_ehm_NO_EHM$dst_1) # p=0.2864 --> NORMAL
hist(data_ehm_SI_EHM$dst_1) # Se ve NORMAL
shapiro.test(data_ehm_SI_EHM$dst_1) # p=0.1684 -->NORMAL

mean(data_ehm_NO_EHM$dst_1) # 35.07692
sd(data_ehm_NO_EHM$dst_1) # 8.389795

mean(data_ehm_SI_EHM$dst_1) # 24.63636
sd(data_ehm_SI_EHM$dst_1) # 8.605344


# NCT-A
hist(data_ehm_NO_EHM$nctA_1) # Se ve NO NORMAL
shapiro.test(data_ehm_NO_EHM$nctA_1) # p=0.07741 --> NORMAL
hist(data_ehm_SI_EHM$nctA_1) # Se ve NORMAL
shapiro.test(data_ehm_SI_EHM$nctA_1) # p=0.5752 -->NORMAL

mean(data_ehm_NO_EHM$nctA_1) # 47.35897
sd(data_ehm_NO_EHM$nctA_1) # 11.20265

mean(data_ehm_SI_EHM$nctA_1) # 78.68182
sd(data_ehm_SI_EHM$nctA_1) # 26.54446


# NCT-B
hist(data_ehm_NO_EHM$nctB_1) # Se ve NORMAL
shapiro.test(data_ehm_NO_EHM$nctB_1) # p=0.05635 --> NORMAL
hist(data_ehm_SI_EHM$nctB_1) # Se ve NORMAL
shapiro.test(data_ehm_SI_EHM$nctB_1) # p=0.6884 -->NORMAL

mean(data_ehm_NO_EHM$nctB_1) # 110.5641
sd(data_ehm_NO_EHM$nctB_1) # 35.09146

mean(data_ehm_SI_EHM$nctB_1) # 187.1818
sd(data_ehm_SI_EHM$nctB_1) # 67.16904


# SDT
hist(data_ehm_NO_EHM$sdt_1) # Se ve NORMAL
shapiro.test(data_ehm_NO_EHM$sdt_1) # p=0.1317 --> NORMAL
hist(data_ehm_SI_EHM$sdt_1) # Se ve NO NORMAL
shapiro.test(data_ehm_SI_EHM$sdt_1) # p=0.02524 --> NO NORMAL

median(data_ehm_NO_EHM$sdt_1) # 81
min(data_ehm_NO_EHM$sdt_1) # 49
max(data_ehm_NO_EHM$sdt_1) # 121

median(data_ehm_SI_EHM$sdt_1) # 100
min(data_ehm_SI_EHM$sdt_1) # 58
max(data_ehm_SI_EHM$sdt_1) # 220

# LDT
hist(data_ehm_NO_EHM$ldt_1) # Se ve NORMAL
shapiro.test(data_ehm_NO_EHM$ldt_1) # p=0.6933 --> NORMAL
hist(data_ehm_SI_EHM$ldt_1) # Se ve NORMAL
shapiro.test(data_ehm_SI_EHM$ldt_1) # p=0.1643 --> NORMAL

mean(data_ehm_NO_EHM$ldt_1) # 86.79487
sd(data_ehm_NO_EHM$ldt_1) # 21.9631

mean(data_ehm_SI_EHM$ldt_1) # 126.4091
sd(data_ehm_SI_EHM$ldt_1) # 34.20479


# PHES
hist(data_ehm_NO_EHM$phes_cal) # Se ve NORMAL
shapiro.test(data_ehm_NO_EHM$phes_cal) # p=0.4552 --> NORMAL
hist(data_ehm_SI_EHM$phes_cal) # Se ve NO NORMAL
shapiro.test(data_ehm_SI_EHM$phes_cal) # p=0.0004241 --> NO NORMAL

median(data_ehm_NO_EHM$phes_cal) # -1.141706
min(data_ehm_NO_EHM$phes_cal) # -4.418552
max(data_ehm_NO_EHM$phes_cal) # 3.299575

median(data_ehm_SI_EHM$phes_cal) # -6.471085
min(data_ehm_SI_EHM$phes_cal) # -17.36268
max(data_ehm_SI_EHM$phes_cal) # -4.630048



### *** SUPLEMENTO 2 (ggpairs()) *** ----

# GRÁFICO DE CORRELACIÓN

data_num <- data_ehm %>% dplyr::select(phes_cal,
                                       edad,
                                       anios_instr,
                                       time_enf_cirrosis,
                                       meld_cal_deci,
                                       meldNA_cal_deci,
                                       inr,
                                       alb,
                                       crea,
                                       bt,
                                       na)
GGally::ggpairs(data_num,
                lower = list(continuous = "smooth"),
                diag = list(continuous = "densityDiag"),
                axisLabels = "none",
                columnLabels = c("PHES (points)",
                                 "Age",
                                 "Instruction (years)",
                                 "Time with cirrhosis (years)",
                                 "MELD score",
                                 "MELD-Na score",
                                 "INR",
                                 "Albumin",
                                 "Creatinine",
                                 "Total bilirubin",
                                 "Serum sodium")
)



### *** SUPLEMENTO 3 (asociación con categóricos) *** ----

# ASOCIACIÓN CON PHES NUMÉRICO

data_cat <- data_ehm %>% dplyr::select(phes_cal,
                                       sexo_cod,
                                       comorb_di_cod,
                                       etio_cru_cod,
                                       varices_tri,
                                       asci_label_cod,
                                       child_cat_cod)


# PHES VS sexo
table(data_cat$sexo_cod)
data_cat_Fem <- subset(data_cat, sexo_cod == 0)
data_cat_Masc <- subset(data_cat, sexo_cod == 1)

hist(data_cat_Fem$phes_cal) # Se ve Normal
shapiro.test(data_cat_Fem$phes_cal) # p=0.741 --> NORMAL
hist(data_cat_Masc$phes_cal) # Se ve NO Normal
shapiro.test(data_cat_Masc$phes_cal) # p=0.002775 --> NO NORMAL

median(data_cat_Fem$phes_cal) # -3.115553
min(data_cat_Fem$phes_cal) # -8.348556
max(data_cat_Fem$phes_cal) # 3.299575

median(data_cat_Masc$phes_cal) # -2.826505
min(data_cat_Masc$phes_cal) # -17.36268
max(data_cat_Masc$phes_cal) # 1.935596

#library(coin)
coin::wilcox_test(phes_cal ~ factor(sexo_cod), data = data_cat, distribution = "asymptotic")
  # p=0.8849
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# PHES VS comorbilidad
table(data_cat$comorb_di_cod)
data_cat_NO_comorb <- subset(data_cat, comorb_di_cod == 0)
data_cat_SI_comorb <- subset(data_cat, comorb_di_cod == 1)

hist(data_cat_NO_comorb$phes_cal) # Se ve NO Normal
shapiro.test(data_cat_NO_comorb$phes_cal) # p=0.0005287 --> NO NORMAL
hist(data_cat_SI_comorb$phes_cal) # Se ve NO Normal
shapiro.test(data_cat_SI_comorb$phes_cal) # p=0.2925 --> NORMAL

median(data_cat_NO_comorb$phes_cal) # -2.712522
min(data_cat_NO_comorb$phes_cal) # -17.36268
max(data_cat_NO_comorb$phes_cal) # 1.935596

median(data_cat_SI_comorb$phes_cal) # -4.124256
min(data_cat_SI_comorb$phes_cal) # -7.423583
max(data_cat_SI_comorb$phes_cal) # 3.299575

#library(coin)
coin::wilcox_test(phes_cal ~ factor(comorb_di_cod), data = data_cat, distribution = "asymptotic")
  # p=0.9738
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates


# PHES VS etiología
table(data_cat$etio_cru_cod)
data_cat_etio_1 <- subset(data_cat, etio_cru_cod == 1)
data_cat_etio_2 <- subset(data_cat, etio_cru_cod == 2)
data_cat_etio_3 <- subset(data_cat, etio_cru_cod == 3)
data_cat_etio_4 <- subset(data_cat, etio_cru_cod == 4)
data_cat_etio_5 <- subset(data_cat, etio_cru_cod == 5)

median(data_cat_etio_1$phes_cal) # -3.557621 --> -3.55
min(data_cat_etio_1$phes_cal) # -13.68743 --> -13.68
max(data_cat_etio_1$phes_cal) # 3.299575 --> 3.30

median(data_cat_etio_2$phes_cal) # -1.995768 --> -1.99
min(data_cat_etio_2$phes_cal) # -7.477341 --> -7.47
max(data_cat_etio_2$phes_cal) # -0.7836499 --> -0.77

median(data_cat_etio_3$phes_cal) # -4.271194 --> -4.26
min(data_cat_etio_3$phes_cal) # -8.319517 --> -8.31
max(data_cat_etio_3$phes_cal) # -0.001517832 --> 0.00

median(data_cat_etio_4$phes_cal) # -2.222843 --> -2.21
min(data_cat_etio_4$phes_cal) # -17.36268 --> -17.35
max(data_cat_etio_4$phes_cal) # 0.3906165 --> 0.39

median(data_cat_etio_5$phes_cal) # -1.297659 --> -1.29
min(data_cat_etio_5$phes_cal) # -4.418552 --> -4.41
max(data_cat_etio_5$phes_cal) # 1.935596 --> 1.94

kruskal.test(data_cat$phes_cal ~ data_cat$etio_cru_cod)
  # p=0.7587


# PHES VS várices esofágicas
table(data_cat$varices_tri)
data_cat_NO_vari <- subset(data_cat, varices_tri == "Ausencia")
data_cat_PE_vari <- subset(data_cat, varices_tri == "Pequeñas")
data_cat_GR_vari <- subset(data_cat, varices_tri == "Grandes")

hist(data_cat_NO_vari$phes_cal) # Se ve NO Normal
shapiro.test(data_cat_NO_vari$phes_cal) # p=0.4709 --> NORMAL
hist(data_cat_PE_vari$phes_cal) # Se ve Normal
shapiro.test(data_cat_PE_vari$phes_cal) # p=0.9032 --> NORMAL
hist(data_cat_GR_vari$phes_cal) # Se ve Normal
shapiro.test(data_cat_GR_vari$phes_cal) # p=0.1154 --> NORMAL

mean(data_cat_NO_vari$phes_cal) # -2.741814
sd(data_cat_NO_vari$phes_cal) # 2.480059

mean(data_cat_PE_vari$phes_cal) # -1.715527
sd(data_cat_PE_vari$phes_cal) # 3.195002

mean(data_cat_GR_vari$phes_cal) # -5.127714
sd(data_cat_GR_vari$phes_cal) # 4.717243

anova <- aov(phes_cal ~ varices_tri, data = data_cat)
summary(anova) # p=0.0202


# PHES VS CHILD
table(data_cat$child_cat_cod)
data_cat_child_A <- subset(data_cat, child_cat_cod == 1)
data_cat_child_B <- subset(data_cat, child_cat_cod == 2)

hist(data_cat_child_A$phes_cal) # Se ve NO Normal
shapiro.test(data_cat_child_A$phes_cal) # p=0.0006395 --> NO NORMAL
hist(data_cat_child_B$phes_cal) # Se ve Normal
shapiro.test(data_cat_child_B$phes_cal) # p=0.9679 --> NORMAL

median(data_cat_child_A$phes_cal) # -2.81336 --> -2.80
min(data_cat_child_A$phes_cal) # -17.36268 --> -17.35
max(data_cat_child_A$phes_cal) # 3.299575 --> 3.30

median(data_cat_child_B$phes_cal) # -5.039717 --> -5.03
min(data_cat_child_B$phes_cal) # -13.32937 --> -13.32
max(data_cat_child_B$phes_cal) # 1.935596 --> 1.94

#library(coin)
coin::wilcox_test(phes_cal ~ factor(child_cat_cod), data = data_cat, distribution = "asymptotic")
  # p=0.1985
  # Se usó distribution = "asymptotic" porque la muestra es grande y no botó advertencia por empates



### *** SUPLEMENTO 4 (Regresión Lineal y Splines) *** ----

## S(4.1) MODELO 7: REGRESIÓN LINEAL (phes = várices + tiempo_enf) ----

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Ajustar el modelo de Regresión Lineal ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_6_lm <- lm(phes_cal ~ varices_tri + time_enf_cirrosis,
                  data = data_ehm)
modelo_6_lm
summary(modelo_6_lm)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 2: Evaluación de supuestos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# %%%%%%%%%%%%% Linealidad %%%%%%%%%%%%% #
lmtest::resettest(modelo_6_lm)
  # p=0.3106 --> LINEALIDAD

ggplot(data = data_ehm,
       aes(time_enf_cirrosis, modelo_6_lm$residuals)) +
  geom_point() +
  geom_smooth (color = "firebrick") +
  geom_hline (yintercept = 0) + theme_bw ()
  # NO LINEALIDAD

# %%%%%%%%%%%%% Independencia de los errores %%%%%%%%%%%%% #
car::durbinWatsonTest(modelo_6_lm)
  # p=0.462 --> SÍ INDEPENDENCIA

plot(residuals(modelo_6_lm), pch=19, col="blue")
abline(h = 0, col = "red", lty = 2, lwd = 2)

# %%%%%%%%%%%%% Normalidad de los errores %%%%%%%%%%%%% #
nortest::lillie.test(modelo_6_lm$residuals)
  # p=0.1064 --> SÍ NORMALIDAD

car::qqPlot(modelo_6_lm)

# %%%%%%%%%%%%% Heterocedasticidad %%%%%%%%%%%%% #
lmtest::bgtest(modelo_6_lm)
  # p=0.09193 --> SI HOMOCEDASTICIDAD

ggplot(data = data_ehm,
       aes(modelo_6_lm$fitted.values, modelo_6_lm$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) + theme_bw()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 3: Evaluación de modificación de efecto ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_6_lm_int <- lm(phes_cal ~ varices_tri*time_enf_cirrosis,
                      data = data_ehm)
modelo_6_lm_int
summary(modelo_6_lm_int)

AIC(modelo_6_lm, modelo_6_lm_int) # Se compara el AIC del modelo sin y con interacción
  # No mejoró AIC, queda sin interacción
BIC(modelo_6_lm, modelo_6_lm_int) # Se compara el BIC del modelo sin y con interacción
  # No mejoró BIC, queda sin interacción

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 4: Resultados con el modelo final ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
summary(modelo_6_lm)


## S(4.2) MODELO 8: REGRESIÓN NO LINEAL: SPLINES (phes = várices + tiempo_enf) ----

  # COMENTARIO: Se hará el ejercicio de un modelo de regresión no lineal en base a los resultados
  # gráficos no lineales del supuesto de linealidad de la regresión lineal

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Creación de SPLINES NATURALES con diferente grado de libertad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
library(splines)
modelo_6_spline_df3 <- lm(phes_cal ~ varices_tri + ns(time_enf_cirrosis, df = 3), data = data_ehm)
summary(modelo_6_spline_df3)

modelo_6_spline_df4 <- lm(phes_cal ~ varices_tri + ns(time_enf_cirrosis, df = 4), data = data_ehm)
summary(modelo_6_spline_df4)

modelo_6_spline_df5 <- lm(phes_cal ~ varices_tri + ns(time_enf_cirrosis, df = 5), data = data_ehm)
summary(modelo_6_spline_df5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 2: Evaluación de supuestos para diferente grado de libertad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Linealidad
lmtest::resettest(modelo_6_spline_df3)
  # p=0.378 --> SÍ LINEALIDAD
lmtest::resettest(modelo_6_spline_df4)
  # p=0.3 --> SÍ LINEALIDAD
lmtest::resettest(modelo_6_spline_df5)
  # p=0.09704 --> SÍ LINEALIDAD

ggplot(data = data_ehm,
       aes(time_enf_cirrosis, modelo_6_spline_df3$residuals)) +
  geom_point() + geom_smooth (color = "firebrick") + geom_hline (yintercept = 0) + theme_bw ()
ggplot(data = data_ehm,
       aes(time_enf_cirrosis, modelo_6_spline_df4$residuals)) +
  geom_point() + geom_smooth (color = "firebrick") + geom_hline (yintercept = 0) + theme_bw ()
ggplot(data = data_ehm,
       aes(time_enf_cirrosis, modelo_6_spline_df5$residuals)) +
  geom_point() + geom_smooth (color = "firebrick") + geom_hline (yintercept = 0) + theme_bw ()


# Independencia de los errores
car::durbinWatsonTest(modelo_6_spline_df3)
  # p=0.246 --> SÍ INDEPENDENCIA
car::durbinWatsonTest(modelo_6_spline_df4)
  # p=0.092 --> SÍ INDEPENDENCIA
car::durbinWatsonTest(modelo_6_spline_df5)
  # p=0.324 --> SÍ INDEPENDENCIA

plot(residuals(modelo_6_spline_df3), pch=19, col="blue")
abline(h = 0, col = "red", lty = 2, lwd = 2)
plot(residuals(modelo_6_spline_df4), pch=19, col="blue")
abline(h = 0, col = "red", lty = 2, lwd = 2)
plot(residuals(modelo_6_spline_df5), pch=19, col="blue")
abline(h = 0, col = "red", lty = 2, lwd = 2)


# Normalidad de los errores
nortest::lillie.test(modelo_6_spline_df3$residuals)
  # p=0.6363 --> SÍ NORMALIDAD
nortest::lillie.test(modelo_6_spline_df4$residuals)
  # p=0.8138 --> SÍ NORMALIDAD
nortest::lillie.test(modelo_6_spline_df5$residuals)
  # p=0.5083 --> SÍ NORMALIDAD

car::qqPlot(modelo_6_spline_df3)
car::qqPlot(modelo_6_spline_df4)
car::qqPlot(modelo_6_spline_df5)


# Heterocedasticidad
lmtest::bgtest(modelo_6_spline_df3)
  # p=0.0882 --> SI HOMOCEDASTICIDAD
lmtest::bgtest(modelo_6_spline_df4)
  # p=0.03239 --> NO HOMOCEDASTICIDAD
lmtest::bgtest(modelo_6_spline_df5)
  # p=0.1201 --> SI HOMOCEDASTICIDAD

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 3: Evaluación de AIC y BIC para cada grado de libertad ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

AIC(modelo_6_spline_df3, modelo_6_spline_df4, modelo_6_spline_df5)
  # Mejor es el modelo con df=5
BIC(modelo_6_spline_df3, modelo_6_spline_df4, modelo_6_spline_df5)
  # Mejor es el modelo con df=3


## S(4.3) MODELO 9: ANÁLISIS DE SENSIBILIDAD (phes = várices + tiempo_enf) ----
  
  # COMENTARIO: El MODELO 6 (modelo de regresión lineal --> modelo_6_lm) se somete a una evaluación
  # de valores atípicos dado que su gráfico del supuesto de linealidad

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Paso 1: Detección de datos atípicos ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
car::influencePlot(modelo_6_lm)
  # Luego de observar el gráfico se concluye lo siguiente:
    # Punto 61:
      # Residuo estudientizado: -4.0558180 --> Es mayor a ±3, es un punto influyente
      # Leverange: 0.25198886 --> 0.25/(4/61) = 3.8125. Es un leverange casi muy alto por ser casi 4
      # Distancia de Cook: 1.08995533 --> Es mayor de 1, es un punto influyente
plot(modelo_6_lm, which = 4)
 # Se decide retirar el dato en posición 61

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 2: Modelo sin el dato atípico ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
modelo_6_lm_sin61 <- lm(phes_cal ~ varices_tri + time_enf_cirrosis, data = data_ehm[-61, ])
summary(modelo_6_lm_sin61)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Paso 3: Comparación modelo con y sin dato atípico ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
summary(modelo_6_lm)
summary(modelo_6_lm_sin61)

AIC(modelo_6_lm, modelo_6_lm_sin61)
BIC(modelo_6_lm, modelo_6_lm_sin61)



# a) Linealidad
lmtest::resettest(modelo_6_lm_sin61)
  # p=0.9573 --> LINEALIDAD


# b) Independencia de los errores
car::durbinWatsonTest(modelo_6_lm_sin61)
  # p=0.512 --> SÍ INDEPENDENCIA


# c) Normalidad de los errores
nortest::lillie.test(modelo_6_lm_sin61$residuals)
  # p=0.5349 --> SÍ NORMALIDAD


# d) Heterocedasticidad
lmtest::bgtest(modelo_6_lm_sin61)
  # p=0.3923 --> SI HOMOCEDASTICIDAD





