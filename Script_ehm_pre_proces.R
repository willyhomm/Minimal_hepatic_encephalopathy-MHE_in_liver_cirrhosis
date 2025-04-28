
# 1. Subida de bases de datos y exploración de variables ----
data_ehm <- BASE_DE_DATOS
colnames(data_ehm) # Hay variables que están de más y deben ser retiradas.Es
  # necesario conocer la base y el contexto para proceder con el retiro
#data_ehm$id <- 1:nrow(data_ehm) # Se decide crear una variable "id"



# 2. Modificaciones realizadas por indicación del clínico ----

# NOTA: En la base de datos figura "asci" como 0. Se debe cambiar a 1. Esto debido
# a que un puntaje de 1 indica ascitis leve.

table(data_ehm$asci)
data_ehm$asci <- ifelse(data_ehm$asci==0, 1, data_ehm$asci)
table(data_ehm$asci)



# 3. Evaluación de datos perdidos ----
per.miss.col <- (colSums(is.na(data_ehm))/dim(data_ehm)[1]) * 100
per.miss.col
  # No hay datos perdidos en alguna columna



# 4. Evaluación de atípicos ----

## 4.1. En variables numéricas ----

### 4.1.1. Data con variables numéricas ----

#### a) Creación de dataframe numérico ----
library(dplyr)
data_ehm_num <- data_ehm %>% select(edad,
                                    time_enf_cirrosis,
                                    anios_instr,
                                    inr,
                                    alb,
                                    crea,
                                    bt,
                                    na,
                                    dst_1,
                                    dst_2,
                                    dst_fin,
                                    nctA_1,
                                    nctA_2,
                                    nctA_fin,
                                    nctB_1,
                                    nctB_2,
                                    nctB_fin,
                                    sdt_1,
                                    sdt_2,
                                    sdt_fin,
                                    ldt_1,
                                    ldt_2,
                                    ldt_fin
                                    )

#### b) Descripción el dataframe numérico ----
summarytools::descr(data_ehm_num)
  # Mínimo y máximo de edad está bien; sin embargo es raro que pacientes jóvenes tengan cirrosis. Se
    # revisarán la consistencia del valor mínimo.
  # Mínimo y máximo de time_enf_cirrosis está bien; sin embargo, es raro que hayan pacientes con más
    # de 48 meses que no hayan presentando síntomas de encefalopatía. Se revisará la consistencia de
    # todos aquellos que tengan tiempo de enfermedad mayor a 48 meses.


### 4.1.2. Box plot de cada variable numéricas ----

par(mfrow=c(3, 3))

boxplot(data_ehm_num[, "edad"],
        col=c("antiquewhite"),
        main="Edad (años)") # Revisar outlier

boxplot(data_ehm_num[, "time_enf_cirrosis"],
        col=c("azure"),
        main="Tiempo de enfermedad desde el dx. de cirrosis (meses)") # Revisar outlier

boxplot(data_ehm_num[, "anios_instr"],
        col=c("aquamarine"),
        main="Años de estudio (años)")

boxplot(data_ehm_num[, "inr"],
        col=c("bisque"),
        main="INR") # Revisar outlier

boxplot(data_ehm_num[, "alb"],
        col=c("blue"),
        main="Albúmina") # Revisar outlier

boxplot(data_ehm_num[, "crea"],
        col=c("brown"),
        main="Creatinina") # Revisar outlier

boxplot(data_ehm_num[, "bt"],
        col=c("burlywood"),
        main="Bilirrubina") # Revisar outlier

boxplot(data_ehm_num[, "na"],
        col=c("cadetblue"),
        main="Sodio sérico")

par(mfrow=c(1, 1))


par(mfrow=c(3, 3))

boxplot(data_ehm_num[, "dst_1"],
        col=c("coral"),
        main="Prueba de números y símbolos (DST_manual)")

boxplot(data_ehm_num[, "dst_fin"],
        col=c("coral"),
        main="Prueba de números y símbolos (DST)")

boxplot(data_ehm_num[, "nctA_1"],
        col=c("cornsilk"),
        main="Prueba de secuencias de números (NCT-A_manual)") # Revisar outlier

boxplot(data_ehm_num[, "nctA_fin"],
        col=c("cornsilk"),
        main="Prueba de secuencias de números (NCT-A)")

boxplot(data_ehm_num[, "nctB_1"],
        col=c("cyan"),
        main="Prueba de secuencias de números y letra (NCT-B_manual)") # Revisar outlier

boxplot(data_ehm_num[, "nctB_fin"],
        col=c("cyan"),
        main="Prueba de secuencias de números y letra (NCT-B)")

boxplot(data_ehm_num[, "sdt_1"],
        col=c("darkgoldenrod"),
        main="Prueba de serie de puntos (SDT_manual)") # Revisar outlier

boxplot(data_ehm_num[, "sdt_fin"],
        col=c("darkgoldenrod"),
        main="Prueba de serie de puntos (SDT)")

par(mfrow=c(1, 1))


par(mfrow=c(1, 2))

boxplot(data_ehm_num[, "ldt_1"],
        col=c("darkolivegreen"),
        main="Prueba de trazado de línea (LDT_manual)") # Revisar outlier

boxplot(data_ehm_num[, "ldt_fin"],
        col=c("darkolivegreen"),
        main="Prueba de trazado de línea (LDT)")

par(mfrow=c(1, 1))


### 4.1.3. Conclusión de la evaluación de atípicos numéricos ----

# La variables con datos atípicos a revisar son:
  # edad
  # time_enf_cirrosis
  # inr
  # alb
  # crea
  # bt
  # nctA_1
  # nctB_1
  # sdt_1
  # ldt_1


## 4.2. En variables categóricas ----

### 4.2.1. Data con variables categóricas ----

#### a) Creación de dataframe categórico ----
excluir <- colnames(data_ehm_num)
data_ehm_cat <- data_ehm %>% select(-all_of(excluir))
data_ehm_cat$child <- NULL
data_ehm_cat$meld <- NULL
data_ehm_cat$meld_na <- NULL
data_ehm_cat$phes_cal <- NULL


#### b) Descripción del dataframe categórico ----
summary(data_ehm_cat)
  # las variables anio, endosco_diges_vvee, west_haven y asci se transforman a character
data_ehm_cat$anio               <- as.character(data_ehm_cat$anio)
data_ehm_cat$endosco_diges_vvee <- as.character(data_ehm_cat$endosco_diges_vvee)
data_ehm_cat$west_haven         <- as.character(data_ehm_cat$west_haven)
data_ehm_cat$asci               <- as.character(data_ehm_cat$asci)
summary(data_ehm_cat)

# *** Variable año (anio) ***
table(data_ehm_cat$anio)
round(prop.table(table(data_ehm_cat$anio))*100, 2) # categorías de años son plausible

# *** Variable ocupación (ocupacion) ***
table(data_ehm_cat$ocupacion)
round(prop.table(table(data_ehm_cat$ocupacion))*100, 2)  # categorías de ocupación son plausibles

# *** Variable sexo (sexo) ***
table(data_ehm_cat$sexo)
round(prop.table(table(data_ehm_cat$sexo))*100, 2) # categorías de sexo son plausibles

# *** Variable etiología (etiologia) ***
table(data_ehm_cat$etiologia)
round(prop.table(table(data_ehm_cat$etiologia))*100, 2)
  # OVERLAP es CBP + HAI, se debe cambiar a CBP + HAI
  # VHC es lo mismo que HVC, se cambia todo a HVC
  # Se debe cambiar OH + MAFLD a MAFLD + OH
  # Se debe cambiar MAFLD + VHC a MAFLD + HCV
  # Según especialista, se debe crear la variable de doble etiología donde se incluya MAFLD + OH y
    # MAFLD + HCV
  # Según especialista, HAI, CBP y CBP + HAI pueden agruparse como enfermedades autoinmunes

# *** Variable comorbilidad 1 (comorb_1) ***
table(data_ehm_cat$comorb_1)
table(data_ehm_cat$comorb_1[data_ehm_cat$comorb_1 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$comorb_1))*100, 2)
round(prop.table(table(data_ehm_cat$comorb_1[data_ehm_cat$comorb_1 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es DM2.

# *** Variable comorbilidad 2 (comorb_2) ***
table(data_ehm_cat$comorb_2)
table(data_ehm_cat$comorb_2[data_ehm_cat$comorb_2 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$comorb_2))*100, 2)
round(prop.table(table(data_ehm_cat$comorb_2[data_ehm_cat$comorb_2 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es HTA.

# *** Variable medicación 1 (medica_1) ***
table(data_ehm_cat$medica_1)
table(data_ehm_cat$medica_1[data_ehm_cat$medica_1 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$medica_1))*100, 2)
round(prop.table(table(data_ehm_cat$medica_1[data_ehm_cat$medica_1 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es PROPRANOLOL.

# *** Variable medicación 2 (medica_2) ***
table(data_ehm_cat$medica_2)
table(data_ehm_cat$medica_2[data_ehm_cat$medica_2 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$medica_2))*100, 2)
round(prop.table(table(data_ehm_cat$medica_2[data_ehm_cat$medica_2 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es PROPRANOLOL y AC FOLICO

# *** Variable medicación 3 (medica_3) ***
table(data_ehm_cat$medica_3)
table(data_ehm_cat$medica_3[data_ehm_cat$medica_3 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$medica_3))*100, 2)
round(prop.table(table(data_ehm_cat$medica_3[data_ehm_cat$medica_3 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es AC FOLICO

# *** Variable medicación 4 (medica_4) ***
table(data_ehm_cat$medica_4)
table(data_ehm_cat$medica_4[data_ehm_cat$medica_4 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$medica_4))*100, 2)
round(prop.table(table(data_ehm_cat$medica_4[data_ehm_cat$medica_4 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es PREDNISONA

# *** Variable medicación 5 (medica_5) ***
table(data_ehm_cat$medica_5)
table(data_ehm_cat$medica_5[data_ehm_cat$medica_5 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$medica_5))*100, 2)
round(prop.table(table(data_ehm_cat$medica_5[data_ehm_cat$medica_5 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es TIAMINA

# *** Variable medicación 6 (medica_6) ***
table(data_ehm_cat$medica_6)
table(data_ehm_cat$medica_6[data_ehm_cat$medica_6 != "no reporta"]) # excluyendo los "no reporta"
round(prop.table(table(data_ehm_cat$medica_6))*100, 2)
round(prop.table(table(data_ehm_cat$medica_6[data_ehm_cat$medica_6 != "no reporta"]))*100, 2)
  # excluyendo los "no reporta", la categoría más frecuente es AC FOLICO

# *** Variable tipo dé várice esofágica (endosco_diges_vvee) ***
table(data_ehm_cat$endosco_diges_vvee)
prop.table(table(data_ehm_cat$endosco_diges_vvee))*100
  # categorías de sexo son plausibles; sin embargo, se debe crear una variable etiquetando
  # cada categoría.

# *** Variable presencia de várices esofágicas (vvee_cal) ***
table(data_ehm_cat$vvee_cal)
round(prop.table(table(data_ehm_cat$vvee_cal))*100, 2) # categorías de presencia de várices
  # esofágicas son plausibles.

# *** Variable encefalopatía (west_haven) ***
table(data_ehm_cat$west_haven)
round(prop.table(table(data_ehm_cat$west_haven))*100, 2) # categorías de encefalopatía son plausibles;
  # sin embargo, se debe crear una variable etiquetando cada categoría.

# *** Variable ascitis (asci) ***
table(data_ehm_cat$asci)
round(prop.table(table(data_ehm_cat$asci))*100, 2) # categorías de ascitis son plausibles; sin embargo,
  # se debe crear una variable etiquetando cada categoría.

# *** Variable diagnóstico final de ehm (dx_final_cal) ***
table(data_ehm_cat$dx_final_cal)
round(prop.table(table(data_ehm_cat$dx_final_cal))*100, 2) # categorías de diagnótico final de ehm
  # son plausibles


### 4.2.2. Conclusión de la evaluación de atípicos categóricos ----

# No hay datos atípicos categóricos; sin embargo se revisará lo siguiente:
  # etiologia
  # endosco_diges_vvee
  # west_haven
  # asci



# 5. Modificación de variables posterior a conversación con clínico ----

# NOTA 5.1: ----
# Producto de visualizado en el punto 4.1.2, se procede a extraer el id de los pacientes
# con outliers atípicos en las variables descritas en 4.1.3.

subset(data_ehm, edad<31, select = c(id, edad)) # id=7(27).
subset(data_ehm, time_enf_cirrosis>=48, select = c(id, time_enf_cirrosis)) # id=9(60), 27(60), 53(96), 59(60), 61(96).
subset(data_ehm, inr>1.8, select = c(id, inr)) # id=40(2.14), 43(2.2).
subset(data_ehm, alb<3.3, select = c(id, alb)) # id=13(2.62), 34(3.26), 47(3.14), 59(3).
subset(data_ehm, crea>1.2, select = c(id, crea)) # id=7(1.4).
subset(data_ehm, bt>3.2, select = c(id, bt)) # id=9(5.01), 33(3.67), 43(3.95), 45(4.25).
subset(data_ehm, nctA_1>120, select = c(id, nctA_1)) # id=14(129).
subset(data_ehm, nctB_1>260, select = c(id, nctB_1)) # id=4(291), 36(278), 48(295), 49(276).
subset(data_ehm, sdt_1>180, select = c(id, sdt_1)) # id=52(220), 61(210).
subset(data_ehm, ldt_1>180, select = c(id, ldt_1)) # id=15(204), 61(205).

# RESPUESTA A NOTA 5.1: ----
# Especialista corroboró que todos los datos son correctos


# NOTA 5.2: ----
# Producto de lo observado y descrito en el punto 4.2.1, se procede a realizar modificaciones
# en las variabes descritas en 4.2.2.

# *** Variable etiología ***
table(data_ehm$etiologia)
data_ehm$etiologia <- ifelse(data_ehm$etiologia == "OVERLAP", "CBP + HAI", data_ehm$etiologia)
table(data_ehm$etiologia)
data_ehm$etiologia <- ifelse(data_ehm$etiologia == "VHC", "HVC", data_ehm$etiologia)
table(data_ehm$etiologia)
data_ehm$etiologia <- ifelse(data_ehm$etiologia == "OH + MAFLD", "MAFLD + OH", data_ehm$etiologia)
table(data_ehm$etiologia)
data_ehm$etiologia <- ifelse(data_ehm$etiologia == "MAFLD + VHC", "MAFLD + HVC", data_ehm$etiologia)
table(data_ehm$etiologia)

# *** Variable endosco_diges_vvee ***
table(data_ehm$endosco_diges_vvee)

data_ehm$endosco_diges_vvee_label <- factor(data_ehm$endosco_diges_vvee,
                                            levels = c(0, 1, 2, 3),
                                            labels = c("0. no hay várices",
                                                       "1. várices pequeñas",
                                                       "2. várices no más del 50 % del lumen",
                                                       "3. várices grandes")
                                            )
table(data_ehm$endosco_diges_vvee_label)
round(prop.table(table(data_ehm$endosco_diges_vvee_label))*100, 2)
data_ehm <- data_ehm %>% dplyr::relocate(endosco_diges_vvee_label, .after = endosco_diges_vvee)
data_ehm <- data_ehm %>% dplyr::relocate(vvee_cal, .after = endosco_diges_vvee_label)
colnames(data_ehm)

# *** Variable west_haven ***
table(data_ehm$west_haven)
data_ehm$encefa_label <- factor(data_ehm$west_haven,
                                levels = c(0),
                                labels = c("ausente - I")
                                )
table(data_ehm$encefa_label)
round(prop.table(table(data_ehm$encefa_label))*100, 2)
data_ehm <- data_ehm %>% dplyr::relocate(encefa_label, .after = west_haven)

# *** Variable asci ***
table(data_ehm$asci)

data_ehm$asci_label <- factor(data_ehm$asci,
                              levels = c(1, 2, 3),
                              labels = c("ascitis leve",
                                         "ascitis moderada",
                                         "ascitis severa")
                              )
table(data_ehm$asci_label)
round(prop.table(table(data_ehm$asci_label))*100, 2)
data_ehm <- data_ehm %>% dplyr::relocate(asci_label, .after = asci)

# RESPUESTA A NOTA 5.2: ----
# Se realizó las modificaciones de manera satisfactoria



# 6. Dependencia de variables ----

# NOTA: Esta dependencia de variables se realiza posterior a la corección de otliers

colnames(data_ehm)
## 6.1. La variable "child" depende de: ----
  # bilirrubina total (bt)    --> <2 : 1pto.    2-3 : 2ptos.      >3 : 3ptos.
  # albúmina (alb)          --> >3.5 : 1pto   2.8-3.5 : 2ptos.  <2.8 : 3ptos.
  # INR (inr)                 --> <1.7 : 1pto.  1.7-2.3 : 2ptos.  >2.3 : 3ptos.
  # encefalopatía (west_haven) --> 0 (ausente - I) : 1pto.     1 (grado II) : 2ptos.  2 (grado III) : 3ptos.
  # ascitis (asci)            --> leve : 1pto.  moderada : 2ptos.   severa : 3ptos.
# Child A: 5-6ptos. --> Descompensación leve
# Child B: 7-9ptos. --> Descmpensación moderada
# Child C: 10-15ptos. --> Descompensación severa

# Creación del puntaje de bt
data_ehm$bt_pto <- ifelse(data_ehm$bt < 2, 1, ifelse((data_ehm$bt >= 2 & data_ehm$bt <= 3), 2, 3))
data_ehm <- data_ehm %>% dplyr::relocate(bt_pto, .after = bt)

# Creación del puntaje de alb
data_ehm$alb_pto <- ifelse(data_ehm$alb > 3.5, 1, ifelse((data_ehm$alb >= 2.8 & data_ehm$alb <= 3.5), 2, 3))
data_ehm <- data_ehm %>% dplyr::relocate(alb_pto, .after = alb)

# Creación del puntaje de inr
data_ehm$inr_pto <- ifelse(data_ehm$inr < 1.7, 1, ifelse((data_ehm$inr >= 1.7 & data_ehm$inr <= 2.3), 2, 3))
data_ehm <- data_ehm %>% dplyr::relocate(inr_pto, .after = inr)

# Creación del puntaje de west_haven
data_ehm$encefa_pto <- ifelse(data_ehm$encefa_label == "ausente - I", 1,
                              )
data_ehm <- data_ehm %>% dplyr::relocate(encefa_pto, .after = west_haven)

# Creación del puntaje de asci
data_ehm$asci_pto <- ifelse(data_ehm$asci_label == "ascitis leve",
                            1,
                            ifelse(data_ehm$asci_label == "ascitis moderada", 2, 3)
                            )
data_ehm <- data_ehm %>% dplyr::relocate(asci_pto, .after = asci)

# Creación del puntaje child
data_ehm$child_suma <- data_ehm$bt_pto + data_ehm$alb_pto + data_ehm$inr_pto + data_ehm$encefa_pto + data_ehm$asci_pto
data_ehm <- data_ehm %>% dplyr::relocate(child_suma, .after = child)

subset(data_ehm, child != child_suma, select = c(id,
                                                 bt,
                                                 alb,
                                                 inr,
                                                 encefa_label,
                                                 asci_label,
                                                 child,
                                                 child_suma)
       ) # listado de pacientes con puntuje de child distinto al cálculado por el especialista

# NOTA: Se revisó los puntajes y lo correcto es lo obtenido en la variable "child_suma"

# Creación de la categoría Child
data_ehm$child_cat <- ifelse((data_ehm$child_suma >= 5 & data_ehm$child_suma <= 6),
                             "CHILD A",
                             ifelse((data_ehm$child_suma >= 7 & data_ehm$child_suma <=9),
                                    "CHILD B",
                                    "CHILD C")
                             )
data_ehm <- data_ehm %>% dplyr::relocate(child_cat, .after = child_suma)
table(data_ehm$child_cat)
round(prop.table(table(data_ehm$child_cat))*100, 2)


## 6.2. La variable "meld" depende de: ----
  # creat
  # bt
  # inr
# Se calcula con la fórmula de Kamath PS, et al. (2001)


# Creación de la variable MELD (meld_cal)
library(transplantr)
data_ehm$meld_cal <- transplantr::meld_US(INR = data_ehm$inr,
                                          bili = data_ehm$bt,
                                          creat = data_ehm$crea,
                                          dialysis = 0
                                          )
data_ehm$meld_cal_deci <- data_ehm$meld_cal
data_ehm <- data_ehm %>% dplyr::relocate(meld_cal, .after = meld)
data_ehm <- data_ehm %>% dplyr::relocate(meld_cal_deci, .after = meld_cal)

data_ehm$meld_cal <- round(data_ehm$meld_cal, 0)

subset(data_ehm, meld != meld_cal, select = c(id,
                                              crea,
                                              bt,
                                              inr,
                                              meld,
                                              meld_cal)
       ) # listado de pacientes con puntaje de meld distinto al cálculado por el especialista

# NOTA: Se revisó los puntajes y se le hará caso al puntaje obtenido por la función meld_US()
  # Se agrega que los pacientes con id 28 y 45 se obtuvieron resultados meld de 11 y 17, respectivamente
  # usando la plataforma:
    # https://www.mdcalc.com/calc/2693/meld-score-original-pre-2016-model-end-stage-liver-disease#evidence


## 6.3. La variable "meld_na" depende de: ----
  # creat
  # bt
  # inr
  # na
# Se calcula con la fórmula de Kim WR, et al. (2008)


# Creación de la variable MELD NA (meldNA_cal)
# MELD-Na = MELD - Na - [0.025 × MELD × (140-Na)] + 140
data_ehm$meldNA_cal <- data_ehm$meld_cal_deci - data_ehm$na - (0.025*data_ehm$meld_cal_deci*(140-data_ehm$na)) + 140
data_ehm$meldNA_cal_deci <- data_ehm$meldNA_cal
data_ehm <- data_ehm %>% dplyr::relocate(meldNA_cal, .after = meld_na)
data_ehm <- data_ehm %>% dplyr::relocate(meldNA_cal_deci, .after = meldNA_cal)

data_ehm$meldNA_cal <- round(data_ehm$meldNA_cal, 0)

print(subset(data_ehm, meld_na != meldNA_cal, select = c(id,
                                                         crea,
                                                         bt,
                                                         inr,
                                                         meld,
                                                         meld_cal,
                                                         meld_cal_deci,
                                                         na,
                                                         meld_na,
                                                         meldNA_cal,
                                                         meldNA_cal_deci)
             ), n=18) # listado de pacientes con puntaje de meld_na distinto al cálculado por el especialista

# PROBLEMAS:
  # id=7 --> en web sale 17 y con formula sale 13
  # id=23 --> en web sale 7 y con fórmula sale 5
  # id=44 --> en web sale 10 y con fórmula sale 8
  # id=45 --> en la web sale 20 y con fórmula sale 18
  # id=49 --> en la web sale 8 y con fórmula sale 5


## 6.4 La variable "phes" depende de: ----
  # dst
  # nct_a
  # nct_b
  # sdt
  # ldt
# Esta dependencia se corrigió cuando se manipuló el excel al inicio

table(data_ehm$phes_cal)

## 6.5. La variable "dx_final" depende de: ----
  # phes: <-4: Sí EHM  >= -4: No EHM
# Esta dependencia se corrigió cuando se manipuló el excel al inicio

table(data_ehm$dx_final_cal)

# 7. EXPORTAR EXCEL ----

xlsx::write.xlsx(data_ehm, "data_ehm.xlsx")

