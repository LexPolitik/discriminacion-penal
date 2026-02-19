### Víctor Castellanos
##ENTORNO##

# Ruta del proyecto
ruta_proyecto <- "C:/Users/victor.castellanos/Desktop/transaparencia-primer-repo/discriminacion-penal"

# Crear carpetas
dir.create(file.path(ruta_proyecto, "1 scripts"), showWarnings = FALSE)
dir.create(file.path(ruta_proyecto, "2 visuals"), showWarnings = FALSE)
dir.create(file.path(ruta_proyecto, "3 reports"), showWarnings = FALSE)

# Mensaje de confirmación
cat("Carpetas creadas correctamente.\n")


#Instalar paquetes
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(dslabs)
library(boot)
library(car)
library(QuantPsyc)
library(ggplot2)

#Bajar bases


ENPOL2021_SOC <- read.csv("~/OneDrive/1. Docto/3 sem/Econometría II/Artículo econometría/Entorno/01_Bases/ENPOL2021_SOC.csv")
ENPOL2021_2_3 <- read.csv("~/OneDrive/1. Docto/3 sem/Econometría II/Artículo econometría/Entorno/01_Bases/ENPOL2021_2_3.csv", comment.char="#")
ENPOL2021_4 <- read.csv("~/OneDrive/1. Docto/3 sem/Econometría II/Artículo econometría/Entorno/01_Bases/ENPOL2021_4.csv", comment.char="#")
ENPOL2021_5 <- read.csv("~/OneDrive/1. Docto/3 sem/Econometría II/Artículo econometría/Entorno/01_Bases/ENPOL2021_5.csv", comment.char="#")
ENPOL2021_6 <- read.csv("~/OneDrive/1. Docto/3 sem/Econometría II/Artículo econometría/Entorno/01_Bases/ENPOL2021_6.csv", comment.char="#")
ENPOL2021_7 <- read.csv("~/OneDrive/1. Docto/3 sem/Econometría II/Artículo econometría/Entorno/01_Bases/ENPOL2021_7.csv", comment.char="#")
ENPOL2021_8_9_10_11 <- read.csv("~/OneDrive/1. Docto/3 sem/Econometría II/Artículo econometría/Entorno/01_Bases/ENPOL2021_8_9_10_11.csv", comment.char="#")


#Merge bases

tab <- left_join(ENPOL2021_SOC, ENPOL2021_2_3, by = c("ID_PER")) 
tab <- left_join(tab, ENPOL2021_4, by = c("ID_PER")) 
tab <- left_join(tab, ENPOL2021_5, by = c("ID_PER")) 
tab <- left_join(tab, ENPOL2021_6, by = c("ID_PER")) 
tab <- left_join(tab, ENPOL2021_7, by = c("ID_PER")) 
tab <- left_join(tab, ENPOL2021_8_9_10_11, by = c("ID_PER")) 

#Cuantas mujeres y hombres (1=49743 2=11706)

table(tab$SEXO)

table(tab$P1_3)

table(tab$P5_43)

#Explorar base tab

names(tab)
class(P5_4_A)

######AÑOS DE SENTENCIA P5_4_A######

#manejo de NA para años de sentencia 

is.na(tab$P5_4_A)

asentencia <- (tab$P5_4_A)

asentencia[asentencia == 00] <- NA
asentencia[asentencia == 98] <- NA
asentencia[asentencia == 99] <- NA
asentencia <- as.numeric(asentencia)

summary(asentencia)

######## Edad P1_3#########

# Verificar estructura y valores iniciales
is.na(tab$P1_3)       # Verificar valores faltantes
class(tab$P1_3)       # Confirmar el tipo de variable
summary(tab$P1_3)     # Resumen estadístico inicial
table(tab$P1_3)       # Frecuencias de la variable original

# Crear una copia de la variable edad y asegurar formato numérico
edad <- as.numeric(tab$P1_3)  # Convertir a numérico si no lo es
summary(edad)

# Manejo de valores 98 y 99 (asignarlos como NA)
edad[edad %in% c(98, 99)] <- NA

# Verificación después de recodificar
is.na(edad)           # Verificar NAs
summary(edad)         # Resumen después del ajuste
table(edad, useNA = "ifany")  # Frecuencias incluyendo NA

# Añadir la variable de edad limpia al dataframe original
tab$edad <- edad



##### SEXO P1_2 ####

# Verificar la clase y los valores únicos de la variable original

class(tab$SEXO)  # Verificar tipo de dato
table(tab$SEXO, useNA = "ifany")  # Frecuencias incluyendo NA

# Recodificar la variable SEXO en una nueva variable Genero
# Asignar 1 a hombres (SEXO == 1) y 0 a mujeres (u otros valores)
Genero <- ifelse(tab$SEXO == 1, 1, 0)

# Verificar la nueva variable Genero
table(Genero, useNA = "ifany")  # Tabla de frecuencias incluyendo NA


######Indígena P1_15#######
#########  si respondió ser indígena. No indígena si respondió no ser indígena, no saberlo, no respondió. 
# Verificar la clase y valores únicos
class(tab$P1_15)
table(tab$P1_15, useNA = "ifany")

# Recodificar: 1 si indígena (2), 0 para no indígena, no sabe y no responde (3, 8, 9)
indigena <- ifelse(tab$P1_15 == 2, 1,  # Se considera indígena
                   ifelse(tab$P1_15 %in% c(3, 8, 9), 0, NA))  # Resto como No indígena

# Verificar la variable recodificada
table(indigena, useNA = "ifany")  # Frecuencias de la nueva variable

# Añadir la variable al dataframe original
tab$indigena <- indigena



######### Afro P1_15 ###########

# Verificar la clase y los valores únicos
class(tab$P1_15)
table(tab$P1_15, useNA = "ifany")

# Recodificar: 1 si afrodescendiente (1), 0 para las demás respuestas (3, 8, 9)
afro <- ifelse(tab$P1_15 == 1, 1,    # Se considera afrodescendiente
               ifelse(tab$P1_15 %in% c(3, 8, 9), 0, NA))  # Resto como No afrodescendiente

# Verificar la variable recodificada
table(afro, useNA = "ifany")  # Tabla de frecuencias

# Añadir la variable al dataframe original
tab$afro <- afro

cor(tab$indigena, tab$afro, use = "complete.obs")

##############Orientación sexual LGB y hetero P1_23 ############

# Verificar valores originales de P1_23
table(tab$P1_23, useNA = "ifany")  # Frecuencias incluyendo valores faltantes

# Reemplazar valores no válidos (4, 8, 9) con NA en LGB
LGB <- tab$P1_23  # Crear una copia de la variable original
LGB[LGB %in% c(4, 8, 9)] <- NA  # Asignar NA a valores inválidos

# Recodificar para obtener una variable binaria de heterosexualidad
hetero <- ifelse(LGB == 3, 1, 0)  # 1: Heterosexual, 0: No heterosexual

# Verificar las dos variables
table(LGB, useNA = "ifany")  # Frecuencias de LGB
table(hetero, useNA = "ifany")  # Frecuencias de hetero

# Añadir al dataframe original
tab$LGB <- LGB
tab$hetero <- hetero




######################## Crear dataframe con las variables de delitos#########

TabDeli <- data.frame(
  Robveh = tab$P5_11_01, Robcasa = tab$P5_11_02, Robneg = tab$P5_11_03,
  Robtrans = tab$P5_11_04, Robtranseu = tab$P5_11_05, Robautop = tab$P5_11_06,
  Robotro = tab$P5_11_07, Posdrog = tab$P5_11_08, Comercdrog = tab$P5_11_09,
  Lesiones = tab$P5_11_10, Homiculp = tab$P5_11_11, Homidolo = tab$P5_11_12,
  Portarma = tab$P5_11_13, Obligfam = tab$P5_11_14, Violefam = tab$P5_11_15,
  Danoprop = tab$P5_11_16, Secuestro = tab$P5_11_17, Violasex = tab$P5_11_18,
  Fraude = tab$P5_11_19, Delincorga = tab$P5_11_20, Delitsexotro = tab$P5_11_21,
  Extorsion = tab$P5_11_22, Privdelib = tab$P5_11_23, Abusconf = tab$P5_11_24,
  Amenaza = tab$P5_11_25, Otro = tab$P5_11_26
)

# Reemplazar 0 por NA en todas las columnas de delitos
TabDeli[TabDeli == 0] <- NA

# Crear una nueva columna con la suma de todos los delitos, ignorando NA
TabDeli$TotalDelitos <- rowSums(TabDeli, na.rm = TRUE)

# Asegurarse de que las filas que tenían todos los delitos como NA tengan TotalDelitos como NA
TabDeli$TotalDelitos[rowSums(!is.na(TabDeli[, -ncol(TabDeli)])) == 0] <- NA

# Ver el resultado
summary(TabDeli$TotalDelitos)

# Crear una nueva columna con la suma de todos los delitos
TotalDelitos<- TabDeli$TotalDelitos

summary(TotalDelitos)

################# Variable sobre si tuvo asesoría antes de estar con un juez de control o antes de estar con el MP(P4_1_05)
##################

# Asesor MP P4_1_05 está en el dataframe 'datos'

DefensorMP <- tab$P4_1_05

# Reemplazar los valores 8, 9 y 'b' por NA
DefensorMP[DefensorMP %in% c(8, 9, 'b')] <- NA

# Convertir la variable: 1 para 'Sí' (1) y 0 para 'No' (2)
DefensorMP <- ifelse(DefensorMP == 1, 1,
                     ifelse(DefensorMP == 2, 0, NA))

# Verificar los cambios
summary(DefensorMP)

table(DefensorMP)


############ Asesoría Juez de control variable P5_1 está en el dataframe 'tab' #####

DefensorJuez <- tab$P5_1

# Reemplazar los valores 8, 9 y 'b' por NA
DefensorJuez[DefensorJuez %in% c(8, 9, 'b')] <- NA

# Convertir la variable: 1 para 'Sí' (1) y 0 para 'No' (2)
DefensorJuez <- ifelse(DefensorJuez == 1, 1,
                       ifelse(DefensorJuez == 2, 0, NA))

# Verificar los cambios
summary(DefensorJuez)

table(DefensorJuez)

table(tab$P5_1, useNA = "ifany")




# Crear una nueva variable indicando si tuvo asesoría en cualquier etapa inicial
DefensorInicial <- ifelse(DefensorMP == 1 | DefensorJuez == 1, 1, 0)






# Generar variables de defensor basado en la imagen

# Crear una nueva variable para identificar si tuvo defensor privado
defensor_privado <- ifelse(tab$P5_21_8 == 1 | tab$P5_21_9 == 1, NA,  # No sabe o no responde -> NA
                           ifelse(tab$P5_21_1 == 1, 1, 0))  # Privado: 1, No privado: 0

# Crear una nueva variable para identificar si tuvo defensor público
defensor_publico <- ifelse(tab$P5_21_8 == 1 | tab$P5_21_9 == 1, NA,  # No sabe o no responde -> NA
                           ifelse(tab$P5_21_2 == 1, 1, 0))  # Público: 1, No público: 0

# Crear una nueva variable para defensor mixto (ambos privado y público)
defensor_mixto <- ifelse(tab$P5_21_8 == 1 | tab$P5_21_9 == 1, NA,  # No sabe o no responde -> NA
                         ifelse(tab$P5_21_1 == 1 & tab$P5_21_2 == 1, 1, 0))  # Mixto: 1, No mixto: 0

# Verificar las nuevas variables
table(defensor_privado, useNA = "ifany")
table(defensor_publico, useNA = "ifany")
table(defensor_mixto, useNA = "ifany")

# Crear la variable sobre si le explicaron sus derechos (P4_1_04)
le_explicaron_derechos <- ifelse(tab$P4_1_04 == 1, 1,  # Sí
                                 ifelse(tab$P4_1_04 == 2, 0,  # No
                                        NA))                 # No sabe, No responde, Blanco

# Verificar la transformación
table(le_explicaron_derechos, useNA = "ifany")  # Tabla de frecuencias con NA


#Partes presentes durante el juicio
#Respuestas 1 (Siempre) y 2 (La mayoría de veces) → 1 (Sí, presente).
#Respuestas 3 (Pocas veces), 4 (Nunca), 6, 8, y 9 → 0 (No, ausente).
#Valores b → NA (Blancos).

# Variable: Abogado defensor presente (P5_16_1)
Defensor_presente <- ifelse(tab$P5_16_1 %in% c(1, 2), 1, 
                            ifelse(tab$P5_16_1 %in% c(3, 4, 6, 8, 9, 'b'), 0, NA))



# Variable: Juez presente (P5_16_2)
juez_presente <- ifelse(tab$P5_16_2 %in% c(1, 2), 1, 
                        ifelse(tab$P5_16_2 %in% c(3, 4, 8, 9, 'b'), 0, NA))



# Variable: Fiscal o MP presente (P5_16_3)
fiscal_presente <- ifelse(tab$P5_16_3 %in% c(1, 2), 1, 
                          ifelse(tab$P5_16_3 %in% c(3, 4, 8, 9, 'b'), 0, NA))

# Verificar las transformaciones
table(Defensor_presente, useNA = "ifany")
table(juez_presente, useNA = "ifany")
table(fiscal_presente, useNA = "ifany")



# Agregar las nuevas variables al nuevo data frame tab_transformado
tab_transformado <- data.frame(
  ID_PER = tab$ID_PER,              # Identificador único, si es necesario
  asentencia,                       # Años de sentencia
  edad,                             # Edad continua
  Genero,                           # Género (1: hombre, 0: mujer)
  indigena,                         # Indígena (1: sí, 0: no)
  afro,                             # Afrodescendiente (1: sí, 0: no)
  LGB,                              # Orientación sexual (1: LGB, 0 no LGB)
  TotalDelitos,                     # Total de delitos
  DefensorMP,                         # Asesoría por MP (1: sí, 0: no)
  DefensorJuez,                       # Asesoría por Juez de control (1: sí, 0: no)
  DefensorInicial,                    # Asesoría por Juez y MP
  defensor_privado,                 # Nuevo: Defensor privado
  defensor_publico,                 # Nuevo: Defensor público
  defensor_mixto,                    # Nuevo: Defensor mixto
  le_explicaron_derechos,            # Nueva variable: Le explicaron sus derechos
  Defensor_presente,  # Abogado presente
  juez_presente,     # Juez presente
  fiscal_presente    # Fiscal presente
)

# Verificar el nuevo data frame
str(tab_transformado)  # Estructura del data frame
summary(tab_transformado)  # Resumen estadístico


######Majeo grupos de edad#######

summary(tab_transformado$edad)
hist(tab_transformado$edad, breaks = 10, main = "Distribución de la Edad", xlab = "Edad")
# Crear grupos de edad
tab_transformado$grupo_edad <- cut(tab_transformado$edad,
                                   breaks = c(0, 24, 34, 44, 54, Inf),
                                   labels = c("18-24", "25-34", "35-44", "45-54", "55+"),
                                   right = TRUE)

# Verificar la nueva variable
table(tab_transformado$grupo_edad, useNA = "ifany")



# Seleccionar únicamente las variables utilizadas en el modelo y el ID_PER
vars_usadas <- tab_transformado[, c("ID_PER", "edad", "asentencia", "Genero", "grupo_edad", "LGB", 
                                    "indigena", "DefensorInicial", "defensor_publico", 
                                    "defensor_privado", "Defensor_presente", "juez_presente", "fiscal_presente", 
                                    "le_explicaron_derechos", "TotalDelitos")]

# Eliminar filas con valores faltantes (NA) en las variables seleccionadas
tab_final <- na.omit(vars_usadas)

# Verificar el resultado
str(tab_final)           # Estructura del nuevo data frame
summary(tab_final)       # Resumen estadístico
nrow(tab_final)          # Confirmar número de observaciones (34,211)







############################################################################
############################################################################
# Crear un resumen estadístico de cada variable en el data frame 'tab_final'
############################################################################
############################################################################

# Instalar y cargar la librería psych si no está instalada
if (!require(psych)) install.packages("psych")
library(psych)

# Seleccionar las variables de interés para la tabla
vars_descriptivas <- tab_final[, c("asentencia", "edad", "Genero", "grupo_edad", "indigena", "LGB", 
                                   "DefensorInicial", "defensor_publico", 
                                   "le_explicaron_derechos", "Defensor_presente", 
                                   "juez_presente", "fiscal_presente", "TotalDelitos")]

# Obtener estadística descriptiva
tabla_descriptiva <- describe(vars_descriptivas)

# Visualizar la tabla
print(tabla_descriptiva)







############################################################################
############################################################################
#Correlaciónes
############################################################################
############################################################################

# Seleccionar solo variables numéricas (continuas y binarias)
vars_pearson <- vars_descriptivas[, c("asentencia", "edad", "Genero", "indigena", "LGB", 
                                      "DefensorInicial", "defensor_publico", 
                                      "le_explicaron_derechos", "Defensor_presente", 
                                      "juez_presente", "TotalDelitos")]



# Matriz de correlación de Pearson
correlacion_pearson <- cor(vars_pearson, use = "pairwise.complete.obs")
print(correlacion_pearson)



# Instalar y cargar paquetes necesarios
install.packages(c("corrplot", "ggcorrplot", "pheatmap"), dependencies = TRUE)
library(corrplot)
library(ggcorrplot)
library(pheatmap)

# Seleccionar variables numéricas y binarias
vars_pearson <- vars_descriptivas[, c("asentencia", "edad", "Genero", "indigena", "LGB", 
                                      "DefensorInicial", "defensor_publico", 
                                      "le_explicaron_derechos", "Defensor_presente", 
                                      "juez_presente", "fiscal_presente", "TotalDelitos")]

# Calcular la matriz de correlación de Pearson
correlacion_pearson <- cor(vars_pearson, use = "pairwise.complete.obs")

# 1. Visualización con corrplot
corrplot(correlacion_pearson, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", number.cex = 0.7)

# 2. Visualización con ggcorrplot
ggcorrplot(correlacion_pearson, type = "upper", lab = TRUE, 
           lab_size = 3, colors = c("red", "white", "blue"), 
           title = "Matriz de Correlación Pearson", ggtheme = theme_minimal())

# 3. Visualización con pheatmap
pheatmap(correlacion_pearson, 
         display_numbers = TRUE, 
         color = colorRampPalette(c("red", "white", "blue"))(50), 
         main = "Matriz de Correlación Pearson")








# Cargar librerías necesarias
install.packages("dplyr")
install.packages("knitr")
install.packages("kableExtra")
install.packages("officer")

library(dplyr)
library(knitr)
library(kableExtra)
library(officer)

# Definir el directorio para guardar la tabla
wd.visuales <- paste0(wd.dir, "/04_Visuales")
if (!dir.exists(wd.visuales)) dir.create(wd.visuales, recursive = TRUE)

# Seleccionar variables categóricas
vars_categoricas <- tab_final[, c("Genero", "indigena", "LGB", "DefensorInicial", 
                                  "defensor_publico", "le_explicaron_derechos", 
                                  "Defensor_presente", "juez_presente", "fiscal_presente", "grupo_edad")]

# Crear una función para calcular el Chi-cuadrado
calcular_chi <- function(var1, var2) {
  tabla <- table(var1, var2)
  test <- chisq.test(tabla, correct = TRUE)
  return(c(chi2 = test$statistic, df = test$parameter, p.value = test$p.value))
}

# Lista de pares de variables para el test
resultados_chi <- list(
  "Genero y Sentencia" = calcular_chi(tab_final$Genero, tab_final$asentencia > 15),
  "Indígena y Sentencia" = calcular_chi(tab_final$indigena, tab_final$asentencia > 15),
  "LGB y Sentencia" = calcular_chi(tab_final$LGB, tab_final$asentencia > 15),
  "Defensor Inicial y Sentencia" = calcular_chi(tab_final$DefensorInicial, tab_final$asentencia > 15),
  "No explicar derechos y Sentencia" = calcular_chi(tab_final$le_explicaron_derechos, tab_final$asentencia > 15),
  "Grupo Edad y Sentencia" = calcular_chi(tab_final$grupo_edad, tab_final$asentencia > 15)
)

# Convertir resultados a data.frame
tabla_resultados <- do.call(rbind, lapply(names(resultados_chi), function(x) {
  data.frame(Variables = x, 
             Chi_cuadrado = resultados_chi[[x]][1], 
             Grados_libertad = resultados_chi[[x]][2], 
             P_valor = resultados_chi[[x]][3])
}))

# Redondear valores
tabla_resultados <- tabla_resultados %>% 
  mutate(Chi_cuadrado = round(Chi_cuadrado, 4), 
         P_valor = ifelse(P_valor < 0.01, "<0.01", round(P_valor, 4)))

# Generar la tabla en formato kable
tabla_kable <- kable(tabla_resultados, format = "html", 
                     caption = "Correlaciones chi-cuadrado") %>%
  kable_styling(full_width = FALSE, position = "center", bootstrap_options = c("striped", "hover", "condensed"))

# Guardar la tabla como imagen temporal
temp_file <- tempfile(fileext = ".png")
save_kable(tabla_kable, file = temp_file)

# Exportar la tabla a Word
doc <- read_docx() %>%
  body_add_par("Correlaciones chi-cuadrado", style = "heading 1") %>%
  body_add_img(src = temp_file, width = 6, height = 4)

# Guardar el documento Word
output_file <- paste0(wd.visuales, "/correlaciones_chi_cuadrado.docx")
print(doc, target = output_file)

# Confirmación
cat("La tabla ha sido guardada exitosamente en:", output_file)




############################################################################
############################################################################
Regresiones lineales con ee robustos
############################################################################
############################################################################


# Cargar paquetes necesarios
library(sandwich)  # Para calcular errores estándar robustos
library(lmtest)    # Para realizar pruebas con errores robustos

# Modelo para hombres con errores estándar robustos
modelo_hombres <- lm(asentencia ~ grupo_edad + LGB + indigena + 
                       DefensorInicial + defensor_publico + Defensor_presente +
                       juez_presente + le_explicaron_derechos + TotalDelitos, 
                     data = subset(tab_final, Genero == 1))
# Resumen del modelo
summary(modelo_hombres)

# Resumen del modelo con errores robustos
coeftest(modelo_hombres, vcov = vcovHC(modelo_hombres, type = "HC1"))

# Modelo para mujeres con errores estándar robustos
modelo_mujeres <- lm(asentencia ~ grupo_edad + LGB + indigena + 
                       DefensorInicial + defensor_publico + Defensor_presente +
                       juez_presente + le_explicaron_derechos + TotalDelitos, 
                     data = subset(tab_final, Genero == 0))
# Resumen del modelo
summary(modelo_mujeres)

# Resumen del modelo con errores robustos
coeftest(modelo_mujeres, vcov = vcovHC(modelo_mujeres, type = "HC1"))

# Modelo mixto con errores estándar robustos
modelo_mixto <-  lm(asentencia ~ Genero + grupo_edad + LGB + indigena + 
                      DefensorInicial + defensor_publico + Defensor_presente +
                      juez_presente + le_explicaron_derechos + TotalDelitos, 
                    data = tab_final)

# Resumen del modelo
summary(modelo_mixto)

# Resumen del modelo con errores robustos
coeftest(modelo_mixto, vcov = vcovHC(modelo_mixto, type = "HC1"))




#########################################
##############################################
###################################################
Pruebas supuestos
###################################################
##############################################
#########################################

#########1. VIF

# Cargar el paquete necesario
library(car)  # Para calcular el VIF

# Calcular VIF para el modelo de hombres
vif_hombres <- vif(modelo_hombres)
print("VIF para el modelo de hombres:")
print(vif_hombres)

# Calcular VIF para el modelo de mujeres
vif_mujeres <- vif(modelo_mujeres)
print("VIF para el modelo de mujeres:")
print(vif_mujeres)

# Calcular VIF para el modelo mixto
vif_mixto <- vif(modelo_mixto)
print("VIF para el modelo mixto:")
print(vif_mixto)




###### 2. Prueba Breusch-Pagan heteroscedasticiadad


# Cargar el paquete necesario
library(lmtest)

# Prueba Breusch-Pagan para el modelo de hombres
bp_hombres <- bptest(modelo_hombres)
print("Prueba Breusch-Pagan para el modelo de hombres:")
print(bp_hombres)

# Prueba Breusch-Pagan para el modelo de mujeres
bp_mujeres <- bptest(modelo_mujeres)
print("Prueba Breusch-Pagan para el modelo de mujeres:")
print(bp_mujeres)

# Prueba Breusch-Pagan para el modelo mixto
bp_mixto <- bptest(modelo_mixto)
print("Prueba Breusch-Pagan para el modelo mixto:")
print(bp_mixto)



##################################
##### 3. Prueba de Durbin-Watson

# Cargar el paquete necesario
library(lmtest)

# Prueba Durbin-Watson para el modelo de hombres
dw_hombres <- dwtest(modelo_hombres)
print("Prueba Durbin-Watson para el modelo de hombres:")
print(dw_hombres)

# Prueba Durbin-Watson para el modelo de mujeres
dw_mujeres <- dwtest(modelo_mujeres)
print("Prueba Durbin-Watson para el modelo de mujeres:")
print(dw_mujeres)

# Prueba Durbin-Watson para el modelo mixto
dw_mixto <- dwtest(modelo_mixto)
print("Prueba Durbin-Watson para el modelo mixto:")
print(dw_mixto)




##################################
##### 4. Prueba de correcta especificación, estricta exogeneidad 

# Cargar el paquete necesario
library(lmtest)

# Prueba RESET para el modelo de hombres
reset_hombres <- resettest(modelo_hombres, power = 2:3, type = "fitted")
print("Prueba RESET para el modelo de hombres:")
print(reset_hombres)

# Prueba RESET para el modelo de mujeres
reset_mujeres <- resettest(modelo_mujeres, power = 2:3, type = "fitted")
print("Prueba RESET para el modelo de mujeres:")
print(reset_mujeres)

# Prueba RESET para el modelo mixto
reset_mixto <- resettest(modelo_mixto, power = 2:3, type = "fitted")
print("Prueba RESET para el modelo mixto:")
print(reset_mixto)


##################################
##### 5.	Prueba de normalidad Kolmogorov-Smirnov

# Cargar paquete necesario
library(stats)

# Obtener residuos estandarizados de los modelos
residuos_hombres <- rstandard(modelo_hombres)
residuos_mujeres <- rstandard(modelo_mujeres)
residuos_mixto <- rstandard(modelo_mixto)

# Prueba Kolmogorov-Smirnov para el modelo de hombres
ks_hombres <- ks.test(residuos_hombres, "pnorm", mean = mean(residuos_hombres), sd = sd(residuos_hombres))
print("Prueba Kolmogorov-Smirnov para el modelo de hombres:")
print(ks_hombres)

# Prueba Kolmogorov-Smirnov para el modelo de mujeres
ks_mujeres <- ks.test(residuos_mujeres, "pnorm", mean = mean(residuos_mujeres), sd = sd(residuos_mujeres))
print("Prueba Kolmogorov-Smirnov para el modelo de mujeres:")
print(ks_mujeres)

# Prueba Kolmogorov-Smirnov para el modelo mixto
ks_mixto <- ks.test(residuos_mixto, "pnorm", mean = mean(residuos_mixto), sd = sd(residuos_mixto))
print("Prueba Kolmogorov-Smirnov para el modelo mixto:")
print(ks_mixto)


######
####
########







modelo_opt <- step(lm(asentencia ~ grupo_edad * Genero + LGB * Genero + indigena * Genero + 
                        DefensorInicial * Genero + defensor_publico * Genero + defensor_privado * Genero + 
                        Defensor_presente * Genero + juez_presente * Genero + 
                        le_explicaron_derechos * Genero + TotalDelitos * Genero, 
                      data = tab_transformado))
summary(modelo_opt)

library(ggplot2)
ggplot(tab_transformado, aes(x = grupo_edad, y = asentencia, color = as.factor(Genero))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "Interacción entre grupo de edad y género",
       x = "Grupo de edad", y = "Años de sentencia", color = "Género") +
  theme_minimal()



library(ggplot2)
ggplot(tab_transformado, aes(x = grupo_edad, y = asentencia, color = as.factor(Genero))) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.5), size = 3) +
  stat_summary(fun = mean, geom = "line", aes(group = Genero), position = position_dodge(width = 0.5)) +
  labs(title = "Promedio de años de sentencia por grupo de edad y género",
       x = "Grupo de edad", y = "Años de sentencia", color = "Género") +
  theme_minimal()






