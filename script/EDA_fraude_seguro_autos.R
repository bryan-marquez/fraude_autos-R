#### Análisis de Datos de Fraude en Seguros de Auto

### Análisis Exploratorio de Datos (EDA)

## carga de Datos

# Cargamos los datos
archivo <- "/cloud/project/data/Dataset.csv"
car_fraud <- read.csv(archivo)
head(car_fraud)

# Mostramos el número de filas y columnas
dim(car_fraud)

# Mostramos los tipos de datos
sapply(car_fraud, class)

## Estadística Descriptiva

# Resumimos las variables numéricas y categóricas
summary(car_fraud)

# Obtenemos la desviación estándar
sapply(car_fraud, sd)

# Calculamos el sesgo o asimietría de las variables numéricas NO categóricas
# Inclinación positiva (derecha) o negativa (izquierda)
# Los valores más cercanos a cero tienen menos sesgo
#library(e1071)
#sesgo <- apply(car_fraud$ClaimSize, 2, skewness)

# Elaboramos la matriz de correlación
#correlacion <- cor(car_fraud)

## Análisis de la Variable Objetivo

# Mostramos la proporción de la variable objetivo
y <- car_fraud$FraudFound_P
cbind(Frecuencia = table(y), Porcentaje = prop.table(table(y))*100)

## Visualización de Variables Numéricas

# Creamos un dataframe solo para fraude encontrado
library(tidyverse)
fraud_found <- filter(car_fraud, FraudFound_P == 1)
dim(fraud_found)

# Cómo se distribuye el fraude por edad del asegurado
hist(fraud_found$Age, main = 'Edad del Asegurado', xlab = 'Age')
