#### Análisis de Datos de Fraude en Seguros de Auto

### 1. Análisis Exploratorio de Datos (EDA)

## 1.1 Carga de Datos

# Cargamos los datos
archivo <- "/cloud/project/data/Dataset.csv"
car_fraud <- read.csv(archivo)
head(car_fraud)

# Mostramos el número de filas y columnas
dim(car_fraud)

# Mostramos los tipos de datos
str(car_fraud)
sapply(car_fraud, class)

## 1.2 Estadística Descriptiva

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

## 1.3 Análisis de la Variable Objetivo

# Mostramos la proporción de la variable objetivo
y <- car_fraud$FraudFound_P
cbind(Frecuencia = table(y), Porcentaje = prop.table(table(y))*100)

## 1.3.1 Visualización de Variables Categóricas

# Cómo se distribuye el fraude por día de la semana de la reclamación
ggplot(data = car_fraud,
       mapping = aes(x = factor(DayOfWeekClaimed),
                     fill = factor(FraudFound_P))) +
  geom_bar(position = 'dodge', stat = 'count')

# Cómo se distribuye el fraude por mes de la reclamación
ggplot(data = car_fraud,
       mapping = aes(x = factor(MonthClaimed),
                     fill = factor(FraudFound_P))) +
  geom_bar(position = 'dodge', stat = 'count')

## 1.3.2 Visualización de Variables Numéricas

# Creamos un dataframe solo para fraude encontrado
library(tidyverse)
fraud_found <- filter(car_fraud, FraudFound_P == 1)
dim(fraud_found)

# Cómo se distribuye el fraude por edad del asegurado
hist(fraud_found$Age, main = 'Edad del Asegurado', xlab = 'Age')

ggplot(data = car_fraud[car_fraud$FraudFound_P == 1,],
       mapping = aes(x = Age,)) +
  geom_histogram(bins = 9)

ggplot(data = car_fraud,
       mapping = aes(x = factor(FraudFound_P),
                     y = Age,
                     fill = FraudFound_P)) +
  geom_boxplot() +
  xlab('Age')

# Cómo se distribuye el fraude por tamaño de la reclamación
hist(fraud_found$ClaimSize, main = 'Tamaño de la Reclamación', xlab = 'ClaimSize')

ggplot(data = car_fraud[car_fraud$FraudFound_P == 1,],
       mapping = aes(x = ClaimSize,)) +
  geom_histogram(bins = 9)

ggplot(data = car_fraud,
       mapping = aes(x = factor(FraudFound_P),
                     y = ClaimSize,
                     fill = FraudFound_P)) +
  geom_boxplot() +
  xlab('FraudFound_P')