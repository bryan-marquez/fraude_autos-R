#### Análisis de Datos de Fraude en Seguros de Auto

### 2. Procesamiento de los Datos

## 2.1 Carga de Datos

# Cargamos los datos
archivo <- "/cloud/project/data/Dataset.csv"
car_fraud <- read.csv(archivo)
head(car_fraud)
summary(car_fraud)

## 2.2 División de los Datos

# Dividimos los datos en X
X <- select(car_fraud, -FraudFound_P, -PolicyNumber)
dim(X)

## 2.3 Limpieza de Datos y Escalamiento de Variables

# Mostramos la suma de valores nulos
sapply(X, function(x) sum(is.na(x)))

# Mostramos la suma de valores cero
sapply(X, function(x) sum(x == 0, na.rm = TRUE))

# Reemplazamos los valores iguales a cero por NA
X[X == 0] <- NA
sapply(X, function(x) sum(is.na(x)))

# Imputamos los valores nulos con "knnImpute" y estandarizamos los datos númericos con "center" y "scale"
imputer <- preProcess(X, method = c('knnImpute'))
imputer

# Transformamos los valores nulos y transformamos los datos númericos a media = 0 y sd = 1
transformed <- predict(imputer, car_fraud)
head(transformed)
sapply(transformed, function(x) sum(is.na(x)))

## 2.4 Sesgo / Asimetría

# Aplicamos la transformación de Box-Cox o Yeo-Johnson a los atributos con sesgo
# BoxCox (valores positivos)
# YeoJohnson (valores negativos)
#boxcox <- preProcess(select(transformed, Age, ClaimSize), method = c('BoxCox'))
#boxcox

# Transformamos los atributos con sesgo
#transformed2 <- predict(boxcox, transformed)
#head(transformed2)
