#### Análisis de Datos de Fraude en Seguros de Auto

### 2. Procesamiento de los Datos


## 2.1 Carga de librerías

# Cargamos las liberías a utilizar
library(dplyr)
library(caret)
library(e1071)
library(performanceEstimation)


## 2.2 Carga de Datos

# Cargamos los datos
archivo <- "/cloud/project/data/Dataset.csv"
car_fraud <- read.csv(archivo)
head(car_fraud)

# Mostramos el número de filas y columnas
dim(car_fraud)

# Mostramos los tipos de datos
str(car_fraud)
sapply(car_fraud, class)


## 2.3 División de los Datos

# Dividimos los datos
dataframe <- dplyr::select(car_fraud, -PolicyNumber) # quitamos la variable identidad
dim(dataframe)
X <- dplyr::select(dataframe, -FraudFound_P) # quitamos la variable objetivo
dim(X)


## 2.4 Limpieza de Datos y Escalamiento de Variables

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
transformed <- predict(imputer, dataframe)
head(transformed)
sapply(transformed, function(x) sum(is.na(x)))


## 2.5 Codificación de Datos Categóricos

# Convertimos a factor las variables categóricas ordinales y nominales
transformed$FraudFound_P <- factor(transformed$FraudFound_P)
transformed$Month <- factor(transformed$Month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
transformed$DayOfWeek <- factor(transformed$DayOfWeek, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
transformed$Make <- factor(transformed$Make, levels = c('Accura', 'BMW', 'Chevrolet', 'Dodge', 'Ferrari', 'Ford', 'Honda', 'Jaguar', 'Lexus', 'Mazda', 'Mecedes', 'Mercury', 'Nisson', 'Pontiac', 'Porche', 'Saab', 'Saturn', 'Toyota', 'VW'))
transformed$AccidentArea <- factor(transformed$AccidentArea, levels = c('Rural', 'Urban'))
transformed$DayOfWeekClaimed <- factor(transformed$DayOfWeekClaimed, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
transformed$MonthClaimed <- factor(transformed$MonthClaimed, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
transformed$Sex <- factor(transformed$Sex, levels = c('Male', 'Female'))
transformed$MaritalStatus <- factor(transformed$MaritalStatus, levels = c('Single', 'Married', 'Widow', 'Divorced'))
transformed$Fault <- factor(transformed$Fault, levels = c('Policy Holder', 'Third Party'))
transformed$PolicyType <- factor(transformed$PolicyType, levels = c('Sport - Liability', 'Sport - Collision', 'Sedan - Liability', 'Sedan - All Perils', 'Sedan - Collision', 'Utility - Collision', 'Utility - Liability', 'Utility - All Perils', 'Sport - All Perils'))
transformed$VehicleCategory <- factor(transformed$VehicleCategory, levels = c('Sport', 'Sedan', 'Utility'))
transformed$VehiclePrice <- factor(transformed$VehiclePrice, levels = c('less than 20000', '20000 to 29000', '30000 to 39000', '40000 to 59000', '60000 to 69000', 'more than 69000'))
transformed$Days_Policy_Accident <- factor(transformed$Days_Policy_Accident, levels = c('none', '1 to 7', '8 to 15', '15 to 30', 'more than 30'))
transformed$Days_Policy_Claim <- factor(transformed$Days_Policy_Claim, levels = c('none', '8 to 15', '15 to 30', 'more than 30'))
transformed$PastNumberOfClaims <- factor(transformed$PastNumberOfClaims, levels = c('none', '1', '2 to 4', 'more than 4'))
transformed$AgeOfVehicle <- factor(transformed$AgeOfVehicle, levels = c('new', '2 years', '3 years', '4 years', '5 years', '6 years', '7 years', 'more than 7'))
transformed$AgeOfPolicyHolder <- factor(transformed$AgeOfPolicyHolder, levels = c('16 to 17', '18 to 20', '21 to 25', '26 to 30', '31 to 35', '36 to 40', '41 to 50', '51 to 65', 'over 65'))
transformed$PoliceReportFiled <- factor(transformed$PoliceReportFiled, levels = c('No', 'Yes'))
transformed$WitnessPresent <- factor(transformed$WitnessPresent, levels = c('No', 'Yes'))
transformed$AgentType <- factor(transformed$AgentType, levels = c('External', 'Internal'))
transformed$NumberOfSuppliments <- factor(transformed$NumberOfSuppliments, levels = c('none', '1 to 2', '3 to 5', 'more than 5'))
transformed$AddressChange_Claim <- factor(transformed$AddressChange_Claim, levels = c('no change', 'under 6 months', '1 year', '2 to 3 years', '4 to 8 years'))
transformed$NumberOfCars <- factor(transformed$NumberOfCars, levels = c('1 vehicle', '2 vehicles', '3 to 4', '5 to 8', 'more than 9'))
transformed$BasePolicy <- factor(transformed$BasePolicy, levels = c('Liability', 'Collision', 'All Perils'))

str(transformed)
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


## 2.6 Sobremuestreo de los Datos

# Aplicamos sobremuestreo para balancear la clase usando SMOTE
dataSMOTE <- smote(FraudFound_P~., data = transformed)
dim(dataSMOTE)
y <- dataSMOTE$FraudFound_P
cbind(Frecuencia = table(y), Porcentaje = prop.table(table(y))*100)
sapply(dataSMOTE, function(x) sum(is.na(x)))


## 2.7 Remuestreo de los Datos

# Boostrap
#control <- trainControl(method = "boot", number = 100)

# Validación Cruzada
control <- trainControl(method = "cv", number = 10)


## 2.8 Selección de Características

# Seleccionamos las mejores características basadas en el modelo LVQ
set.seed(7)
model <- train(FraudFound_P~., data = dataSMOTE, method = "lvq", trControl = control)
importance <- varImp(model, scale = FALSE)
importance


### 3 Modelado
set.seed(7)


## 3.1 Lineales

# Ajustamos el modelo de Regresión Logística
fit_glm <- train(FraudFound_P~., data = dataSMOTE, trControl = control, method = "glm", metric = "Accuracy", na.action = na.exclude)
fit_glm

# Hacemos las predicciones
pred_glm <- predict(fit_glm, newdata = dplyr::select(dataSMOTE, -FraudFound_P))
pred_glm

# Elaboramos la matriz de confusión
cf_glm <- confusionMatrix(data = pred_glm, reference = dataSMOTE$FraudFound_P)
cf_glm

# Ajustamos el modelo de Regresión Regularizada
fit_glmnet <- train(FraudFound_P~., data = dataSMOTE, trControl = control, method = "glmnet", metric = "Accuracy", na.action = na.exclude)
fit_glmnet

# Hacemos las predicciones
pred_glmnet <- predict(fit_glmnet, newdata = dplyr::select(dataSMOTE, -FraudFound_P))
pred_glmnet

# Elaboramos la matriz de confusión
cf_glmnet <- confusionMatrix(data = pred_glmnet, reference = dataSMOTE$FraudFound_P)
cf_glmnet


## 3.2 No Lineales

# Ajustamos el modelo de Naive Bayes
fit_nb <- train(FraudFound_P~., data = dataSMOTE, trControl = control, method = "nb", metric = "Accuracy", na.action = na.exclude)
summary(fit_nb)

# Hacemos las predicciones
pred_nb <- predict(fit_nb, newdata = dplyr::select(dataSMOTE, -FraudFound_P))
pred_nb

# Elaboramos la matriz de confusión
cf_nb <- confusionMatrix(data = pred_nb, reference = dataSMOTE$FraudFound_P)
cf_nb

# Ajustamos el modelo de KNN
fit_knn <- train(FraudFound_P~., data = dataSMOTE, trControl = control, method = "knn", metric = "Accuracy", na.action = na.exclude)
fit_knn

# Hacemos las predicciones
pred_knn <- predict(fit_knn, newdata = dplyr::select(dataSMOTE, -FraudFound_P))
pred_knn

# Elaboramos la matriz de confusión
cf_knn <- confusionMatrix(data = pred_knn, reference = dataSMOTE$FraudFound_P)
cf_knn

# Ajustamos el modelo de SVM
fit_svm <- train(FraudFound_P~., data = dataSMOTE, trControl = control, method = "svmRadial", metric = "Accuracy", na.action = na.exclude)
fit_svm

# Hacemos las predicciones
pred_svm <- predict(fit_svm, newdata = dplyr::select(dataSMOTE, -FraudFound_P))
pred_svm

# Elaboramos la matriz de confusión
cf_svm <- confusionMatrix(data = pred_svm, reference = dataSMOTE$FraudFound_P)
cf_svm

# Ajustamos el modelo de CART
fit_cart <- train(FraudFound_P~., data = dataSMOTE, trControl = control, method = "rpart", metric = "Accuracy", na.action = na.exclude)
fit_cart

# Hacemos las predicciones
pred_cart <- predict(fit_cart, newdata = dplyr::select(dataSMOTE, -FraudFound_P))
pred_cart

# Elaboramos la matriz de confusión
cf_cart <- confusionMatrix(data = pred_cart, reference = dataSMOTE$FraudFound_P)
cf_cart
