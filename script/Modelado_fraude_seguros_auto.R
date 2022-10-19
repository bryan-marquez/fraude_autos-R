#### Análisis de Datos de Fraude en Seguros de Auto

### 2. Procesamiento de los Datos

## 2.1 Carga de Datos

# Cargamos los datos
archivo <- "/cloud/project/data/Dataset.csv"
car_fraud <- read.csv(archivo)
head(car_fraud)

# Mostramos el número de filas y columnas
dim(car_fraud)

# Mostramos los tipos de datos
str(car_fraud)
sapply(car_fraud, class)

## 2.2 Codificación de Datos Categóricos

# Convertimos a factor las variables categóricas ordinales y nominales

car_fraud$FraudFound_P <- factor(car_fraud$FraudFound_P)
car_fraud$Month <- factor(car_fraud$Month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
car_fraud$DayOfWeek <- factor(car_fraud$DayOfWeek, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
car_fraud$Make <- factor(car_fraud$Make, levels = c('Accura', 'BMW', 'Chevrolet', 'Dodge', 'Ferrari', 'Ford', 'Honda', 'Jaguar', 'Lexus', 'Mazda', 'Mecedes', 'Mercury', 'Nisson', 'Pontiac', 'Porche', 'Saab', 'Saturn', 'Toyota', 'VW'))
car_fraud$AccidentArea <- factor(car_fraud$AccidentArea, levels = c('Rural', 'Urban'))
car_fraud$DayOfWeekClaimed <- factor(car_fraud$DayOfWeekClaimed, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
car_fraud$MonthClaimed <- factor(car_fraud$MonthClaimed, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
car_fraud$Sex <- factor(car_fraud$Sex, levels = c('Male', 'Female'))
car_fraud$MaritalStatus <- factor(car_fraud$MaritalStatus, levels = c('Single', 'Married', 'Widow', 'Divorced'))
car_fraud$Fault <- factor(car_fraud$Fault, levels = c('Policy Holder', 'Third Party'))
car_fraud$PolicyType <- factor(car_fraud$PolicyType, levels = c('Sport - Liability', 'Sport - Collision', 'Sedan - Liability', 'Sedan - All Perils', 'Sedan - Collision', 'Utility - Collision', 'Utility - Liability', 'Utility - All Perils', 'Sport - All Perils'))
car_fraud$VehicleCategory <- factor(car_fraud$VehicleCategory, levels = c('Sport', 'Sedan', 'Utility'))
car_fraud$VehiclePrice <- factor(car_fraud$VehiclePrice, levels = c('less than 20000', '20000 to 29000', '30000 to 39000', '40000 to 59000', '60000 to 69000', 'more than 69000'))
car_fraud$Days_Policy_Accident <- factor(car_fraud$Days_Policy_Accident, levels = c('none', '1 to 7', '8 to 15', '15 to 30', 'more than 30'))
car_fraud$Days_Policy_Claim <- factor(car_fraud$Days_Policy_Claim, levels = c('none', '8 to 15', '15 to 30', 'more than 30'))
car_fraud$PastNumberOfClaims <- factor(car_fraud$PastNumberOfClaims, levels = c('none', '1', '2 to 4', 'more than 4'))
car_fraud$AgeOfVehicle <- factor(car_fraud$AgeOfVehicle, levels = c('new', '2 years', '3 years', '4 years', '5 years', '6 years', '7 years', 'more than 7'))
car_fraud$AgeOfPolicyHolder <- factor(car_fraud$AgeOfPolicyHolder, levels = c('16 to 17', '18 to 20', '21 to 25', '26 to 30', '31 to 35', '36 to 40', '41 to 50', '51 to 65', 'over 65'))
car_fraud$PoliceReportFiled <- factor(car_fraud$PoliceReportFiled, levels = c('No', 'Yes'))
car_fraud$WitnessPresent <- factor(car_fraud$WitnessPresent, levels = c('No', 'Yes'))
car_fraud$AgentType <- factor(car_fraud$AgentType, levels = c('External', 'Internal'))
car_fraud$NumberOfSuppliments <- factor(car_fraud$NumberOfSuppliments, levels = c('none', '1 to 2', '3 to 5', 'more than 5'))
car_fraud$AddressChange_Claim <- factor(car_fraud$AddressChange_Claim, levels = c('no change', 'under 6 months', '1 year', '2 to 3 years', '4 to 8 years'))
car_fraud$NumberOfCars <- factor(car_fraud$NumberOfCars, levels = c('1 vehicle', '2 vehicles', '3 to 4', '5 to 8', 'more than 9'))
car_fraud$BasePolicy <- factor(car_fraud$BasePolicy, levels = c('Liability', 'Collision', 'All Perils'))

str(car_fraud)
sapply(car_fraud, class)

## 2.3 División de los Datos

# Dividimos los datos
dataframe <- dplyr::select(car_fraud, -PolicyNumber) # quitamos PolicyNumber
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

## 2.4 Sesgo / Asimetría

# Aplicamos la transformación de Box-Cox o Yeo-Johnson a los atributos con sesgo
# BoxCox (valores positivos)
# YeoJohnson (valores negativos)
#boxcox <- preProcess(select(transformed, Age, ClaimSize), method = c('BoxCox'))
#boxcox

# Transformamos los atributos con sesgo
#transformed2 <- predict(boxcox, transformed)
#head(transformed2)

## 2.5 Remuestreo de los Datos

# Dividimos los datos de entrenamiento y prueba
trainIndex <- createDataPartition(transformed$FraudFound_P, p = 0.8, list = FALSE)
dataTrain <- transformed[trainIndex, ]
dataTest <- transformed[-trainIndex, ]

## 2.6 Modelado

# Ajustamos el modelo
fit <- naiveBayes(FraudFound_P~., data = dataTrain)

# Hacemos las predicciones
predictions <- predict(fit, select(dataTest, -FraudFound_P))

# Elaboramos la matriz de confusión
confusionMatrix(predictions, dataTest$FraudFound_P)

## Boostrap
control <- trainControl(method = "boot", number = 100)
fit <- train(FraudFound_P~., data = transformed[complete.cases(transformed),], trControl = control, method = "rf", na.action = na.exclude)
predictions <- predict(fit, select(transformed, -FraudFound_P))
confusionMatrix(predictions, transformed$FraudFound_P)