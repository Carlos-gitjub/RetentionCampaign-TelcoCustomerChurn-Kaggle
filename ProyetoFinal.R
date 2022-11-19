knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
options(width=100)

library(knitr)
library(dplyr)
library(ggplot2)
library(GGally)
library(reshape2)
library(tidyr)


# Código para lectura de datos. NOTA: se pasan las cadenas de texto a tipo factor
churn_data<- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", sep = ",", stringsAsFactors = TRUE)
# se pasa la variable Senior Citizen de numeric a factor para que sea interpretada como categórica
churn_data$SeniorCitizen<-factor(churn_data$SeniorCitizen)

# tipo de dato columnas
str(churn_data)

# resumen dataframe
summary(churn_data)

# diagramas de frecuencias de churn y variables cualitativas
variables_cualitativas<- c("gender", "SeniorCitizen", "Partner", "Dependents",
                           "PhoneService", "MultipleLines", "InternetService",
                           "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                           "TechSupport", "StreamingTV", "StreamingMovies", 
                           "Contract", "PaperlessBilling", "PaymentMethod",
                           "Churn")
tmp.cuant.churn<- melt(churn_data, id="Churn", measure=variables_cualitativas)
ggplot(tmp.cuant.churn) + geom_bar(aes(value, fill=Churn)) + facet_wrap(~variable, scales = "free_x") +
  theme( axis.text.x = element_text(angle = 15))

# histogramas de churn y variables cuantitativas
variables_cuantitativas<- c("tenure","MonthlyCharges", "TotalCharges")
tmp.cual.churn<- melt(churn_data, id="Churn", measure=variables_cuantitativas)
ggplot(tmp.cual.churn) + geom_histogram(aes(value, fill=Churn)) +
  facet_wrap(~variable, scales = "free_x")

# boxplot de la distribución del gasto mensual para cada valor de las variables categóricas
tmp.cual.monthly<- melt(churn_data[c(variables_cualitativas,"MonthlyCharges")], 
                        id="MonthlyCharges", measure=variables_cualitativas)

ggplot(tmp.cual.monthly) + geom_boxplot(aes(value, MonthlyCharges)) +
  facet_wrap(~variable, scales = "free_x") +
  theme(axis.text.x = element_text(angle=15))

# matriz de graficos de dispersión para visualizar la correlación entre variables.
tmp.cuant.monthly<- churn_data[variables_cuantitativas]
ggpairs(tmp.cuant.monthly)

# presencia de NAs en el dataset
colSums(is.na(churn_data))

# Código para creación de conjuntos de entrenamiento y test 
churn_train<-churn_data[1:5000,]
churn_test<-churn_data[5001:nrow(churn_data),]

# Modelo de clasificación usando todas las variables. (Regresion logística)

mod.rlogit=glm(Churn ~ ., data = churn_train[, !names(churn_train) %in% "customerID"], family=binomial)

probs<-predict(mod.rlogit, newdata = churn_test, type = "response")
churn_pred<-factor(ifelse(probs>0.5, "Yes", "No"))
(t=table(churn_test$Churn, churn_pred))

# precisión global
( precision_global<-(t[1,1] + t[2,2])/nrow(churn_test) )
# ratio de falsos positivos
( ratio_falsos_positivos<-t[1,2]/(t[1,2]+t[1,1]) )
# ratio de falsos negativos
( ratio_falsos_negativos<-t[1,1]/(t[1,1]+t[2,2]) )
# ratio de verdaderos positivos
( ratio_verdaderos_positivos <-t[2,2]/(t[2,2]+t[2,1]) )

# Modelo de clasificación usando las variables: Contract, Tenure e InternetService.

mod.rlogit2<-glm(Churn ~ ., data = churn_train[,c("Churn", "Contract", "tenure", "InternetService")], family = binomial)

probs<-predict(mod.rlogit2, newdata=churn_test, type="response")
churn_pred2=factor(ifelse(probs>0.5, "Yes", "No"))
# matriz de confusion
( t<-table(churn_test$Churn, churn_pred2) )

# precisión global
( precision_global<-(t[1,1] + t[2,2])/nrow(churn_test) )
# ratio de falsos positivos
( ratio_falsos_positivos<-t[1,2]/(t[1,2]+t[1,1]) )
# ratio de falsos negativos
( ratio_falsos_negativos<-t[1,1]/(t[1,1]+t[2,2]) )
# ratio de verdaderos positivos
( ratio_verdaderos_positivos <-t[2,2]/(t[2,2]+t[2,1]) )


