options(width=100)

library(knitr)
library(dplyr)
library(ggplot2)
library(GGally)
library(reshape2)
library(tidyr)

# Lectura de datos. NOTA: se pasan las cadenas de texto a tipo factor
churn_data<- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", sep = ",", stringsAsFactors = TRUE)
# se pasa la variable Senior Citizen de numeric a factor para que sea interpretada como categórica
churn_data$SeniorCitizen<-factor(churn_data$SeniorCitizen)

# tipos de dato de cada columna, etc
str(churn_data)
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

# Modelo para predecir clientes que abandonarán. Regresión Logística (se usan todas las variables)
mod.rlogit=glm(Churn ~ ., data = churn_train[, !names(churn_train) %in% "customerID"], family=binomial)

probs<-predict(mod.rlogit, newdata = churn_test, type = "response")
churn_pred<-factor(ifelse(probs>0.5, "Yes", "No"))
# matriz de confusión de la predicción del modelo
(t=table(churn_test$Churn, churn_pred))

# precisión global modelo
( precision_global<-(t[1,1] + t[2,2])/nrow(churn_test) )
# ratio de falsos positivos
( ratio_falsos_positivos<-t[1,2]/(t[1,2]+t[1,1]) )
# ratio de falsos negativos
( ratio_falsos_negativos<-t[1,1]/(t[1,1]+t[2,2]) )
# ratio de verdaderos positivos
( ratio_verdaderos_positivos <-t[2,2]/(t[2,2]+t[2,1]) )


# Modelo similar para predecir clientes que abandonarán usando Regresión Logística. Esta vez se usan las variables:  Contract, Tenure e InternetService
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


# Simulación de campaña de Retención
probs<-predict(mod.rlogit, newdata=churn_test, type = "response")
up<-seq(0, 1, by=0.1)
preds.umbrales<-data.frame(probs=probs, Churn=churn_test$Churn)

# predicciones distintos umbrales de probabilidad
for(u in up){
  x<-factor(ifelse(probs>u, "Yes", "No"), levels = c("No", "Yes"))
  preds.umbrales<-cbind(preds.umbrales, as.data.frame(x))
}
colnames(preds.umbrales)<-c("probs","Churn", paste0("churn_pred_", up))

# matrices de confusión
t.list<-list()
for (i in 3:ncol(preds.umbrales)) {
  t<- table(churn_test$Churn, preds.umbrales[,i])
  t.list<-append(t.list, list(t))
}
print(t.list)

# Escenario 1
I=200
AR=0.4
R=500
vector_Beneficios<-c()
for (i in 1:length(t.list)) {
  TN=t.list[[i]][1,1]
  FP=t.list[[i]][1,2]
  FN=t.list[[i]][2,1]
  TP=t.list[[i]][2,2]
  
  ResultadoConCampania= - FP*AR*I - TP*AR*I - TP*(1-AR)*R - FN*R
  ResultadoSinCampania= - (FN + TP)*R
  Beneficio= ResultadoConCampania - ResultadoSinCampania
  
  cat(colnames(preds.umbrales)[i+2],": ", Beneficio,"\n", sep = "")
  vector_Beneficios<-c(vector_Beneficios, Beneficio)
}

# Beneficios que generaría el mejor umbral
( max_bens1<-max(vector_Beneficios) )

# Escenario 2
I=400
AR=0.8
R=500
vector_Beneficios<-c()
for (i in 1:length(t.list)) {
  TN=t.list[[i]][1,1]
  FP=t.list[[i]][1,2]
  FN=t.list[[i]][2,1]
  TP=t.list[[i]][2,2]
  
  ResultadoConCampania= - FP*AR*I - TP*AR*I - TP*(1-AR)*R - FN*R
  ResultadoSinCampania= - (FN + TP)*R
  Beneficio= ResultadoConCampania - ResultadoSinCampania
  
  cat(colnames(preds.umbrales)[i+2],": ", Beneficio,"\n", sep = "")
  vector_Beneficios<-c(vector_Beneficios, Beneficio)
}

max(vector_Beneficios)

# Se obtienen mejores resultados con la campaña con menor incentivo.
