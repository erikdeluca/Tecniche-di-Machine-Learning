library(gbm)
library(insuranceData)
library(randomForest)
library(ROSE)
library(dplyr)
library(tidyverse)
library(tree)
library(smotefamily)


# importo i dati
data(dataOhlsson)
df = dataOhlsson

# trasformo la variabile antskad e rimuovo skadkost
df = df[,-9]
df$antskad = factor(ifelse(df$antskad > 0, T, F), labels = c("No claim", "Claim"))
table(df$antskad)

# suddivido il dataset in train e test
set.seed(1)
train = sample(1:nrow(df), .7 * nrow(df))

# amplio i dati con ROSE
dfRose = ROSE(antskad ~ . , data = df[train,], seed = 1)$data
table(dfRose$antskad)

# suddivido il dataset creato con ROSE
set.seed(1)
trRose = sample(1:nrow(dfRose), .7 * nrow(dfRose))

# random forest per ROSE
rfRose = randomForest(antskad ~ ., data = dfRose[trRose,])
pred = predict(rfRose, newdata = dfRose[-trRose,])
# AUC
ModelMetrics::auc(dfRose$antskad[-trRose], pred)
# matrice di confusione
confusionMatrix(table(dfRose$antskad[-trRose],pred),positive = "Claim")

# random forest SENZA rose
rf = randomForest(antskad ~ ., data = df[train, ])
pred = predict(rf, newdata = df[-train,])
ModelMetrics::auc(df$antskad[-train], pred)
# matrice di confusione
confusionMatrix(table(df$antskad[-train],pred),positive = "Claim")

# tree SENZA ROSE
albero = tree(antskad ~ ., data = df[train, ])
pred = predict(albero, newdata = df[-train,],type = "class")
ModelMetrics::auc(df$antskad[-train], pred)
# matrice di confusione
confusionMatrix(table(df$antskad[-train],pred),positive = "Claim")

# Boosting CON ROSE
modBoos = gbm(antskad ~ .,
              data = dfRose[trRose,],
              distribution = "gaussian",
              n.trees = 300,
              interaction.depth = 10)
pred = factor(ifelse(predict(modBoos, newdata = dfRose[-trRose,])<1.5,1,2),
              labels = c("No claim","Claim"))
ModelMetrics::auc(dfRose$antskad[-trRose],pred)
# matrice di confusione
confusionMatrix(table(dfRose$antskad[-trRose],pred),positive = "Claim")




# funzione costruisci matrici di confusione per più modelli
listaCampionamenti
predLista = lapply(1:5, function(i) factor(ifelse(predSmote<.5, 0, 1), labels = c("No claim", "Claim")))

multiConfusionMatrix = function(oss, predLista, nomiModelli)
{
  if(is.matrix(predLista[[1]]))
  {
    for(i in 1:length(predLista))
      predLista[[i]] = factor(ifelse(predLista[[i]][,2] < .5, 0, 1), 
                            levels = c(0,1),
                            labels = c("No claim", "Claim"))
  } else if(is.numeric(predLista[[1]]))
  {
    for(i in 1:length(predLista))
      predLista[[i]] = factor(ifelse(predLista[[i]] < .5, 0, 1), 
                              levels = c(0,1),
                              labels = c("No claim", "Claim"))
  }
  tabelle = data.frame(table(oss, predLista[[1]])[,1], 
                       table(oss, predLista[[1]])[,2])
  if(length(predLista) > 1)
  {
    for(i in 2:length(predLista))
    {
      tabelle = cbind(tabelle,
                      table(oss, predLista[[i]])[,1], 
                      table(oss, predLista[[i]])[,2])
    }
  }
  colnames(tabelle)[c(1:length(predLista)*2-1)] = paste(rownames(tabelle)[1], nomiModelli)
  colnames(tabelle)[1:length(predLista)*2] = paste(rownames(tabelle)[2], nomiModelli)
  return(tabelle)
}

multiConfusionMatrix(oss = df$antskad[-train],
                     lapply(1:2, function(i) get(listaCampionamenti[i])),
                     listaCampionamenti[1:2])


