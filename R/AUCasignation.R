#####################
### Andres Velez
### AUCasignation
#####################


AUCasignation <- function(population,yTrain,train){
  
  # Asigns AUC to each chromosome
  importance = sfApply(population,2,AUCNaiveBayes,yTrain = yTrain, train = train,
                       yTest = yTrain,test = train)
  
  
  # Extracts AUC from "importance"
  AUC = sapply(importance, "[[","auc")
  
  # storages ID and AUC within data.fraame
  AUCTable = data.frame(ID = 1:length(AUC),AUC)
  
  return(AUCTable)
  
}