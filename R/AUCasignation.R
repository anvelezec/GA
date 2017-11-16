#' AUCasignation function
#'
#' This function allows you to scores chromosomes.
#' @param population bolean vector.
#' @param yTrain y values in train dataset.
#' @param train X values in train dataset.
#' @keywords fitness function
#' @export
#' @examples
#' AUCasignation(population,yTrain,train)

AUCasignation <- function(population,yTrain,train){
  
  
  # Finds numbers of cores 
  ncores = detectCores()
  
  # Initializes cluster
  sfInit(parallel=TRUE, cpus = ncores)
  
  sfLibrary(e1071)
  sfLibrary(ROCR)
  
  # Asigns AUC to each chromosome
  importance = sfApply(population,2,AUCNaiveBayes,yTrain = yTrain, train = train,
                       yTest = yTrain,test = train)
  
  
  # Extracts AUC from "importance"
  AUC = sapply(importance, "[[","auc")
  
  # storages ID and AUC within data.fraame
  AUCTable = data.frame(ID = 1:length(AUC),AUC)
  
  
  sfStop()
  
  return(AUCTable)
  
}
