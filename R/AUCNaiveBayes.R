#' AUCNaiveBayes function
#'
#' This function allows you to scores chromosomes.
#' @param chromosome bolean vector.
#' @param yTrain y values in train dataset.
#' @param train X values in train dataset.
#' @param yTest y values in test dataset.
#' @param test X values in test dataset.
#' @keywords fitness function
#' @export
#' @examples
#' AUCNaiveBayes(chromosome,yTrain,train,yTest,test)

AUCNaiveBayes <- function(chromosome,yTrain,train,yTest,test){
  
  #####################
  # Selection
  
  train = train[,chromosome == "1"]
  dataTrainPolulation = data.frame(y = yTrain,train)
  
  tryCatch({
    
    #naiveBayesCredi = naiveBayes(Target ~ . ,data = train)
    naiveBayesCredi = naiveBayes(y ~ . ,data = dataTrainPolulation)
    
  },error = function(error){
    try(print(error))
    
    
    
  })
  
  #####################
  # Asigns AUC value
  
  predictions = predict(naiveBayesCredi,test,type = "raw")
  pred1 = prediction(predictions[,2], yTest)
  perf1 = performance(pred1,"tpr","fpr")
  
  auc = performance(pred1,"auc")@y.values[[1]]
  result = list(naiveBayesCredi,auc)
  names(result) = c("naiveBayesCredi","auc")
  
  return(result)
}