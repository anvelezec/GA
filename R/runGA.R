#' runGA function
#'
#' This function allows you execute a run of GA.
#' @param Nruns Number of runs to execute.
#' @param N Initial population.
#' @param NChrSel Number of chromosomes to select.
#' @param pMutation Mutation probability.
#' @param pCrossover Crossover probability.
#' @param yTrain y values in train dataset.
#' @param train X values in train dataset.
#' @param yTest y values in test dataset.
#' @param test X values in test dataset.
#' @keywords cats
#' @export
#' @examples
#' require(GA)
#'
#' # Target variable for training
#' y = dataBalanced$train$target
#'
#' # Create training feature data frame
#' trainData = dataBalanced$train[,-1] 
#'
#' # runGA execution
#' resulTime = system.time(
#'   
#'  resultRunGA <- runGA(Nruns = 1,N = 2,NChrSel = 2,
#'                       pMutation = 0.05,pCrossover = 0.8,
#'                       yTrain = y,train = trainData,
#'                       yTest = y,test = trainData)
#' )
#'
#'
runGA <- function(Nruns,N,NChrSel,pMutation,pCrossover,
                  yTrain, train,yTest,test){
    
  require(snowfall)
  require(parallel)
  require(dplyr)  
  
  # Finds numbers of cores 
  ncores = detectCores()
  
  # Initializes cluster
  sfInit(parallel=TRUE, cpus = ncores)
  
  # Exports functions to clusters
  # sfSource("Functions/operators.R")
  
  # Exports libraries to clusters
  sfLibrary(ROCR)
  sfLibrary(e1071)
  sfLibrary(GA)
  
  # Random Chromosome generation
  binaryMatrix = cbind(rep(0,N),1)
  population = apply(binaryMatrix,1,sample,size = ncol(trainData),replace = TRUE)
  histBestAUC = 0 
  
  for(i in 1:Nruns){
    
    newPolpulation = generationGAparallel(population,yTrain = yTrain, train = train,
                                          yTest = yTest,test = test,
                                          N = N,NChrSel = NChrSel,
                                          pMutation = pMutation,
                                          pCrossover = pCrossover)

    population = newPolpulation$newPolpulation
    
    # Historical best AUC
    if(newPolpulation$selectedChromosomeAUC > histBestAUC){
      
      histBestAUC = newPolpulation$selectedChromosomeAUC
      histChromosome = newPolpulation$selectedChromosome
      
    }
    
  }

  sfStop()

  finalData = trainData[,newPolpulation$selectedChromosome == "1"]
  
  result = list(selectedChromosome = newPolpulation$selectedChromosome,
                AUC = newPolpulation$selectedChromosomeAUC,
                finalData = finalData,
                histBestAUC = histBestAUC,
                histChromosome = histChromosome)
  
  return(result)

}


