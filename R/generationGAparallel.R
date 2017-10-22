#' generationGAparallel function
#'
#' This function allows you execute generations of GA.
#' @param chromosome bolean vector.
#' @param yTrain y values in train dataset.
#' @param train X values in train dataset.
#' @param yTest y values in test dataset.
#' @param test X values in test dataset.
#' @param N Initial population.
#' @param NChrSel Number of chromosomes to select.
#' @param pCrossover Crossover probability.
#' @param pMutation Mutation probability.
#' @param paralell Bolean indicationg if the process should run in parallel or sequential mode, defauls is paralell = TRUE.
#' @keywords Generation
#' @export
#' @examples
#' generationGAparallel(population,yTrain, train,yTest,test,N,NChrSel,pMutation,pCrossover,paralell = TRUE)

generationGAparallel <- function(population,yTrain, train,yTest,test,
                         N,NChrSel,pMutation,pCrossover,paralell = TRUE){
    
    
  # Asigns AUC to each chromosome
  importance = sfApply(population,2,AUCNaiveBayes,yTrain = yTrain, train = train,
                         yTest = yTest,test = test)
    
  
  # Extracts AUC from "importance"
  AUC = sapply(importance, "[[","auc")
  
  # storages ID and AUC within data.fraame
  AUCTable = data.frame(ID = 1:length(AUC),AUC)
  
  # chromosome selection
  selectionChromosomes = selection(AUCTable,population,NChrSel)
  selectedChromosomes = selectionChromosomes$chrSelected
  
  # final chromosome selection
  selectionChromosome = selection(AUCTable,population,NChrSel = 1)
  selectedChromosome = selectionChromosome$chrSelected
  selectedChromosomeAUC = selectionChromosome$bestAUC
  
  # Crossover
  crossoverChromosomes = apply(selectedChromosomes,2,crossOver,
                               pCrossover = pCrossover,
                               chromosomes = selectedChromosomes)
  
  # Mutation
  mutatedChromosome = apply(crossoverChromosomes,2,mutation,pMutation = pMutation)
  
  
  # new population creation
  newPolpulation = cbind(selectedChromosomes,mutatedChromosome)
  
  result = list(selectedChromosome = selectedChromosome,
                selectedChromosomeAUC = selectedChromosomeAUC,
                newPolpulation = newPolpulation)
  
  # Returns new population
  return(result)
  
}