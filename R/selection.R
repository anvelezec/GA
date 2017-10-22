#' Selection function
#'
#' This function allows you to select chromosomes based in scored given by the fitness function.
#' @param AUCTable Table with chromosomes scored.
#' @param population Total population of chromosomes
#' @param NChrSel Number of chromosomes to select.
#' @keywords cats
#' @export
#' @examples
#' Selection(AUCTable,population,NChrSel)

selection <- function(AUCTable,population,NChrSel){
  
  # arranges AUC by magnitude
  AUCTableOrdered = AUCTable %>% arrange(desc(AUC))
  
  # ID Selection
  NChrSelected = AUCTableOrdered$ID[1:NChrSel]
  
  # Chromosome Selection
  chrSelected = population[,NChrSelected]  
  
  # best AUC selection
  bestAUC = AUCTableOrdered$AUC[1:NChrSel]
  
  result = list(chrSelected = chrSelected,
                bestAUC = bestAUC)
  
  return(result)
}