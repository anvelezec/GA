#' crossOver function
#'
#' This function allows you to generate chromosomes by subsamples of parents.
#' @param chromosome bolean vector.
#' @param pCrossover Crossover probability.
#' @param chromosomes Matrix with chromosomes. Each column is chromosomes and rows the alleles.
#' @keywords Mix chromosomes.
#' @export
#' @examples
#' crossOver(chromosome,pCrossover,chromosomes)

crossOver <- function(chromosome,pCrossover,chromosomes){
  
  chanceCrossover = pnorm(rnorm(1))
  
  if(chanceCrossover < pCrossover){
    
    # selects chromosome to extract chain from
    extraccionChro = sample(1:ncol(chromosomes),1)
    
    # From where to the extraccion takes accion
    Ncrossover = ceiling(runif(1,min = 1,max = (length(chromosome)-1)))
    
    # Creates sequence with from and to values
    chain = sample(1:length(chromosome),Ncrossover)
    
    # Applays crossover
    chromosome[chain] = chromosomes[chain,extraccionChro]
  }
  
  # Returns chromosome
  return(chromosome)
}

