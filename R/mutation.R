#' mutation function
#'
#' This function allows you to mutate allels in a chromosome chain.
#' @param chromosome bolean vector.
#' @param pMutation Mutation probability.
#' @keywords cats
#' @export
#' @examples
#' mutation(chromosome,pMutation)

mutation <- function(chromosome,pMutation){
  
  chanceMutation = pnorm(rnorm(1))
  
  if(chanceMutation < pMutation){
    
    position = sample(1:length(chromosome),1)
    
    # Applays crossover
    valuePossition = chromosome[position]
    
    if(valuePossition == 1){
      
      valuePossition = 0
      
    }else{
      
      valuePossition = 1
      
    }
    
    # Replace position
    chromosome[position] = valuePossition
    
    # print(position) 
  }
  
  # Returns chromosome
  return(chromosome)
}

