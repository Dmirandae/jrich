#'
#' Jack-knife indices in n topologies  m times
#'
#' The function calculates the indices values for a MultiData list  n (=replicates) times 
#'

MultiJack <- function (MultiData = MultiData,times = 100, jtip = 0, jtopol = 0) {
  
  results <- list() 
  
  for(i in 1:times){
    results[[i]] <- Rank.Indices(Multi.Index.Calc(MultiData, jtip, jtopol))
  }
  
  return(results)
}
