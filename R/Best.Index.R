#'
#' @title 
#' Jack-knife indices in a single topology m times and evaluates a success rule.
#'
#' @description 
#' The function Jack-knifes the terminals jtip (=replicates) times, and calculates the number of times 
#' that each replicate recovers the same initial initial.Rankinging (X:Y positions), by default success is 1:3 positions.
#' The function returns the number of hits.
#'
#' @param tree is a single tree with n terminals, an ape phylo object.
#' 
#' @param distrib species distributions in n areas, a data.frame
#' 
#' @param jtip is the number of terminals, an integer.
#' 
#' @param replicates is the number of replicates, an integer.
#' 
#' @param success the measure of the success, a vector.
#' 
#' @return The number of hits
#'
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'




Best.Index <-
function (tree = tree, distrib = distrib, jtip = jtip,
          replicates = replicates, success = (c(1:3)) ) {
          
  initial.Ranking <- Rank.Indices(Calculate.Index(tree = tree,distrib = distrib))
  
  success <- NULL
  
  success$I <- success$Ie <- success$Is <- success$Ise <- success$W <- success$We <- success$Ws <- success$Wse <-0
    
  for (i in 1:replicates){
    
    jack.Ranking<- Rank.Indices(Calculate.Index(tree = tree, distrib = distrib, jtip))
    
    if(all(initial.Ranking$I[success] == jack$I[success])){
		ok = 1}else{
		ok=0
    }
    
    success$I <- success$I+ok
    
    if(all(initial.Ranking$Ie[success] == jack$Ie[success])){
		ok = 1}else{
		ok=0
	}
    success$Ie <- success$Ie+ok
    
    if(all(initial.Ranking$Is[success] == jack$Is[success])){
		ok = 1}else{
		ok=0
	}
    success$Is <- success$Is+ok
    
    if(all(initial.Ranking$Ise[success] == jack$Ise[success])){
		ok = 1}else{
		ok=0
	}
    success$Ise <- success$Ise+ok
      
    if(all(initial.Ranking$W[success] == jack$W[success])){
		ok = 1}else{
		ok=0
	}
    success$W <- success$I+ok
    
    if(all(initial.Ranking$We[success] == jack$We[success])){
		ok = 1}else{
		ok=0
	}
    success$We <- success$We+ok
    
    if(all(initial.Ranking$Ws[success] == jack$Ws[success])){
		ok = 1}else{
		ok=0
	}
    success$Ws <- success$Ws+ok
    
    if(all(initial.Ranking$Wse[success] == jack$Wse[success])){
		ok = 1}else{
		ok=0
	}
    success$Wse <- success$Wse+ok      
  }
  
  success <- as.data.frame(success)
  
  success <- success/replicates*100

  return(success)
  
}
