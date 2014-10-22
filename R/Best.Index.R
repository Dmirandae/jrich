#'
#' Jack-knife indices in a single topology m times and evaluates a 
#' success rule.
#'
#' The function jack-knifes the terminals and
#' calculates the indices value m (=replicates) times.  
#' The function returns the success that correspond to  
#' obtain the same ranking for X,Y positions, established as the vector
#' success (by default success is 1:3 positions)).
#'

Best.Index <-
function (tree = tree, distrib = distrib, jtip = jtip,
          replicates = replicates, success = (c(1:3)) ) {
          
  rank <- Rank.Indices(Calculate.Index(tree = tree,distrib = distrib))
  
  aciertos <- NULL
  
  aciertos$I <- aciertos$Ie <- aciertos$Is <- aciertos$Ise <- aciertos$W <- aciertos$We <- aciertos$Ws <- aciertos$Wse <-0
    
  for (i in 1:replicates){
    
    jack <- Rank.Indices(Calculate.Index(tree = tree, distrib = distrib, jtip))
    
    if(all(rank$I[success] == jack$I[success])){
		ok = 1}else{
		ok=0
    }
    
    aciertos$I <- aciertos$I+ok
    
    if(all(rank$Ie[success] == jack$Ie[success])){
		ok = 1}else{
		ok=0
	}
    aciertos$Ie <- aciertos$Ie+ok
    
    if(all(rank$Is[success] == jack$Is[success])){
		ok = 1}else{
		ok=0
	}
    aciertos$Is <- aciertos$Is+ok
    
    if(all(rank$Ise[success] == jack$Ise[success])){
		ok = 1}else{
		ok=0
	}
    aciertos$Ise <- aciertos$Ise+ok
      
    if(all(rank$W[success] == jack$W[success])){
		ok = 1}else{
		ok=0
	}
    aciertos$W <- aciertos$I+ok
    
    if(all(rank$We[success] == jack$We[success])){
		ok = 1}else{
		ok=0
	}
    aciertos$We <- aciertos$We+ok
    
    if(all(rank$Ws[success] == jack$Ws[success])){
		ok = 1}else{
		ok=0
	}
    aciertos$Ws <- aciertos$Ws+ok
    
    if(all(rank$Wse[success] == jack$Wse[success])){
		ok = 1}else{
		ok=0
	}
    aciertos$Wse <- aciertos$Wse+ok      
  }
  
  aciertos <- as.data.frame(aciertos)
  
  aciertos <- aciertos/replicates*100

  return(aciertos)
  
}
