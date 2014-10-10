#'
#' Jack-knife indices in a single topology n times
#'
#' The function jack-knifes the terminals and
#' calculates the indices value n (=replicates) times.  
#' The function returns the success that correspond to  
#' obtain the same ranking for 1:3 positions.

Jack.Indices <-
function (tree = tree, distrib = distrib, jtopol = jtopol,
          replicates=replicates) {
          
  rank <- Rank.Indices(Calculate.Index(tree = tree,distrib = distrib))
  
  aciertos <- NULL
  
  aciertos$I <- aciertos$Ie <- aciertos$Is <- aciertos$Ise <- aciertos$W <- aciertos$We <- aciertos$Ws <- aciertos$Wse <-0
    
  for (i in 1:replicates){
    
    jack <- Rank.Indices(Jack.Index(tree, distrib, jtopol))
    
    if(all(rank$I[1:3] == jack$I[1:3])){
		ok = 1}else{
		ok=0
    }
    aciertos$I <- aciertos$I+ok
    
    if(all(rank$Ie[1:3] == jack$Ie[1:3])){
		ok = 1}else{
		ok=0
	}
    aciertos$Ie <- aciertos$Ie+ok
    
    if(all(rank$Is[1:3] == jack$Is[1:3])){
		ok = 1}else{
		ok=0
	}
    aciertos$Is <- aciertos$Is+ok
    
    if(all(rank$Ise[1:3] == jack$Ise[1:3])){
		ok = 1}else{
		ok=0
	}
    aciertos$Ise <- aciertos$Ise+ok
      
    if(all(rank$W[1:3] == jack$W[1:3])){
		ok = 1}else{
		ok=0
	}
    aciertos$W <- aciertos$I+ok
    
    if(all(rank$We[1:3] == jack$We[1:3])){
		ok = 1}else{
		ok=0
	}
    aciertos$We <- aciertos$We+ok
    
    if(all(rank$Ws[1:3] == jack$Ws[1:3])){
		ok = 1}else{
		ok=0
	}
    aciertos$Ws <- aciertos$Ws+ok
    
    if(all(rank$Wse[1:3] == jack$Wse[1:3])){
		ok = 1}else{
		ok=0
	}
    aciertos$Wse <- aciertos$Wse+ok      
  }
  
  aciertos <- as.data.frame(aciertos)
  
  return(aciertos)
  
}
