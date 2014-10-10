#'
#' W index value for a single topology
#'
#' This function assigns the weight according to the ramification patters 
#' (see Van-Wright et al., 1981). 
#' The input tree is reordered in post order.
#' 
#' Returns a vector with weights.
#'
#' @examples
#' # indexw <- IndexW(tree)
#' # newTree <- tree
#' # newTree$Labels  <- indexw
#' # plot(newTree)


IndexW <- function (tree=tree) {
  
  matriz  <-  matrix(0,nrow=1,ncol=(length(tree$tip.label)*2-1))
  
  tree <- reorder.phylo(tree,order="postorder")
  
  raiz <-length(tree$tip.label)+1
  
  matriz[1,raiz] <- 1
  
  hijos        <- Children(tree=tree,node=raiz)
  
  matriz[1,hijos] <- matriz[1,raiz]
  
  for (i in (((length(tree$tip.label)+2)):(length(tree$tip.label)+tree$Nnode))){

    matriz[1,i]     <-  matriz[1,Ancestor(tree=tree,node=i)]+1 
    hijos           <-  Children(tree=tree,node=i)
    matriz[1,hijos] <-  matriz[1,i]
        
  }
  
  matriz <- matriz[1,1:length(tree$tip.label)]
  matriz <- sum(matriz)/matriz
  matriz <- matriz/min(matriz)

  return(matriz)
}
