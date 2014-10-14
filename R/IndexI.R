#'
#' I index value for a single topology
#'
#' This function assigns the same weight to sister clades
#' (see Van-Wright et al., 1981). 
#' The input tree is reordered in post order.
#' 
#' Returns a vector with weights.
#'
#' @examples
#'  indexi <- IndexI(tree)
#'  newTree <- tree
#'  newTree$Labels  <- indexi
#'  plot(newTree)


IndexI <-function (tree=tree) {
    
  matriz = matrix(0,nrow=1,ncol=(length(tree$tip.label)*2-1))
   
  tree <- reorder.phylo(tree,order="postorder")
  
#! pares de tips como 1
  matriz <- Weight.sister.tips(tree,matriz)
  
  #! completado
  matriz <- Weight.other.nodes(tree,matriz)
    #! revisar si es mejor escribir un data frame 
    #! con los nombres de las especies y sus pesos
  
  return(matriz)
}
