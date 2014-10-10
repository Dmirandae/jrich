#' All the sisters of a node are tips?
#'
#' Input: a tree and a node
#' Return a boolean

Sisters.tip <-
function (tree=tree, node=node) {
  ##
  ## recibe un arbol y un nodo y reporta si todos los hijos del ANCESTRO de ese nodo
  ## son tips
  ##  
  
  if (all(Sisters(tree,node) <= length(tree$tip.label))) {
    return(TRUE)
  }
  else{
    return(FALSE)
  }  
  
}
