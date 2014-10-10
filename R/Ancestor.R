#'
#' Ancestor of a node
#'
#' Input: a tree and a node
#' Returns the ancestor of that node
#'

Ancestor <- function (tree=tree,node=node) {
  
  ancestro <- tree$edge[(tree$edge[,2] == node)][1]
  
  return(ancestro)
  
}
