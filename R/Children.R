
#'
#' @title Children of a node.
#'
#' @description Get the children of a node in a tree.
#'
#' @param tree is a single tree
#' @param node is an integer, representing the node in APE notation
#'
#' @return The children nodes of the internal node; in most cases, two integers.
#'


Children <-
function (tree=tree,node=node) {

  child <- tree$edge[c(which(tree$edge[,1] == node)),2]
  
  return(child)
}
