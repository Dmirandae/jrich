#'
#' @title Children of an internal node.
#'
#' @description Get the children of an internal node in a tree.
#'
#' @param tree is a single tree with n terminals, an ape phylo object.
#' 
#' @param node representing the node in APE notation, an integer.
#'
#' @return The children nodes of the internal node; in most cases, two integers.
#'
#'
#' @examples
#'  library(jrich)
#'  
#'  data(tree)
#'
#' Children(tree,node=7)
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'


Children <-
function (tree=tree,node=node) {
  
  if (node <= length(tree$tip.label)){stop("The node is a terminal, we have no children here")}
  
  child <- tree$edge[c(which(tree$edge[,1] == node)),2]
  
  return(child)
}
