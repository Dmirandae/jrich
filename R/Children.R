
#'
#' children of a node
#'
#' Get the children of a node in a tree
#'

Children <-
function (tree=tree,node=node) {

  child <- tree$edge[c(which(tree$edge[,1] == node)),2]
  
  return(child)
}
