#'
#' All the sisters of a node
#' 
#' Input: a tree and a node
#' Returns the list of sisters

Sisters <-
function (tree=tree, node=node) {
  
  # get the ancest0r
  ancestro<-tree$edge[(tree$edge[,2] == node)][1]
  
  # get children of that ancestor
  sisters<-tree$edge[c(which(tree$edge[,1] == ancestro)),2]
  
  return(sisters)
}
