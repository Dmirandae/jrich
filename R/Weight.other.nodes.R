#' Weight the tips that are sisters of internal nodes
#'
#' Input: a tree and a table of weights
#' Returns a distribution
#

Weight.other.nodes <-
function (tree=tree,matriz=matriz) {
  
  ## recorrer los tips para asignar pesos que no han sido asignados
  
  for (i in ((length(tree$tip)+tree$Nnode):(length(tree$tip)+1))){
    
    if((any(Children(tree=tree,node=i) <= length (tree$tip.label))) &
       (any(Children(tree=tree,node=i) >  length (tree$tip.label)))){
      matriz[1,(Children(tree=tree,node=i))] <- max(matriz[1,Children(tree=tree,node=i)]) 
      matriz[1,i] <- sum(matriz[1,Children(tree=tree,node=i)])  
      }else{
      matriz[1,i] <- sum(matriz[1,Children(tree=tree,node=i)])  
    } 
  }
  
  return(matriz[1,1:length(tree$tip)])
  
}
