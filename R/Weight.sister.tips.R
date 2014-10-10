#' Weight the tips that are sisters of tips
#'
#' Input: a tree and a table of weights
#' Returns a distribution
#

Weight.sister.tips <-
function (tree=tree, matriz=matriz) {
  
  ## llena de 1 la matriz de pesos iniciales 
  ## para terminales hermanas
  
  for (node in 1:length(tree$tip.label)){
    if ((node <= length(tree$tip.label)) & Sisters.tip(tree,node)){
      matriz[1,node]  <- 1
   
      #! ancestro           <-    tree$edge[(tree$edge[,2] == node)][1]
      #! matriz[1,ancestro] <-    matriz[1,ancestro]+1
      #! print(c(node,ancestro))
      
    }
  }
  return(matriz)
}
