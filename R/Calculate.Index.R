#'
#' Indices values for a single topology
#'
#' The funtion calculates standard indices I and W,
#' along with Posadas et al. (2001) modifications.
#'


Calculate.Index <-
function (tree=tree, distrib=distrib) {
  
  #!!   tree <- tree.replica
  #!!   distrib<-puranius.distrib2
  
  ##! get errors on trees / distributions
  ##! names and numbers
  
  if (names(distrib)[length(names(distrib))] == "especie"){
      names(distrib)[length(names(distrib))] <- "species"
  }
  
  if (length(tree$tip.label) == length(distrib$species)){
    if (all(tree$tip.label[order(tree$tip.label)] == distrib$species[order(distrib$species)])){
    }else{
          stop("distributions and tree(s) MUST have the same names for species and terminals")
         } 
  }else{
      stop("distributions and tree(s) MUST have the same number of species and terminals")    
       }
   
  filas<-length(names(distrib))-1
  resultados <-as.data.frame(matrix(data=0,nrow=filas,ncol=13))
  names(resultados)<-c("area","I","Ie","Is","Ise","W","We","Ws","Wse","rich","endem","jtopol","jtip")
  resultados$area <- names(distrib)[names(distrib)!="species"]

  
  ##!
  ##! si arboles es multiphylo hacer el calculo por arbol, sumar los indices y promediar
  ##! como los arboles no tienen (o si?) la misma secuencia de terminales
  ##! ordenarlos por terminales
  ##!

  tree <- reorder.phylo(tree,order="postorder")
  
  W <- IndexW(tree=tree)
  I <- IndexI(tree=tree)
  
  names(I) <- names(W) <- tree$tip.label
  
  match <- match(tree$tip.label,distrib$species)
  
  distrib<- distrib[match,]
  
  distrib<- distrib[,names(distrib)!="species"]

  resultados$rich <-  apply(distrib,2,sum)


	endemicity <- function (x) {
		sum(x)
		if (sum(x)==1){
		return(1)}else{
			return(0)
		} 
	}


	resultados$endem <-  apply(distrib,2,endemicity)
	
	
  indiceI.areas <- I*distrib
  indiceW.areas <- W*distrib
  
  resultados$I <-  apply(indiceI.areas,2,sum)
  resultados$W <-  apply(indiceW.areas,2,sum)
  
  resultados$Is <- resultados$I/sum(resultados$I)
  resultados$Ws <- resultados$W/sum(resultados$W)  
  
  
  funct <- function(x) x/sum(x)
  
  indices2 <-t(apply(distrib,1,funct))
  
  indiceIe.areas <- I*indices2
  indiceWe.areas <- W*indices2
  
  resultados$Ie <-  apply(indiceIe.areas,2,sum)
  resultados$We <-  apply(indiceWe.areas,2,sum)
  
  resultados$Ise <- resultados$Ie/sum(resultados$Ie)  
  resultados$Wse <- resultados$We/sum(resultados$We)  
  
  
  resultados[,c(2:13)] <- round(resultados[,c(2:13)],2)
  return(resultados)
}
