#'
#' @title Sums two tables with indices values
#' 
#' @description You input two tables with indices 
#' and it function returns a single table with the sum.
#' 
#' @param indices1
#' 
#' @param indices2
#' 
#' @return a single table with the sum of the two indices.
#' 
#'
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'




Sum.Indices.2.Topologies <-
function (indices1=indices1, indices2=indices2) {
    
  if((indices1[1,12] != indices2[1,12]) | 
     (indices1[1,13] != indices2[1,13])){
    return("You are trying to combine two tables with different jtip/jtop values")
    stop()
  }
  
  all.Areas <- (sort(union(indices1$area, indices2$area)))

  table.Sum <- as.data.frame(matrix(data=0,nrow=length(all.Areas),ncol=13))
  names(table.Sum) <- c("area","I","Ie","Is","Ise","W","We","Ws","Wse","rich","endem","jtopol","jtip")
  
  table.Sum$area <- all.Areas
  
  for (i in 1: length(all.Areas)){
    x1<- indices1[which(indices1$area == all.Areas[i]),]
      if(length(x1[,1]) < 1) {x1[1,-1] <- (rep(0,12))}
 
    x2 <- indices2[which(indices2$area == all.Areas[i]),]
      if (length(x2[,1]) < 1) {x2[1,-1] <- (rep(0,12))}
    
    x3  <-   Reduce("+",list(x1[1,2:11],x2[1,2:11]))

    table.Sum[table.Sum$area == all.Areas[i],2:11]  <- x3

    #!table.Sum[table.Sum$area == all.Areas[i],12] <- indices1[i,12]
    #!table.Sum[table.Sum$area == all.Areas[i],13] <- indices1[i,13] 
  
  }
  
  table.Sum[,12] <- x1[1,12]
  table.Sum[,13] <- x1[1,13]
  
  table.Sum[table.Sum=="NaN"] <- 0

 return(table.Sum)  
}
