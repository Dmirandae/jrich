
#'
#' Jack-knife indices in n topologies one time
#'
#' The function calculates the indices values for a MultiData list 
#' one time 
#'

Multi.Index.Calc <-
function (MultiData = MultiData, jtip = 0, jtopol = 0) {
  
  for (i in 1:length(MultiData)){    
    if (jtopol > runif(1)){
      temp.Index.Value <- Jack.Index(tree = MultiData[[i]][[1]],
                                     distrib = MultiData[[i]][[2]], jtip)
    }else{
      temp.Index.Value <- Jack.Index(tree = MultiData[[i]][[1]],
                                     distrib = MultiData[[i]][[2]], 0)
      temp.Index.Value$jtip <- abs(jtip)
    }
    
    if (i == 1){
      def.Index.Value <- temp.Index.Value 
    }else{
      def.Index.Value <-  Sum.Indices.2.Topologies(temp.Index.Value, def.Index.Value)
    }
  }
  
  def.Index.Value$jtopol <-  abs(jtopol)
  
  return(def.Index.Value)
}
