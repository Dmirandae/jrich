#'
#' Read distributions as a csv with two columns species and area
#' 

Read.Data <-
function (data.File) {
    
  initial.Distribution <- read.csv(data.File,header=T,sep=" ")
  
  final.Distribution1 <- table(initial.Distribution$species,initial.Distribution$area)
  
  final.Distribution2 <-as.data.frame.array(final.Distribution1)
  
  final.Distribution2$species <- levels(as.factor(initial.Distribution$species))
  
  return(final.Distribution2)

}
