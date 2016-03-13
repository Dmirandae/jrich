#'
#' @title Read distributions.
#'
#' @description Read distributions as a csv with two columns labeled "species" and "area"
#'
#' @param data.File a csv file to read, with two columns labeled "species" and "area"
#'
#' @return a data.frame object with the distribution by species 
#'
#'@author Miranda-Esquivel Daniel R.
#'
#'

Read.Data <-
function (data.File) {
    
    initial.Distribution <- read.csv(data.File,header=T,sep=" ")

    if (all(colnames(initial.Distribution)==c("species","area"))){
  
  final.Distribution1 <- table(initial.Distribution$species,initial.Distribution$area)
  
  final.Distribution2 <-as.data.frame.array(final.Distribution1)
  
  final.Distribution2$species <- levels(as.factor(initial.Distribution$species))
  
        return(final.Distribution2)
    }else
    {stop("wrong format in data file")}

}

