
############################################################################################################
##                                                                                                        ##
##            A Jrich worked example to reproduce figure 1 in Miranda-Esquivel (2015)                     ##
##                                                                                                        ##
############################################################################################################

##
## DRME
## 2014 10 09
##

# 
# ## Remove everything
# 
# 
rm(list=ls())
# 
# ## If there is an open graphic device, close it
# 
#
if (dev.cur()!=1){dev.off()}
# 
# ## Get the latest version of the library, from GitHub
# 
# 
library("devtools")
# 
# 
install_github("Dmirandae/jrich")


## Load library

library(jrich)

## set working directory to R Data
## You must change it to your own directory

#
setwd("~/Dropbox/papers/Jrich-R/jrich/test/")



##########


## read data

## read tree 1
## The tree is in newick format

tree.figure1 <- read.tree ("figure1.tre")

## You might want plot the tree

#
plot(tree.figure1)

## read distributions using species area format

## the distibutions could a csv 
## each line has a species name and an area
## multiple areas for the same species means 
## multiple lines 

## the function creates a data frame

distrib.figure1 <- Read.Data("figure1.csv")

distrib.figure1

class(distrib.figure1)



initial.Values <-  Jack.Index(tree=tree.figure1, distrib = distrib.figure1,verbose=TRUE)


##
## Note that the figures for Is/Ws indices  here are different from figure 1 in DRME 2015 as
## here are re-scaled to sum 1, but the proportions are exactly the same.
##

## Plot the initial Values, for the Index that explains the most

library(ggplot2)


## 1. Correlations between values

correlations <-  cor(initial.Values[,2:10],initial.Values[,2:10])


## to avoid the highest "autocorrelation"

diag(correlations) <- 0.0


## 'Best' descriptor 

best.Index <-   which.max(apply(correlations,2,sum))


## richness in not a good predictor for all indices  

which(abs(correlations[,"rich"])==max(abs(correlations[,"rich"])))


## 2. The index that explains the most is

best.Index <-   which.max(apply(correlations,2,sum))

best.Index

qplot(initial.Values$area,initial.Values[,names(best.Index)], xlab = "Areas", 
      ylab =paste(names(best.Index)," values"), main = paste("Figure 1, ",names(best.Index)," Index"))


## In this example Areas A / F / G / H have the same ranking, as area A harbors species I, while
## areas F / G / H have the highest absolute richness



## Jack-knife with a jtip value of 0.5, 100 replicates


  jack.Ranking.100 <- list()

  for (i in 1:100){
    print(paste("replicate #",i))
  
    jack.Ranking.100[[i]] <-  as.data.frame(Rank.Indices(Jack.Index(tree=tree.figure1, distrib = distrib.figure1,verbose=FALSE,jtip = 0.5)))
    
  }



##Compare the whole ranking for THE best index, 100 times

initial.Ranking <-  as.data.frame(Rank.Indices(initial.Values))

jack.Ranking.100.comparison <- NULL

for (i in 1:100){
  jack.Ranking.100.comparison[i] <- all.equal(initial.Ranking[,best.Index], 
                                              jack.Ranking.100[[i]][,best.Index])
}


## hits
length(which(jack.Ranking.100.comparison==TRUE))

## error
length(which(as.data.frame(jack.Ranking.100.comparison)!=TRUE))

##
## These two figues indicate that THE best index is not I
##


## counting the  error

jack.Mismatch <- jack.Ranking.100.comparison[jack.Ranking.100.comparison!=TRUE]

count.Jack.Mismatch <- gsub(" string mismatches","",jack.Mismatch)

count.Jack.Mismatch <- as.numeric(count.Jack.Mismatch) 

## the distribution of the error, not so bell shaped
hist(sort(count.Jack.Mismatch))

