
##############################################################################
##                                                                          ##
##                        A Jrich worked example                            ##
##                                                                          ##
##############################################################################

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

  tree.Puranius <- read.tree ("puranius.tre")

## You might want plot the tree

  #plot(tree)

## read distributions using species area format

## the distibutions could a csv 
## each line has a species name and an area
## multiple areas for the same species means 
## multiple lines 

## the function creates a data frame

  distrib.Puranius <- Read.Data("puranius.csv.gz")


## You could read the distribution as a table in any format,  
## and reshape it to the accepted format as a data frame
## e.g.
## B C D F G H I K M N species
## 1 0 0 0 0 0 0 0 0 0 1       1
## 2 0 0 0 0 0 0 0 1 0 0       2
## 3 0 0 0 1 0 0 0 0 0 0       3
## 4 0 0 0 0 1 1 1 0 0 0       4
##

  class(distrib.Puranius)



## Default values
## Here We use the generic function Jack.Index with two parameters tree and distrib.
## The default value is 0 Jack (nothing is deleted) 

  initial.Values <-  Jack.Index(tree=tree.Puranius, distrib = distrib.Puranius,verbose=TRUE)



##Plot the initial Values, for the Index that explains the most
 
  library(ggplot2)


## plotting as example, the index value that explains the most:


## 1. Correlations between values
 
  correlations <-  cor(initial.Values[,2:10],initial.Values[,2:10])


## to avoid the highest "autocorrelation"

  diag(correlations) <- 0.0



## richness in not a good predictor for all indices  

  which(abs(correlations[,"rich"])==max(abs(correlations[,"rich"])))


## 2. The index that explains the most is

  best.Index <-   which.max(apply(correlations,2,sum))

  best.Index

  library(ggplot2)

  qplot(initial.Values$area,initial.Values[,names(best.Index)], xlab = "Areas", 
        ylab =paste(names(best.Index)," values"), main = paste(names(best.Index)," Index"))



## A single Jack-knife replicate with a jtip value of 0.5

  jack.Values <-  Jack.Index(tree=tree.Puranius, distrib = distrib.Puranius,jtip = 0.5)


## The absolute difference between these two outputs

  all.equal(initial.Values, jack.Values)


## But a single replicate is not intersting, therefore we repeat the process 100 times, using a wrap to the 
## previuous function, and evaluating the number of times we recover 1/2/3 position in the ranking.
## note that Jack.Index recovers the index values while Jack.Indices recovers the ranking comparison

  jack.Puranius.jtip05.100replicates <- Jack.Indices(tree=tree.Puranius, distrib = distrib.Puranius,jtopol = 0.5, replicates = 100)

  jack.Puranius.jtip05.100replicates


################################      Multiple Taxa       #################################################################

## A list to join Tree and distribution
## This could be made with a function but I prefer this approach 

  data.Puranius  <- list()

  data.Puranius[[1]] <- tree.Puranius
  data.Puranius[[2]] <- distrib.Puranius

  data.Puranius



## Tree and distribution for an hypotethical Taxon: Janus

  tree.Janus <- read.tree ("Janus.tre")

  distrib.Janus <- Read.Data("Janus.csv.gz")

  data.Janus  <- list()

  data.Janus[[1]] <- tree.Janus
  data.Janus[[2]] <- distrib.Janus


## A list to handle multiple datasets

  Multitaxon1 <- list()

  Multitaxon1[[1]] <- data.Janus
  Multitaxon1[[2]] <- data.Puranius

  Multitaxon1

## Initial ranking for Multitaxon

## Default values

  initial.Values.Multi <-  Multi.Index.Calc(Multitaxon1)


##Plot the initial Values, for the Index that explains the most

  library(ggplot2)


## plotting as example, the index value that explains the most:


## 1. Correlations between values

correlations.Multi <-  cor(initial.Values.Multi[,2:10], initial.Values.Multi[,2:10])

##### names(initial.Values.Multi)[c(-1,-11,-12)]

## to avoid the highest "autocorrelation"

  diag(correlations.Multi) <- 0.0


## 2. The index that explains the most is

best.Index.Multi <-   which.max(apply(correlations.Multi,2,sum))

qplot(initial.Values.Multi$area,initial.Values.Multi[,names(best.Index.Multi)], xlab = "Areas", ylab =names(best.Index.Multi), main = "Index value")



##3. A  delete experiment.  prob jtip jtopol 0.5 

  jack.Multi <-  Multi.Index.Calc(Multitaxon1,jtip = 0.5,jtopol = 0.5) 

## As you can see the jack topol and jtip present the deleted proportions, and the twoo data.frames are different 


  all.equal(jack.Multi,initial.Values.Multi)

vector <-   which.max(apply(correlations,2,sum))


## Jack Ranking Multitaxon 100 times

jack.Ranking.100 <- list()

  for (i in 1:100){
    print(paste("replicate #",i))
    jack.Ranking.100[[i]] <-  as.data.frame(Rank.Indices(Multi.Index.Calc(Multitaxon1,jtip = 0.5,jtopol = 0.5)))
  }

## In the first run we got the ranking 

  jack.Ranking.100[[1]][,vector]


## convert the initial values to a ranking


# initial ranking for Multitaxon

initial.Ranking <-  as.data.frame(Rank.Indices(Multi.Index.Calc(Multitaxon1)))


initial.Ranking[,vector]

##Compare the whole ranking for THE best index, 100 times


jack.Ranking.100.comparison <- NULL

for (i in 1:100){
  jack.Ranking.100.comparison[i] <- all.equal(initial.Ranking[,vector], 
                                              jack.Ranking.100[[i]][,vector])
  }


## hits
length(which(jack.Ranking.100.comparison==TRUE))

## error
length(which(as.data.frame(jack.Ranking.100.comparison)!=TRUE))


## type of error

 jack.Mismatch <- jack.Ranking.100.comparison[jack.Ranking.100.comparison!=TRUE]

 count.Jack.Mismatch <- gsub(" string mismatches","",jack.Mismatch)

  count.Jack.Mismatch <- as.numeric(count.Jack.Mismatch) 

## the distribution of the error, rather bell shaped
  hist(sort(count.Jack.Mismatch))
