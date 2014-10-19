
#####################################################################################
##                                                                                 ##
##                   A Jrich worked example                                        ## 
##                                                                                 ##
##   2. An example with two topologies and distributions                           ##
##                                                                                 ##
##                                                                                 ##
#####################################################################################

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
    jack.Ranking.100[[i]] <-  as.data.frame(Rank.Indices(Multi.Index.Calc(Multitaxon1,
                                                              jtip = 0.5,jtopol = 0.5)))
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

