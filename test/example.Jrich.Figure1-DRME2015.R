
#####################################################################################
##                                                                                 ##
##                   A Jrich worked example                                        ## 
##                                                                                 ##
##   1. An example with a single topology and distribution                         ##
##      to reproduce Figure 1 in Miranda-Esquivel (2014)                           ##
##                                                                                 ##
#####################################################################################

##
## DRME
## 2014 10 22
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

## or you can upload the data set from data

data(tree)


## You might want plot the tree

#
plot(tree.figure1)

## read distributions using species area format

## the distibutions could a csv 
## each line has a species name and an area
## multiple areas for the same species means 
## multiple lines 

## the function creates a data frame

distrib.figure1 <- Read.Data("endemism.csv")

distrib.figure1

class(distrib.figure1)


## And the initial Index calculation, with a verbose output


initial.Values <-  Calculate.Index(tree=tree.figure1, distrib = distrib.figure1,verbose=T)

initial.Values 

##
## Note that the figures for Is/Ws indices are different from Figure 1 in DRME 2014 as
## here are re-scaled to sum 1, but the proportions are exactly the same.
##
## To obtain the same figures for Is/Ws indices as Figure 1 in DRME 2014, 
## you must use
##

figure1.Values <-  Calculate.Index(tree=tree.figure1, 
                                   distrib = distrib.figure1,verbose=F,standard = "tree")

figure1.Values

all.equal(initial.Values,figure1.Values)



## Plot the initial Values, for the Index that "explains the most"

library(ggplot2)


## 1. Correlations between values

correlations <-  cor(initial.Values[,2:10],initial.Values[,2:10])


## to avoid the highest "autocorrelation"

diag(correlations) <- 0.0


## 'Best' descriptor 

best.Index <-   which.max(apply(correlations,2,sum))


## richness in not a good predictor for all indices  

which(abs(correlations[,"rich"])==max(abs(correlations[,"rich"])))


## 2. The index that "explains the most" (without resampling) is

best.Index <-   which.max(apply(correlations,2,sum))

best.Index

qplot(initial.Values$area,initial.Values[,names(best.Index)], xlab = "Areas", 
      ylab =paste(names(best.Index)," values"), main = paste("Figure.  ",
                                                             names(best.Index)," Index"))


## In this example Areas A / F / G / H have the same ranking, as area A harbors species I, while
## areas F / G / H have the highest absolute richness



## A single Jack-knife replicate with a jtip value of 0.5

jack.Values <-  Calculate.Index(tree=tree.figure1, distrib = distrib.figure1,jtip = 0.5)


## The absolute difference between these two outputs

all.equal(initial.Values, jack.Values)


## 2. But a single replicate is not interesting, therefore we repeat the process 100 times, using two approaches


## 2.1 Jack-knife with a jtip value of 0.5, 100 replicates, using Calculate.Index function


jack.Ranking.100 <- list()

for (i in 1:100){
  print(paste("replicate #",i))
  
  jack.Ranking.100[[i]] <-  as.data.frame(Rank.Indices(Calculate.Index(tree=tree.figure1, 
                                           distrib = distrib.figure1,
                                           verbose=FALSE,jtip = 0.5)))
  
}



##Compare the whole ranking for THE best index, 100 times

initial.Ranking <-  as.data.frame(Rank.Indices(initial.Values))

jack.Ranking.100.comparison <- NULL

for (i in 1:500){
  
  if(!all(jack.Ranking.100[[i]][,best.Index] == "X0X")){
    jack.Ranking.100.comparison[i] <- all.equal(initial.Ranking[,best.Index], 
                                                  jack.Ranking.100[[i]][,best.Index])  
  }else jack.Ranking.100.comparison[i] <- 0
  
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
hist(sort(count.Jack.Mismatch,na.last = NA))


# 2.2 a wrap to the  previuous function, and evaluating the number of times we recover 1/2/3 position in the ranking.
## note that Calculate.Index recovers the index values while Best.Index recovers the ranking comparison

jack.figure1.jtip05.100replicates <- Best.Index(tree=tree.figure1, distrib = distrib.figure1,
                                                jtip = 0.5, replicates = 500, success = c(1:2))

jack.figure1.jtip05.100replicates

best.Index = names(jack.figure1.jtip05.100replicates)[c(which(jack.figure1.jtip05.100replicates == 
                                                    max(jack.figure1.jtip05.100replicates)))]


## W / Ws explains better than I, as they have a jack-knife value. In this context we can plot Ws

for (i in 1:( length(best.Index) )){

print(best.Index[i])

print(qplot(initial.Values$area,initial.Values[,best.Index[i]], xlab = "Areas", 
      ylab =paste(best.Index[i]," values"), main = paste("Figure.  ",best.Index[i]," Index")))

}

# Areas F/G/H have a higher value as they have higher richness, but even so, the support is relatively low


