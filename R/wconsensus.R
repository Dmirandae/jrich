#
#' @title   Wconsensus 
# status     :   beta version
# Copyright  :   GPL 3.x
# Author     :   Daniel Rafael Miranda Esquivel
# email      :   dmiranda@uis.edu.co
# updated    :   2014 Sep 25
#
#

#
# Main function
#
#' @description wconsensus calculates the weighted consensus as presented in Sharkey et al. (2013)
#' The initial trees are hierarchically and nested clustered, according to common nodes
#' and weighted in accordance to the clusters created.
#'
#' The function receives as input data the initial trees (as a multiphylo object) 
#' and returns a collapsed tree according to the calculated weights, the default cut
#' value is 0.5
#'
#' Usage:
#'
#' ConsensusTree <- wconsensus (trees)
#' calculates  the weights and collapses the maj rule consensus

#' ConsensusTree <- wconsensus (trees, collapse = FALSE)
#' calculates the weights BUT does not collapse the maj rule consensus

#' ConsensusTree <- wconsensus (trees, cutvalue = 0.65)
#' calculates  the weights and collapses the maj rule consensus using
#' a cut value of 0.65
#'


wconsensus <- function (InitialTrees, collapse = TRUE, 
                        cutvalue = 0.5001) {

RoundDigit <- 4


  #
  # Calculate the number of trees
  #
  NumTrees <- length(InitialTrees)

  #
  # Stop if there is only one tree
  #
  if (NumTrees == 1){
    stop("Just a single tree, nothing to calculate")
    }

  #
  # Calculate the max. number nodes
  #
  MaxNumberNodes <-  Ntip(InitialTrees[[1]])-1

  #
  # Assign vectors
  #
  LisTrees <- as.numeric(1:NumTrees)
  MatrizNodes <- matrix(NA,NumTrees,MaxNumberNodes)
  TreeWeight <- TreeGroup <-  vector0 <- mat.or.vec(1,NumTrees)

  #
  # Convert nodes to md5sum
  #
  for (i in 1:NumTrees){
    InitialTrees[[i]] <- makeNodeLabel(InitialTrees[[i]], "md5sum")

     #
     # Fill the matrix of nodes with md5sum values
     #
     for (j in 1:InitialTrees[[i]]$Nnode){
       MatrizNodes[i,j] <- InitialTrees[[i]]$node.label[j]
     }

   #
   # Initialize groups and weights
   #
   TreeWeight[i] <- 1
   TreeGroup[i]  <- 0
    }

  #
  # Create another vector
  #
  BasicNodes <- levels(as.factor(MatrizNodes))
  MatrizBasicNodes <- mat.or.vec(length(BasicNodes),NumTrees)

  #
  # The reference group
  #
  RefGroup <- 0

  #
  # Searching the partitions to create sub partitions
  #
    #
    # As I do not know beforehand the number of partitions
    # I use the max number of (possible) partitions
    # It is not optimal in terms of speed
    # but it guarantees that all groups will be checked
    #
    for (particion in 0:NumTrees){
      parar          <-  0
      GroupToExtract <-  particion
      GroupExtracted <-  LisTrees[TreeGroup == GroupToExtract]

      #
      # if the group has more than one tree
      #
      if(length(GroupExtracted) > 1){

        #
        # Check the nodes for the trees in the group extracted
        #
        for (nodo in 1:MaxNumberNodes){

          VectorExtracted   <-  MatrizNodes[GroupExtracted,nodo]
          LevelSecondVector <-  GroupAssigned <- VectorExtracted
          LevelsExtracted   <- levels(factor(VectorExtracted))

          if ((length(LevelsExtracted) > 1 ) & ( parar == 0 )){
            for (i in 1:length(LevelsExtracted)){ 
              LevelSecondVector                <-  VectorExtracted == LevelsExtracted[i]
              GroupAssigned[LevelSecondVector] <-  (i+RefGroup)
            }

          parar <- 1

          #
          # Assign weights and groups
          #
          TreeWeight[GroupExtracted]  <-  TreeWeight[GroupExtracted]/length(LevelsExtracted)
          RefGroup                    <-  RefGroup+length(LevelsExtracted)
          TreeGroup[GroupExtracted]   <-  GroupAssigned[]

          }


        if (parar == 1){break}

      }
    }
  }

    # Weights are standarized to avoid sums larger than 1
  
    TreeWeight <- TreeWeight/sum(TreeWeight) 
    
    #
    # Assign weights to groups
    #
    for (j in 1:length(BasicNodes)){
      for (i in 1:NumTrees){
       if (any(BasicNodes[j]  %in% InitialTrees[[i]]$node.label)){
         MatrizBasicNodes[j,i] <- TreeWeight[i]
       }
      }
    }

  #
  # Matrices for weights
  #
  WeightsBasicNodes <- mat.or.vec(1,length(BasicNodes))

    for (i in 1:length(BasicNodes)){
     WeightsBasicNodes[i] <- sum(MatrizBasicNodes[i,])
    }

  #
  # Calculate the maj rule tree and converts nodes to md5sum
  #
  MajTreeFinal <- consensus(InitialTrees, p=0.5001)
  MajTreeFinal <- makeNodeLabel(MajTreeFinal, "md5sum")

  #
  # Convert node labels to length, therefore we can collapse nodes
  #

    #
    # Initialize length
    #
    MajTreeFinal <-  compute.brlen(MajTreeFinal,1)

    # 
    # Assign labels to length
    #
    for (k in 1:length(MajTreeFinal$node.label)){
      for (j in 1:length(BasicNodes)){
        if (MajTreeFinal$node.label[k]==BasicNodes[j]) {

          #
          # Assign the weight as node value AND round it
          #
          MajTreeFinal$node.label[k]  <-  round(WeightsBasicNodes[j],RoundDigit)

         #
         # Routine to assign the right values as the structure of the tree in R 
         # Could be misleading 
         #
         NodeInUse                           <-  which(MajTreeFinal$edge[, 2] == k + Ntip(MajTreeFinal))
         MajTreeFinal$edge.length[NodeInUse] <-  WeightsBasicNodes[j]

        }
      }
    }


    #
    # If stated Collapse maj rule,  use cutvalue
    #
    if (collapse){
    MajTreeFinal <- di2multi(MajTreeFinal, tol = cutvalue+1e-5)
    }

  #
  # Return the majrule consensus with node values
  # 
  return(MajTreeFinal)
}
