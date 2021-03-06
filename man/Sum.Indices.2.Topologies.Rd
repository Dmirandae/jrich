% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sum.indices.2.topologies.R
\name{Sum.Indices.2.Topologies}
\alias{Sum.Indices.2.Topologies}
\title{Sums two tables with indices values.}
\usage{
Sum.Indices.2.Topologies(indices1 = indices1, indices2 = indices2)
}
\arguments{
\item{indices1}{a table of indices values.}

\item{indices2}{a table of indices values.}
}
\value{
a single table with the sum of the two indices.
}
\description{
You input two tables with indices 
and it function returns a single table with the sum.
}
\examples{

 ## get the library
 library(jrich)
 
 ## load the data
 data(Multitaxon1) 
 
 ## calculate indices for two trees and their distributions
 temp.Index.Value1 <- Calculate.Index(tree = Multitaxon1[[1]][[1]],
                                      distribution = Multitaxon1[[1]][[2]],0)

 temp.Index.Value2 <- Calculate.Index(tree = Multitaxon1[[2]][[1]],
                                      distribution = Multitaxon1[[2]][[2]],0)
 
 ## sum the indices values
 Sum.Indices.2.Topologies(temp.Index.Value1, temp.Index.Value2)
 

}
\author{
Miranda-Esquivel Daniel R.
}

