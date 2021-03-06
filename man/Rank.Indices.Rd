% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rank.Indices.R
\name{Rank.Indices}
\alias{Rank.Indices}
\title{Rank indices.}
\usage{
Rank.Indices(index.Value = index.Value)
}
\arguments{
\item{index.Value}{a table with indices values.}
}
\value{
a table with the decreasing order of the areas
It presents the ties alphabetically
}
\description{
Renk indices according to the areas' absolute position. If the index value is empty, the function assigns a dummy position "X0X"
}
\examples{
 ## get the library
 library(jrich)
 
 ## load the data
 data(tree) 
 data(distribution) 
 
Rank.Indices(Calculate.Index(tree=tree, distrib = distribution, verbose=FALSE))

}
\author{
Miranda-Esquivel Daniel R.
}

