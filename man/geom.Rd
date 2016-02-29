\name{geom}
\alias{geom}

\title{
Geometric mean
}
\description{
Computes the geometric mean of a numeric vector.
}
\usage{
geom(x)
}

\arguments{
  \item{x}{
A numeric vector
}
}

\value{
The value of the geometric mean. 
}

\author{
Thomas Wieland
}

\examples{
numvec <- c(10,15,20,25,30)  
# Creates a numeric vector "numvec"
mean(numvec)  
# Mean of numvec
geom(numvec)  
# Geometric mean of numvec
}