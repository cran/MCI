\name{var.zetas}
\alias{var.zetas}

\title{
Zeta-squared transformation
}
\description{
Zeta-squared transformation for MCI input variables
}
\usage{
var.zetas(x)
}

\arguments{
  \item{x}{
a \code{numeric} vector
}
}
\details{
The function applies a zeta-squared transformation to a numeric vector. The transformation can be used e.g. to transform interval scale information (such as scoring in consumer surveys) to positive ratio scales.
}
\value{
A \code{numeric} vector containing the zeta-squared transformation of the input vector
}
\references{
Cooper, L.G./Nakanishi, M. (1983): \dQuote{Standardizing Variables in Multiplicative Choice Models}. In: \emph{Journal of Consumer Research}, \bold{10}, 1, p. 96-108.
}
\author{
Thomas Wieland
}
\examples{
numvec <- -5:5
# a vector containing zero and negative values
var.zetas(numvec)
# returns a vector only with positive values 
# (zeta-squared transformation)
}