\name{huff.decay}
\alias{huff.decay}

\title{
Distance decay function in the Huff model
}
\description{
This function estimates a distance decay function from observed data and compares different function types
}
\usage{
huff.decay(dataset, x, y, plots = FALSE)
}

\arguments{
  \item{dataset}{
A \code{data.frame} containing the observed interaction data and the transport costs 
}
  \item{x}{
A numeric vector containing the independent variable, the transport costs (e.g. traveling time or street distance)
}
  \item{y}{
A numeric vector containing the dependent variable, the interaction measure (e.g. local market shares, trip volume, visitors per capita) 
}
  \item{plots}{
logical argument that indicates if the curves are plotted
}
}
\details{
The \emph{distance decay function} is a classic concept in quantitative economic geography and describes the relationship between transport costs and trip volume between origins (\eqn{i}) and a destination (\eqn{j}). The dependent variable is an indicator of trip volume, such as local market shares or visitors per capita etc., which are explained by the transport costs between all \eqn{i} and the destination \eqn{j}, \eqn{d_{ij}}. The non-linear modeling of transport costs is also a key concept of the \emph{Huff model} (see the function \code{huff.shares}). This function estimates and compares different types of possible \emph{distance decay functions} (linear, power, exponential, logistic) based on observed interaction data.  
}

\value{
A \code{data.frame} containing the function parameters (\code{Intercept}, \code{Slope}), their p values in the regression function (\code{p Intercept}, \code{p Slope}) and fitting measures (\code{R-Squared}, \code{Adj. R-Squared}). Additionally, a plot of the four estimated functions and the observed data.
}

\references{
Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Isard, W. (1960): \dQuote{Methods of Regional Analysis: an Introduction to Regional Science}. Cambridge.

Kanhaeusser, C. (2007): \dQuote{Modellierung und Prognose von Marktgebieten am Beispiel des Moebeleinzelhandels}. In: Klein, R./Rauh, J. (eds.): \emph{Analysemethodik und Modellierung in der geographischen Handelsforschung}. \emph{Geographische Handelsforschung}, \bold{13}. Passau. p. 75-110.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

}

\author{
Thomas Wieland
}

\seealso{
\code{\link{huff.shares}}, \code{\link{huff.attrac}}, \code{\link{huff.fit}}, \code{\link{mci.fit}}
}

\examples{
data(ce)
# Loads the data
huff.decay (ce[ce$store_code=="E04",], "traveltime", "ms_obs")
# Distance decay function for the store E01 in dataset ce
# returns the model results as data frame and plot
}