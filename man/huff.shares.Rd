\name{huff.shares}
\alias{huff.shares}
\title{
Huff model market share/market area simulations
}
\description{
Calculating market areas/local market shares using the probabilistic market area model by Huff
}
\usage{
huff.shares(huffdataset, origins, locations, attrac, dist, gamma = 1, lambda = -2, 
atype = "pow", dtype = "pow", gamma2 = NULL, lambda2 = NULL, check_df = TRUE)
}

\arguments{
  \item{huffdataset}{
an interaction matrix which is a \code{data.frame} containing the origins, locations and the explanatory variables (attractivity, transport costs)
}
  \item{origins}{
the column in the interaction matrix \code{huffdataset} containing the origins (e.g. ZIP codes)
}
  \item{locations}{
the column in the interaction matrix \code{huffdataset} containing the locations (e.g. store codes)
}
  \item{attrac}{
the column in the interaction matrix \code{huffdataset} containing the attractivity variable (e.g. sales area)
}
  \item{dist}{
the column in the interaction matrix \code{huffdataset} containing the transport costs (e.g. travelling time or street distance)
}
  \item{gamma}{
a single numeric value of \eqn{\gamma} for the exponential weighting of the attractivity variable (default: 1)
}
  \item{lambda}{
a single numeric value of \eqn{\lambda} for the (exponential) weighting of distance (transport costs, default: -2)
}
  \item{atype}{
Type of attractivity weighting function: \code{atype = "pow"} (power function), \code{atype = "exp"} (exponential function) or \code{atype = "logistic"} (default: \code{atype = "pow"})
}
  \item{dtype}{
Type of distance weighting function: \code{dtype = "pow"} (power function), \code{dtype = "exp"} (exponential function) or \code{dtype = "logistic"} (default: \code{dtype = "pow"})
}
  \item{gamma2}{
if \code{atype = "logistic"} a second \eqn{\gamma} parameter is needed
}
  \item{lambda2}{
if \code{dtype = "logistic"} a second \eqn{\lambda} parameter is needed
}
  \item{check_df}{
logical argument that indicates if the given dataset is checked for correct input, only for internal use, should not be deselected (default: \code{TRUE})
}
}
\details{
The \emph{Huff model} (Huff 1962, 1963, 1964) is the most popular \emph{spatial interaction model} for retailing and services and belongs to the family of \emph{probabilistic market area models}. The basic idea of the model, derived from the \emph{Luce choice axiom}, is that consumer decisions are not deterministic but probabilistic, so the decision of customers for a shopping location in a competitive environment cannot be predicted exactly. The results of the model are probabilities for these decisions, which can be interpreted as market shares of the regarded locations (\eqn{j}) in the customer origins (\eqn{i}), \eqn{p_{ij}}. The model results can be regarded as an equilibrium solution (consumer equilibrium) with logically consistent market shares (0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). From a theoretical perspective, the model is based on an utility function with two explanatory variables ("attractivity" of the locations, transport costs between origins and locations), which are weighted by an exponent: \eqn{U_{ij}=A_{j}^\gamma d_{ij}^{-\lambda}}. But the weighting functions can also be exponential or logistic. This function computes the market shares from a given interaction matrix and given weighting parameters. The result matrix can be processed by the function \code{shares.total()} to calculate the total values (e.g. annual sales) and shares.
}
\value{
Returns the input interaction matrix including the calculated shares (\code{p_ij}) as \code{data.frame}.
}
\references{
Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{huff.attrac}}, \code{\link{huff.fit}}, \code{\link{huff.decay}}
}
\examples{
data(Freiburg1)
data(Freiburg2)
# Loads the data

huff.shares (Freiburg1, "district", "store", "salesarea", "distance")
# Standard weighting (power function with gamma=1 and lambda=-2)
}