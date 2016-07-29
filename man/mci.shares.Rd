\name{mci.shares}
\alias{mci.shares}

\title{
MCI market share/market area simulations
}
\description{
This function calculates (local) market shares based on specified explanatory variables and their weighting parameters in a given MCI interaction matrix.
}
\usage{
mci.shares(mcidataset, submarkets, suppliers, ..., mcitrans = "lc", interc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers and the explanatory variables
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{\dots}{
the column(s) of the explanatory variable(s) (at least one), numeric and positive (or dummy [1,0]), and their weighting parameter(s). The parameter(s) must follow the particular variable(s): \code{mcivariable1, parameter1, ...}
}
  \item{mcitrans}{
defines if the regular multiplicative formula is used or the \emph{inverse log-centering transformation} where the explanatory variables are MCI-transformed and linked by addition in an exponential function instead of multiplication. This transformation is necessary if an intercept is included in the model and/or if dummy variables are used as explanatories (default: \code{mcitrans = "lc"}, which indicates the regular log-centering transformation)
}
  \item{interc}{
if \code{mcitrans = "ilc"}: logical argument that indicates if an intercept is included in the model (default \code{interc = NULL})
}
}
\details{
The input dataset, a MCI interaction matrix, is used for a calculation of (local) market shares (\eqn{p_{ij}}), based on (at least one) given explanatory variable(s) and (a) given weighting parameter(s). In the MCI model the dependent variable is the market share of the \eqn{j} suppliers in the \eqn{i} submarkets (\eqn{p_{ij}}), in which the shares are \emph{logically consistent} (that means: 0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). If an intercept is included in the model and/or if dummy variables are used as explanatories, the \emph{inverse log-centering transformation} by Nakanishi/Cooper (1982) has to be used for simulations (\code{mcitrans = "ilc"}).
}
\value{
The function \code{mci.shares()} returns the input interaction matrix (\code{data.frame}) with new variables/columns, where the last one (\code{p_ij}) is the one of interest, containing the (local) market shares of the \eqn{j} suppliers in the \eqn{i} submarkets (\eqn{p_{ij}}).
}
\references{
Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322.

Wieland, T. (2013): \dQuote{Einkaufsstaettenwahl, Einzelhandelscluster und raeumliche Versorgungsdisparitaeten - Modellierung von Marktgebieten im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten}. In: Schrenk, M./Popovich, V./Zeile, P./Elisei, P. (eds.): REAL CORP 2013. Planning Times. Proceedings of 18th International Conference on Urban Planning, Regional Development and Information Society. Schwechat. p. 275-284. \url{http://www.corp.at/archive/CORP2013_98.pdf}

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.fit}}, \code{\link{mci.transmat}}, \code{\link{mci.transvar}}, \code{\link{shares.total}}
}

\examples{
data(Freiburg1)
data(Freiburg2)
# Loads the data

mynewmatrix <- mci.shares(Freiburg1, "district", "store", "salesarea", 1, "distance", -2)
# Calculating shares based on two attractivity/utility variables

mynewmatrix_alldata <- merge(mynewmatrix, Freiburg2)
# Merge interaction matrix with district data (purchasing power)

shares.total (mynewmatrix_alldata, "district", "store", "p_ij", "ppower")
# Calculation of total sales 
}