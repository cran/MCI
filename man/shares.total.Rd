\name{shares.total}
\alias{shares.total}

\title{
Total market shares/market areas
}

\description{
This function calculates the total sales and market shares (or total market area) of the suppliers based on a given interaction matrix which already contains (local) market shares.
}

\usage{
shares.total(mcidataset, submarkets, suppliers, shares, localmarket, 
plotChart = FALSE, plotChart.title = "Total sales", plotChart.unit = "sales", 
check_df = TRUE)
}

\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers, the market shares and a variable for the local market potential (e.g. purchasing power, number of customers, population)
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{shares}{
the column in the interaction matrix \code{mcidataset} containing the the (local) market shares
}
  \item{localmarket}{
the column in the interaction matrix \code{mcidataset} containing the local market potential
}
  \item{plotChart}{
logical argument that indicates if the total values shall be visualized in a bar plot (default: \code{plotChart = FALSE})
}
  \item{plotChart.title}{
If \code{plotChart = TRUE}: Title of the plot
}
  \item{plotChart.unit}{
If \code{plotChart = TRUE}: Unit of the plotted total values (e.g. a currency), used as plot subtitle
}
  \item{check_df}{
logical argument that indicates if the input (dataset, column names) is checked (default: \code{check_df = TRUE} (should not be changed, only for internal use))
}
}

\details{
If (local) market shares are observed and estimated, respectively, it is possible to link them to a (local) market potential to estimate the total sales and shares of the given suppliers. In this function, the input dataset (interaction matrix with local market shares) is used for the calculation of total sales (or total number of customers) and total market shares of all \eqn{j} regarded suppliers. Optionally, the function also returns a simple bar plot of the total values. 
}

\value{
Returns a new \code{data.frame} with the total sales (\code{sum_E_j}) and the over-all market shares of the \eqn{j} suppliers (\code{share_j}).
}

\references{
Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.fit}}, \code{\link{mci.transmat}}, \code{\link{mci.transvar}}, \code{\link{mci.shares}}
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