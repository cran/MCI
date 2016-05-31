\name{ijmatrix.create}
\alias{ijmatrix.create}

\title{
Interaction matrix with market shares
}
\description{
Creation of an interaction matrix with market shares (\eqn{p_{ij}}) of every supplier (\eqn{j}) in every submarket (\eqn{i}) based on the frequencies in the raw data (e.g. household or POS survey). 
}

\usage{
ijmatrix.create(rawdataset, submarkets, suppliers)
}

\arguments{
  \item{rawdataset}{
a \code{data.frame} containing the raw data
}
  \item{submarkets}{
the column in the dataset containing the submarkets (e.g. ZIP codes)
}
  \item{suppliers}{
the column in the dataset containing the suppliers (e.g. store codes) 
}
}
\details{
This function creates an interaction matrix for all \eqn{i} submarkets (e.g. geographical regions) and all \eqn{j} suppliers (e.g. store locations). An interaction matrix is a special case of table where every row is an \eqn{i} x \eqn{j} combination and the market shares of \eqn{j} in \eqn{i} (\eqn{p_{ij}}) are saved in a new column. An interaction matrix is the base for MCI analyses. In the MCI model the dependent variable is the market share of the \eqn{j} suppliers in the \eqn{i} submarkets, in which the shares are \emph{logically consistent} (that means: 0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). 
}
\value{
An interaction matrix which is a \code{data.frame} containing the \eqn{i} x \eqn{j} combinations (\code{'interaction'}), the submarkets (column is named as in raw data), the suppliers (column is named as in raw data), the observed absolute frequencies of every \eqn{j} in every \eqn{i} (\code{'freq_ij_abs'}), the observed absolute frequencies in every \eqn{i} (\code{'freq_i_total'}) and the observed market shares of every \eqn{j} in every \eqn{i} (\code{'p_ij_obs'}). The first three variables are factors, the calculated values are numeric.
}
\references{

Cooper, L. G./Nakanishi, M. (1988): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008.

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.

}
\author{
Thomas Wieland
}

\seealso{
\code{\link{ijmatrix.shares}}
}

\examples{
data(grocery1)
# Loads the data
ijmatrix.create (grocery1, "plz_submarket", "store_code")
# Creates an interaction matrix with local market shares

mynewmcidata <- ijmatrix.create (grocery1, "plz_submarket", "store_code")
# Save results directly in a new dataset
}