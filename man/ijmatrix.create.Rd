\name{ijmatrix.create}
\alias{ijmatrix.create}

\title{
Interaction matrix with market shares
}
\description{
Creation of an interaction matrix with market shares (\emph{p_ij}) of every supplier (\emph{j}) in every submarket (\emph{i}) based on the frequencies in the raw data (e.g. household or POS survey). 
}

\usage{
ijmatrix.create(rawdataset, submarkets, suppliers)
}

\arguments{
  \item{rawdataset}{
a \code{data.frame} containing the raw data
}
  \item{submarkets}{
the column in the dataset containing the submarkets (e.g. ZIP codes), should be a factor
}
  \item{suppliers}{
the column in the dataset containing the suppliers (e.g. store codes), should be a factor 
}
}
\details{
This function creates an interaction matrix for all \emph{i} submarkets (e.g. geographical regions) and all \emph{j} suppliers (e.g. store locations). An interaction matrix is a special case of table where every row is an \emph{i} x \emph{j} combination and the market shares of \emph{j} in \emph{i} (\emph{p_ij}) are saved in a new column. An interaction matrix is the base for MCI analyses. In the MCI model the dependent variable is the market share of the \emph{j} suppliers in the \emph{i} submarkets, in which the shares are \emph{logically consistent} (that means: 0 < \emph{p_ij} < 1, sum of \emph{p_ij} = 1). 
}
\value{
An interaction matrix which is a \code{data.frame} containing the \emph{i} x \emph{j} combinations (\code{'interaction'}), the submarkets (column is named as in raw data), the suppliers (column is named as in raw data), the observed absolute frequencies of every \emph{j} in every \emph{i} (\code{'freq_ij_abs'}), the observed absolute frequencies in every \emph{i} (\code{'freq_i_total'}) and the observed market shares of every \emph{j} in every \emph{i} (\code{'p_ij_obs'}). The first three variables are factors, the calculated values are numeric.
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