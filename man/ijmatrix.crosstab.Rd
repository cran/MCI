\name{ijmatrix.crosstab}
\alias{ijmatrix.crosstab}

\title{
Converting interaction matrix with market shares to crosstable
}
\description{
This function converts a given interaction matrix with (local) market shares to a crosstable where the rows are the submarkets and the columns contain the market shares 
}
\usage{
ijmatrix.crosstab(mcidataset, submarkets, suppliers, shares)
}

\arguments{
  \item{mcidataset}{
The interaction matrix containing the submarkets/origins, suppliers/locations and local market shares
}
  \item{submarkets}{
the column in the dataset containing the submarkets (e.g. ZIP codes)
}
  \item{suppliers}{
the column in the dataset containing the suppliers (e.g. store codes)
}
  \item{shares}{
the column in the dataset containing the local market shares
}
}
\details{
In many cases the results of a market area analysis shall be visualized in a map, e.g. by pie charts or contour lines which belongs to the standard map types in Geographical Information Systems (GIS). An interaction matrix can not be processed directly in a GIS due to its linear character. This function converts an interaction matrix into a special kind of crosstable where the rows contain the origins \eqn{i} and the local market shares \eqn{p_{ij}} are represented by the columns. The submarkets/origins ID (rows) can be joined directly to the geodata (e.g. point shapefile) while the columns can be used for visualization.    
}
\value{
A \code{data.frame} containing \eqn{i} rows and \eqn{j+1} columns (suppliers/locations and one column containing the submarkets/origins).
}

\references{
Cooper, L. G./Nakanishi, M. (2010): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer (first published 1988). E-book version from 2010: \url{http://www.anderson.ucla.edu/faculty/lee.cooper/MCI_Book/BOOKI2010.pdf}

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{ijmatrix.create}}, \code{\link{ijmatrix.shares}}
}

\examples{
data(grocery2)
# Loads the data

grocery2_cross <- ijmatrix.crosstab(grocery2, "plz_submarket", "store_code", "p_ij_obs")
# Converts the market shares in the grocery2 dataset to a crosstable
}