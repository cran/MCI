\name{ijmatrix.shares}
\alias{ijmatrix.shares}

\title{
Market shares in interaction matrix
}
\description{
Calculating market shares in an interaction matrix based on the observations of the regarded variable. 
}
\usage{
ijmatrix.shares(rawmatrix, submarkets, suppliers, observations)
}

\arguments{
  \item{rawmatrix}{
a \code{data.frame} containing the submarkets, suppliers and the observed data
}
  \item{submarkets}{
the column in the dataset containing the submarkets (e.g. ZIP codes), should be a factor
}
  \item{suppliers}{
the column in the dataset containing the suppliers (e.g. store codes), should be a factor
}
  \item{observations}{
the column with the regarded variable (e.g. frequencies, expenditures, turnovers)
}
}
\details{
This function calculates the market shares of every \emph{j} in every \emph{i} (\emph{p_ij}) based on an existing interaction matrix. In the MCI model the dependent variable is the market share of the \emph{j} suppliers in the \emph{i} submarkets, in which the shares are \emph{logically consistent} (that means: 0 < \emph{p_ij} < 1, sum of \emph{p_ij} = 1).
}
\value{
The input interaction matrix which is a \code{data.frame} with a new column \code{'p_ij_obs'} or, if used after \code{\link{ijmatrix.create}}, an update of the columns \code{'freq_i_total'} and \code{'p_ij_obs'}.
}
\references{
Cooper, L. G./Nakanishi, M. (1988): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer.

Hartmann, M. (2005): \dQuote{Gravitationsmodelle als Verfahren der Standortanalyse im Einzelhandel}. \emph{Statistik Regional Electronic Papers}, \bold{02/2005}. Halle.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008.

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{ijmatrix.create}}
}

\examples{
data(grocery1)
# Loads the data
mymcidata <- ijmatrix.create (grocery1, "plz_submarket", "store_code")
# Creates an interaction matrix with market shares based on the frequencies 
# of visited grocery stores and saves results directly in a new dataset
mymcidata$freq_ij_corr <- var.correct(mymcidata$freq_ij_abs, 1)
# Corrects the frequency variable (no zero or negative values allowed)
mymcidata_shares <- ijmatrix.shares(mymcidata, "plz_submarket", "store_code", "freq_ij_corr")
# Calculates market shares based on the corrected frequencies
# and saves the results as a new dataset

}
