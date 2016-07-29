\name{ijmatrix.shares}
\alias{ijmatrix.shares}

\title{
Market shares in interaction matrix
}
\description{
Calculating market shares in an interaction matrix based on the observations of the regarded variable. 
}
\usage{
ijmatrix.shares(rawmatrix, submarkets, suppliers, observations, 
varname_total = "freq_i_total", varname_shares = "p_ij_obs")
}

\arguments{
  \item{rawmatrix}{
a \code{data.frame} containing the submarkets, suppliers and the observed data
}
  \item{submarkets}{
the column in the dataset containing the submarkets (e.g. ZIP codes)
}
  \item{suppliers}{
the column in the dataset containing the suppliers (e.g. store codes)
}
  \item{observations}{
the column with the regarded variable (e.g. frequencies, expenditures, turnovers)
}
  \item{varname_total}{
character value, name of the variable for the total absolute values of the \eqn{i} submarkets in the output (default: \code{varname_total = "freq_i_total"})
}
  \item{varname_shares}{
character value, name of the variable for the market shares \eqn{p_{ij}} in the output (default: \code{varname_shares = "p_ij_obs"})
}
}
\details{
This function calculates the market shares of every \eqn{j} in every \eqn{i} (\eqn{p_{ij}}) based on an existing interaction matrix. In the MCI model the dependent variable is the market share of the \eqn{j} suppliers in the \eqn{i} submarkets, in which the shares are \emph{logically consistent} (that means: 0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}).
}
\value{
The input interaction matrix which is a \code{data.frame} with a new column \code{'p_ij_obs'} (or another stated name in the argument \code{varname_shares}) or, if used after \code{\link{ijmatrix.create}}, an update of the columns \code{'freq_i_total'} and \code{'p_ij_obs'} (or different stated names in the arguments \code{varname_total} and/or \code{varname_shares}).
}
\references{
Cooper, L. G./Nakanishi, M. (1988): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

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