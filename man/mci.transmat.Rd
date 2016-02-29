\name{mci.transmat}
\alias{mci.transmat}

\title{
Log-centering transformation of an interaction matrix
}
\description{
This function applies the \emph{log-centering transformation} on the variables in a given MCI interaction matrix.
}

\usage{
mci.transmat(mcidataset, submarkets, suppliers, mcivariable1, ...)
}

\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers and the regarded variables (e.g. the observed market shares, \emph{p_ij}, and the explanatory variables)
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets, should usually be a factor
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers, should usually be a factor
}
  \item{mcivariable1}{
the column of the first variable to be transformed, numeric and positive (or dummy [1,0])
}
  \item{\dots}{
the columns of other variables to be transformed, numeric and positive (or dummy [1,0])
}
}
\details{
The input dataset is transformed to regression-ready data with the \emph{log-centering transformation} by Nakanishi/Cooper (1974). The resulting \code{data.frame} can be fitted with the base \code{lm()} function (to combine these two steps in one, use \code{mci.fit()}). The \emph{log-centering transformation} can be regarded as the key concept of the MCI model because it enables the model to be estimated by OLS (ordinary least squares) regression. The function identifies dummy variables which are not transformend (because they do not have to be). 
}
\value{
Returns a new \code{data.frame} with regression-ready data where the input variables are transformed by the the \emph{log-centering transformation}. The names of the input variables are passed to the new \code{data.frame} marked with a "_t" to indicate that they were transformed (e.g. "shares_t" is the transformation of "shares").
}
\references{
Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008.

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.fit}}, \code{\link{mci.transvar}}
}
\examples{
data(grocery2)
# Loads the data
mci.transmat (grocery2, "plz_submarket", "store_code", "p_ij_obs", "dist_km", "salesarea_qm")
# Output: submarkets (zipcodes), store codes and transformations
# of "p_ij_obs", "dist_km" and "salesarea_qm"

transf_mcimat <- mci.transmat (grocery2, "plz_submarket", "store_code", "p_ij_obs", "dist_km")
# Save in a new data frame called "transf_mcimat"
}