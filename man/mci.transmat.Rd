\name{mci.transmat}
\alias{mci.transmat}

\title{
Log-centering transformation of an interaction matrix
}
\description{
This function applies the \emph{log-centering transformation} to the variables in a given MCI interaction matrix.
}

\usage{
mci.transmat(mcidataset, submarkets, suppliers, mcivariable1, ...)
}

\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers and the regarded variables (e.g. the observed market shares, \eqn{p_{ij}}, and the explanatory variables)
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{mcivariable1}{
the column of the first variable to be transformed, numeric and positive (or dummy [1,0])
}
  \item{\dots}{
the columns of other variables to be transformed, numeric and positive (or dummy [1,0])
}
}
\details{
The input dataset is transformed to regression-ready data with the \emph{log-centering transformation} by Nakanishi/Cooper (1974). The resulting \code{data.frame} can be fitted with the \code{lm()} function (to combine these two steps in one, use \code{mci.fit()}). The \emph{log-centering transformation} can be regarded as the key concept of the MCI model because it enables the model to be estimated by OLS (ordinary least squares) regression. The function identifies dummy variables which are not transformed (because they do not have to be). 
}
\value{
Returns a new \code{data.frame} with regression-ready data where the input variables are transformed by the the \emph{log-centering transformation}. The names of the input variables are passed to the new \code{data.frame} marked with a "_t" to indicate that they were transformed (e.g. "shares_t" is the transformation of "shares").
}
\references{
Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008.

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Wieland, T. (2013): \dQuote{Einkaufsstaettenwahl, Einzelhandelscluster und raeumliche Versorgungsdisparitaeten - Modellierung von Marktgebieten im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten}. In: Schrenk, M./Popovich, V./Zeile, P./Elisei, P. (eds.): REAL CORP 2013. Planning Times. Proceedings of 18th International Conference on Urban Planning, Regional Development and Information Society. Schwechat. p. 275-284. \url{http://www.corp.at/archive/CORP2013_98.pdf}

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.fit}}, \code{\link{mci.transvar}}
}
\examples{
data(ce)
# Loads the data
mci.transmat (ce, "origin_code", "store_code", "ms_obs", "traveltime", "salesarea", 
"conc_all", "conc_comp")
# Output: submarkets (origins), store codes and transformations
# of "ms_obs", "traveltime", "salesarea", "conc_all" and "conc_comp"

transf_mcimat <- mci.transmat (ce, "origin_code", "store_code", "ms_obs", "traveltime", 
"salesarea", "conc_all", "conc_comp")
# Save in a new data frame called "transf_mcimat"
}