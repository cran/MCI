\name{mci.transvar}
\alias{mci.transvar}
\title{
Log-centering transformation of one variable in an interaction matrix
}
\description{
This function applies the \emph{log-centering transformation} to a variable in a given MCI interaction matrix.
}
\usage{
mci.transvar (mcidataset, submarkets, suppliers, mcivariable, 
output_ij = FALSE, output_var = "numeric", check_df = TRUE)
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
  \item{mcivariable}{
the column of the variable to be transformed, numeric and positive (or dummy [1,0])
}
  \item{output_ij}{
logical argument that indicates if the function output has to be a \code{data.frame} with three columns (submarkets, suppliers, transformed variable) or a vector only with the transformed values (default is \code{output_ij = FALSE})
}
  \item{output_var}{
defines the mode of the function output if \code{output_ij = FALSE} (default is \code{output_var = "numeric"}, otherwise \code{"list"})
}
  \item{check_df}{
logical argument that indicates if the input (dataset, column names) is checked (default: \code{check_df = TRUE} (should not be changed, only for internal use))
}
}
\details{
The regarded variable in the input dataset is transformed to regression-ready data with the \emph{log-centering transformation} by Nakanishi/Cooper (1974) (to transform a complete interaction matrix, use \code{mci.transmat()}, for transformation and fitting use \code{mci.fit()}). The \emph{log-centering transformation} can be regarded as the key concept of the MCI model because it enables the model to be estimated by OLS (ordinary least squares) regression. The function identifies dummy variables which are not transformed (because they do not have to be).
}
\value{
The format of the output can be controlled by the last two arguments of the function (see above). Either a new \code{data.frame} with the transformed input variable and the submarkets/suppliers or a vector with the transformed values only. The name of the input variable is passed to the new \code{data.frame} marked with a "_t" to indicate that it was transformed (e.g. "shares_t" is the transformation of "shares").
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
\code{\link{mci.fit}}, \code{\link{mci.transmat}}
}
\examples{
data(ce)
# Loads the data

mci.transvar (ce, "origin_code", "store_code", "ms_obs", output_ij=TRUE)
# Output: submarkets (origins), store codes and transformations of "ms_obs"

mci.transvar (ce, "origin_code", "store_code", "ms_obs")
# Output: a numeric vector containing the transformated values of "ms_obs"

transf_mcivar <- mci.transvar (ce, "origin_code", "store_code", "ms_obs", output_ij=TRUE)
# Save in a new data frame called "transf_mcivar"
}
