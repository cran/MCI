\name{mci.fit}
\alias{mci.fit}

\title{
Fitting the MCI model
}
\description{
This function fits the MCI model based on an interaction matrix.   
}
\usage{
mci.fit(mcidataset, submarkets, suppliers, shares, ..., origin = TRUE)
}

\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers, the observed market shares (\eqn{p_{ij}}) and the explanatory variables
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{shares}{
the column in the interaction matrix \code{mcidataset} containing the observed market shares (\eqn{p_{ij}}), numeric
}
  \item{\dots}{
the column(s) of the explanatory variable(s) (at least one), numeric and positive (or dummy [1,0])
}
  \item{origin}{
logical argument that indicates if an intercept is included in the model or it is a regression through the origin (default \code{origin = TRUE})
}
}
\details{
The input dataset is transformed to regression-ready-data with the \emph{log-centering transformation} by Nakanishi/Cooper (1974) and the data is fitted by a linear regression model. The return of the function \code{mci.fit()} can be treated exactly like the output of the \code{lm()} function. In the MCI model the dependent variable is the market share of the \eqn{j} suppliers in the \eqn{i} submarkets (\eqn{p_{ij}}), in which the shares are \emph{logically consistent} (that means: 0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). The column in the interaction matrix \code{mcidataset} containing the shares is the 4th parameter of the function (\code{shares}). The further arguments (\code{...}) are the columns with the explanatory variables (attractivity/utility values of the \eqn{j} alternatives, characteristics of the submarkets \eqn{i}). The function identifies dummy variables which are not transformend (because they do not have to be). Normally, in MCI analyzes no intercept is included into the transformed linear model due to the requirement of logically consistent market shares as model results (see above), so the default is a regression through the origin (\code{origin = TRUE}). Note: If an intercept is included (\code{origin = FALSE}) (and also if dummy variables are used as explanatories), the \emph{inverse log-centering transformation} by Nakanishi/Cooper (1982) has to be used for simulations.   
}
\value{
The function \code{mci.fit()} returns an object of class \code{lm}. The full information (estimates, significance, R-squared etc.) can be adressed by the function \code{summary()}. The explanatory variables are marked with a "_t" to indicate that they were transformed by log-centering transformation.
}
\references{
Colome Perales, R. (2002): \dQuote{Consumer Choice in Competitive Location Models}. Barcelona.

Hartmann, M. (2005): \dQuote{Gravitationsmodelle als Verfahren der Standortanalyse im Einzelhandel}. \emph{Statistik Regional Electronic Papers}, \bold{02/2005}. Halle.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008.

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Suarez-Vega, R./Gutierrez-Acuna, J. L./Rodriguez-Diaz, M. (2015): \dQuote{Locating a supermarket using a locally calibrated Huff model}. In: \emph{International Journal of Geographical Information Science}, \bold{29}, 2, p. 217-233.

Tihi, B./Oruc, N. (2012): \dQuote{Competitive Location Assessment - the MCI Approach}. In: \emph{South East European Journal of Economics and Business}, \bold{7}, 2, p. 35-49. 

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.transmat}}, \code{\link{mci.transvar}}
}

\examples{
data(ce)
# Loads the data

mci.fit (ce, "origin_code", "store_code", "ms_obs", "traveltime", 
"salesarea", "conc_all", "conc_comp")
# Output of the coefficients

# Use like lm():
mymcimodel <- mci.fit (ce, "origin_code", "store_code", "ms_obs", "traveltime", 
"salesarea", "conc_all", "conc_comp")
# Save the results in new lm object
summary(mymcimodel)
# Summary of the model
mymcimodel$coefficients
# Output of the regression coefficients
mymcimodel$coefficients[1]
# Output of regression coefficient 1
}
