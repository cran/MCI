\name{var.correct}
\alias{var.correct}

\title{
Correcting MCI input variables
}
\description{
This function corrects a numeric variable to match the MCI standards.
}
\usage{
var.correct(x, corr.mode = "inc", incby = 1)
}

\arguments{
  \item{x}{
a numeric vector
}
  \item{corr.mode}{
character value for the mode of variable correction: \code{corr.mode = "inc"} increases the values by the value of \code{incby} (default: \code{incby = 1}). If \code{corr.mode = "incabs"} the values are increased by the absolute value of their minimum + \code{incby}. If \code{corr.mode = "zetas"} a zeta squared transformation is applied to the numeric vector
}
  \item{incby}{
value to increase the values with (default \code{ = 1})
}
 
}
\details{
In the MCI model, only numeric variables with values greater than zero are accepted (From the theoretical perspective, a zero or negative attractivity/utility is just as impossible as negative market shares. In the log-centering transformation, those values cannot be processed.). This function corrects a numeric variable with zero and/or negative values to match the MCI standards. The most frequent case is that some absolute values which shall be used to calculate market shares (e.g. observed frequencies or expenditures) are equal to zero and must be increased by 1. Alternatively, they can be increased automatically by the absolute value of their minimum + \code{incby}. Another option which is especially designed to transform interval scale data (such as scoring in consumer surveys) is to apply a zeta-squared transformation (Cooper/Nakanishi 1983) to the numeric vector (\code{corr.mode = "zetas"}).
}
\value{
Returns a numeric vector with the corrected values.
}
\references{
Colome Perales, R. (2002): \dQuote{Consumer Choice in Competitive Location Models}. Barcelona.

Cooper, L.G./Nakanishi, M. (1983): \dQuote{Standardizing Variables in Multiplicative Choice Models}. In: \emph{Journal of Consumer Research}, \bold{10}, 1, p. 96-108.

Cooper, L. G./Nakanishi, M. (2010): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer (first published 1988). E-book version from 2010: \url{http://www.anderson.ucla.edu/faculty/lee.cooper/MCI_Book/BOOKI2010.pdf}

Hartmann, M. (2005): \dQuote{Gravitationsmodelle als Verfahren der Standortanalyse im Einzelhandel}. \emph{Statistik Regional Electronic Papers}, \bold{02/2005}. Halle.

Tihi, B./Oruc, N. (2012): \dQuote{Competitive Location Assessment - the MCI Approach}. In: \emph{South East European Journal of Economics and Business}, \bold{7}, 2, p. 35-49. 

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}

\examples{
var1 <- c(11, 17.5, 24.1, 0.9, 21.2, 0)
# a vector containing one zero value
var.correct(var1)
# returns a vector with input values increased by 1

var2 <- -5:5
# a vector containing zero and negative values
var.correct(var2, corr.mode = "incabs", incby = 1)
# returns a vector with minimum value equal to 1

var.correct(var2, corr.mode = "zetas")
# returns a vector only with positive values 
# (zeta-squared transformation)
}