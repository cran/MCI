\name{var.correct}
\alias{var.correct}

\title{
Correcting MCI input variables
}
\description{
This function corrects a numeric variable to match the MCI standards.
}
\usage{
var.correct(x, incby = 1, auto = FALSE)
}

\arguments{
  \item{x}{
a numeric vector
}
  \item{incby}{
value to increase the values with (default \code{ = 1})
}
  \item{auto}{
logical argument (default \code{auto = FALSE}) if invalid values are increased automatically by the absolute value of the minimum + incby
}
}
\details{
In the MCI model, only numeric variables with values greater than zero are accepted (From the theoretical perspective, a zero or negative attractivity/utility is just as impossible as negative market shares. In the log-centering transformation, those values cannot be processed.). This function corrects a numeric variable with zero and/or negative values. The most frequent case is that some market shares (or absolute values) are equal to zero and are increased by 1. 
}
\value{
Returns a numeric vector with the corrected values.
}
\references{
Colome Perales, R. (2002): \dQuote{Consumer Choice in Competitive Location Models}. Barcelona.

Hartmann, M. (2005): \dQuote{Gravitationsmodelle als Verfahren der Standortanalyse im Einzelhandel}. \emph{Statistik Regional Electronic Papers}, \bold{02/2005}. Halle.

Tihi, B./Oruc, N. (2012): \dQuote{Competitive Location Assessment - the MCI Approach}. In: \emph{South East European Journal of Economics and Business}, \bold{7}, 2, p. 35-49. 

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}


\seealso{
\code{\link{var.check}}
}

\examples{
var1 <- c(11, 17.5, 24.1, 0.9, 21.2, 0)
# a vector containing one zero value
var.correct(var1)
# returns a vector with input values increased by 1

var2 <- -5:5
# a vector with zero/negative values
var.correct(var2, auto = TRUE)
# returns a vector with minimum value equal to 1
}
