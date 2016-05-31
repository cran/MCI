\name{var.check}
\alias{var.check}

\title{
Checking MCI input variables
}
\description{
This function tests a variable if it matches the standards of the MCI model analysis.
}
\usage{
var.check(x, asdummy = FALSE)
}

\arguments{
  \item{x}{
A vector
}
  \item{asdummy}{
logical argument that indicates if a character vector should be regarded as to be transformed to dummy variables
}
}
\details{
In the MCI model, only numeric variables with values greater than zero are accepted (From the theoretical perspective, a zero or negative attractivity/utility is just as impossible as negative market shares. In the log-centering transformation, those values cannot be processed.). This function tests a variable if it matches these standards. 
}
\value{
Returns a character value depending on the test result: \code{"valid_n"} indicates that the variable is valid for MCI analyzes. Otherwise it contains zero/and or negative values (\code{"invalid_zn"}) or character values (\code{"invalid_s"}). If \code{asdummy = TRUE}, a character variable is tested as \code{"asdummy"}.
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
\code{\link{var.correct}}
}

\examples{
testvar1 <- 1:10
var.check(testvar1)
# returns "valid_n"

testvar2 <- -5:5
var.check(testvar2)
# returns "invalid_zn"

testvar3 <- c("1","5.5", "4", "10.2", "k", "7.6")
var.check(testvar3)
# returns "invalid_s"

testvar4 <- c("Peter", "Paul", "Peter", "Mary", "Peter", "Paul")
var.check(testvar4, asdummy=TRUE)
# returns "asdummy"
}
