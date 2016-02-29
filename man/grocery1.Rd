\name{grocery1}
\alias{grocery1}
\docType{data}
\title{
Grocery store choices in Goettingen
}
\description{
Results from a POS survey in Goettingen (Germany) from June 2015 (raw data). Amongst other things, the participants were asked about their last grocery shopping trip (store choice and expenditures) and their place of residence (ZIP code). The survey dataset contains 179 cases/interviewed individuals. The survey is \emph{not} representative and should be regarded as an example.
}
\usage{data("grocery1")}
\format{
  A data frame with 179 observations on the following 5 variables.
  \describe{
    \item{\code{interview_nr}}{a numeric vector, interview/individual identifier}
    \item{\code{store_code}}{a factor with 32 levels (\code{ALDI1}, \code{ALDI3}, ...,  \code{EDEKA1}, ... \code{REWE1}, ...), identifying the store code of the mentioned grocery store in the study area, data from Wieland (2011)}
    \item{\code{store_chain}}{a factor with 11 levels (\code{Aldi}, \code{Edeka}, \code{Kaufland}, ...) for the store chain of the grocery stores in the study area, data from Wieland (2011)} 
    \item{\code{trip_expen}}{a numeric vector containing the individual trip expenditures at the last visited grocery store}
    \item{\code{plz_submarket}}{a factor with 7 levels (\code{PLZ_37073}, \code{PLZ_37075}, ...) representing the individuals' place of residence based on the five-digit ZIP codes in the study area}
  }
}

\source{
Wieland, T. (2011): \dQuote{Nahversorgung mit Lebensmitteln in Goettingen 2011 - Eine Analyse der Angebotssituation im Goettinger Lebensmitteleinzelhandel unter besonderer Beruecksichtigung der Versorgungsqualitaet}. \emph{Goettinger Statistik Aktuell}, \bold{35}. Goettingen. 
URL: \emph{http://www.goesis.goettingen.de/pdf/Aktuell35.pdf}

Primary empirical sources:
POS (point of sale) survey in the authors' course (\dQuote{Seminar Angewandte Geographie 1: Stadtentwicklung und Citymarketing an einem konkreten Fallbeispiel}, University of Goettingen/Institute of Geography, June 2015), own calculations
}

\seealso{
\code{\link{grocery2}}
}

\examples{
data(grocery1)
# Loads the data
ijmatrix.create (grocery1, "plz_submarket", "store_code")
# Creates an interaction table with local market shares 
}

\keyword{datasets}
