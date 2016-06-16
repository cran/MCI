\name{grocery2}
\alias{grocery2}
\docType{data}
\title{
Grocery store market areas in Goettingen
}
\description{
Market areas of grocery stores in Goettingen, generated from a POS survey in Goettingen (Germany) from June 2015. The survey dataset contains 224 cases (\eqn{i} = 7 submarkets x \eqn{j} = 32 suppliers). The data is the result of a survey that is \emph{not} representative (see \emph{grocery1}) and also biased due to the data preparation. The data should be regarded as an example.
}
\usage{data("grocery2")}
\format{
  A data frame with 224 observations on the following 8 variables.
  \describe{
    \item{\code{plz_submarket}}{a factor with 7 levels (\code{PLZ_37073},   
    \code{PLZ_37075}, ...) representing the submarkets (places of residence based 
    on the five-digit ZIP codes) in the study area}
    \item{\code{store_code}}{a factor with 32 levels (\code{ALDI1}, \code{ALDI3}, ...,  \code{EDEKA1}, ... \code{REWE1}, ...), identifying the store code of the mentioned grocery store in the study area, data from Wieland (2011)}
    \item{\code{store_chain}}{a factor with 11 levels (\code{Aldi}, \code{Edeka}, ..., \code{Kaufland}, ...) for the store chain of the grocery stores in the study area, data from Wieland (2011)}
    \item{\code{store_type}}{a factor with 3 levels for the store type (\code{Biosup} = bio-supermarkt, \code{Disc} = discounter, \code{Sup} = supermarket) }
    \item{\code{salesarea_qm}}{a numeric vector for the sales area of the grocery stores in sqm, data from Wieland (2011)}
    \item{\code{pricelevel_euro}}{a numeric vector for the price level of the grocery chain (standardized basket in EUR), based on the data from DISQ (2015)}
    \item{\code{dist_km}}{a numeric vector for the distance from the places of residence (ZIP codes) to the grocery stores in km}
    \item{\code{p_ij_obs}}{a numeric vector for the empirically observed (and corrected) market shares (\eqn{p_{ij}}) of the stores in the submarkets}
  }
}

\source{
DISQ (Deutsches Institut fuer Servicequalitaet) (2015) \dQuote{Discounter guenstig, Vollsortimenter serviceorientiert. Studie Lebensmittelmaerkte (15.10.2015)}. 
\url{http://disq.de/2015/20151015-Lebensmittelmaerkte.html}

Wieland, T. (2011): \dQuote{Nahversorgung mit Lebensmitteln in Goettingen 2011 - Eine Analyse der Angebotssituation im Goettinger Lebensmitteleinzelhandel unter besonderer Beruecksichtigung der Versorgungsqualitaet}. \emph{Goettinger Statistik Aktuell}, \bold{35}. Goettingen. 
\url{http://www.goesis.goettingen.de/pdf/Aktuell35.pdf}

Primary empirical sources:
POS (point of sale) survey in the authors' course (\dQuote{Seminar Angewandte Geographie 1: Stadtentwicklung und Citymarketing an einem konkreten Fallbeispiel}, University of Goettingen/Institute of Geography, June 2015), own calculations
}

\seealso{
\code{\link{grocery1}}
}

\examples{
data(grocery2)
# Loads the data
mci.transmat (grocery2, "plz_submarket", "store_code", "p_ij_obs", "dist_km", "salesarea_qm")
# Applies the log-centering transformation to the dataset using the function mci.transmat
}

\keyword{datasets}