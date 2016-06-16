\name{ce}
\alias{ce}
\docType{data}
\title{
Market areas of consumer electronic stores
}
\description{
Market areas of five consumer electronic (CE) stores in a german study area and their explanatory variables. The market areas (or local market shares) are generated from a representative household survey from 2012. The other variables were gathered and/or calculated at the same time (by mapping stores etc.). The survey dataset contains 95 cases (\eqn{i} = 19 submarkets x \eqn{j} = 5 suppliers).
}
\usage{data("ce")}
\format{
  A data frame with 95 observations on the following 7 variables.
  \describe{
    \item{\code{origin_code}}{a numeric vector representing the submarkets (places of residence       of the surveyed households) in the study area}
    \item{\code{store_code}}{a factor with five levels (\code{E01}, \code{E02}...), identifying       the store code of the mentioned CE store in the study area}
    \item{\code{ms_obs}}{a numeric vector for the empirically observed market shares (\eqn{p_{ij}}) of the CE stores in the submarkets}
    \item{\code{traveltime}}{a numeric vector for the transport costs (travel time in minutes) between the places of residence and the stores}
    \item{\code{salesarea}}{a numeric vector for the sales area of the CE stores in sqm}
    \item{\code{conc_all}}{a numeric vector that reflects the spatial concentration (or rate of agglomeration) of the single store regarding other (non-competitive) retail stores (e.g. grocery, clothing, DIY), based on the \emph{Hansen accessibility}}
    \item{\code{conc_comp}}{a numeric vector that reflects the spatial concentration (or rate of agglomeration) of the single store regarding competing retail stores (other CE stores), based on the \emph{Hansen accessibility}}
  }
}

\source{
Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\references{
Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\examples{
data(ce)
# Loads the data

mymcimodel <- mci.fit (ce, "origin_code", "store_code", "ms_obs", "traveltime", 
"salesarea", "conc_all", "conc_comp")
# Save the results in new lm object
summary(mymcimodel)
# Summary of the model
}

\keyword{datasets}
