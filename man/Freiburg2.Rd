\name{Freiburg2}
\alias{Freiburg2}
\docType{data}
\title{
Statistical districts of Freiburg
}
\description{
The 42 statistical districts of Freiburg (Germany) and the estimated annual purchasing power for groceries, based on average expenditures and population.
}
\usage{data("Freiburg2")}
\format{
  A data frame with 42 observations on the following 2 variables.
  \describe{
    \item{\code{district}}{a numeric vector representing the 42 statistical districts of Freiburg}
    \item{\code{ppower}}{a numeric vector containing the estimated absolute value of annual purchasing power for groceries in the district in EUR}
  }
}
\source{
Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}
\references{
Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}

\examples{
data(Freiburg1)
data(Freiburg2)
data(Freiburg3)
# Loads the data

huff_mat <- huff.shares (Freiburg1, "district", "store", "salesarea", "distance")
# Market area estimation using the Huff Model with standard parameters
# (gamma = 1, lambda = -2)

huff_mat_pp <- merge (huff_mat, Freiburg2)
# Adding the purchasing power data for the city districts

huff_total <- shares.total (huff_mat_pp, "district", "store", "p_ij", "ppower")
# Total expected sales and shares

huff_total_control <- merge (huff_total, Freiburg3, by.x = "suppliers_single", 
by.y = "store")

model.fit(huff_total_control$annualsales, huff_total_control$sum_E_j, plotVal = TRUE)
}