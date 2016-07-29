\name{Freiburg3}
\alias{Freiburg3}
\docType{data}
\title{
Grocery stores in Freiburg
}
\description{
The 63 grocery stores in Freiburg (Germany) and the estimated annual sales in EUR.
}
\usage{data("Freiburg3")}
\format{
  A data frame with 63 observations on the following 2 variables.
  \describe{
    \item{\code{store}}{a numeric vector identifying the store code of the mentioned grocery store in the study area}
    \item{\code{annualsales}}{a numeric vector containing the estimated annual sales of the store in EUR}
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
# Loading the three Freiburg datasets

# NOTE: This may take a while!
# huff.attrac(Freiburg1, "district", "store", "salesarea", "distance", lambda = -2, dtype= "pow",
# lambda2 = NULL, Freiburg2, "ppower", Freiburg3, "store", "annualsales", tolerance = 5, 
# output = "total")
# Local optimization of store attractivity using the function huff.attrac()
# returns a data frame with total values (observed and expected after optimization)
}