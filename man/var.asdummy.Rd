\name{var.asdummy}
\alias{var.asdummy}

\title{
Creates dummy variables
}
\description{
This function creates a dataset of dummy variables based on an input character vector.
}
\usage{
var.asdummy(x)
}

\arguments{
  \item{x}{
A character vector
}
}
\details{
In MCI analyzes (as in OLS regression models generally) only \emph{quantitative} information (that means: numeric) is allowed. \emph{Qualitative} information (e.g. brands, companies, retail chains) can be added using dummy variables [1,0]. This function transforms a character vector \code{x} with \emph{c} characteristics to a set of \emph{c} dummy variables whose column names correspond to these characteristics marked with \dQuote{_DUMMY}.    
}
\value{
A \code{data.frame} with dummy variables corresponding to the levels of the input variable.
}

\references{
Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Tihi, B./Oruc, N. (2012): \dQuote{Competitive Location Assessment - the MCI Approach}. In: \emph{South East European Journal of Economics and Business}, \bold{7}, 2, p. 35-49. 

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

\author{
Thomas Wieland
}


\examples{
charvec <- c("Peter", "Paul", "Peter", "Mary", "Peter", "Paul")
# Creates a vector with three names (Peter, Paul, Mary)
var.asdummy(charvec)
# Returns a data frame with 3 dummy variables
# (Mary_DUMMY, Paul_DUMMY, Peter_DUMMY)

data(grocery2)
# Loads the data
dummyvars <- var.asdummy(grocery2$store_chain)
# Save the dummy variable set into a new dataset
mynewmcidata <- data.frame(grocery2, dummyvars)
# Add the dummy dataset to the input dataset
}
