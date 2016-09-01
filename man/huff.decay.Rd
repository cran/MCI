\name{huff.decay}
\alias{huff.decay}

\title{
Distance decay function in the Huff model
}
\description{
This function estimates a distance decay function from observed data and compares different function types
}
\usage{
huff.decay(dataset, x, y, plotFunc = TRUE)
}

\arguments{
  \item{dataset}{
A \code{data.frame} containing the observed interaction data and the transport costs 
}
  \item{x}{
A numeric vector containing the independent variable, the transport costs (e.g. traveling time or street distance)
}
  \item{y}{
A numeric vector containing the dependent variable, the interaction measure (e.g. local market shares, trip volume, visitors per capita) 
}
  \item{plotFunc}{
logical argument that indicates if the curves are plotted (default: \code{plotFunc = TRUE})
}
}
\details{
The \emph{distance decay function} is a classic concept in quantitative economic geography and describes the relationship between transport costs and trip volume between origins (\eqn{i}) and a destination (\eqn{j}). The dependent variable is an indicator of trip volume, such as local market shares or visitors per capita etc., which are explained by the transport costs between all \eqn{i} and the destination \eqn{j}, \eqn{d_{ij}}. 

The non-linear modeling of transport costs is a key concept of the \emph{Huff model} (see the function \code{huff.shares}). This function estimates and compares different types of possible \emph{distance decay functions} (linear, power, exponential, logistic) based on observed interaction data.  
}

\value{
A \code{data.frame} containing the function parameters (\code{Intercept}, \code{Slope}), their p values in the regression function (\code{p Intercept}, \code{p Slope}) and fitting measures (\code{R-Squared}, \code{Adj. R-Squared}). Optionally, a plot of the four estimated functions and the observed data.
}

\references{
Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Isard, W. (1960): \dQuote{Methods of Regional Analysis: an Introduction to Regional Science}. Cambridge.

Kanhaeusser, C. (2007): \dQuote{Modellierung und Prognose von Marktgebieten am Beispiel des Moebeleinzelhandels}. In: Klein, R./Rauh, J. (eds.): \emph{Analysemethodik und Modellierung in der geographischen Handelsforschung}. \emph{Geographische Handelsforschung}, \bold{13}. Passau. p. 75-110.

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

}

\author{
Thomas Wieland
}

\seealso{
\code{\link{huff.shares}}, \code{\link{huff.attrac}}, \code{\link{huff.fit}}, \code{\link{mci.fit}}
}

\examples{
# Market area analysis based on the POS survey in shopping1 #

data(shopping1)
# The survey dataset
data(shopping2)
# Dataset with distances and travel times

shopping1_adj <- shopping1[(shopping1$weekday != 3) & (shopping1$holiday != 1) 
& (shopping1$survey != "pretest"),]
# Removing every case from tuesday, holidays and the ones belonging to the pretest

ijmatrix_POS <- ijmatrix.create(shopping1_adj, "resid_code", "POS", "POS_expen")
# Creates an interaction matrix based on the observed frequencies (automatically)
# and the POS expenditures (Variable "POS_expen" separately stated)

ijmatrix_POS_data <- merge(ijmatrix_POS, shopping2, by.x="interaction", by.y="route", 
all.x = TRUE)
# Adding the distances and travel times

ijmatrix_POS_data$freq_ij_abs_cor <- var.correct(ijmatrix_POS_data$freq_ij_abs, 
corr.mode = "inc", incby = 0.1)
# Correcting the absolute values (frequencies) by increasing by 0.1

data(shopping3)
ijmatrix_POS_data_residdata <- merge(ijmatrix_POS_data, shopping3)
# Adding the information about the origins (places of residence) stored in shopping3

ijmatrix_POS_data_residdata$visitper1000 <- (ijmatrix_POS_data_residdata$
freq_ij_abs_cor/ijmatrix_POS_data_residdata$resid_pop2015)*1000
# Calculating the dependent variable
# visitper1000: surveyed customers per 1.000 inhabitants of the origin

ijmatrix_POS_data_residdata <- 
ijmatrix_POS_data_residdata[(!is.na(ijmatrix_POS_data_residdata$
visitper1000)) & (!is.na(ijmatrix_POS_data_residdata$d_time)),]
# Removing NAs (data for some outlier origins and routes not available)

ijmatrix_POS_data_residdata_POS1 <- 
ijmatrix_POS_data_residdata[ijmatrix_POS_data_residdata$POS=="POS1",]
# Dataset for POS1 (town centre)

ijmatrix_POS_data_residdata_POS2 <- 
ijmatrix_POS_data_residdata[ijmatrix_POS_data_residdata$POS=="POS2",]
# Dataset for POS2 (out-of-town shopping centre)

huff.decay(ijmatrix_POS_data_residdata_POS1, "d_km", "visitper1000")
huff.decay(ijmatrix_POS_data_residdata_POS1, "d_time", "visitper1000")
huff.decay(ijmatrix_POS_data_residdata_POS2, "d_km", "visitper1000")
huff.decay(ijmatrix_POS_data_residdata_POS2, "d_time", "visitper1000")
}