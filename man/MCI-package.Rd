\name{MCI-package}
\alias{MCI-package}
\alias{MCI}
\docType{package}
\title{
Multiplicative Competitive Interaction (MCI) Model
}
\description{
The \emph{Multiplicative Competitive Interaction (MCI) Model} (Nakanishi/Cooper 1974) is an econometric model for analyzing market shares and/or market areas in a market divided in \eqn{i} submarkets (e.g. groups of persons or geographical regions) and served by \eqn{j} suppliers (e.g. firms or locations). The explained/response variable of the model is \eqn{p_{ij}}, the market shares of \eqn{j} in \eqn{i}, which are \emph{logically consistent} (that means: 0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). The model is non-linear but can be transformed to be estimated by OLS (ordinary least squares) regression using the \emph{log-centering transformation}. The MCI model can especially be used in retail location analysis because it is also an econometric approach to estimate the parameters of the popular \emph{Huff model} for market areas (Huff 1962). The functions in this package include fitting the MCI model, MCI shares simulations, the \emph{log-centering transformation} of MCI datasets, creation of interaction matrices and tools for data preparation. 
}

\author{
Thomas Wieland

Maintainer: Thomas Wieland \email{thomas.wieland.geo@googlemail.com}
}

\references{
Cooper, L. G./Nakanishi, M. (1988): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer.

Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008.

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

