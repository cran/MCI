\name{MCI-package}
\alias{MCI-package}
\alias{MCI}
\docType{package}
\title{
Multiplicative Competitive Interaction (MCI) Model
}
\description{
The \emph{Multiplicative Competitive Interaction (MCI) Model} (Nakanishi/Cooper 1974) is an econometric model for analyzing market shares and/or market areas in a competitive environment where the market is divided in \eqn{i} submarkets (e.g. groups of customers, time periods or geographical regions) and served by \eqn{j} suppliers (e.g. firms, brands or locations). The explained/response variable of the model is \eqn{p_{ij}}, the market shares of \eqn{j} in \eqn{i}, which are \emph{logically consistent} (that means: 0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). The market shares depend on the attractivity/utility of the alternative \eqn{j} in the choice situation/submarket \eqn{i}, \eqn{A_{ij}} resp. \eqn{U_{ij}}. The model is non-linear (multiplicative attractivity/utility function with exponential weighting) but can be transformed to be estimated by OLS (ordinary least squares) regression using the multi-step \emph{log-centering transformation}. Before the \emph{log-centering transformation} can be applied, which is required for fitting the model, also a re-arrangement of the raw data (e.g. household surveys) in an \emph{interaction matrix} is necessary. The MCI model is a special case of market share model (which fulfills the requirement of logical consistency in the output), but can especially be used as a market area model (or \emph{spatial MCI model}) in retail location analysis because it is also an econometric approach to estimate the parameters of the popular \emph{Huff model} for market areas (Huff 1962). The functions in this package include fitting the MCI model, MCI shares simulations, the \emph{log-centering transformation} of MCI datasets, creation of interaction matrices and tools for data preparation. 
}

\author{
Thomas Wieland

Maintainer: Thomas Wieland \email{thomas.wieland.geo@googlemail.com}
}

\references{
Cooper, L. G./Nakanishi, M. (1988): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer.

Cliquet, G. (2006):  \dQuote{Retail Location Models}. In: Cliquet, G. (ed.): \emph{Geomarketing. Models and Strategies in Spatial Marketing}. London : ISTE. p. 137-163. 

Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008.

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

