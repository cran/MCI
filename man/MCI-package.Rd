\name{MCI-package}
\alias{MCI-package}
\alias{MCI}
\docType{package}
\title{
Multiplicative Competitive Interaction (MCI) Model
}
\description{
The \emph{Huff model} (Huff 1962, 1963, 1964) is the most popular \emph{spatial interaction model} for retailing and services and belongs to the family of \emph{probabilistic market area models}. The basic idea of the model, derived from the \emph{Luce choice axiom}, is that consumer decisions are not deterministic but probabilistic. Thus, the decision of customers for a shopping location in a competitive environment cannot be predicted exactly. The results of the model are probabilities for these decisions (\emph{interaction probabilities}), which can be interpreted as market shares of the regarded locations (\eqn{j}) in the customer origins (\eqn{i}), \eqn{p_{ij}} (\emph{local market shares}). The model results can be regarded as an equilibrium solution (consumer equilibrium) with logically consistent market shares (0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). From a theoretical perspective, the model is based on an utility function with two explanatory variables ("attraction" of the locations, transport costs between origins and locations), which are weighted by an exponent: \eqn{U_{ij}=A_{j}^\gamma d_{ij}^{-\lambda}}. But the \emph{distance decay function} reflecting the disutility of transport costs can also be exponential or logistic. The model can also be used for the estimation of market areas based on location sales or total patronage using nonlinear optimization algorithms. When the "real" local market shares were observed, the model can be parametrized by using the \emph{Multiplicative Competitive Interaction (MCI) Model}.

The \emph{Multiplicative Competitive Interaction (MCI) Model} (Nakanishi/Cooper 1974) is an econometric model for analyzing market shares and/or market areas in a competitive environment where the market is divided in \eqn{i} submarkets (e.g. groups of customers, time periods or geographical regions) and served by \eqn{j} suppliers (e.g. firms, brands or locations). The dependent variable of the model is \eqn{p_{ij}}, the market shares of \eqn{j} in \eqn{i}, which are \emph{logically consistent} (0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). The market shares depend on the attraction/utility of the alternative \eqn{j} in the choice situation/submarket \eqn{i}, \eqn{A_{ij}} or \eqn{U_{ij}}. The model is nonlinear (multiplicative attractivity/utility function with exponential weighting) but can be transformed to be estimated by OLS (ordinary least squares) regression using the multi-step \emph{log-centering transformation}. Before the \emph{log-centering transformation} can be applied, which is required for fitting the model, also a re-arrangement of the raw data (e.g. household surveys) in an \emph{interaction matrix} is necessary. An \emph{interaction matrix} is a special case of table where every row is an \eqn{i} x \eqn{j} combination and the market shares of \eqn{j} in \eqn{i} (\eqn{p_{ij}}) are saved in a new column (Linear table, the opposite of crosstable). The MCI model is a special case of market share model (which fulfills the requirement of logical consistency in the output), but can especially be used as a market area model (or \emph{spatial MCI model}) in retail location analysis since it is an econometric approach to estimate the parameters of the \emph{Huff model} mentioned above. 

The functions in this package include fitting the MCI model, MCI shares simulations, the \emph{log-centering transformation} of MCI datasets, creation of \emph{interaction matrices} from empirical raw data and several tools for data preparation. Additionally, the package provides applications for the \emph{Huff model}, including a nonlinear optimization algorithm to estimate market areas on condition that total market areas (customers, sales) of the stores/locations are known.
}

\author{
Thomas Wieland

Maintainer: Thomas Wieland \email{thomas.wieland.geo@googlemail.com}
}

\references{
Cooper, L. G./Nakanishi, M. (2010): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer (first published 1988). E-book version from 2010: \url{http://www.anderson.ucla.edu/faculty/lee.cooper/MCI_Book/BOOKI2010.pdf}

Cliquet, G. (2006): \dQuote{Retail Location Models}. In: Cliquet, G. (ed.): \emph{Geomarketing. Models and Strategies in Spatial Marketing}. London : ISTE. p. 137-163. 

Guessefeldt, J. (2002): \dQuote{Zur Modellierung von raeumlichen Kaufkraftstroemen in unvollkommenen Maerkten}. In: \emph{Erdkunde}, \bold{56}, 4, p. 351-370.

Huff, D. L. (1962): \dQuote{Determination of Intra-Urban Retail Trade Areas}. Los Angeles : University of California.

Huff, D. L. (1963): \dQuote{A Probabilistic Analysis of Shopping Center Trade Areas}. In: \emph{Land Economics}, \bold{39}, 1, p. 81-90.

Huff, D. L. (1964): \dQuote{Defining and Estimating a Trading Area}. In: \emph{Journal of Marketing}, \bold{28}, 4, p. 34-38.

Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Loeffler, G. (1998): \dQuote{Market areas - a methodological reflection on their boundaries}. In: \emph{GeoJournal}, \bold{45}, 4, p. 265-272.

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Wieland, T. (2013): \dQuote{Einkaufsstaettenwahl, Einzelhandelscluster und raeumliche Versorgungsdisparitaeten - Modellierung von Marktgebieten im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten}. In: Schrenk, M./Popovich, V./Zeile, P./Elisei, P. (eds.): REAL CORP 2013. Planning Times. Proceedings of 18th International Conference on Urban Planning, Regional Development and Information Society. Schwechat. p. 275-284. \url{http://www.corp.at/archive/CORP2013_98.pdf}

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.

Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}