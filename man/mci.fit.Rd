\name{mci.fit}
\alias{mci.fit}

\title{
Fitting the MCI model
}
\description{
This function fits the MCI model based on a given MCI interaction matrix.
}
\usage{
mci.fit(mcidataset, submarkets, suppliers, shares, ..., origin = TRUE, show_proc = FALSE)
}

\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers, the observed market shares (\eqn{p_{ij}}) and the explanatory variables
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{shares}{
the column in the interaction matrix \code{mcidataset} containing the observed market shares (\eqn{p_{ij}}), numeric
}
  \item{\dots}{
the column(s) of the explanatory variable(s) (at least one), numeric and positive (or dummy [1,0])
}
  \item{origin}{
logical argument that indicates if an intercept is included in the model or it is a regression through the origin (default \code{origin = TRUE})
}
  \item{show_proc}{
logical argument that indicates if the function prints messages about the state of process during the work (e.g. \dQuote{Processing variable xyz ...} or \dQuote{Variable xyz is regarded as dummy variable}). Default: \code{show_proc = FALSE} (messages off)
}
}

\details{
The function transforms the input dataset (MCI interaction matrix) to regression-ready-data with the \emph{log-centering transformation} by Nakanishi/Cooper (1974) and, therefore, the data is fitted by a linear regression model. The return of the function \code{mci.fit()} can be treated exactly like the output of the \code{lm()} function. The column in the interaction matrix \code{mcidataset} containing the shares is the 4th parameter of the function (\code{shares}). The further arguments (\code{...}) are the columns with the explanatory variables (attractivity/utility values of the \eqn{j} alternatives, characteristics of the \eqn{i} submarkets). The function identifies dummy variables which are not transformed (because they do not have to be). Normally, in MCI analyzes no intercept is included into the transformed linear model due to the requirement of logically consistent market shares as model results (see above), so the default is a regression through the origin (\code{origin = TRUE}). Note: If an intercept is included (\code{origin = FALSE}) (and also if dummy variables are used as explanatories), the \emph{inverse log-centering transformation} by Nakanishi/Cooper (1982) has to be used for simulations.
}

\value{
The function \code{mci.fit()} returns an object of class \code{lm}. The full information (estimates, significance, R-squared etc.) can be adressed by the function \code{summary()}. The explanatory variables are marked with a "_t" to indicate that they were transformed by log-centering transformation.
}

\references{
Colome Perales, R. (2002): \dQuote{Consumer Choice in Competitive Location Models}. Barcelona.

Gonzalez-Benito, O./Greatorex, M./Munos-Gallego, P. A. (2000): \dQuote{Assessment of potential retail segmentation variables - An approach based on a subjective MCI resource allocation model}. In: \emph{Journal of Retailing and Consumer Services}, \bold{7}, 3, p. 171-179.

Hartmann, M. (2005): \dQuote{Gravitationsmodelle als Verfahren der Standortanalyse im Einzelhandel}. \emph{Statistik Regional Electronic Papers}, \bold{02/2005}. Halle.

Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Nakanishi, M./Cooper, L. G. (1982): \dQuote{Simplified Estimation Procedures for MCI Models}. In: \emph{Marketing Science}, \bold{1}, 3, p. 314-322. 

Suarez-Vega, R./Gutierrez-Acuna, J. L./Rodriguez-Diaz, M. (2015): \dQuote{Locating a supermarket using a locally calibrated Huff model}. In: \emph{International Journal of Geographical Information Science}, \bold{29}, 2, p. 217-233.

Tihi, B./Oruc, N. (2012): \dQuote{Competitive Location Assessment - the MCI Approach}. In: \emph{South East European Journal of Economics and Business}, \bold{7}, 2, p. 35-49. 

Wieland, T. (2013): \dQuote{Einkaufsstaettenwahl, Einzelhandelscluster und raeumliche Versorgungsdisparitaeten - Modellierung von Marktgebieten im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten}. In: Schrenk, M./Popovich, V./Zeile, P./Elisei, P. (eds.): REAL CORP 2013. Planning Times. Proceedings of 18th International Conference on Urban Planning, Regional Development and Information Society. Schwechat. p. 275-284. \url{http://www.corp.at/archive/CORP2013_98.pdf}

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.transmat}}, \code{\link{mci.transvar}}, \code{\link{mci.shares}}
}

\examples{
# MCI analysis for the grocery store market areas based on the POS survey in shopping1 #

data(shopping1)
# Loading the survey dataset
data(shopping2)
# Loading the distance/travel time dataset
data(shopping3)
# Loading the dataset containing information about the city districts
data(shopping4)
# Loading the grocery store data

shopping1_KAeast <- shopping1[shopping1$resid_code \%in\% 
shopping3$resid_code[shopping3$KA_east == 1],]
# Extracting only inhabitants of the eastern districts of Karlsruhe

ijmatrix_gro_adj <- ijmatrix.create(shopping1_KAeast, "resid_code",
"gro_purchase_code", "gro_purchase_expen", remSing = TRUE, remSing.val = 1,
remSingSupp.val = 2, correctVar = TRUE, correctVar.val = 0.1)
# Removing singular instances/outliers (remSing = TRUE) incorporating
# only suppliers which are at least obtained three times (remSingSupp.val = 2)
# Correcting the values (correctVar = TRUE)
# by adding 0.1 to the absolute values (correctVar.val = 0.1)

ijmatrix_gro_adj <- ijmatrix_gro_adj[(ijmatrix_gro_adj$gro_purchase_code !=
"REFORMHAUSBOESER") & (ijmatrix_gro_adj$gro_purchase_code != "WMARKT_DURLACH")
& (ijmatrix_gro_adj$gro_purchase_code != "X_INCOMPLETE_STORE"),]
# Remove non-regarded observations

ijmatrix_gro_adj_dist <- merge (ijmatrix_gro_adj, shopping2, by.x="interaction",
by.y="route")
# Include the distances and travel times (shopping2)
ijmatrix_gro_adj_dist_stores <- merge (ijmatrix_gro_adj_dist, shopping4,
by.x = "gro_purchase_code", by.y = "location_code")
# Adding the store information (shopping4)

mci.transvar(ijmatrix_gro_adj_dist_stores, "resid_code", "gro_purchase_code", 
"p_ij_obs")
# Log-centering transformation of one variable (p_ij_obs)

ijmatrix_gro_transf <- mci.transmat(ijmatrix_gro_adj_dist_stores, "resid_code",
"gro_purchase_code", "p_ij_obs", "d_time", "salesarea_qm")
# Log-centering transformation of the interaction matrix

mcimodel_gro_trips <- mci.fit(ijmatrix_gro_adj_dist_stores, "resid_code",
"gro_purchase_code", "p_ij_obs", "d_time", "salesarea_qm")
# MCI model for the grocery store market areas
# shares: "p_ij_obs", explanatory variables: "d_time", "salesarea_qm"

summary(mcimodel_gro_trips)
# Use like lm
}