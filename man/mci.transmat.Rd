\name{mci.transmat}
\alias{mci.transmat}

\title{
Log-centering transformation of an MCI interaction matrix
}

\description{
This function applies the \emph{log-centering transformation} to the variables in a given MCI interaction matrix.
}

\usage{
mci.transmat(mcidataset, submarkets, suppliers, mcivariable1, ..., show_proc = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers and the regarded variables (e.g. the observed market shares, \eqn{p_{ij}}, and the explanatory variables)
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{mcivariable1}{
the column of the first variable to be transformed, numeric and positive (or dummy [1,0])
}
  \item{\dots}{
the columns of other variables to be transformed, numeric and positive (or dummy [1,0])
}
  \item{show_proc}{
logical argument that indicates if the function prints messages about the state of process during the work (e.g. \dQuote{Processing variable xyz ...} or \dQuote{Variable xyz is regarded as dummy variable}). Default: \code{show_proc = FALSE} (messages off)
}
}

\details{
This function transformes the input dataset (MCI interaction matrix) to regression-ready data with the \emph{log-centering transformation} by Nakanishi/Cooper (1974). The resulting \code{data.frame} can be fitted with the \code{lm()} function (to combine these two steps in one, use \code{mci.fit()}). The \emph{log-centering transformation} can be regarded as the key concept of the MCI model because it enables the model to be estimated by OLS (ordinary least squares) regression. The function identifies dummy variables which are not transformed (because they do not have to be). 
}

\value{
Returns a new \code{data.frame} with regression-ready data where the input variables are transformed by the the \emph{log-centering transformation}. The names of the input variables are passed to the new \code{data.frame} marked with a "_t" to indicate that they were transformed (e.g. "shares_t" is the transformation of "shares").
}

\references{
Huff, D. L./Batsell, R. R. (1975): \dQuote{Conceptual and Operational Problems with Market Share Models of Consumer Spatial Behavior}. In: \emph{Advances in Consumer Research}, \bold{2}, p. 165-172.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Nakanishi, M./Cooper, L. G. (1974): \dQuote{Parameter Estimation for a Multiplicative Competitive Interaction Model - Least Squares Approach}. In: \emph{Journal of Marketing Research}, \bold{11}, 3, p. 303-311.

Wieland, T. (2013): \dQuote{Einkaufsstaettenwahl, Einzelhandelscluster und raeumliche Versorgungsdisparitaeten - Modellierung von Marktgebieten im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten}. In: Schrenk, M./Popovich, V./Zeile, P./Elisei, P. (eds.): REAL CORP 2013. Planning Times. Proceedings of 18th International Conference on Urban Planning, Regional Development and Information Society. Schwechat. p. 275-284. \url{http://www.corp.at/archive/CORP2013_98.pdf}

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.fit}}, \code{\link{mci.transvar}}
}

\examples{
# MCI analysis for the grocery store market areas in grocery2 #
data(grocery2)
# Loads the data
mci.transmat (grocery2, "plz_submarket", "store_code", "p_ij_obs", "dist_km", "salesarea_qm")
# Applies the log-centering transformation to the dataset using the function mci.transmat



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