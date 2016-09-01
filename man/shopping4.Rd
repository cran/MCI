\name{shopping4}
\alias{shopping4}
\docType{data}
\title{
Grocery store data for the point-of-sale survey in Karlsruhe
}
\description{
This dataset contains information about the regarded grocery stores in the east of Karlsruhe, based on the POS survey stored in the dataset \code{shopping1} and the related information in \code{shopping2}..
}
\usage{data("shopping4")}
\format{
  A data frame with 11 observations on the following 4 variables.
  \describe{
    \item{\code{location_code}}{a factor containing the grocery store codes}
    \item{\code{salesarea_qm}}{a numeric vector containing the sales area of the stores in sqm}
    \item{\code{storetype_dc}}{a numeric vector containing a dummy variable that indicates if the store is a discounter or not}
    \item{\code{store_chain}}{a factor containing the store chain}
  }
}

\source{
Primary empirical sources:
POS (point of sale) survey in the authors' course (\dQuote{Praktikum Empirische Sozialforschung: Stadtteilzentren als Einzelhandelsstandorte - Das Fallbeispiel Karlsruhe-Durlach}, Karlsruhe Institute of Technology, Institute for Geography and Geoecology, May 2016), own calculations

Mapping of grocery stores in the east of Karlsruhe in June 2016 with additional research
}

\seealso{
\code{\link{shopping1}}, \code{\link{shopping2}}, \code{\link{shopping3}}
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