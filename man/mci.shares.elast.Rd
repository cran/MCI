\name{mci.shares.elast}
\alias{mci.shares.elast}

\title{
Market share elasticities
}

\description{
This function calculates the market share elasticities (point elasticity) with respect to an attraction/utility variable and its given weighting parameter.
}

\usage{
mci.shares.elast (mcidataset, submarkets, suppliers, shares, mcivar, mciparam, 
check_df = TRUE)  
}

\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers, the market shares and a variable for the local market potential (e.g. purchasing power, number of customers, population)
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{shares}{
the column in the interaction matrix \code{mcidataset} containing the (local) market shares
}
  \item{mcivar}{
the column in the interaction matrix \code{mcidataset} containing the attraction/utility variable
}
  \item{mciparam}{
single value of the (empirically estimated) weighting parameter corresponding to the attraction/utility variable
}
  \item{check_df}{
logical argument that indicates if the input (dataset, column names) is checked (default: \code{check_df = TRUE} (should not be changed, only for internal use))
}
}

\details{
Market-share elasticity is defined as the ratio of the relative change in the market share corresponding to a relative change in an explanatory (attraction/utility) variable, such as price or, in the context of retailing, driving time, sales area or price level. The elasticities calculated here are point elasticities (not arc elasticities): \eqn{e_{s_{i}} = (d p_{i} / d X_{ki}) * (X_{ki} / {p_i})}, which are calculated for the MCI model via: \eqn{e_{s_{i}}=\beta _{k}*(1-p_i)}, where \eqn{\beta _k} is the corresponding weighting parameter. If the (absolute) elasticity value is greater than one, the suppliers' market share is called elastic, if it is smaller than one, the share is unelastic. E.g. if the share elasticity of a products' price is -2, a relative price reduction of 5\% results in a share increase of 10\% (Cooper/Nakanishi 2010). Note that the elasticity depends on the empirical shares: The greater the actual share, the smaller is the elasticity.  
}

\value{
The function \code{mci.shares.elast()} returns the input interaction matrix (\code{data.frame}) with a new column containing the calculated share elasticities for every combination of \eqn{i} and \eqn{j}.
}

\references{
Cooper, L. G./Nakanishi, M. (2010): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer (first published 1988). E-book version from 2010: \url{http://www.anderson.ucla.edu/faculty/lee.cooper/MCI_Book/BOOKI2010.pdf}
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{mci.fit}}, \code{\link{mci.shares}}
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

# Calculating market share elasticity:
ijmatrix_gro_adj_dist_stores_elas1 <- mci.shares.elast (ijmatrix_gro_adj_dist_stores, 
"resid_code", "gro_purchase_code", "p_ij_obs", "d_time", -1.2443)
# Share elasticities of driving time

ijmatrix_gro_adj_dist_stores_elas2 <- mci.shares.elast (ijmatrix_gro_adj_dist_stores, 
"resid_code", "gro_purchase_code", "p_ij_obs", "salesarea_qm", 0.9413)
# Share elasticities of sales area of the stores
}