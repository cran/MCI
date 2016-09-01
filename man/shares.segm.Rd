\name{shares.segm}
\alias{shares.segm}

\title{
Segmentation of market areas by a criterion
}
\description{
This function segments the contents of an interaction matrix based on a criterion, such as distance or market penetration.
}
\usage{
shares.segm(mcidataset, submarkets, suppliers, segmentation, observations, 
..., check_df = TRUE)
}

\arguments{
  \item{mcidataset}{
an interaction matrix which is a \code{data.frame} containing the submarkets, suppliers, the total values (e.g. shopping trips, expenditures) and a variable containing the criterion for the segmentation (e.g. distance, travel time, market penetration)
}
  \item{submarkets}{
the column in the interaction matrix \code{mcidataset} containing the submarkets
}
  \item{suppliers}{
the column in the interaction matrix \code{mcidataset} containing the suppliers
}
  \item{segmentation}{
The column in the interaction matrix \code{mcidataset} containing the variable which is to be used for segmentation
}
  \item{observations}{
The column in the interaction matrix \code{mcidataset} containing the total values which have to segmented
}
  \item{\dots}{
The stated limits of class segments (e.g. 0, 10, 20, 30)
}
  \item{check_df}{
logical argument that indicates if the input (dataset, column names) is checked (default: \code{check_df = TRUE} (should not be changed, only for internal use))
}
}
\details{
For practical reasons, a market/market area can be zoned into segments based on a criterion (such as distance or travel time zones, zones of market penetration). Based on an existing interaction matrix, this function returns zones of a market/market area.
}
\value{
Returns a new \code{data.frame} with the classification segments, the sum of the total observed values with respect to each class and the corresponding percentage.
}
\references{
Berman, B. R./Evans, J. R. (2013): \dQuote{Retail Management: A Strategic Approach}. Pearson, 12 edition, 2013.
}
\author{
Thomas Wieland
}

\seealso{
\code{\link{shares.total}}
}
\examples{
# Market area segmentation based on the POS survey in shopping1 #

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

ijmatrix_POS_data_segm_visit <- shares.segm(ijmatrix_POS_data, "resid_code", "POS",
"d_time", "freq_ij_abs", 0,10,20,30)
# Segmentation by travel time using the number of customers/visitors
# Parameters: interaction matrix (data frame), columns with origins and destinations,
# variable to divide in classes, absolute frequencies/expenditures, class segments

ijmatrix_POS_data_segm_exp <- shares.segm(ijmatrix_POS_data, "resid_code", "POS",
"d_time", "freq_ij_abs_POS_expen", 0,10,20,30)
# Segmentation by travel time using the POS expenditures
}