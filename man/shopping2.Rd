\name{shopping2}
\alias{shopping2}
\docType{data}
\title{
Distance matrix for the point-of-sale survey in Karlsruhe
}
\description{
The dataset contains a distance matrix (OD matrix: Origins-Destinations matrix) including the street distance and the travel time from the customer origins to the shopping destinations, both stored in the dataset \code{shopping1}.
}
\usage{data("shopping2")}
\format{
  A data frame with 3723 observations on the following 5 variables.
  \describe{
    \item{\code{from}}{a factor containing the customer origin (place of residence) as internal code}
    \item{\code{to}}{a factor containing the shopping destination}
    \item{\code{d_km}}{a numeric vector containing the street distance from the origins to the destinations in km}
    \item{\code{d_time}}{a numeric vector containing the driving time from the origins to the destinations in km}
    \item{\code{route}}{a factor containing the interaction/route code between origins and destinations (from-to)}
  }
}

\source{
Primary empirical sources:
POS (point of sale) survey in the authors' course (\dQuote{Praktikum Empirische Sozialforschung: Stadtteilzentren als Einzelhandelsstandorte - Das Fallbeispiel Karlsruhe-Durlach}, Karlsruhe Institute of Technology, Institute for Geography and Geoecology, May 2016), own calculations

The street distance and travel time was calculated using the package \pkg{ggmap}.
}

\seealso{
\code{\link{shopping1}}, \code{\link{shopping3}}, \code{\link{shopping4}}
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