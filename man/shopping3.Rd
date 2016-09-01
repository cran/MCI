\name{shopping3}
\alias{shopping3}
\docType{data}
\title{
Market area data for the point-of-sale survey in Karlsruhe
}
\description{
The dataset contains information about
}
\usage{data("shopping3")}
\format{
  A data frame with 70 observations on the following 5 variables.
  \describe{
    \item{\code{resid_name}}{a factor containing the customer origin (place of residence) as name of the corresponding city or city district}
    \item{\code{resid_name_offical}}{a factor containing the customer origin (place of residence) as official names of the corresponding city or city district}
    \item{\code{resid_pop2015}}{a numeric vector containing the population size of the area}
    \item{\code{KA_east}}{a numeric vector containing a dummy variable indicating whether the area belongs to the east of Karlsruhe or not}
    \item{\code{resid_code}}{a factor containing the customer origin (place of residence) as internal code}
  }
}


\source{
Primary empirical sources:
POS (point of sale) survey in the authors' course (\dQuote{Praktikum Empirische Sozialforschung: Stadtteilzentren als Einzelhandelsstandorte - Das Fallbeispiel Karlsruhe-Durlach}, Karlsruhe Institute of Technology, Institute for Geography and Geoecology, May 2016), own calculations

Stadt Karlsruhe, Amt fuer Stadtentwicklung (2016): \dQuote{Die Karlsruher Bevoelkerung im Dezember 2015}. Stadt Karlsruhe.
}

\seealso{
\code{\link{shopping1}}, \code{\link{shopping2}}, \code{\link{shopping4}}
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