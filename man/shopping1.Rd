\name{shopping1}
\alias{shopping1}
\docType{data}
\title{Point-of-sale survey in Karlsruhe}
\description{
The dataset contains a point-of-sale (POS) survey conducted at two retail supply locations (town centre and planned shopping centre) in the east of Karlsruhe (Germany) from May 2016 (raw data). Amongst other things, the participants were asked about their last shopping trip with respect to groceries, clothing als consumer electronics (store choice and expenditures) and their place of residence (ZIP code and city district, respectively). The survey dataset contains 434 cases/interviewed individuals. The survey is \emph{not} representative and should be regarded as an example. 
}
\usage{data("shopping1")}
\format{
  A data frame with 434 observations on the following 29 variables.
  \describe{
    \item{\code{POS}}{a factor indicating the survey location: \code{POS1} (town centre) or \code{POS2} (shopping centre)}
    \item{\code{time}}{a numeric vector containing the code for the time period the interview was conducted}
    \item{\code{date}}{a POSIXct containing the date the interview was conducted}
    \item{\code{POS_traffic}}{a numeric vector containing the code for the traffic mode the respondet used to come to the supply location}
    \item{\code{POS_stay}}{a numeric vector containing the respondents' duration of stay at the supply location}
    \item{\code{POS_expen}}{a numeric vector containing the respondents' expenditures at the supply location}
    \item{\code{POS1_freq}}{a numeric vector containing the frequency of visiting the supply location POS1}
    \item{\code{POS2_freq}}{a numeric vector containing the frequency of visiting the supply location POS2}
    \item{\code{gro_purchase_code}}{a factor containing the destination of the last grocery shopping trip}
    \item{\code{gro_purchase_brand}}{a factor containing the brand (store chain) of the destination of the last grocery shopping trip}
    \item{\code{gro_purchase_channel}}{a factor containing the shopping channel of the destination of the last grocery shopping trip: \code{ambulant}, \code{online} and \code{store}}
    \item{\code{gro_purchase_expen}}{a numeric vector containing the expenditures corresponding to the last grocery shopping trip}
    \item{\code{cloth_purchase_code}}{a factor containing the destination of the last clothing shopping trip}
    \item{\code{cloth_purchase_brand}}{a factor containing the brand (store chain) of the destination of the last clothing shopping trip}
    \item{\code{cloth_purchase_channel}}{a factor containing the shopping channel of the destination of the last clothing shopping trip: \code{mail order}, \code{online} or \code{store}}
    \item{\code{cloth_purchase_expen}}{a numeric vector containing the expenditures corresponding to the last clothing shopping trip}
    \item{\code{ce_purchase_code}}{a factor containing the destination of the last shopping trip with respect to consumer electronics (CE)}
    \item{\code{ce_purchase_brand}}{a factor containing the brand (store chain) of the destination of the last CE shopping trip}
    \item{\code{ce_purchase_channel}}{a factor containing the shopping channel of the destination of the last CE shopping trip: \code{online} or \code{store}}
    \item{\code{ce_purchase_expen}}{a numeric vector containing the expenditures corresponding to the last CE shopping trip}
    \item{\code{resid_PLZ}}{a factor containing the customer origin (place of residence) as ZIP code}
    \item{\code{resid_name}}{a factor containing the customer origin (place of residence) as name of the corresponding city or city district}
    \item{\code{resid_name_official}}{a factor containing the customer origin (place of residence) as official names of the corresponding city or city district}
    \item{\code{resid_code}}{a factor containing the customer origin (place of residence) as internal code}
    \item{\code{age_cat}}{a numeric vector containing the age category of the respondent}
    \item{\code{sex}}{a numeric vector containing the sex of the respondent}
    \item{\code{weekday}}{a numeric vector containing the weekday where the interview took place}
    \item{\code{holiday}}{a numeric vector containing a dummy variable which indicates whether the interview was conducted on a holiday or not}
    \item{\code{survey}}{a factor reflecting the mode of survey: \code{main} is the main survey while \code{pretest} marks the cases from the pretest}
  }
}

\source{
Primary empirical sources:
POS (point of sale) survey in the authors' course (\dQuote{Praktikum Empirische Sozialforschung: Stadtteilzentren als Einzelhandelsstandorte - Das Fallbeispiel Karlsruhe-Durlach}, Karlsruhe Institute of Technology, Institute for Geography and Geoecology, May 2016), own calculations
}

\seealso{
\code{\link{shopping2}}, \code{\link{shopping3}}, \code{\link{shopping4}}
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