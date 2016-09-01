\name{huff.fit}
\alias{huff.fit}

\title{
Fitting the Huff model using local optimization of attractivity 
}
\description{
This function fits the Huff model with a given interaction matrix by optimizing the attractivity values of suppliers/locations iteratively and calculates the market shares/market areas
}
\usage{
huff.fit(huffdataset, origins, locations, attrac, dist, lambda = -2, dtype = "pow", 
lambda2 = NULL, localmarket_dataset, origin_id, localmarket, location_dataset, 
location_id, location_total, tolerance = 5, iterations = 3, output = "total", 
show_proc = FALSE, check_df = TRUE)
}

\arguments{
  \item{huffdataset}{
an interaction matrix which is a \code{data.frame} containing the origins, locations and the explanatory variables (attraction, transport costs)
}
  \item{origins}{
the column in the interaction matrix \code{huffdataset} containing the origins (e.g. ZIP codes)
}
  \item{locations}{
the column in the interaction matrix \code{huffdataset} containing the locations (e.g. store codes)
}
  \item{attrac}{
the column in the interaction matrix \code{huffdataset} containing the attraction variable (e.g. sales area)
}
  \item{dist}{
the column in the interaction matrix \code{huffdataset} containing the transport costs (e.g. travelling time or street distance)
}
  \item{lambda}{
a single numeric value of \eqn{\lambda} for the (exponential) weighting of distance (transport costs, default: -2)
}
  \item{dtype}{
Type of distance weighting function: \code{"pow"} (power function), \code{"exp"} (exponential function) or \code{"logistic"} (logistic function) (default: \code{dtype = "pow"})
}
  \item{lambda2}{
if \code{dtype = "logistic"} a second \eqn{\lambda} parameter is needed
}
  \item{localmarket_dataset}{
A \code{data.frame} containing the origins saved in a column which has the same name as in \code{huffdataset} and another column containing the local market potential 
}
  \item{origin_id}{
the column in the dataset \code{localmarket_dataset} containing the origins (e.g. statistical districts, ZIP codes)
}
  \item{localmarket}{
the column in the dataset \code{localmarket_dataset} containing the local market potential (e.g. purchasing power, number of customers)
}
  \item{location_dataset}{
A \code{data.frame} containing the suppliers/locations and their observed total values 
}
  \item{location_id}{
the column in the dataset \code{location_dataset} containing the locations (e.g. store codes), \eqn{j}, according to the codes in \code{huffdataset}
}
  \item{location_total}{
the column in the dataset \code{location_dataset} containing the observed total values of suppliers/locations, \eqn{T_{j,obs}} (e.g. annual sales, total number of customers)
}
  \item{tolerance}{
accepted value of absolute percentage error between observed (\eqn{T_{j,obs}}) and modeled total values (\eqn{T_{j,exp}}) to skip a local optimization of location/supplier \eqn{j}
}
  \item{iterations}{
a single numeric value for the desired number of iterations
}
  \item{output}{
Type of function output: \code{output = "matrix"} returns a Huff interaction matrix with the optimized attractivity values and the expected market shares/market areas. If \code{output = "total"}, the old (observed) and the new (expected) total values are returned. If \code{output = "diag"}, the function returns the diagnosis results (fitting measures) 
}
  \item{show_proc}{
logical argument that indicates if the function prints messages about the state of process during the work (e.g. \dQuote{Processing variable xyz ...} or \dQuote{Variable xyz is regarded as dummy variable}). Default: \code{show_proc = FALSE} (messages off)
}
  \item{check_df}{
logical argument that indicates if the given dataset is checked for correct input, only for internal use, should not be deselected (default: \code{TRUE})
}
}

\details{
In many cases, only total empirical values of the suppliers/locations can be used for market area estimation. This function fits the \emph{Huff model} not by estimating the parameters but by optimizing the attraction variable (transport cost weighting by \eqn{\lambda} is given) using an optimization algorithm based on the idea of the \emph{local optimization of attraction} algorithm developed by Guessefeldt (2002) and other model fit approaches. The fitting process in the \code{huff.fit} includes of given number of (\eqn{m}) iterations, while the fit gets better with every iteration. The algorithm results can be evaluated by several diagnosis criteria which have been frequently used to evaluate \emph{Huff model} results: Besides the sum of squared residuals, the function also calculates a \emph{Pseudo-R-squared} measure and the \emph{MAPE (mean average percentage error)}, both used by De Beule et al. (2014), and the \emph{global error} used by Klein (1988).      
}
\value{
The function output can be controlled by the function argument \code{output}. If \code{output = "matrix"} the function returns a Huff interaction matrix with the optimized attractivity values and the expected market shares/market areas. If \code{output = "total"}, the old (observed) and the new (expected) total values are returned. If \code{output = "diag"}, the diagnosis results (fitting measures) are returned. All results are \code{data.frame}. 
}

\references{
De Beule, M./Van den Poel, D./Van de Weghe, N. (2014): \dQuote{An extended Huff-model for robustly benchmarking and predicting retail network performance}. In: \emph{Applied Geography}, \bold{46}, 1, p. 80-89.

Guessefeldt, J. (2002): \dQuote{Zur Modellierung von raeumlichen Kaufkraftstroemen in unvollkommenen Maerkten}. In: \emph{Erdkunde}, \bold{56}, 4, p. 351-370.

Klein, R. (1988): \dQuote{Der Lebensmittel-Einzelhandel im Raum Verden. Raeumliches Einkaufsverhalten unter sich wandelnden Bedingungen}. \emph{Flensburger Arbeitspapiere zur Landeskunde und Raumordnung}, \bold{6}. Flensburg.

Wieland, T. (2015): \dQuote{Nahversorgung im Kontext raumoekonomischer Entwicklungen im Lebensmitteleinzelhandel - Konzeption und Durchfuehrung einer GIS-gestuetzten Analyse der Strukturen des Lebensmitteleinzelhandels und der Nahversorgung in Freiburg im Breisgau}. Projektbericht. Goettingen : GOEDOC, Dokumenten- und Publikationsserver der Georg-August-Universitaet Goettingen. \url{http://webdoc.sub.gwdg.de/pub/mon/2015/5-wieland.pdf}
}

\author{
Thomas Wieland
}

\note{
Note that the iterations can be time-consuming and depend on the number of suppliers/locations. Use \code{show_proc = TRUE} for monitoring the iteration process.
}

\seealso{
\code{\link{huff.attrac}}, \code{\link{huff.shares}}, \code{\link{huff.decay}}
}

\examples{
data(Freiburg1)
data(Freiburg2)
data(Freiburg3)
# Loading the three Freiburg datasets

# NOTE: This may take a while!
# huff_total_opt2 <- huff.fit(Freiburg1, "district", "store", "salesarea", "distance", 
# lambda = -2, dtype= "pow", lambda2 = NULL, Freiburg2, "district", "ppower", 
# Freiburg3, "store", "annualsales", tolerance = 1, iterations = 2, output = "total", 
# show_proc = TRUE)
# 2 iterations of the optimization algorithm with an accepted difference of +/- 1 %
# Output of total sales/shares, stored in dataset huff_total_opt10

# model.fit(huff_total_opt2$total_obs, huff_total_opt2$sum_E_j, plotVal = TRUE)
# total_obs = observed total values, originally from dataset Freiburg3
# sum_E_j = expected total values
}