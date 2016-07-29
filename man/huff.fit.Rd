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
lambda2 = NULL, localmarket_dataset, localmarket, location_dataset, location_id, 
location_total, tolerance = 5, iterations = 3, output = "total", show_proc = FALSE, 
check_df = TRUE)
}

\arguments{
  \item{huffdataset}{
an interaction matrix which is a \code{data.frame} containing the origins, locations and the explanatory variables (attractivity, transport costs)
}
  \item{origins}{
the column in the interaction matrix \code{huffdataset} containing the origins (e.g. ZIP codes)
}
  \item{locations}{
the column in the interaction matrix \code{huffdataset} containing the locations (e.g. store codes)
}
  \item{attrac}{
the column in the interaction matrix \code{huffdataset} containing the attractivity variable (e.g. sales area)
}
  \item{dist}{
the column in the interaction matrix \code{huffdataset} containing the transport costs (e.g. travelling time or street distance)
}
  \item{lambda}{
a single numeric value of \eqn{\lambda} for the (exponential) weighting of distance (transport costs, default: -2)
}
  \item{dtype}{
Type of distance weighting function: \code{dtype = "pow"} (power function), \code{dtype = "exp"} (exponential function) or \code{dtype = "logistic"} (default: \code{dtype = "pow"})
}
  \item{lambda2}{
if \code{dtype = "logistic"} a second \eqn{\lambda} parameter is needed
}
  \item{localmarket_dataset}{
A \code{data.frame} containing the origins saved in a column which has the same name as in \code{huffdataset} and another column containing the local market potential 
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
accepted value of relative difference between observed (\eqn{T_{j,obs}}) and modeled total values \eqn{T_{j,exp}} to skip a local optimization of location/supplier \eqn{j}
}
  \item{iterations}{
a single numeric value for the desired number of iterations
}
  \item{output}{
Type of function output: \code{output = "matrix"} returns a Huff interaction matrix with the optimized attractivity values and the expected market shares/market areas. If \code{output = "total"}, the old (observed) and the new (expected) total values are returned. If \code{output = "total"}, the optimized attractivity values are returned. 
}
  \item{show_proc}{
logical argument that indicates if the function prints messages about the state of process during the work (e.g. \dQuote{Processing variable xyz ...} or \dQuote{Variable xyz is regarded as dummy variable}). Default: \code{show_proc = FALSE} (messages off)
}
  \item{check_df}{
logical argument that indicates if the given dataset is checked for correct input, only for internal use, should not be deselected (default: \code{TRUE})
}
}

\details{
The \emph{Huff model} (Huff 1962, 1963, 1964) is the most popular \emph{spatial interaction model} for retailing and services and belongs to the family of \emph{probabilistic market area models}. The basic idea of the model, derived from the \emph{Luce choice axiom}, is that consumer decisions are not deterministic but probabilistic, so the decision of customers for a shopping location in a competitive environment cannot be predicted exactly. The results of the model are probabilities for these decisions, which can be interpreted as market shares of the regarded locations (\eqn{j}) in the customer origins (\eqn{i}), \eqn{p_{ij}}. The model results can be regarded as an equilibrium solution (consumer equilibrium) with logically consistent market shares (0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). From a theoretical perspective, the model is based on an utility function with two explanatory variables ("attractivity" of the locations, transport costs between origins and locations), which are weighted by an exponent: \eqn{U_{ij}=A_{j}^\gamma d_{ij}^{-\lambda}}. But the weighting functions can also be exponential or logistic. When the "real" local market shares were observed, the model can be fit by using the \emph{log-centering transformation} resp. the \emph{MCI model} (see the functions \code{mci.fit()}, \code{mci.transmat()} and \code{mci.transvar()}). In many cases, only total empirical values of the suppliers/locations can be used for fitting the model. This function fits the \emph{Huff model} not by estimating the parameters but by optimizing the attractivity variable iteratively (transport cost weighting by \eqn{\lambda} is given) using a modified version of the \emph{local optimization of attractivity} algorithm developed by Guessefeldt (2002). The fitting process includes of given number of (\eqn{m}) iterations, while the fit gets better with every iteration. The algorithm results can be evaluated by several diagnosis criteria which have been frequently used to evaluate \emph{Huff model} results: Besides the sum of squared residuals, the function also calculates a \emph{Pseudo-R-squared} measure and the \emph{MAPE (mean average percentage error)}, both used by De Beule et al. (2014), and the \emph{global error} used by Klein (1988).      
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
# Loading the three Freiburg datasets
# huff.fit(Freiburg1, "district", "store", "salesarea", "distance", lambda = -2, dtype= "pow",
# lambda2 = NULL, Freiburg2, "ppower", Freiburg3, "store", "annualsales", tolerance = 5,
# iterations = 2, output = "total")
# Local optimization of store attractivity with two iterations using the function huff.fit()
# returns a data frame with total values (observed and expected after optimization)
}