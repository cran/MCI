\name{huff.lambda}
\alias{huff.lambda}

\title{
Fitting the distance parameter lambda in the Huff model
}

\description{
This function estimates a distance decay parameter from observed total store/location data (e.g. complete annual turnovers) using bisection or "trial and error"
}

\usage{
huff.lambda(huffdataset, origins, locations, attrac, dist, gamma = 1, atype = "pow", 
gamma2 = NULL, lambda_startv = -1, lambda_endv = -3, dtype = "pow", 
localmarket_dataset, origin_id, localmarket, 
location_dataset, location_id, location_total, 
method = "bisection", iterations = 10, output = "matrix", 
plotVal = FALSE, show_proc = FALSE, check_df = TRUE)
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
  \item{gamma}{
a single numeric value of \eqn{\gamma} for the exponential weighting of the attraction variable (default: 1)
}
  \item{atype}{
Type of attraction weighting function: \code{atype = "pow"} (power function), \code{atype = "exp"} (exponential function) or \code{atype = "logistic"} (default: \code{atype = "pow"})
}
  \item{gamma2}{
if \code{atype = "logistic"} a second \eqn{\gamma} parameter is needed
}
  \item{lambda_startv}{
Start value for \eqn{\lambda} search
}
  \item{lambda_endv}{
End value for \eqn{\lambda} search
}
  \item{dtype}{
Type of distance weighting function: \code{dtype = "pow"} (power function), \code{dtype = "exp"} (exponential function) or \code{dtype = "logistic"} (default: \code{dtype = "pow"})
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
  \item{method}{
If \code{method = "bisection"} (default value), the optimal \eqn{\lambda} is found by bisection. If \code{method = "compare"}, several values are tried and the best one is found ("trial and error")
}
  \item{iterations}{
a single numeric value for the desired number of iterations
}
  \item{output}{
If \code{output = "iterations"}, the results for every single iteration is shown. If \code{output = "total"}, total sales and market shares (or total market area) of the suppliers are shown. 
}
  \item{plotVal}{
If \code{plotVal = TRUE}, the function shows a simple plot of \eqn{\lambda} values against residuals
}
  \item{show_proc}{
logical argument that indicates if the function prints messages about the state of process during the work (e.g. \dQuote{Processing variable xyz ...}). Default: \code{show_proc = FALSE} (messages off)
}
  \item{check_df}{
logical argument that indicates if the given dataset is checked for correct input, only for internal use, should not be deselected (default: \code{TRUE})
}
}

\details{
In many cases, only total empirical values of the suppliers/locations (e.g. annual turnover) can be used for market area estimation. This function fits the \emph{Huff model} by estimating the \eqn{\lambda} parameter iteratively using an optimization algorithm based on the idea of Klein (1988). The fitting process in the \code{huff.lambda} includes of given number of iterations, while the fit gets better with every iteration, measured using the sum of squared residuals of observed vs. expected total values. The iterative optimization can be done via bisection (see Kaw et al. 2011, ch. 03.03) or "trial and error" (see Fuelop et al. 2011).
}

\value{
The function output can be controlled by the function argument \code{output}. If \code{output = "iterations"}, the results for every single iteration is shown (\code{data.frame}). If \code{output = "total"}, total sales and market shares (or total market area) of the suppliers are shown (\code{data.frame}). The default output is a \code{list} with \eqn{\gamma} and \eqn{\lambda}. 
}

\references{
Fuelop, G./Kopetsch, T./Schoepe, P. (2011): \dQuote{Catchment areas of medical practices and the role played by geographical distance in the patient's choice of doctor}. In: \emph{The Annals of Regional Science}, \bold{46}, 3, p. 691-706.

Kaw, A. K./Kalu, E. E./Nguyen, D. (2011): \dQuote{Numerical Methods with Applications}. \url{http://nm.mathforcollege.com/topics/textbook_index.html}

Klein, R. (1988): \dQuote{Der Lebensmittel-Einzelhandel im Raum Verden. Raeumliches Einkaufsverhalten unter sich wandelnden Bedingungen}. \emph{Flensburger Arbeitspapiere zur Landeskunde und Raumordnung}, \bold{6}. Flensburg.
}

\author{
Thomas Wieland
}

\note{
Note that the iterations can be time-consuming and depend on the number of suppliers/locations. Use \code{show_proc = TRUE} for monitoring the iteration process.
}

\seealso{
\code{\link{huff.attrac}}, \code{\link{huff.shares}}, \code{\link{huff.decay}}, \code{\link{huff.fit}}
}

\examples{
data(DIY1)
data(DIY2)
data(DIY3)
# Loading the three DIY store datasets

DIY_alldata <- merge (DIY1, DIY2, by.x = "j_destination", by.y = "j_destination")
# Add store data to distance matrix

huff_DIY <- huff.shares (DIY_alldata, "i_origin", "j_destination", "A_j_salesarea_sqm", 
"t_ij_min", gamma = 1, lambda = -2)
# Calculating Huff local market shares
# Gamma = 1, Lambda = -2

huff_DIY <- merge (huff_DIY, DIY3, by.x = "i_origin", by.y = "district")
# Add data for origins

huff_DIY_total <- shares.total (huff_DIY, "i_origin", "j_destination", "p_ij", 
"population")
# Calculating total market areas (=sums of customers)

colnames(DIY3) <- c("district", "pop")
# Change column name to "pop" (must be other name)

huff.lambda (huff_DIY, "i_origin", "j_destination", "A_j_salesarea_sqm", 
"t_ij_min", gamma = 1, atype = "pow", gamma2 = NULL,
lambda_startv = -1, lambda_endv = -2.5, dtype= "pow",
DIY3, "district", "pop", huff_DIY_total, "suppliers_single", "sum_E_j", 
method = "bisection", iterations = 10)
# Iterative search for the best lambda value using bisection
# Output: gamma and lambda

huff.lambda (huff_DIY, "i_origin", "j_destination", "A_j_salesarea_sqm", 
"t_ij_min", gamma = 1, atype = "pow", gamma2 = NULL,
lambda_startv = -1, lambda_endv = -2.5, dtype= "pow",
DIY3, "district", "pop", huff_DIY_total, "suppliers_single", "sum_E_j", 
method = "bisection", iterations = 10, output = "iterations", show_proc = TRUE)
# Same procedure, output: single iterations  

huff.lambda (huff_DIY, "i_origin", "j_destination", "A_j_salesarea_sqm", 
"t_ij_min", gamma = 1, atype = "pow", gamma2 = NULL,
lambda_startv = -1, lambda_endv = -2.5, dtype= "pow",
DIY3, "district", "pop", huff_DIY_total, "suppliers_single", "sum_E_j", 
method = "compare", iterations = 10, output = "iterations", show_proc = TRUE, plotVal = TRUE)
# Using compare method, output: single iterations and plot 
}