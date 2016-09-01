\name{model.fit}
\alias{model.fit}

\title{
Goodness of fit statistics for the Huff model
}
\description{
This function calculates several goodness of fit values to evaluate how good the observed values fit the empirical observations.
}
\usage{
model.fit(y_obs, y_exp, plotVal = FALSE)
}

\arguments{
  \item{y_obs}{
Observed values of the dependent variable \eqn{y}
}
  \item{y_exp}{
Expected values of the dependent variable \eqn{y} (model results)
}
  \item{plotVal}{
Logical argument that indicates if the function plots a graph comparing observed and expected values
}
}
\details{
This function computes several goodness of fit statistics to evaluate the results of non-linear fitting procedures for the Huff model (see the functions \code{huff.attrac} and \code{huff.fit}). Besides the sum of squared residuals, the function also calculates a \emph{Pseudo-R-squared} measure and the \emph{MAPE (mean average percentage error)}, both used by De Beule et al. (2014), and the \emph{global error} used by Klein (1988).
}

\value{
list:
\item{resids_sq_sum }{Sum of squared residuals}
\item{pseudorsq }{Pseudo-R-squared}
\item{globerr }{Global error}
\item{mape }{Mean average percentage error}

}
\references{
De Beule, M./Van den Poel, D./Van de Weghe, N. (2014): \dQuote{An extended Huff-model for robustly benchmarking and predicting retail network performance}. In: \emph{Applied Geography}, \bold{46}, 1, p. 80-89.

Klein, R. (1988): \dQuote{Der Lebensmittel-Einzelhandel im Raum Verden. Raeumliches Einkaufsverhalten unter sich wandelnden Bedingungen}. \emph{Flensburger Arbeitspapiere zur Landeskunde und Raumordnung}, \bold{6}. Flensburg.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{huff.fit}}
}

\examples{
# Controlling the fit of a Huff Model market area estimation #

data(Freiburg1)
data(Freiburg2)
data(Freiburg3)
# Loads the data

huff_mat <- huff.shares (Freiburg1, "district", "store", "salesarea", "distance")
# Market area estimation using the Huff Model with standard parameters
# (gamma = 1, lambda = -2)

huff_mat_pp <- merge (huff_mat, Freiburg2)
# Adding the purchasing power data for the city districts

huff_total <- shares.total (huff_mat_pp, "district", "store", "p_ij", "ppower")
# Total expected sales and shares

huff_total_control <- merge (huff_total, Freiburg3, by.x = "suppliers_single",
by.y = "store")

model.fit(huff_total_control$annualsales, huff_total_control$sum_E_j, plotVal = TRUE)
# Observed vs. expected

# Results can be adressed directly:
huff_fit <- model.fit(huff_total_control$annualsales, huff_total_control$sum_E_j, plotVal = TRUE)
huff_fit$mape
}