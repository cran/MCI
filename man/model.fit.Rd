\name{model.fit}
\alias{model.fit}

\title{
Goodness of fit statistics for the Huff model
}
\description{
This function calculates several goodness of fit values to evaluate how good the Huff model fits the empirical observations.
}
\usage{
model.fit(y_obs, y_exp)
}

\arguments{
  \item{y_obs}{
Observed values of the dependent variable \eqn{y}
}
  \item{y_exp}{
Expected values of the dependent variable \eqn{y} (model results)
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
data(Freiburg1)
data(Freiburg2)
data(Freiburg3)
# Loads the data

# NOTE: This may take a while!
# huffmat_total <- huff.attrac(Freiburg1, "district", "store", "salesarea", "distance", lambda = -2,
# dtype= "pow", lambda2 = NULL, Freiburg2, "ppower", Freiburg3, "store", "annualsales", 
# output = "total", show_proc = TRUE)
# Local optimization of store attractivity using the function huff.attrac()
# returns a data frame with total values (observed and expected after optimization)
# which is stored into huffmat_total

# model.fit(huffmat_total$total_obs, huffmat_total$sum_E_j)
# returns a list with fit statistics (sum of sq. resid., pseudo-R-squared, global error, mape)

# Results can be adressed directly:
# huff_fit <- model.fit(huffmat_total$total_obs, huffmat_total$sum_E_j)
# huff_fit$mape
}