\name{ijmatrix.create}
\alias{ijmatrix.create}

\title{
Interaction matrix with market shares
}
\description{
Creation of an interaction matrix with market shares (\eqn{p_{ij}}) of every supplier (\eqn{j}) in every submarket (\eqn{i}) based on the frequencies in the raw data (e.g. household or POS survey). 
}
\usage{
ijmatrix.create(rawdataset, submarkets, suppliers, ..., remNA = TRUE, 
remSing = FALSE, remSing.val = 1, remSingSupp.val = 1, 
correctVar = FALSE, correctVar.val = 1)
}

\arguments{
  \item{rawdataset}{
a \code{data.frame} containing the raw data
}
  \item{submarkets}{
the column in the dataset containing the submarkets (e.g. ZIP codes)
}
  \item{suppliers}{
the column in the dataset containing the suppliers (e.g. store codes) 
}
  \item{\dots}{
other numeric variables in the raw data which were observed and shall be used to calculate market shares (e.g. expenditures)
}
  \item{remNA}{
logical argument that indicates if \code{NA} values of the submarkets and suppliers are removed or not (default: \code{remNA = TRUE}, otherwise: \code{remNA = FALSE}, which means that \code{NA} are considered in the interaction matrix, recoded as factor \code{99999})
}
  \item{remSing}{
logical argument that indicates if singular instances of the submarkets and suppliers are removed or not (default: \code{remSing = FALSE})
}
  \item{remSing.val}{
if \code{remSing = TRUE}: numeric value that indicates the limit value of singular instances in the submarket set to be removed from the data
}
  \item{remSingSupp.val}{
if \code{remSing = TRUE}: numeric value that indicates the limit value of singular instances in the supplier set to be removed from the data
}
  \item{correctVar}{
logical argument that indicates if the calculated market shares shall be corrected when they do not match the MCI standards (\eqn{p_{ij} = 0}) (default: \code{correctVar = FALSE})
}
  \item{correctVar.val}{
if \code{correctVar = TRUE}: numeric value which is added to the absolute values before calculating market shares (default: 1)
}
}
\details{
This function creates an interaction matrix for all \eqn{i} submarkets (e.g. geographical regions) and all \eqn{j} suppliers (e.g. store locations). An interaction matrix is a special case of table where every row is an \eqn{i} x \eqn{j} combination and the market shares of \eqn{j} in \eqn{i} (\eqn{p_{ij}}) are saved in a new column (Linear table, the opposite of crosstable). An interaction matrix is the base for MCI analyses. In the MCI model the dependent variable is the market share of the \eqn{j} suppliers in the \eqn{i} submarkets, in which the shares are \emph{logically consistent} (that means: 0 < \eqn{p_{ij}} < 1, \eqn{\sum_{j=1}^n{p_{ij} = 1}}). The function calculates \eqn{p_{ij}} based on the frequencies and, optionally, further market shares calculated from other observed variables in the given raw dataset (e.g. expenditures from submarket \eqn{i} at supplier \eqn{j}). Single observations with missing submarket or supplier (\code{NA}) are removed from the data automatically (unless \code{remNA = FALSE}). Optionally, singular instances (e.g. some submarkets or suppliers are only represented once or twice in the whole dataset) can also be removed (\code{remSing = TRUE}), where the limit values for extraction can be set by \code{remSing.val} and \code{remSingSupp.val} (e.g. \code{remSing.val = 2} and \code{remSingSupp.val = 1} removes every submarket from the interaction matrix which was observed \eqn{\le 2} and every supplier observed \eqn{\le 1}).
}
\value{
An interaction matrix which is a \code{data.frame} containing the \eqn{i} x \eqn{j} combinations (\code{'interaction'}), the submarkets (column is named as in raw data), the suppliers (column is named as in raw data), the observed absolute frequencies of every \eqn{j} in every \eqn{i} (\code{'freq_ij_abs'}), the observed absolute frequencies in every \eqn{i} (\code{'freq_i_total'}) and the observed market shares of every \eqn{j} in every \eqn{i} (\code{'p_ij_obs'}). If additional variables are stated (e.g. expenditures) which shall be turned into (local) market shares, the output interaction matrix contains absolute values for every interaction, total values for every \eqn{i} submarket and market shares (\eqn{p_{ij}}) for these variables, too, which are automatically named based on the given variable name (e.g. the market shares based on a raw data variable called \code{expen} is named \code{p_ij_obs_expen}). The first three variables of the output matrix are factors, the calculated values are numeric.
}

\references{
Cooper, L. G./Nakanishi, M. (1988): \dQuote{Market-Share Analysis: Evaluating competitive marketing effectiveness}. Boston, Dordrecht, London : Kluwer.

Huff, D. L./McCallum, D. (2008): \dQuote{Calibrating the Huff Model Using ArcGIS Business Analyst}. \emph{ESRI White Paper}, September 2008. \url{https://www.esri.com/library/whitepapers/pdfs/calibrating-huff-model.pdf}

Wieland, T. (2015): \dQuote{Raeumliches Einkaufsverhalten und Standortpolitik im Einzelhandel unter Beruecksichtigung von Agglomerationseffekten. Theoretische Erklaerungsansaetze, modellanalytische Zugaenge und eine empirisch-oekonometrische Marktgebietsanalyse anhand eines Fallbeispiels aus dem laendlichen Raum Ostwestfalens/Suedniedersachsens}. \emph{Geographische Handelsforschung}, \bold{23}. 289 pages. Mannheim : MetaGIS.
}

\author{
Thomas Wieland
}

\seealso{
\code{\link{ijmatrix.shares}}, \code{\link{ijmatrix.crosstab}}
}
\examples{
data(grocery1)
# Loads the data
ijmatrix.create (grocery1, "plz_submarket", "store_code")
# Creates an interaction matrix with local market shares based on frequencies

mynewmcidata <- ijmatrix.create (grocery1, "plz_submarket", "store_code")
# Save results directly in a new dataset

ijmatrix.create (grocery1, "plz_submarket", "store_code", "trip_expen")
# Creates an interaction matrix with local market shares based on frequencies
# and expenditures (Variable "trip_expen")
}