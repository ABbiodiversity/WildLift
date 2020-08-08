\name{linear}
\alias{wildlift_linear}
\title{
Linear feature effects on lambda
}
\description{
Linear feature restoration (i.e. forest regeneration)
and linear feature deactivation (e.g. blocking entrances) effects on lambda.
}
\usage{
wildlift_linear(tmax=20, pop.start=100,
    area=10000, lin=0, seism=0, young=0,
    cost=12, yr_deact=5, yr_restor=15)
}
\arguments{
  \item{tmax}{
positive integer, number of years to forecast after initial year 0.
}
  \item{pop.start}{
positive integer, initial population size in year 0.
}
  \item{area}{
positive numeric, area of range in km^2.
}
  \item{lin}{
non-negative numeric, linear features (total length in km)
including \code{seism}ic lines (to be restored or deactivated)
and permanent linear featured (roads etc.).
}
  \item{seism}{
non-negative numeric, seismic lines (total length in km)
to be restored or deactivated.
}
  \item{young}{
numeric between 0 and 100, percent of young (<30 yrs) forest.
}
  \item{cost}{
non-negative numeric, yearly cost of management action (in canadian dollars).
}
  \item{yr_deact}{
non-negative numeric, years it takes for deactivation to reach a level where
it begins to affect lambda.
}
  \item{yr_restor}{
non-negative numeric, years it takes for restoration to reach a level where
it begins to affect lambda.
}
}
\details{
Lambda is calculated as
1.0184 - 0.0234 * (\code{lin} / \code{area}) - 0.0021 * \code{young}.
}
\value{
A list with population table and costs.
}
\seealso{
\code{\link{wildlift_forecast}}
}
\examples{
## nothing to restore/deactivate
wildlift_linear()

## all linear restored/deactivated
wildlift_linear(lin=1000, seism=1000, young=10)

## half of linear is to be restored/deactivated
wildlift_linear(lin=1000, seism=500, young=10)
}
\keyword{ ts }
