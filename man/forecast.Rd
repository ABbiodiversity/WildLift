\name{WildLift}
\alias{WildLift-package}
\alias{wildlift_settings}
\alias{wildlift_forecast}
\alias{wildlift_breakeven}
\alias{print.wildlift_settings}
\alias{print.wildlift_forecast}
\alias{plot.wildlift_forecast}
\alias{lines.wildlift_forecast}
\alias{summary.wildlift_forecast}
\alias{print.summary.wildlift_forecast}
\title{
Population Forecasting
}
\description{
Functions to set demographic and cost parameter, to perform
forecasting, and to inspect the results.
}
\usage{
wildlift_settings(pen.type = c("mat.pen",  "pred.excl",
    "moose.red", "wolf.red", "cons.breed"), herd = NULL, ...)

wildlift_forecast(settings, tmax = 20, pop.start = 100,
    fpen.prop, fpen.inds)

wildlift_breakeven(forecast, lambda=1,
    type=c("prop", "inds"), max=10^4, tol=0.01)

\method{print}{wildlift_settings}(x, ...)
\method{print}{wildlift_forecast}(x, ...)
\method{plot}{wildlift_forecast}(x, plot = TRUE, ...)
\method{lines}{wildlift_forecast}(x, pen = TRUE, plot = TRUE, ...)
\method{summary}{wildlift_forecast}(object, ...)
\method{print}{summary.wildlift_forecast}(x, ...)

}
\arguments{
  \item{pen.type}{
character: maternity penning, predator exclusion, moose reduction,
wolf reduction, or conservation breeding.
It can also be a settings object.
}
  \item{herd}{
\code{NULL} (default, East Side Athabasca) or character.
The following herds are available:
\code{"EastSideAthabasca"} (Boreal ecotype)
\code{"ColumbiaNorth"}, \code{"ColumbiaSouth"},
\code{"FrisbyQueest"}, \code{"WellsGreySouth"},
\code{"Groundhog"}, \code{"Parsnip"},
\code{"KennedySiding"}, \code{"KlinsezaMoberly"}, \code{"Quintette"}
(Southern Mountain ecotype).
Note: only \code{"KennedySiding"}, \code{"KlinsezaMoberly"},
or \code{"Quintette"} can be selected when \code{pen.type = "wolf.red"},
beside a generic herd setting when herd is \code{NULL} that is
specific to the 'wolf removal' treatment type (wild, i.e. 'no wolf removal'
settings for these herds are available under the default
\code{pen.type = "mat.pen"} treatment but no separate generic herd
settings are provided for the 'no wolf removal' treatment).
}
  \item{settings}{
a settings object returned by \code{wildlift_settings}.
}
  \item{tmax}{
positive integer, number of years to forecast after initial year 0.
}
  \item{pop.start}{
positive integer, initial population size in year 0.
}
  \item{fpen.prop, fpen.inds}{
\code{fpen.prop} is a single numeric value between 0 and 1
for the proportion of females penned;
\code{fpen.inds} is a non-negative integer
for the number of females penned, can be a vector giving
number of females penned in each year to be accumulated.
Only one of \code{fpen.prop} or \code{fpen.inds} can be provided at a time.
Any of the two arguments can be \code{NULL} or missing, but
not both at the same time.
}
  \item{forecast}{
a forecast object returned by \code{wildlift_forecast}.
}
  \item{lambda}{
numeric, annual intrinsic growth rate, must be >0.
}
  \item{type}{
what to provide as output: proportion (\code{"prop"})
or numbers (\code{"inds"}) of females penned
at breakeven point.
}
  \item{max}{
numeric (>0), maximum value for breakeven optimization when
\code{type = "inds"}.
}
  \item{tol}{
numeric, tolerance limit, i.e. the maximum acceptable deviation
from \code{lambda} when solving for 'breakeven'
proportion of females penned.
}
  \item{x, object}{
an object to print, summarize, plot, etc.
}
  \item{plot}{
logical, whether a plot is to be produced.
}
  \item{pen}{
logical, whether to display the pen or no-pen (baseline) results.
}
  \item{\dots}{
additional arguments to functions. For \code{wildlift_settings} see Details.
}
}
\details{
The following cost parameters can be set by \code{wildlift_settings}:

\code{pen.cap}: how many adult females can live in a
single maternity pen.

\code{pen.cost.setup}: initial cost in thousands to set up pen.

\code{pen.cost.proj}: annual costs in thousands for project manager.

\code{pen.cost.maint}: annual cost in thousands for patrolling
and repairing fence.

\code{pen.cost.capt}: annual cost in thousands to capture cows, monitor,
survey, calf collar.

\code{pen.cost.pred}: annual cost in thousands for removing predators.

The following demographic parameters can be set by \code{wildlift_settings}:

\code{c.surv.wild}: calf survival rate in the wild, annual.

\code{c.surv.capt}: calf survival rate when captive, annual.

\code{f.surv.wild}: adult female survival when wild, annual.

\code{f.surv.capt}: adult female survival when captive, annual.

\code{f.preg.wild}: pregnancy rate when wild.

\code{f.preg.capt}: pregnancy rate when captive.
}
\value{
\code{wildlift_settings} returns a settings object.

\code{wildlift_forecast} returns a forecast object.

\code{wildlift_breakeven} returns a numeric value representing the
'breakeven' proportion of females penned where \code{lambda}
is within \code{tol}erance. It returns \code{NA} when
proportion satisfying the \code{lambda} criterion cannot be found,
alongside a warning.

The \code{print} method returns the input object \code{x} invisibly.

The \code{summary} method returns population and cost summaries
for the forecast object (cost is in million dollars).

The \code{plot} and \code{lines} methods return the plotted data invisibly
(years, pen and no-pen population size for \code{plot};
years, pen or no-pen population size for \code{lines}).
Both methods produce plots as a side effect.
}
\seealso{
\code{\link{wildlift_matrix}}, \code{\link{wildlift_breeding}}
}
\examples{
## Predefined settings
(s1 <- wildlift_settings("mat.pen"))
(s2 <- wildlift_settings("pred.excl"))

## Modifying predefined settings
wildlift_settings("mat.pen", c.surv.capt=0.65, pen.cap=30)
wildlift_settings(s1, c.surv.capt=0.65, pen.cap=30)

## Forecast based on settings for 75\% females penned
(f1 <- wildlift_forecast(s1, fpen.prop = 0.75))
(f2 <- wildlift_forecast(s2, fpen.prop = 0.75))

## Get population and cost summaries
summary(f1)
summary(f2)

## Plot the results
plot(f2)
lines(f1, col = 2)
legend("topleft", col = c(1,1,2), lty = c(2,1,1),
    legend = c("No pen", "Mat pen", "Pred excl"))

## Find 'breakeven' proportion of females penned where lambda=1
(b1 <- wildlift_breakeven(f1, lambda = 1))
(b2 <- wildlift_breakeven(f2, lambda = 1))
f3 <- wildlift_forecast(s1, fpen.prop = b1)
f4 <- wildlift_forecast(s2, fpen.prop = b2)
## See that lines are truly flat
op <- par(mfrow = c(1, 2))
plot(f3, main = "Mat pen")
plot(f4, main = "Pred excl")
par(op)
}
\keyword{ ts }
\keyword{ methods }
\keyword{ utilities }
