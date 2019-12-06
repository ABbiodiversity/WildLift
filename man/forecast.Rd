\name{CaribouBC}
\alias{CaribouBC-package}
\alias{caribou_settings}
\alias{caribou_forecast}
\alias{caribou_breakeven}
\alias{print.caribou_settings}
\alias{print.caribou_forecast}
\alias{plot.caribou_forecast}
\alias{lines.caribou_forecast}
\alias{summary.caribou_forecast}
\alias{print.summary.caribou_forecast}
\title{
Caribou Population Forecasting
}
\description{
Functions to set demographic and cost parameter, to perform
forecasting, and to inspect the results.
}
\usage{
caribou_settings(pen.type = c("mat.pen",  "pred.excl",
    "moose.red", "wolf.red"), herd = NULL, ...)

caribou_forecast(settings, tmax = 20, pop.start = 100,
    fpen.prop, fpen.inds)

caribou_breakeven(forecast, lambda=1,
    type=c("prop", "inds"), max=10^4, tol=0.01)

\method{print}{caribou_settings}(x, ...)
\method{print}{caribou_forecast}(x, ...)
\method{plot}{caribou_forecast}(x, plot = TRUE, ...)
\method{lines}{caribou_forecast}(x, pen = TRUE, plot = TRUE, ...)
\method{summary}{caribou_forecast}(object, ...)
\method{print}{summary.caribou_forecast}(x, ...)

}
\arguments{
  \item{pen.type}{
character: maternity penning, predator exclusion, moose reduction,
or wolf reduction.
It can also be a settings object.
}
  \item{herd}{
\code{NULL} (default) or character. The following herds are available:
\code{"ColumbiaNorth"}, \code{"ColumbiaSouth"},
\code{"FrisbyQueest"}, \code{"WellsGreySouth"},
\code{"Groundhog"}, \code{"Parsnip"},
\code{"KennedySiding"}, \code{"KlinsezaMoberly"}, \code{"Quintette"}.
Note: only \code{"KennedySiding"}, \code{"KlinsezaMoberly"},
or \code{"Quintette"} can be selected when \code{pen.type = "wolf.red"},
beside a generic herd setting when herd is \code{NULL} that is
specific to the 'wolf removal' treatment type (wild, i.e. 'no wolf removal'
settings for these herds are available under the default
\code{pen.type = "mat.pen"} treatment but no separate generic herd
settings are provided for the 'no wolf removal' treatment).
}
  \item{settings}{
a settings object returned by \code{caribou_settings}.
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
a forecast object returned by \code{caribou_forecast}.
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
additional arguments to functions. For \code{caribou_settings} see Details.
}
}
\details{
The following cost parameters can be set by \code{caribou_settings}:

\code{pen.cap}: how many individual Caribou can live in a
single maternity pen.

\code{pen.cost.setup}: initial cost in thousands to set up pen.

\code{pen.cost.proj}: annual costs in thousands for project manager.

\code{pen.cost.maint}: annual cost in thousands for patrolling
and repairing fence.

\code{pen.cost.capt}: annual cost in thousands to capture cows, monitor,
survey, calf collar.

\code{pen.cost.pred}: annual cost in thousands for removing predators.

The following demographic parameters can be set by \code{caribou_settings}:

\code{c.surv.wild}: calf survival rate in the wild, annual.

\code{c.surv.capt}: calf survival rate when captive, annual.

\code{f.surv.wild}: adult female survival when wild, annual.

\code{f.surv.capt}: adult female survival when captive, annual.

\code{f.preg.wild}: pregnancy rate when wild.

\code{f.preg.capt}: pregnancy rate when captive.
}
\value{
\code{caribou_settings} returns a settings object.

\code{caribou_forecast} returns a forecast object.

\code{caribou_breakeven} returns a numeric value representing the
'breakeven' proportion of females penned where \code{lambda}
is within \code{tol}erance. It returns \code{NA} when
proportion satisfying the \code{lambda} criterion cannot be found,
alongside a warning.

The \code{print} method returns the input object \code{x} invisibly.

The \code{summary} method returns population and cost summaries
for the forecast object.

The \code{plot} and \code{lines} methods return the plotted data invisibly
(years, pen and no-pen population size for \code{plot};
years, pen or no-pen population size for \code{lines}).
Both methods produce plots as a side effect.
}
\seealso{
\code{\link{caribou_matrix}}, \code{\link{caribou_breeding}}
}
\examples{
## Predefined settings
(s1 <- caribou_settings("mat.pen"))
(s2 <- caribou_settings("pred.excl"))

## Modifying predefined settings
caribou_settings("mat.pen", c.surv.capt=0.65, pen.cap=30)
caribou_settings(s1, c.surv.capt=0.65, pen.cap=30)

## Forecast based on settings for 75\% females penned
(f1 <- caribou_forecast(s1, fpen.prop = 0.75))
(f2 <- caribou_forecast(s2, fpen.prop = 0.75))

## Get population and cost summaries
summary(f1)
summary(f2)

## Plot the results
plot(f2)
lines(f1, col = 2)
legend("topleft", col = c(1,1,2), lty = c(2,1,1),
    legend = c("No pen", "Mat pen", "Pred excl"))

## Find 'breakeven' proportion of females penned where lambda=1
(b1 <- caribou_breakeven(f1, lambda = 1))
(b2 <- caribou_breakeven(f2, lambda = 1))
f3 <- caribou_forecast(s1, fpen.prop = b1)
f4 <- caribou_forecast(s2, fpen.prop = b2)
## See that lines are truly flat
op <- par(mfrow = c(1, 2))
plot(f3, main = "Mat pen")
plot(f4, main = "Pred excl")
par(op)
}
\keyword{ ts }
\keyword{ methods }
\keyword{ utilities }
