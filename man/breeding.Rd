\name{breeding}
\alias{caribou_matrix}
\alias{caribou_breeding}
\alias{print.caribou_breeding}
\alias{summary.caribou_breeding}
\alias{plot.caribou_breeding}
\title{
Caribou Conservation Breeding
}
\description{
Functions evaluate different options for conservation breeding.
}
\usage{
caribou_matrix(settings, wild=TRUE,
    age.cens=3, age.1st.litter=3, age.calf.max=1)

caribou_breeding(settings,
    in.inds=10, out.prop=1, f.surv.trans=1,
    j.surv.trans=1, j.surv.red=1,
    tmax=20, pop.start=100)

\method{print}{caribou_breeding}(x, ...)
\method{summary}{caribou_breeding}(object, ...)
\method{plot}{caribou_breeding}(x, plot = TRUE, ...)
}
\arguments{
  \item{settings}{
a settings object returned by \code{\link{caribou_settings}}.
}
  \item{wild}{
logical, whether to use the penned or wild vital \code{settings}.
}
  \item{age.cens}{
censoring age. The projection matrix will contain one-year
age classes up to \code{age.cens}, all age classes above
this are combined into a single age class.
}
  \item{age.1st.litter}{
integer, female age class at 1st litter.
}
  \item{age.calf.max}{
integer, maximum age class to be considered as calf, [0-2) by default
(i.e. age class starting with year 1).
}
  \item{in.inds}{
integer vector, number of adult females put into pen each year.
0s are appended to the value meaning that no females
are added after the last value (up until year \code{tmax}).
}
  \item{out.prop}{
numeric vector between 0 and 1, the proportion of juvenile females
transferred from the penned to the recipient herd.
The last value is repeated when length is less than \code{tmax}.
Can be useful if juvenile female transfer should be delayed
to build up stock first.
}
  \item{f.surv.trans}{
numeric between 0 and 1, adult female survival during
capture and transportation into the penned population.
}
  \item{j.surv.trans}{
numeric between 0 and 1, juvenile female survival during
capture and transportation from penned to recipient herd.
}
  \item{j.surv.red}{
numeric between 0 and 1,
transported juvenile female survival reduction factor
for 1 year following capture and traportation.
}
  \item{tmax}{
positive integer, number of years to forecast after initial year 0.
}
  \item{pop.start}{
positive integer, initial population size in year 0 for the
recipient and status quo populations (wild not receiving females).
}
  \item{x, object}{
an object to print, summarize, plot, etc.
}
  \item{plot}{
logical, whether a plot is to be produced.
}
  \item{\dots}{
additional arguments to functions.
}
}
\details{
The conservation breeding functionality is based on vital rates
provided via the \code{settings} argument.
These rates are turned into a projection matrix
using \code{caribou_matrix}. This projection matrix
reflects survival and reproduction rates based on
the settings, tracking females only based on a sex ratio of 0.5.

The \code{caribou_breeding} function tracks changes
in the conservation population starting with adding
females to produce offspring.

Juvenile females are captured and transferred to the recipient herd.
The proportion of juvenile females transferred can be changed through
\code{out.prop} (0: none, 1: all).

The output object tracks the number of adult females added to the
penned population, and the number of juvenile females transferred
from the penned to the recipient herd.

Besides these, a status quo (wild without receiving juveniles)
population trajectory is used as a reference.
This status quo population uses the same vital rates as the recipient herd,
but does not get the extra juvenile females from the penned population.
}
\value{
\code{caribou_matrix} returns a projection matrix.

\code{caribou_breeding} returns a conservation breeding object.
It contains age specific population sizes for the
penned, recipient, and status quo populations (females only).

The \code{print} method returns the input object \code{x} invisibly.

The \code{summary} method returns population summaries
for the conservation breeding object.

The \code{plot} method return the plotted data invisibly
and produces a plot as a side effect.
}
\seealso{
\code{\link{caribou_settings}}, \code{\link{caribou_forecast}}
}
\examples{
## projection matrix
s <- caribou_settings()

caribou_matrix(s, wild=TRUE)  # wild
caribou_matrix(s, wild=FALSE) # captive

## out.prop = 0.5: move half of the juveniles
x0 <- caribou_breeding(s,
    tmax = 20,        # projection horizon
    in.inds = rep(10, 5),
    out.prop = 0.5)
x0
summary(x0)

## out.prop = 1: move all juveniles
x1 <- update(x0, out.prop = 1)
x1

op <- par(mfrow=c(1, 2))
plot(x0, main="out.prop = 0")
plot(x1, main="out.prop = 1")
par(op)
}
\keyword{ ts }
\keyword{ methods }
\keyword{ utilities }
