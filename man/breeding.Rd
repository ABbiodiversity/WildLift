\name{breeding}
\alias{caribou_matrix}
\alias{caribou_breeding}
\alias{print.caribou_breeding}
\alias{summary.caribou_breeding}
\alias{plot.caribou_breeding}
\title{
Caribou Captive Breeding
}
\description{
Functions evaluate different options for captive breeding.
}
\usage{
caribou_matrix(settings, age.cens=3,
    wild=TRUE, age.1st.litter=3, age.calf.max=1)

caribou_breeding(settings, age.cens=3,
    in.age=3, in.inds=10, in.max=35,
    out.age=1, out.prop=1, tmax=20,
    pop.start=100, age.1st.litter=3, age.calf.max=1)

\method{print}{caribou_breeding}(x, ...)
\method{summary}{caribou_breeding}(object, ...)
\method{plot}{caribou_breeding}(x, plot = TRUE, ...)
}
\arguments{
  \item{settings}{
a settings object returned by \code{\link{caribou_settings}}.
}
  \item{age.cens}{
censoring age. The projection matrix will contain one-year
age classes up to \code{age.cens}, all age classes above
this are combined into a single age class.
}
  \item{wild}{
logical, whether to use the captive or wild vital \code{settings}.
}
  \item{age.1st.litter}{
integer, female age class at 1st litter.
}
  \item{age.calf.max}{
integer, maximum age class to be considered as calf, [0-2) by default
(i.e. age class starting with year 1).
}
  \item{in.age}{
integer (single value or vector of values), age classes of
females put into captivity.
}
  \item{in.inds}{
integer (length must match \code{in.age}), number of females
put into captivity, by age class according to \code{in.age}.
The same number of females is added each year until the captive
population reaches \code{in.max}.
}
  \item{in.max}{
numeric, breeding facility capacity, max population size in captivity.
}
  \item{out.age}{
integer (single value or vector of values), age classes of
calves transferred from the captive to the recipient herd.
}
  \item{out.prop}{
numeric between 0 and 1, tuning the number of calves
transferred from the captive to the recipient herd.
The amount varies between the excess number
(\code{out.prop=0}, min of total transferable calves and
individuals above facility capacity) and the total number of
transferable calves (\code{out.prop=0}, the difference
between the remaining population and \code{in.max} is replaced
by incomong females).
}
  \item{tmax}{
positive integer, number of years to forecast after initial year 0.
}
  \item{pop.start}{
positive integer, initial population size in year 0 for the
recipient and wild populations.
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
The captive breeding functionality is based on vital rates
provided via the \code{settings} argument.
These rates are turned into a projection matrix
using \code{caribou_matrix}.

The \code{caribou_breeding} function tracks changes
in the captive population starting with adding
females to produce offspring.

The captive breeding facility hosts a maximum (\code{in.max})
number of animals.
When reached and/or suitable aged calves are produced, those
are transferred to the recipient herd.

The amount of transferred calves varies between the excess number
(individuals above facility capacity) and the total number of
transferable calves depending on the \code{out.prop} argument.

The output object tracks the individuals added to the captive population,
and the individuals transferred from the captive to the recipient herd.
Besides these, a wild population trajectory is used as a reference.
This wild population uses the same vital rates as the recipient herd,
but does not get the extra calves from the breeding population.
}
\value{
\code{caribou_matrix} returns a projection matrix.

\code{caribou_breeding} returns a captive breeding object.
It contains age specific population sizes for the
captive, recipient, and wild populations.

The \code{print} method returns the input object \code{x} invisibly.

The \code{summary} method returns population summaries
for the captive breeding object.

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

## out.prop = 0: move only N[t]-in.max youngs
x0 <- caribou_breeding(s,
    age.cens = 18,    # proj matrix censored at this age
    tmax = 20,        # projection horizon
    in.age = c(3, 4), # 5x 3yr old + 5x 4-yr old female --> captive
    in.inds = c(5, 5),
    in.max = 60,      # capacity of breeding facility
    out.age = c(1, 2),# 1 and 2-yr old calves --> recipient
    out.prop = 0)
x0
summary(x0)

## out.prop = 1: move all youngs and replace with females
x1 <- update(x0, out.prop = 1)

op <- par(mfrow=c(1, 2))
plot(x0, main="out.prop = 0")
plot(x1, main="out.prop = 1")
par(op)
}
\keyword{ ts }
\keyword{ methods }
\keyword{ utilities }
