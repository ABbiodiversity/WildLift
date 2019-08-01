## find fpen.prop where lambda=1 within tolerance
## note, this will not do multiple years (i.e. vector of fpen.inds)
caribou_breakeven <- function(forecast, lambda=1,
type=c("prop", "inds"), max=10^4, tol=0.01) {
    if (lambda <= 0)
        stop("Argument lambda must be > 0.")
    type <- match.arg(type)
    if (type == "prop") {
        fun <- function(x) {
            f <- caribou_forecast(forecast$settings,
                #tmax=forecast$tmax,
                tmax=1L,
                pop.start=forecast$pop.start,
                fpen.prop=x)
            abs(f$Npop$lam.pen[1L] - lambda)
        }
        o <- optimize(fun, c(0, 1))
    } else {
        if (max <= 0)
            stop("Argument max must be > 0.")
        fun <- function(x) {
            f <- caribou_forecast(forecast$settings,
                #tmax=forecast$tmax,
                tmax=1L,
                pop.start=forecast$pop.start,
                fpen.inds=x)
            abs(f$Npop$lam.pen[1L] - lambda)
        }
        o <- optimize(fun, c(0, max))
    }
    if (o$objective > abs(tol)) {
        warning("Could not find break even point within tolerance.")
        o$minimum <- NA
    }
    o$minimum
}
