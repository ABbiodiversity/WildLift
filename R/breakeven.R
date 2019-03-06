## find fpan.prop where lambda=1 within tolerance
caribou_breakeven <- function(forecast, lambda=1, tol=0.001) {
    if (lambda <= 0)
        stop("Argument lambda must be > 0.")
    fun <- function(p) {
        f <- caribou_forecast(forecast$settings,
            tmax=forecast$tmax, pop.start=forecast$pop.start, fpen.prop=p)
        abs(f$Npop$lam.pen[1L] - lambda)
    }
    o <- optimize(fun, c(0, 1))
    if (o$objective > abs(tol)) {
        warning("Could not find break even point within tolerance.")
        o$minimum <- NA
    }
    o$minimum
}
