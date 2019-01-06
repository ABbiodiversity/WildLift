print.caribou_settings <- function(x, ...) {
    cat("Caribou settings - pen type:", attr(x, "pen.type"),
        "\n\n")
    str(x,
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    invisible(x)
}
print.caribou_forecast <- function(x, ...) {
    cat("Caribou forecast - pen type:", attr(x$settings, "pen.type"),
        "\n\n")
    str(x[c("tmax", "pop.start", "fpen.prop")],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    invisible(x)
}
plot.caribou_forecast <- function(x, plot=TRUE, ...) {
    d0 <- lines(x, pen=FALSE, plot=FALSE)
    d1 <- lines(x, pen=TRUE, plot=FALSE)
    if (plot) {
        plot(N ~ Years, d1, type="l", lty=1, ylim=c(0, max(d0$N, d1$N)), ...)
        lines(N ~ Years, d0, lty=2, ...)
    }
    invisible(x)
}
lines.caribou_forecast <- function(x, pen=TRUE, plot=TRUE, ...) {
    d <- data.frame(Years=seq(0, x$tmax),
        N=if (pen) x$Npop$N.pen else x$Npop$N.nopen)
    if (plot)
        lines(N ~ Years, d, ...)
    invisible(d)
}
summary.caribou_forecast <- function(object, ...) {
    object$Npop <- NULL
    class(object) <- "summary.caribou_forecast"
    object
}
print.summary.caribou_forecast <- function(x, ...) {
    cat("Caribou forecast - pen type:", attr(x$settings, "pen.type"),
        "\n\n")
    str(x[c("tmax", "pop.start", "fpen.prop")],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    cat("\n")
    str(x[c("npens", "lam.pen", "lam.nopen",
        "Nend.nopen", "Nend.pen", "Nend.diff", "Cost.total", "Cost.percap")],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    invisible(x)
}
