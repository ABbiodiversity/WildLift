## methods for wildlift_settings

print.wildlift_settings <- function(x, ...) {
    cat("Caribou settings - pen type:", attr(x, "pen.type"),
        "\n\n")
    str(x[names(x) != "call"],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    invisible(x)
}

## methods for wildlift_forecast

print.wildlift_forecast <- function(x, ...) {
    cat("Caribou forecast - pen type:", attr(x$settings, "pen.type"),
        "\n\n")
    WHAT <- if (is.null(x$fpen.prop)) {
        c("tmax", "pop.start", "fpen.inds")
    } else {
        c("tmax", "pop.start", "fpen.prop")
    }
    str(x[WHAT],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    invisible(x)
}
plot.wildlift_forecast <- function(x, plot=TRUE, ...) {
    d0 <- lines(x, pen=FALSE, plot=FALSE)
    d1 <- lines(x, pen=TRUE, plot=FALSE)
    if (plot) {
        plot(N ~ Years, d1, type="l", lty=1, ylim=c(0, max(d0$N, d1$N)), ...)
        lines(N ~ Years, d0, lty=2, ...)
    }
    invisible(data.frame(Years=d0$Years, Nnopen=d0$N, Npen=d1$N))
}
lines.wildlift_forecast <- function(x, pen=TRUE, plot=TRUE, ...) {
    d <- data.frame(Years=seq(0, x$tmax),
        N=if (pen) x$Npop$N.pen else x$Npop$N.nopen)
    if (plot)
        lines(N ~ Years, d, ...)
    invisible(d)
}
summary.wildlift_forecast <- function(object, ...) {
    object$Npop <- NULL
    object$call <- NULL
    object$settings$call <- NULL
    class(object) <- "summary.wildlift_forecast"
    object
}
print.summary.wildlift_forecast <- function(x, ...) {
    cat("Caribou forecast - pen type:", attr(x$settings, "pen.type"),
        "\n\n")
    WHAT <- if (is.null(x$fpen.prop)) {
        c("tmax", "pop.start", "fpen.inds")
    } else {
        c("tmax", "pop.start", "fpen.prop")
    }
    str(x[WHAT],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    cat("\n")
    str(x[c("npens", "lam.pen", "lam.nopen",
        "Nend.nopen", "Nend.pen", "Nend.diff", "Cost.total", "Cost.percap")],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    invisible(x)
}

## methods for wildlift_breeding

print.wildlift_breeding <- function(x, ...) {
    cat("Caribou captive breeding:\n\n")
    WHAT <- c("tmax", "pop.start",
        "f.surv.trans", "j.surv.trans", "j.surv.red")
    str(x[WHAT],
        give.attr=FALSE, give.head=FALSE, comp.str = "- ", no.list=TRUE)
    cat("\n")
    N <- x$population[x$tmax:(x$tmax+1L), c("Ncapt", "Nrecip", "Nwild")]
    m <- rbind(N=N[2L,], lambda=N[2L,]/N[1L,])
    colnames(m) <- c("capt", "recip", "wild")
    print(format(m, digits=getOption("digits")-3L))
    invisible(x)
}

summary.wildlift_breeding <- function(object, ...) {
    object$population
}

plot.wildlift_breeding <- function(x, plot=TRUE, ...) {
    N <- x$population
    if (plot) {
        plot(N$Year, N$Ncapt, ylim=c(0, max(N[,-1])), type="l",
            ylab="Individuals", xlab="Years", ...)
        lines(N$Year, N$Nrecip, col=2)
        lines(N$Year, N$Nwild, col=4)
        lines(N$Year, N$Ncapt-N$Nout, col=1, lty=2)
        lines(N$Year, N$Nin, col=1, lty=3)
        legend("topright", bty="n", lty=1, col=c(1,2,4),
            legend=c("Captive", "Recipient", "Wild"))
    }
    invisible(N)
}

