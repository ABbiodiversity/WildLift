## preset demography parameters
.get_demography <-
function(
pen.type=c("mat.pen", "pred.excl", "moose.red", "wolf.red"),
herd=NULL) {
    pen.type <- match.arg(pen.type)
    parms <- list()
    if (pen.type != "pred.excl") {
        parms$c.surv.wild <- 0.163       # calf survival rate in the wild, annual
        c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
        c.surv2.capt <- 0.6       # calf survival rate when captive, 1-12 months
        parms$c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
        parms$f.surv.wild <- 0.853       # maternal survival when wild, annual
        parms$f.surv.capt <- 0.903      # maternal survival when captive higher than wild
        parms$f.preg.wild <- 0.92         # pregnancy rate, same for captive and wild
        parms$f.preg.capt <- parms$f.preg.wild       # pregnancy rate, same for captive and wild
    } else {
        parms$c.surv.wild <- 0.163       # calf survival rate in the wild, annual
        c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
        c.surv2.capt <- 0.8       # calf survival rate when captive, 1-12 months
        parms$c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
        parms$f.surv.wild <- 0.853       # maternal survival when wild, annual
        parms$f.surv.capt <- 0.95       # maternal survival when captive higher than wild
        parms$f.preg.wild <- 0.92         # pregnancy rate, same for captive and wild
        parms$f.preg.capt <- parms$f.preg.wild       # pregnancy rate, same for captive and wild
    }
    if (pen.type == "wolf.red" && is.null(herd))
        stop("Must specify a herd for wolf reduction.")
    if (!is.null(herd)) {
        if (pen.type == "wolf.red") {
            Herds <- c(
                "KennedySiding",
                "KlinsezaMoberly",
                "Quintette")
            herd <- match.arg(herd, Herds)
            # these are herds from wolf reduction study: WITH wolf reduction
            # no penning (captive) option is considered
            if (herd == "KennedySiding") {
                #parms$f.surv.capt <- 0.894
                parms$f.surv.wild <- 0.962
                #parms$c.surv.capt <- 0.540
                parms$c.surv.wild <- 0.554
            }
            if (herd == "KlinsezaMoberly") {
                #parms$f.surv.capt <- 0.798
                parms$f.surv.wild <- 0.860
                #parms$c.surv.capt <- 0.540
                parms$c.surv.wild <- 0.506
            }
            if (herd == "Quintette") {
                #parms$f.surv.capt <- 0.860
                parms$f.surv.wild <- 0.917
                #parms$c.surv.capt <- 0.540
                parms$c.surv.wild <- 0.489
            }
        } else {
            Herds <- c(
                "ColumbiaNorth",
                "ColumbiaSouth",
                "FrisbyQueest",
                "WellsGreySouth",
                "Groundhog",
                "Parsnip",
                "KennedySiding",
                "KlinsezaMoberly",
                "Quintette")
            herd <- match.arg(herd, Herds)
            #baseline calf survival and AFS for 6 ranges
            if (herd == "ColumbiaNorth") {
                parms$c.surv.wild <- 0.217
                parms$f.surv.wild <- 0.784
            }
            if (herd == "ColumbiaSouth") {
                parms$c.surv.wild <- 0.285
                parms$f.surv.wild <- 0.767
            }
            if (herd == "FrisbyQueest") {
                parms$c.surv.wild <- 0.363
                parms$f.surv.wild <- 0.853 # default value
            }
            if (herd == "WellsGreySouth") {
                parms$c.surv.wild <- 0.239
                parms$f.surv.wild <- 0.868
            }
            if (herd == "Groundhog") {
                parms$c.surv.wild <- 0.234
                parms$f.surv.wild <- 0.853 # default value
            }
            if (herd == "Parsnip") {
                parms$c.surv.wild <- 0.163 # default value
                parms$f.surv.wild <- 0.875
            }

            # these are herds from wolf reduction study: NO wolf reduction
            # no penning (captive) option is considered
            if (herd == "KennedySiding") {
                if (pen.type != "mat.pen")
                    stop(sprintf("%s herd is not available for pen.type=%s.", herd, pen.type))
                #parms$f.surv.capt <- 0.894
                parms$f.surv.wild <- 0.844
                #parms$c.surv.capt <- 0.540
                parms$c.surv.wild <- 0.283
            }
            if (herd == "KlinsezaMoberly") {
                if (pen.type != "mat.pen")
                    stop(sprintf("%s herd is not available for pen.type=%s.", herd, pen.type))
                #parms$f.surv.capt <- 0.798
                parms$f.surv.wild <- 0.748
                #parms$c.surv.capt <- 0.540
                parms$c.surv.wild <- 0.308
            }
            if (herd == "Quintette") {
                if (pen.type != "mat.pen")
                    stop(sprintf("%s herd is not available for pen.type=%s.", herd, pen.type))
                #parms$f.surv.capt <- 0.860
                parms$f.surv.wild <- 0.810
                #parms$c.surv.capt <- 0.540
                parms$c.surv.wild <- 0.294
            }

            # Maternity Pen:
            # - AFS increases by 0.05 for each of the ranges.
            # - Calf Survival always increases to 0.54.
            if (pen.type == "mat.pen") {
                parms$c.surv.capt <- 0.54
                parms$f.surv.capt <- parms$f.surv.wild + 0.05
            }
            # Predator Exclosures:
            # - AFS always increases to 0.95 and
            # - Calf Survival always increases to 0.72.
            if (pen.type == "pred.excl") {
                parms$c.surv.capt <- 0.72
                parms$f.surv.capt <- 0.95
            }
        }
    }
    if (pen.type == "moose.red") {
        parms$f.surv.wild <- 0.879
    }
    parms
}

## preset cost parameters
.get_cost <-
function(pen.type=c("mat.pen", "pred.excl", "moose.red", "wolf.red")) {
    pen.type <- match.arg(pen.type)
    parms <- list()
    if (pen.type != "pred.excl") {
        parms$pen.cap <- 35       # how many individual caribou can live in mat pen?
        parms$pen.cost.setup <- 500 # cost in thousands to set up pen
        parms$pen.cost.proj <- 80 # costs of project manager
        parms$pen.cost.maint <- 300 # cost in thousands for patrolling and repairing fence + shepherd + contingencies
        parms$pen.cost.capt <- 250 # cost in thousands to capture cows, monitor, survey, calf collar
        parms$pen.cost.pred <- 0 # cost in thousands for removing predators annually
    } else {
        parms$pen.cap <- 35        # how many individual caribou can live in big pen?
        parms$pen.cost.setup <- (77*24) + 20 # cost in thousands to set up pen
        parms$pen.cost.proj <- 80 # costs of project manager
        parms$pen.cost.maint <- 600 # cost in thousands for patrolling and repairing fence + contingencies
        parms$pen.cost.capt <- 200 # cost in thousands to capture cows, monitor, survey, calf collar
        parms$pen.cost.pred <- 80 # cost in thousands for removing predators annually
    }
    parms
}

caribou_settings <-
function(
pen.type=c("mat.pen", "pred.excl", "moose.red", "wolf.red"),
herd=NULL,
...) {
    if (inherits(pen.type, "caribou_settings")) {
        parms <- pen.type
    } else {
        parms <- c(.get_demography(pen.type, herd), .get_cost(pen.type))
        attr(parms, "pen.type") <- pen.type
        if (!is.null(herd))
            attr(parms, "herd") <- herd
        class(parms) <- "caribou_settings"
    }
    dots <- list(...)
    if (length(dots) > 0L)
        if (is.null(names(dots)) || any(names(dots) == ""))
            stop("All arguments must be named.")
    for (i in names(dots)) {
        if (i %in% names(parms)) {
            if (length(dots[[i]]) > 1L)
                stop(sprintf("Parameter %s must be scalar.", i))
            parms[[i]] <- dots[[i]]
        } else {
            stop(sprintf("Unexpected parameter: %s.", i))
        }
    }
    parms
}
