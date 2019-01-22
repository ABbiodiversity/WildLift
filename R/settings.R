caribou_settings <-
function(pen.type=c("mat.pen",  "pred.excl", "moose.red"), ...) {
    if (inherits(pen.type, "caribou_settings")) {
        parms <- pen.type
    } else {
        pen.type <- match.arg(pen.type)
        parms <- list()
        if (pen.type=="mat.pen") {
            ## costs
            parms$pen.cap <- 35       # how many individual caribou can live in mat pen?
            parms$pen.cost.setup <- 500 # cost in thousands to set up pen
            parms$pen.cost.proj <- 80 # costs of project manager
            parms$pen.cost.maint <- 300 # cost in thousands for patrolling and repairing fence + shepherd + contingencies
            parms$pen.cost.capt <- 250 # cost in thousands to capture cows, monitor, survey, calf collar
            parms$pen.cost.pred <- 0 # cost in thousands for removing predators annually
            ## demography
            parms$c.surv.wild <- 0.163       # calf survival rate in the wild, annual
            c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
            c.surv2.capt <- 0.6       # calf survival rate when captive, 1-12 months
            parms$c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
            parms$f.surv.wild <- 0.853       # maternal survival when wild, annual
            parms$f.surv.capt <- 0.903      # maternal survival when captive higher than wild
            parms$f.preg.wild <- 0.92         # pregnancy rate, same for captive and wild
            parms$f.preg.capt <- parms$f.preg.wild       # pregnancy rate, same for captive and wild
        }
        if (pen.type=="pred.excl") {
            ## cost
            parms$pen.cap <- 35        # how many individual caribou can live in big pen?
            parms$pen.cost.setup <- (77*24) + 20 # cost in thousands to set up pen
            parms$pen.cost.proj <- 80 # costs of project manager
            parms$pen.cost.maint <- 600 # cost in thousands for patrolling and repairing fence + contingencies
            parms$pen.cost.capt <- 200 # cost in thousands to capture cows, monitor, survey, calf collar
            parms$pen.cost.pred <- 80 # cost in thousands for removing predators annually
            ## demography
            parms$c.surv.wild <- 0.163       # calf survival rate in the wild, annual
            c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
            c.surv2.capt <- 0.8       # calf survival rate when captive, 1-12 months
            parms$c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
            parms$f.surv.wild <- 0.853       # maternal survival when wild, annual
            parms$f.surv.capt <- 0.95       # maternal survival when captive higher than wild
            parms$f.preg.wild <- 0.92         # pregnancy rate, same for captive and wild
            parms$f.preg.capt <- parms$f.preg.wild       # pregnancy rate, same for captive and wild
        }
        if (pen.type=="moose.red") {
            ## costs
            parms$pen.cap <- 35       # how many individual caribou can live in mat pen?
            parms$pen.cost.setup <- 500 # cost in thousands to set up pen
            parms$pen.cost.proj <- 80 # costs of project manager
            parms$pen.cost.maint <- 500 # cost in thousands for patrolling and repairing fence
            parms$pen.cost.capt <- 250 # cost in thousands to capture cows, monitor, survey, calf collar
            parms$pen.cost.pred <- 0 # cost in thousands for removing predators annually
            ## demography
            parms$c.surv.wild <- 0.163       # calf survival rate in the wild, annual
            c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
            c.surv2.capt <- 0.6       # calf survival rate when captive, 1-12 months
            parms$c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
            parms$f.surv.wild <- 0.87       # maternal survival when wild, annual
            parms$f.surv.capt <- 0.903      # maternal survival when captive higher than wild
            parms$f.preg.wild <- 0.92         # pregnancy rate, same for captive and wild
            parms$f.preg.capt <- parms$f.preg.wild       # pregnancy rate, same for captive and wild
        }
        attr(parms, "pen.type") <- pen.type
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
