wildlift_forecast <-
function(settings, tmax=20, pop.start=100, fpen.prop, fpen.inds)
{
    if (tmax < 1)
        stop("Argument tmax must be >= 1.")
    if (abs(round(tmax) - tmax) > 0.0001)
        warning("Argument tmax was rounded to nearest integer.")
    tmax <- as.integer(round(tmax)) # this must be integer
    if (pop.start < 1)
        stop("Argument pop.start must be >= 1.")
    if (abs(round(pop.start) - pop.start) > 0.0001)
        warning("Argument pop.start was rounded to nearest integer.")
    pop.start <- as.integer(round(pop.start))
    ## penned prop or inds
    if (missing(fpen.prop))
        fpen.prop <- NULL
    if (missing(fpen.inds))
        fpen.inds <- NULL
    if (!is.null(fpen.prop) && !is.null(fpen.inds)) {
        stop("Provide fpen.prop or fpen.inds but not both.")
    }
    fpen.inds.vec <- numeric(tmax) # default is 0
    if (is.null(fpen.prop) && is.null(fpen.inds)) {
        USE_PROP <- TRUE
        fpen.prop <- 0
    }
    if (!is.null(fpen.prop) && is.null(fpen.inds)) {
        if (length(fpen.prop) > 1)
            stop("Penned proportion must be a single value.")
        if (fpen.prop > 1 || fpen.prop < 0)
            stop("Argument fpen.prop must be in the [0, 1] interval.")
        USE_PROP <- TRUE
    }
    if (is.null(fpen.prop) && !is.null(fpen.inds)) {
        if (any(fpen.inds < 0))
            stop("Argument fpen.inds must not be negative.")
        USE_PROP <- FALSE
        # do not round
        #fpen.inds <- as.integer(fpen.inds)
        # cannot be longer than tmax - silently truncated
        if (length(fpen.inds) > tmax)
            fpen.inds <- fpen.inds[seq_len(tmax)]
        fpen.inds.vec[seq_along(fpen.inds)] <- fpen.inds
    }
    ## settings
    #pen.type <- attr(settings, "pen.type")
    ## Cost
    pen.cap <- settings$pen.cap
    ## one time cost
    pen.cost1 <- settings$pen.cost.setup
    ## yearly costs
    pen.cost2 <- settings$pen.cost.proj +
        settings$pen.cost.maint +
        settings$pen.cost.capt +
        settings$pen.cost.pred
    ## Caribou vital rates
    c.surv.capt <- settings$c.surv.capt
    c.surv.wild <- settings$c.surv.wild
    f.surv.wild <- settings$f.surv.wild
    f.surv.capt <- settings$f.surv.capt
    preg <- settings$f.preg.wild
    f.preg.capt <- settings$f.preg.capt

    ## get stable stage distribution for year 1
    if (!USE_PROP) {
        ## initial proportion (yr 1)
        fpen.prop <- fpen.inds.vec[1] / pop.start
    }
    # mean female surv = weighted av. of pen/wild vitals
    surv.f <- fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild
    # and for pregnancy
    preg.f <- fpen.prop*f.preg.capt + (1-fpen.prop)*preg
    surv.c <- fpen.prop*c.surv.capt + (1-fpen.prop)*c.surv.wild
    A <- matrix(c(
        0,      0,      0,      0.5*preg.f*surv.f,# Fecundity of stages
        surv.c, 0,      0,      0,      # Survival of stage (age) 0-1
        0,      surv.f, 0,      0,      # Survival of stage (age) 1-2
        0,      0,      surv.f, surv.f),# Survival of stage (age) 2-3, and 3+
        nrow=4, byrow=TRUE)
    # extract stable stage distribution
    Stable.st <- eigen.analysis(A)$stable.stage
    # assign correct # of animals to each age class
    Nstart <- matrix(pop.start*Stable.st, ncol=1)
    #Nstart <- matrix(pop.start*Stable.st/Stable.st[4], ncol=1) # 2018-11-04
    # starting populations for time loop
    N1 <- N2 <- Nstart

    # loop through time to project population
    for(i in seq_len(tmax)) {
        ## reset prop for later years (divide by rep.adult.pen)
        if (!USE_PROP) {
            ## original code says: fpen.prop <- fpen.inds.vec[i] / N1[4,]
            ## but I think it should be the cumulative number,
            ## but we need to incorporate mortality,
            ## therefore new penned inds added to fpen.prop*N1[4,]
            fpen.prop <- (fpen.inds.vec[i] + fpen.prop*N1[4,]) / N1[4,]
        }
        # .f1 denotes vitals for pop that's partially penned
        surv.f1 <- fpen.prop*f.surv.capt + (1-fpen.prop)*f.surv.wild
        preg.f1 <- fpen.prop*f.preg.capt + (1-fpen.prop)*preg
        surv.c1 <- fpen.prop*c.surv.capt + (1-fpen.prop)*c.surv.wild
        # .f2 denotes vitals for pop that's entirely wild
        surv.f2 <- f.surv.wild
        preg.f2 <- preg
        surv.c2 <- c.surv.wild
        A1 <- matrix(c(
            0,   0,   0,   0.5*preg.f1*surv.f1,  # population with penning
            surv.c1, 0,   0,   0,
            0,   surv.f1, 0,   0,
            0,   0,  surv.f1, surv.f1),
            nrow=4, byrow=TRUE)
        A2 <- matrix(c(
            0,   0,   0,   0.5*preg.f2*surv.f2, # population without penning
            surv.c2, 0,   0,   0,
            0,   surv.f2, 0,   0,
            0,   0,  surv.f2, surv.f2),
            nrow=4, byrow=TRUE)
        # performance of pen pop if pen removed, at  t
        pen.removed <- A2 %*% N1
        # project population (w/pen) to t
        N1 <- A1 %*% N1
        # project population (no pen) to t
        N2 <- A2 %*% N2
        # eigen analysis of each population
        eigs.A1 <- eigen.analysis(A1)
        eigs.A2 <- eigen.analysis(A2)
        # demographic boost of the pen, in yr. t
        pen.diff <- N1 - pen.removed
        # additional juves from penning, time t
        juv.from.pen.t <- pen.diff[2,]
        # additional adults from penning, time t?
        adult.from.pen.t <-  sum(pen.diff[3:4,])
        # how many total reproductive adults in penned pop
        rep.adult.pen <- N1[4,]
        # how many total rep. adults in wild pop
        rep.adult.nopen <- N2[4,]
        # total pop. size of penned pop
        tot.pen <- sum(N1[,1])
        # how many adult females in the pen?
        tot.adult.in.pen = N1[4,1]*fpen.prop
        #tot.adult.in.pen = N1[4,1]
        # total pop. size of wild pop
        tot.nopen <- sum(N2[,1])
        # how many new bou made in time t?
        new.bou.t <- tot.pen-tot.nopen

        #### Calculate costs of penning
        # how many pens exist at time t-1?
        if (i==1) {
            pens.avail <- 0
        } else {
            pens.avail <- pens.needed
        }
        # no partial pens allowed... current pen needs.
        pens.needed <- ceiling(round(tot.adult.in.pen)/pen.cap)
        new.pens <- max(0, pens.needed-pens.avail)
        num.pens <- pens.avail + new.pens
        pens.needed <- num.pens
        pens.cost.t <- (pen.cost1*new.pens + # cost to construct new pens
            pen.cost2*num.pens)/1000         # cost to maintain all pens
        # how much will these pens cost per new bou?
        pens.cost.per.bou <- pens.cost.t/new.bou.t
        # cumulative cost of penning
        if (i==1) {
            pens.cost.cum <- pens.cost.t
        } else {
            pens.cost.cum <- sum(pens.cost.t, Npop$pens.cost.t)
        }
        # cumulative caribou produced
        if (i==1) {
            cum.bou <- new.bou.t
        } else {
            cum.bou <- sum(new.bou.t, Npop$new.bou.t)
        }
        # cost per bou: cumulative cost/cum caribou pop diff
        pens.cost.cum.bou <- pens.cost.cum/cum.bou

        Nt <- data.frame(
            lam.pen = eigs.A1$lambda1,
            lam.nopen = eigs.A2$lambda1, # store the data for time t
            N.pen = tot.pen,
            N.nopen = tot.nopen,
            new.bou.t = new.bou.t,
            pens.needed = pens.needed,
            pens.cost.t = pens.cost.t,
            pens.cost.per.bou = pens.cost.per.bou,
            pens.cost.cum = pens.cost.cum,
            pens.cost.cum.bou = pens.cost.cum.bou,
            rep.adult.pen = rep.adult.pen,
            rep.adult.nopen = rep.adult.nopen,
            juv.from.pen.t = juv.from.pen.t,
            adult.from.pen.t = adult.from.pen.t,
            s.c.pop.pen= surv.c1,
            s.c.pop.nopen=surv.c2,
            s.f.pop.pen = surv.f1,
            s.f.pop.nopen = surv.f2)
        # store the data for all t
        if (i==1) {
            Npop <- Nt
        } else {
            Npop <- rbind(Npop, Nt)
        }
    } # end time (years) loop
    Npop.r1 <- data.frame(
        lam.pen = eigs.A1$lambda1,
        lam.nopen = eigs.A2$lambda1, # make data for year 0 (start)
        N.pen = sum(Nstart),
        N.nopen = sum(Nstart),
        new.bou.t=0,
        pens.needed= 0,
        pens.cost.t = 0,
        pens.cost.per.bou = 0,
        pens.cost.cum = 0,
        pens.cost.cum.bou = 0,
        rep.adult.pen = Nstart[4,],
        rep.adult.nopen= Nstart[4,],
        juv.from.pen.t = 0,
        adult.from.pen.t = 0,
        s.c.pop.pen= surv.c1,
        s.c.pop.nopen = surv.c2,
        s.f.pop.pen = surv.f1,
        s.f.pop.nopen=surv.f2)
    # add year 0 data to projection data
    Npop <- rbind(Npop.r1, Npop)
    # summaries
    Nend_nopen <- floor(Npop$N.nopen[tmax + 1L])
    Nend_pen <- floor(Npop$N.pen[tmax + 1L])
    Nend_diff <- Nend_pen - Nend_nopen
    Cost_total <- sum(Npop$pens.cost.t)
    Cost_percap <- if (Nend_diff <= 0) NA else Cost_total / Nend_diff
    out <- list(
        Npop=Npop,
        settings=settings,
        tmax=tmax,
        pop.start=pop.start,
        fpen.prop=NULL,
        fpen.inds=NULL,
        npens=Npop$pens.needed[tmax + 1L],
        lam.pen=Npop$lam.pen[tmax + 1L],
        lam.nopen=Npop$lam.nopen[tmax + 1L],
        Nend.nopen = Nend_nopen,
        Nend.pen = Nend_pen,
        Nend.diff = Nend_diff,
        Cost.total = Cost_total,
        Cost.percap = Cost_percap)
    if (USE_PROP) {
        out$fpen.prop <- fpen.prop
    } else {
        out$fpen.inds <- fpen.inds
    }
    class(out) <- "wildlift_forecast"
    out$call <- match.call()

    out
}
