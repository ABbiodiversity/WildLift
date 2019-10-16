## create projection matrix
caribou_matrix <- function(settings, wild=TRUE,
age.cens=3, age.1st.litter=3, age.calf.max=1) {
    if (wild) {
        surv.c <- settings$c.surv.wild
        surv.f <- settings$f.surv.wild
        preg.f <- settings$f.preg.wild
    } else {
        surv.c <- settings$c.surv.capt
        surv.f <- settings$f.surv.capt
        preg.f <- settings$f.preg.capt
    }
    if (age.1st.litter < age.calf.max)
        stop("age.1st.litter must be >= age.calf.max")
    # i=1 is [0,1), i=2 is [1,2)
    A <- matrix(0, age.cens+1, age.cens+1)
    for (i in seq_len(age.cens)) {
        A[i+1L, i] <- if (i > age.calf.max)
            surv.f else surv.c
    }
    for (i in seq_len(age.cens)+1L) {
        if (i > age.1st.litter)
            A[1L, i] <- 0.5*preg.f*surv.f
    }
    A[age.cens+1L, age.cens+1L] <- if (age.cens+1L > age.calf.max)
        surv.f else surv.c
    l <- seq_len(age.cens+1)
    l <- paste0("[", l-1L, ",", l, ")")
    l[length(l)] <- paste0("[", age.cens, ",", Inf, "]")
    dimnames(A) <- list(l, l)
    A
}

## initial function
.caribou_breeding <- function(settings, age.cens=3,
in.age=3, # ages of females added in each year, matching in.inds
in.inds=10, # number of females added each year
in.max=35, # capacity of captive breeding facility, excess goes into recipient pop
out.age=1, # age of inds pumped out
out.prop=1, # 0=excess only, 1=as many as there, 0-1=proportionally between
tmax=20,
pop.start=100, # wild / recipient population
age.1st.litter=3, age.calf.max=1) {
    Aw <- caribou_matrix(settings, wild=TRUE,
        age.cens=age.cens,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    Ac <- caribou_matrix(settings, wild=FALSE,
        age.cens=age.cens,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    out.age <- as.integer(out.age)
    if (any(out.age <= 0))
        stop("out.age must be positive integer")
    in.age <- as.integer(in.age)
    if (any(in.age <= 0))
        stop("in.age must be positive integer")
    in.inds <- as.integer(in.inds)
    if (any(in.inds <= 0))
        stop("in.inds must be positive integer")
    if (length(in.age) != length(in.inds))
        stop("in.age and in.inds must have same length")
    tmax <- as.integer(round(tmax))
    if (tmax < 1)
        stop("tmax must be > 0")
    Nc <- matrix(0, age.cens+1, tmax+1)
    Nin <- Nout <- Nw <- Nw0 <- Nc
    ## input in yr 0
    N0c <- numeric(age.cens+1)
    N0c[in.age+1] <- in.inds
    Nin[,1] <- N0c
    ## captive
    Nc[,1] <- N0c # year 0 pop in captive
    ## recipient (w) and wild (w0 - no influx)
    Nw[,1] <- pop.start * eigen.analysis(Aw)$stable.stage # stable age dist
    ## wild (without receiving inds)
    Nw0 <- pop.projection(Aw, Nw[,1], tmax+1)$stage.vectors
    dimnames(Nw0) <- NULL
    for (i in seq_len(tmax)) {
        ## projecting to next year: reproduction from last year
        Nc[,i+1L] <- Ac %*% Nc[,i]
        Nw[,i+1L] <- Aw %*% Nw[,i]

        ## add new females
        room <- round(in.max - sum(Nc[,i]))
        if (room >= sum(N0c)) {
            Nin[,i+1L] <- N0c
        } else {
            ## when room is less than sum(in.inds)
            ## add inds one-by-one starting with younger ages
            while(room > 0) {
                for (j in sort(in.age)) {
                    Nin[j+1,i+1L] <- Nin[j+1,i+1L] + 1
                    room <- room - 1
                    if (room <= 0)
                        break
                }
            }
        }
        ## adjust captive pop
        Nc[,i+1L] <- Nc[,i+1L] + Nin[,i+1L]

        ## remove youngs
        excess <- round(sum(Nc[,i+1L]) - in.max)
        movable <- floor(sum(Nc[out.age+1L,i+1L]))
        tomove <- out.prop * movable + (1-out.prop) * excess
        ## captive is > capacity: excess goes to recipient pop
        ## remove inds one-by-one starting with younger ages
        while(tomove > 0) {
            for (j in sort(out.age)) {
                Nout[j+1,i+1L] <- Nout[j+1,i+1L] + 1
                tomove <- tomove - 1
                if (tomove <= 0)
                    break
            }
        }
        ## new Nc must be >= 0
        Nout[,i+1L] <- pmin(Nout[,i+1L], floor(Nc[,i+1L]))
        ## adjust captive pop
        Nc[,i+1L] <- Nc[,i+1L] - Nout[,i+1L]
        ## adjust recipient pop
        Nw[,i+1L] <- Nw[,i+1L] + Nout[,i+1L]

        ## remove incoming females when over in.inds
        ## remove inds one-by-one starting with older ages
        over <- min(sum(Nin[,i+1L]), max(0, round(sum(Nc[,i+1]) - in.max)))
        while(over > 0) {
            for (j in rev(sort(in.age))) {
                if (Nc[j+1,i+1L] > 0) {
                    Nin[j+1,i+1L] <- Nin[j+1,i+1L] - 1
                    Nc[j+1,i+1L] <- Nc[j+1,i+1L] - 1
                    over <- over - 1
                }
                if (over <= 0)
                    break
            }
        }
    }
    out <- list(
        call=match.call(),
        settings=settings,
        age.cens=age.cens,
        in.age=in.age,
        in.inds=in.inds,
        in.max=in.max,
        out.age=out.age,
        out.prop=out.prop,
        tmax=tmax,
        pop.start=pop.start,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max,
        Nin=Nin, Nout=Nout, Ncapt=Nc, Nrecip=Nw, Nwild=Nw0)
    out$population <- data.frame(Years=c(0, seq_len(tmax)),
        sapply(out[c("Nin", "Nout", "Ncapt", "Nrecip", "Nwild")], colSums))
    class(out) <- "caribou_breeding"
    out
}

## revised function
caribou_breeding <- function(settings,
in.inds=10, # number of females added each year
out.prop=1, # remove all (1) or none (0), recycled
tmax=20,
pop.start=100) { # wild / recipient population

    age.1st.litter <- 3
    age.calf.max <- 1
    age.cens <- 18
    in.age <- 3:4 # ages of females added in each year, matching in.inds
    out.age <- 1:2 # age of inds pumped out

    Aw <- caribou_matrix(settings, wild=TRUE,
        age.cens=age.cens,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    Ac <- caribou_matrix(settings, wild=FALSE,
        age.cens=age.cens,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    if (length(out.prop) > 1L)
        stop("out.prop must be of length 1")
    if (out.prop < 0 || out.prop > 1)
        stop("out.prop must be a value between 0 and 1")
    tmax <- as.integer(round(tmax))
    if (tmax < 1)
        stop("tmax must be > 0")
    Nc <- matrix(0, age.cens+1, tmax+1)
    Nin <- Nout <- Nw <- Nw0 <- Nc

    if (length(in.inds) < tmax)
        in.inds <- c(in.inds, rep(0, tmax - length(in.inds)))
    if (length(in.inds) > tmax)
        in.inds <- in.inds[seq_len(tmax)]
    in.inds <- as.integer(in.inds)
    if (any(in.inds < 0))
        stop("in.inds values must not be negative")

    ## input in yr 0
    N0c <- numeric(age.cens+1)
    n0 <- in.inds[1L]
    while (n0 > 0) {
        for (j in sort(in.age)) {
            N0c[j+1] <- N0c[j+1] + 1
            n0 <- n0 - 1
            if (n0 <= 0)
                break
        }
    }
    Nin[,1] <- N0c
    ## captive
    Nc[,1] <- N0c # year 0 pop in captive
    ## recipient (w) and wild (w0 - no influx)
    Nw[,1] <- pop.start * eigen.analysis(Aw)$stable.stage # stable age dist
    ## wild (without receiving inds)
    Nw0 <- pop.projection(Aw, Nw[,1], tmax+1)$stage.vectors
    dimnames(Nw0) <- NULL
    for (i in seq_len(tmax)) {
        ## projecting to next year: reproduction from last year
        Nc[,i+1L] <- Ac %*% Nc[,i]
        Nw[,i+1L] <- Aw %*% Nw[,i]

        ## add new females
        room <- in.inds[i]
        ## add inds one-by-one starting with younger ages
        while(room > 0) {
            for (j in sort(in.age)) {
                Nin[j+1,i+1L] <- Nin[j+1,i+1L] + 1
                room <- room - 1
                if (room <= 0)
                    break
            }
        }
        ## adjust captive pop
        Nc[,i+1L] <- Nc[,i+1L] + Nin[,i+1L]

        ## remove youngs
        tomove <- floor(out.prop * sum(Nc[out.age+1L,i+1L]))
        ## remove inds one-by-one starting with younger ages
        while(tomove > 0) {
            for (j in sort(out.age)) {
                Nout[j+1,i+1L] <- Nout[j+1,i+1L] + 1
                tomove <- tomove - 1
                if (tomove <= 0)
                    break
            }
        }
        ## new Nc must be >= 0
        Nout[,i+1L] <- pmin(Nout[,i+1L], floor(Nc[,i+1L]))
        ## adjust captive pop
        Nc[,i+1L] <- Nc[,i+1L] - Nout[,i+1L]
        ## adjust recipient pop
        Nw[,i+1L] <- Nw[,i+1L] + Nout[,i+1L]

    }
    out <- list(
        call=match.call(),
        settings=settings,
        age.cens=age.cens,
        in.age=in.age,
        in.inds=in.inds,
        out.age=out.age,
        out.prop=out.prop,
        tmax=tmax,
        pop.start=pop.start,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max,
        Nin=Nin, Nout=Nout, Ncapt=Nc, Nrecip=Nw, Nwild=Nw0)
    out$population <- data.frame(Years=c(0, seq_len(tmax)),
        sapply(out[c("Nin", "Nout", "Ncapt", "Nrecip", "Nwild")], colSums))
    class(out) <- "caribou_breeding"
    out
}
