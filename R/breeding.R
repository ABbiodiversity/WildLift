## create projection matrix
wildlift_matrix <- function(settings, wild=TRUE,
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
    if (age.1st.litter <= age.calf.max)
        stop("age.1st.litter must be > age.calf.max")
    # i=1 is [0,1), i=2 is [1,2)
    A <- matrix(0, age.cens+1, age.cens+1)
    for (i in seq_len(age.cens)) {
        A[i+1L, i] <- if (i > age.calf.max)
            surv.f else surv.c
    }
    for (i in seq_len(age.cens)+1L) {
        if (i > age.1st.litter)
            A[1L, i] <- 0.5*preg.f*surv.f # 0.5x b/c we only track females
    }
    A[age.cens+1L, age.cens+1L] <- if (age.cens+1L > age.calf.max)
        surv.f else surv.c
    l <- seq_len(age.cens+1)
    l <- paste0("[", l-1L, ",", l, ")")
    l[length(l)] <- paste0("[", age.cens, ",", Inf, "]")
    dimnames(A) <- list(l, l)
    A
}

wildlift_breeding <- function(settings,
in.inds=10, # number of females added each year
out.prop=1, # remove all (1) or none (0), recycled
f.surv.trans=1, # female survival during transportation into captive
j.surv.trans=1, # juv survival during transportation into recipient
j.surv.red=1, # transported juv survival reduction for 1 yr
tmax=20,
pop.start=100, # wild / recipient population
breed.early=FALSE, # reproduce at age 2 if well fed
f.preg.capt.2 = 0.57) { # fecundity rate for the 2 yrs old

    age.1st.litter.facility <- 3
    age.1st.litter <- 3
    age.calf.max <- 1
    age.cens <- 4
    in.age <- 3:4 # ages of females added in each year, matching in.inds
    out.age <- 1 # age of inds pumped out

    Aw <- wildlift_matrix(settings, wild=TRUE, # wild
        age.cens=age.cens,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    Ac <- wildlift_matrix(settings, wild=FALSE, # in facility
        age.cens=age.cens,
        age.1st.litter=age.1st.litter.facility,
        age.calf.max=age.calf.max)
    ## reproduce at age 2 or 3 depending on conditions
    ## only inside facility
    ## fecundity rate for the 2 yrs old should be 0.57 (Adam et al. 2019)
    if (breed.early)
        Ac["[0,1)", "[2,3)"] <- 0.5 * f.preg.capt.2 * settings$f.surv.capt
    if (f.surv.trans < 0 || f.surv.trans > 1)
        stop("f.surv.trans must be a value between 0 and 1")
    if (j.surv.trans < 0 || j.surv.trans > 1)
        stop("j.surv.trans must be a value between 0 and 1")
    if (j.surv.red < 0 || j.surv.red > 1)
        stop("j.surv.red must be a value between 0 and 1")
    tmax <- as.integer(round(tmax))
    if (tmax < 1)
        stop("tmax must be > 0")
    Nc <- matrix(0, age.cens+1, tmax+1)
    dimnames(Nc) <- list(rownames(Aw), 0:tmax)
    Nin <- Nout <- Nw <- Nw0 <- Nc

    if (length(in.inds) < tmax)
        in.inds <- c(in.inds, rep(0, tmax - length(in.inds)))
    if (length(in.inds) > tmax)
        in.inds <- in.inds[seq_len(tmax)]
    in.inds <- as.integer(in.inds)
    if (any(in.inds < 0))
        stop("in.inds values must not be negative")

    if (length(out.prop) < tmax)
        out.prop <- c(out.prop,
            rep(out.prop[length(out.prop)], tmax - length(out.prop)))
    if (length(out.prop) > tmax)
        out.prop <- out.prop[seq_len(tmax)]
    if (any(out.prop < 0) || any(out.prop > 1))
        stop("out.prop must be a values between 0 and 1")

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
    ## captive, adjust by transport mortality
    Nc[,1] <- f.surv.trans * N0c # year 0 pop in captive
    ## recipient (w) and wild (w0 - no influx)
    Nw[,1] <- pop.start * eigen.analysis(Aw)$stable.stage # stable age dist
    ## wild (without receiving inds)
    Nw0 <- pop.projection(Aw, Nw[,1], tmax+1)$stage.vectors
    dimnames(Nw0) <- dimnames(Nw)
    for (i in seq_len(tmax)) {
        ## adjusting transported juv survival
        Aw2 <- Aw
        ## proportion of wild (not transported) juvs
        pjw <- (Nw[out.age+1L,i] - j.surv.trans * Nout[out.age+1L,i]) /
            Nw[out.age+1L,i]
        ## this adjustment shows up in Nw[out.age+2,i+1]
        ## but Nout[out.age+1,i] is not removed
        ## thus wild != recipient
        for (j in seq_len(out.age)) {
            a <- sort(out.age)[j]+1L
            ## wild juv survival
            sjw <- Aw[a+1L, a]
            ## recently transported juv survival reduction
            sjt <- j.surv.red * sjw
            Aw2[a+1L, a] <- pjw[j] * sjw + (1-pjw[j]) * sjt
        }

        ## projecting to next year: reproduction from last year
        Nc[,i+1L] <- Ac %*% Nc[,i]
        Nw[,i+1L] <- Aw2 %*% Nw[,i]

        ## add new females
        room <- if (i == tmax)
            0 else in.inds[i+1L]
        ## add inds one-by-one starting with younger ages
        while(room > 0) {
            for (j in sort(in.age)) {
                Nin[j+1,i+1L] <- Nin[j+1,i+1L] + 1
                room <- room - 1
                if (room <= 0)
                    break
            }
        }
        ## adjust captive pop, adjust by transport mortality
        Nc[,i+1L] <- Nc[,i+1L] + f.surv.trans * Nin[,i+1L]

        ## remove youngs
        tomove <- floor(out.prop[i] * sum(Nc[out.age+1L,i+1L]))
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
        #Nout[,i+1L] <- pmin(Nout[,i+1L], floor(Nc[,i+1L]))
        ## adjust captive pop
        Nc[,i+1L] <- Nc[,i+1L] - Nout[,i+1L]
        ## adjust recipient pop, adjust by transport mortality
        Nw[,i+1L] <- Nw[,i+1L] + j.surv.trans * Nout[,i+1L]

    }
    out <- list(
        call=match.call(),
        settings=settings,
        age.cens=age.cens,
        in.age=in.age,
        in.inds=in.inds,
        out.age=out.age,
        out.prop=out.prop,
        f.surv.trans=f.surv.trans,
        j.surv.trans=j.surv.trans,
        j.surv.red=j.surv.red,
        tmax=tmax,
        pop.start=pop.start,
        age.1st.litter=age.1st.litter,
        breed.early=breed.early,
        f.preg.capt.2=f.preg.capt.2,
        age.calf.max=age.calf.max,
        Nin=Nin, Nout=Nout, Ncapt=Nc, Nrecip=Nw, Nwild=Nw0)
    out$population <- data.frame(Years=c(0, seq_len(tmax)),
        sapply(out[c("Nin", "Nout", "Ncapt", "Nrecip", "Nwild")], colSums))
    class(out) <- "wildlift_breeding"
    out
}
