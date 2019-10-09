#caribou_breeding <-
#function(settings, tmax=20, pop.start=100, fpen.prop, fpen.inds) {}

if (FALSE) {

library(CaribouBC)

## create projection matrix
make_age_mat <- function(settings, age.cens=3,
wild=TRUE, age.1st.litter=3, age.calf.max=1) {
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
    A
}

caribou_breeding <- function(settings, age.cens=3,
in.age=3, # ages of females added in each year, matching in.inds
in.inds=10, # number of females added each year
in.max=35, # capacity of captive breeding facility, excess goes into recipient pop
out.age=1, # age of inds pumped out
out.prop=1, # 0=excess only, 1=as many as there, 0-1=proportionally between
tmax=20,
pop.start=100, # wild / recipient population
age.1st.litter=3, age.calf.max=1) {
    Aw <- make_age_mat(settings,
        age.cens=age.cens, wild=TRUE,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    Ac <- make_age_mat(settings,
        age.cens=age.cens, wild=FALSE,
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
        tmax=tmax,
        pop.start=pop.start,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max,
        Nin=Nin, Nout=Nout, Nc=Nc, Nw=Nw, Nw0=Nw0)
    class(out) <- "caribou_breeding"
    out
}

x <- caribou_breeding(caribou_settings(), # this has captive/wild vitals
    age.cens=18, # proj matrix censored at this age
    tmax=20, # projection horizon
    in.age=c(3, 4), # 5x 3yr old + 5x 4-yr old female --> captive
    in.inds=c(5, 5),
    in.max=60,      # breeding capacity
    out.age=c(1, 2),# we 1 and 2-yr old calves --> recipient
    out.prop = 0) # 0=move only N[t]-in.max youngs
                  # 1=move all youngs and replace with females

N <- data.frame(year=c(0, seq_len(x$tmax)),
    sapply(x[c("Nin", "Nout", "Nc", "Nw", "Nw0")], colSums))
N

plot(N$year, N$Nc, ylim=c(0, max(N[,-1])), type="l",
    ylab="Individuals", xlab="Years", main="out.prop=0")
lines(N$year, N$Nw, col=2)
lines(N$year, N$Nw0, col=4)
lines(N$year, N$Nc-N$Nout, col=1, lty=2)
lines(N$year, N$Nin, col=1, lty=3)
legend("topright", bty="n", lty=1, col=c(1,2,4),
    legend=c("Captive", "Recipient", "Wild"))

}
