#caribou_breeding <-
#function(settings, tmax=20, pop.start=100, fpen.prop, fpen.inds) {}

if (FALSE) {

## create projection matrix
make_age_mat <- function(settings, age.trunc=3,
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
    A <- matrix(0, age.trunc+1, age.trunc+1)
    for (i in seq_len(age.trunc)) {
        A[i+1L, i] <- if (i > age.calf.max)
            surv.f else surv.c
    }
    for (i in seq_len(age.trunc)+1L) {
        if (i > age.1st.litter)
            A[1L, i] <- 0.5*preg.f*surv.f
    }
    A[age.trunc+1L, age.trunc+1L] <- if (age.trunc+1L > age.calf.max)
        surv.f else surv.c
    A
}

caribou_breeding <- function(settings, age.trunc=3,
f.age=3, f.inds=10, # captive breeding, repeats in every year until we hit pop.max
tmax=20,
pop.start=100, # wild / recipient population
pop.max=20, # captive pop: >60 excess goes into recipient pop
age.1st.litter=3, age.calf.max=1) {
    Aw <- make_age_mat(settings,
        age.trunc=age.trunc, wild=TRUE,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    Ac <- make_age_mat(settings,
        age.trunc=age.trunc, wild=FALSE,
        age.1st.litter=age.1st.litter,
        age.calf.max=age.calf.max)
    f.age <- as.integer(f.age)
    if (f.age <= 0)
        stop("f.age must be positive integer")
    f.inds <- as.integer(f.inds)
    if (f.inds <= 0)
        stop("f.inds must be positive integer")
    N0c <- numeric(age.trunc+1)
    N0c[f.age+1] <- f.inds
    Nc <- matrix(0, age.trunc+1, tmax+1) # captive
    Nw <- Nc # recipient
    Nw0 <- Nw # wild (without receiving inds)
    Nout <- Nc # excess inds
    Nc[,1] <- N0c
    Nw[,1] <- pop.start * eigen.analysis(Aw)$stable.stage
    for (i in seq_len(tmax)) {
        Nc[,i+1L] <- Ac %*% Nc[,i]
        Nw[,i+1L] <- Aw %*% Nw[,i]
        Nw0[,i+1L] <- Nw[,i+1L]
        if (sum(Nc[,i+1L]) > pop.max) {
            Nout[f.age+1,i+1L] <- min(
                floor(Nw[f.age+1,i+1L]),
                floor(sum(Nc[,i+1L]) - pop.max))
            Nc[f.age+1L,i+1L] <- Nc[f.age+1,i+1L] - Nout[f.age+1,i+1L]
            Nw[f.age+1L,i+1L] <- Nw[f.age+1,i+1L] + Nout[f.age+1,i+1L]
        }
    }
    list(Nout=Nout, Nc=Nc, Nw=Nw, Nw0=Nw0)
}

settings <- caribou_settings()

N <- pop.projection(A, data.matrix(N0), 20)

N1 <- A %*% data.matrix(N0)
N2 <- A %*% data.matrix(N1)


Stable.st <- eigen.analysis(A)$stable.stage




}

