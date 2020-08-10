wildlift_linear <- function(tmax=20, pop.start=100,
area=10000, lin=0, seism=0, young=0,
cost=12, yr_deact=5, yr_restor=15) {
    ld <- lin/area
    if (seism > lin) # seismic <= linear
        seism <- lin
    perm <- lin - seism # permanent linear features
    ldperm <- perm/area
    lamfun <- function(ld, young)
        1.0184-0.0234*ld-0.0021*young
    cn <- c("year", "N0", "N1", "Ndeact", "Nrestor",
        "lddeact", "ldrestor", "young", "lam0", "lam1", "lamdeact", "lamrestor")
    out <- matrix(0, tmax+1L, length(cn))
    colnames(out) <- cn
    out[1,c("N0", "N1", "Ndeact", "Nrestor")] <- pop.start
    out[1,"lddeact"] <- ld
    out[1,"ldrestor"] <- ld
    out[,"young"] <- young
    out[,"lam0"] <- lamfun(0, young)
    out[,"lam1"] <- lamfun(ld, young)
    out[1,c("lamdeact", "lamrestor")] <- lamfun(ld, young)
    for (i in seq_len(tmax)+1L) {
        out[i, "year"] <- i
        out[i, "lddeact"] <- max(ldperm, out[i-1L, "lddeact"]-ld/yr_deact)
        out[i, "ldrestor"] <- max(ldperm, out[i-1L, "ldrestor"]-ld/yr_restor)
        out[i, "N0"] <- max(0, floor(out[i-1L, "N0"] * out[i,"lam0"]))
        out[i, "N1"] <- max(0, floor(out[i-1L, "N1"] * out[i,"lam1"]))
        out[i, "lamdeact"] <- lamfun(out[i, "lddeact"], young)
        out[i, "Ndeact"] <- max(0, floor(out[i-1L, "Ndeact"] *out[i, "lamdeact"]))
        out[i, "lamrestor"] <- lamfun(out[i, "ldrestor"], young)
        out[i, "Nrestor"] <- max(0, floor(out[i-1L, "Nrestor"] * out[i, "lamrestor"]))
    }
    out <- list(
        costdeact=diff(range(out[,"lddeact"]))*area*cost/1000,
        costrestor=diff(range(out[,"ldrestor"]))*area*cost/1000,
        pop=out)
    class(out) <- "wildlift_linear"
    out$call <- match.call()
    out
}
