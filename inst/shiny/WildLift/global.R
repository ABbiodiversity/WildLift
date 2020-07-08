#shiny::runApp("inst/shiny/WildLift")

## install/update WildLift package as needed
## need to install from github for rsconnect to work properly
#remotes::install_github("ABbiodiversity/WildLift")

library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
library(openxlsx)
library(WildLift)
library(knitr)

ver <- read.dcf(file=system.file("DESCRIPTION", package="WildLift"),
                fields="Version")

## initialize sliders for the different pen types
inits <- list(
    penning = c(
        fpen.prop = 0.35,
        fpen.inds = 10,
        wildlift_settings("mat.pen")),
    predator = c(
        fpen.prop = 0.35,
        fpen.inds = 10,
        wildlift_settings("pred.excl")),
    moose = c(
        fpen.prop = 0.35,
        fpen.inds = 10,
        wildlift_settings("moose.red")),
    moose0 = c(
        fpen.prop = 0.35,
        fpen.inds = 10,
        wildlift_settings("mat.pen")),
    wolf = wildlift_settings("wolf.red"),
    ## set AFS=0.801 CS=0.295 under no wolf option
    wolf0 = wildlift_settings("mat.pen",
        f.surv.capt=0.801,
        f.surv.wild=0.801,
        c.surv.capt=0.295,
        c.surv.wild=0.295),
    breeding = wildlift_settings("cons.breed", pen.cap=40)
)

get_settings <- function(x, use_perc=TRUE) {
    out <- c(tmax = x$tmax,
        pop.start = x$pop.start,
        fpen=if (use_perc)
            paste0(100*x$fpen.prop, "%") else paste0(x$fpen.inds, collapse=", "),
        unlist(x$settings))
    attr(out, "fpen.prop") <- x$fpen.prop
    attr(out, "fpen.inds") <- x$fpen.inds
    out
}

#get_inds <- function(x) eval(parse(text=paste("c(", x, ")")))

get_summary <- function(x, use_perc=TRUE) {
    xx <- summary(x)
    xx$fpen <- if (use_perc)
        x$fpen.prop else x$fpen.inds
    xx$fpen.prop <- NULL
    xx$fpen.inds <- NULL
    unlist(xx)
}

Herds <- c(
    "Columbia North" = "ColumbiaNorth",
    "Columbia South" = "ColumbiaSouth",
    "Frisby-Queest" = "FrisbyQueest",
    "Wells Grey South" = "WellsGreySouth",
    "Groundhog" = "Groundhog",
    "Parsnip" = "Parsnip")
HerdsWolf <- c(
    "Kennedy Siding" = "KennedySiding",
    "Klinse-za (Moberly)" = "KlinsezaMoberly",
    "Quintette" = "Quintette")

#FooterText <- "<p>Shiny app made by the <a href='https://github.com/bcgov/CaribouBC'>CaribouBC</a> R package.</p>"
FooterText <- ""

hover <- function(x, d=1) {
    tot <- round(rowSums(x), d)
    x <- round(x, d)
    sapply(seq_along(tot), function(i) {
        paste0(
            tot[i], "=[",
            paste0(x[i,], collapse=","),
            "]"
        )
    })
}

stack_breeding <- function(x) {
    tt <- 0:x$tmax
    rr <- rownames(x$Nin)
    N <- rbind(
        data.frame(What="Nin", Year=tt, t(x$Nin)),
        data.frame(What="Nout", Year=tt, t(x$Nout)),
        data.frame(What="Ncapt", Year=tt, t(x$Ncapt)),
        data.frame(What="Nrecip", Year=tt, t(x$Nrecip)),
        data.frame(What="Nwild", Year=tt, t(x$Nwild)))
    colnames(N) <- c("Part", "Year", rr)
    N
}

wildlift_seismic <- function(tmax=20, pop.start=100,
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
    list(costdeact=diff(range(out[,"lddeact"]))*area*cost/1000,
         costrestor=diff(range(out[,"ldrestor"]))*area*cost/1000,
         pop=out)
}


if (FALSE) {

## status quo settings
HERD <- NULL
USE_PROP <- TRUE
TMAX <- 20
POP_START <- 100
VAL <- 0.3


Settings <- list(
    mp    = wildlift_settings("mat.pen", HERD),
    mp_mr = wildlift_settings("mat.pen", HERD,
        f.surv.wild = 0.879),
    mp_wr = wildlift_settings("mat.pen", HERD,
        f.surv.wild = 0.912, c.surv.wild = 0.513),
    pe    = wildlift_settings("pred.excl", HERD),
    pe_mr = wildlift_settings("pred.excl", HERD,
        f.surv.wild = 0.879),
    pe_wr = wildlift_settings("pred.excl", HERD,
        f.surv.wild = 0.912, c.surv.wild = 0.513))

Forecast <- lapply(Settings, function(s) {
    wildlift_forecast(s,
            tmax = TMAX,
            pop.start = POP_START,
            fpen.prop = if (USE_PROP) VAL else NULL,
            fpen.inds = if (USE_PROP) NULL else VAL)

})

Summary <- sapply(Forecast, get_summary, USE_PROP)
Traces <- lapply(Forecast, plot, plot=FALSE)

NAM <- list(
    c("None", "MatPen", "PredExcl"),
    c("None", "MooseRed", "WolfRed"),
    c("lam", "Nend", "CostEnd", "Nnew", "CostNew"))
OUT <- array(0, sapply(NAM, length), NAM)

OUT["None", "None", c("lam", "Nend")] <-
    Summary[c("lam.nopen", "Nend.nopen"), "mp"]
OUT["MatPen", "None", c("lam", "Nend", "CostEnd")] <-
    Summary[c("lam.pen", "Nend.pen", "Cost.total"), "mp"]
OUT["PredExcl", "None", c("lam", "Nend", "CostEnd")] <-
    Summary[c("lam.pen", "Nend.pen", "Cost.total"), "pe"]

## no extra cost
OUT["None", "MooseRed", c("lam", "Nend")] <-
    Summary[c("lam.nopen", "Nend.nopen"), "mp_mr"]
OUT["MatPen", "MooseRed", c("lam", "Nend", "CostEnd")] <-
    Summary[c("lam.pen", "Nend.pen", "Cost.total"), "mp_mr"]
OUT["PredExcl", "MooseRed", c("lam", "Nend", "CostEnd")] <-
    Summary[c("lam.pen", "Nend.pen", "Cost.total"), "pe_mr"]

## add extra cost
# Cost <- input$wolf_nremove * input$tmax * input$wolf_cost1 / 1000
OUT["None", "WolfRed", c("lam", "Nend")] <-
    Summary[c("lam.nopen", "Nend.nopen"), "mp_wr"]
OUT["MatPen", "WolfRed", c("lam", "Nend", "CostEnd")] <-
    Summary[c("lam.pen", "Nend.pen", "Cost.total"), "mp_wr"]
OUT["PredExcl", "WolfRed", c("lam", "Nend", "CostEnd")] <-
    Summary[c("lam.pen", "Nend.pen", "Cost.total"), "pe_wr"]

OUT[,,"Nnew"] <- pmax(0, OUT[,,"Nend"] - OUT["None", "None", "Nend"])
OUT[,,"CostNew"] <- OUT[,,"CostEnd"] / OUT[,,"Nnew"]

PL <- rbind(
    data.frame(Demogr="None", Manage="None", Years=0:TMAX,
        N=Traces$mp$Nnopen, stringsAsFactors = FALSE),
    data.frame(Demogr="MP", Manage="None", Years=0:TMAX,
        N=Traces$mp$Npen, stringsAsFactors = FALSE),
    data.frame(Demogr="PE", Manage="None", Years=0:TMAX,
        N=Traces$pe$Npen, stringsAsFactors = FALSE),
    data.frame(Demogr="None", Manage="MR", Years=0:TMAX,
        N=Traces$mp_mr$Nnopen, stringsAsFactors = FALSE),
    PL_MP_MR <- data.frame(Demogr="MP", Manage="MR", Years=0:TMAX,
        N=Traces$mp_mr$Npen, stringsAsFactors = FALSE),
    PL_PE_MR <- data.frame(Demogr="PE", Manage="MR", Years=0:TMAX,
        N=Traces$pe_mr$Npen, stringsAsFactors = FALSE),
    PL_SQ_WR <- data.frame(Demogr="None", Manage="WR", Years=0:TMAX,
        N=Traces$mp_wr$Nnopen, stringsAsFactors = FALSE),
    data.frame(Demogr="MP", Manage="WR", Years=0:TMAX,
        N=Traces$mp_wr$Npen, stringsAsFactors = FALSE),
    data.frame(Demogr="PE", Manage="WR", Years=0:TMAX,
        N=Traces$pe_wr$Npen, stringsAsFactors = FALSE))
PL$N <- floor(PL$N)
PL$Two <- paste0(PL$Demogr, "+", PL$Manage)
PL$Manage <- factor(PL$Manage, c("None", "MR", "WR"))
PL$Demogr <- factor(PL$Demogr, c("None", "MP", "PE"))
PL$lty <- as.integer(PL$Manage)

p <- ggplot(PL, aes(x=Years, y=N)) +
    geom_line(aes(color=Demogr, linetype=Manage)) +
    geom_hline(yintercept=POP_START, col="grey") +
    facet_grid(rows=vars(Demogr), cols=vars(Manage)) +
#    facet_grid(cols=vars(Demogr)) +
#    facet_grid(cols=vars(Manage)) +
    theme_minimal() +
    NULL

p
ggplotly(p)


}
