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
library(ggplot2)
library(reactable)

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
    breeding = wildlift_settings("cons.breed", pen.cap=40),
    multi1 = c(
        fpen.prop = 0.35,
        fpen.inds = 10,
        f.surv.wild.mr = 0.879,
        c.surv.wild.wr = 0.513,
        f.surv.wild.wr = 0.912,
        wildlift_settings("mat.pen"))
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

wildlift_multilever <- function(Settings,
TMAX, POP_START, VAL, USE_PROP,
area=10000, lin=0, seism=0, young=0,
cost=12, yr_deact=5, yr_restor=15) {

    Forecast <- lapply(Settings, function(s) {
        wildlift_forecast(s,
            tmax = TMAX,
            pop.start = POP_START,
            fpen.prop = if (USE_PROP) VAL else NULL,
            fpen.inds = if (USE_PROP) NULL else VAL)
    })
    ## linear deactivation (ld) and restoration (lr)
    Forecast$mp_ld <- WildLift:::.wildlift_forecast(
        Settings$mp_ld,
        tmax = TMAX,
        pop.start = POP_START,
        fpen.prop = if (USE_PROP) VAL else NULL,
        fpen.inds = if (USE_PROP) NULL else VAL,
        linear=list(area=area, lin=lin, seism=seism, young=young,
            cost=cost, yr_deact=yr_deact, yr_restor=yr_restor),
        deact=TRUE)
    Forecast$mp_lr <- WildLift:::.wildlift_forecast(
        Settings$mp_lr,
        tmax = TMAX,
        pop.start = POP_START,
        fpen.prop = if (USE_PROP) VAL else NULL,
        fpen.inds = if (USE_PROP) NULL else VAL,
        linear=list(area=area, lin=lin, seism=seism, young=young,
            cost=cost, yr_deact=yr_deact, yr_restor=yr_restor),
        deact=FALSE)
    Forecast$pe_ld <- WildLift:::.wildlift_forecast(
        Settings$pe_ld,
        tmax = TMAX,
        pop.start = POP_START,
        fpen.prop = if (USE_PROP) VAL else NULL,
        fpen.inds = if (USE_PROP) NULL else VAL,
        linear=list(area=area, lin=lin, seism=seism, young=young,
            cost=cost, yr_deact=yr_deact, yr_restor=yr_restor),
        deact=TRUE)
    Forecast$pe_lr <- WildLift:::.wildlift_forecast(
        Settings$pe_lr,
        tmax = TMAX,
        pop.start = POP_START,
        fpen.prop = if (USE_PROP) VAL else NULL,
        fpen.inds = if (USE_PROP) NULL else VAL,
        linear=list(area=area, lin=lin, seism=seism, young=young,
            cost=cost, yr_deact=yr_deact, yr_restor=yr_restor),
        deact=FALSE)
    class(Forecast$mp_ld) <- "wildlift_forecast"
    class(Forecast$mp_lr) <- "wildlift_forecast"
    class(Forecast$pe_ld) <- "wildlift_forecast"
    class(Forecast$pe_lr) <- "wildlift_forecast"
    cost_ld <- Forecast$mp_ld$linear$costdeact # mp/pe are the same
    cost_lr <- Forecast$mp_ld$linear$costrestor
    Forecast$mp_ld$linear <- NULL
    Forecast$mp_lr$linear <- NULL
    Forecast$pe_ld$linear <- NULL
    Forecast$pe_lr$linear <- NULL

    Summary <- sapply(Forecast, get_summary, USE_PROP)
    Traces <- lapply(Forecast, plot, plot=FALSE)

    NAM <- list(
        c("None", "MatPen", "PredExcl"),
        c("None", "MooseRed", "WolfRed", "LinDeact", "LinRest"),
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

    ## add LD cost!
    OUT["None", "LinDeact", c("lam", "Nend")] <-
        Summary[c("lam.nopen", "Nend.nopen"), "mp_ld"]
    OUT["MatPen", "LinDeact", c("lam", "Nend", "CostEnd")] <-
        Summary[c("lam.pen", "Nend.pen", "Cost.total"), "mp_ld"]
    OUT["PredExcl", "LinDeact", c("lam", "Nend", "CostEnd")] <-
        Summary[c("lam.pen", "Nend.pen", "Cost.total"), "pe_ld"]
    OUT["None","LinDeact","CostEnd"] <- cost_ld
    OUT["MatPen","LinDeact","CostEnd"] <-
        OUT["MatPen","LinDeact","CostEnd"] + cost_ld
    OUT["PredExcl","LinDeact","CostEnd"] <-
        OUT["PredExcl","LinDeact","CostEnd"] + cost_ld

    ## add LR cost!
    OUT["None", "LinRest", c("lam", "Nend")] <-
        Summary[c("lam.nopen", "Nend.nopen"), "mp_lr"]
    OUT["MatPen", "LinRest", c("lam", "Nend", "CostEnd")] <-
        Summary[c("lam.pen", "Nend.pen", "Cost.total"), "mp_lr"]
    OUT["PredExcl", "LinRest", c("lam", "Nend", "CostEnd")] <-
        Summary[c("lam.pen", "Nend.pen", "Cost.total"), "pe_lr"]
    OUT["None","LinRest","CostEnd"] <- cost_lr
    OUT["MatPen","LinRest","CostEnd"] <-
        OUT["MatPen","LinRest","CostEnd"] + cost_lr
    OUT["PredExcl","LinRest","CostEnd"] <-
        OUT["PredExcl","LinRest","CostEnd"] + cost_lr

    OUT[,,"Nnew"] <- pmax(0, OUT[,,"Nend"] - OUT["None", "None", "Nend"])
    OUT[,,"CostNew"] <- OUT[,,"CostEnd"] / OUT[,,"Nnew"]
    OUT[,,"CostNew"][is.na(OUT[,,"CostNew"])] <- 0

    TB <- data.frame(
        Demogr = factor(rep(c("None", "MP", "PE"), dim(OUT)[2]),
                        c("None", "MP", "PE")),
        Manage = factor(rep(c("None", "MR", "WR", "LD", "LR"), each=dim(OUT)[1]),
                        c("None", "MR", "WR", "LD", "LR")))
    TB$lambda <- as.numeric(OUT[,,"lam"])
    TB$Nend <- as.numeric(OUT[,,"Nend"])
    TB$Nnew <- as.numeric(OUT[,,"Nnew"])
    TB$Cend <- as.numeric(OUT[,,"CostEnd"])
    TB$Cnew <- as.numeric(OUT[,,"CostNew"])
    TB$Demogr <- as.character(TB$Demogr)
    TB$Manage <- as.character(TB$Manage)
    rownames(TB) <- paste0(
        ifelse(TB$Demogr == "None", "", TB$Demogr),
        ifelse(TB$Demogr != "None" & TB$Manage != "None", "+", ""),
        ifelse(TB$Manage == "None", "", TB$Manage))
    rownames(TB)[1] <- "Status quo"

    list(summary=TB, traces=Traces)
}

plot_multilever <- function(ML, type=c("all", "dem", "man", "fac")) {

    type <- match.arg(type)
    Traces <- ML$traces
    TMAX <- nrow(ML$traces[[1]])-1L
    POP_START <- ML$traces[[1]][1,2]

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
        theme_minimal() +
        geom_hline(yintercept=POP_START, col="grey")

    if (type == "fac")
        p <- p + facet_grid(rows=vars(Demogr), cols=vars(Manage))
    if (type == "man")
        p <- p + facet_grid(cols=vars(Manage))
    if (type == "dem")
        p <- p + facet_grid(cols=vars(Demogr))

    p
}


if (FALSE) {
        HERD <- NULL
        Settings <- list(
            mp    = wildlift_settings("mat.pen", herd=HERD
            ),
            mp_mr = wildlift_settings("mat.pen", herd=HERD
            ),
            mp_wr = wildlift_settings("mat.pen", herd=HERD
            ),
            mp_ld    = wildlift_settings("mat.pen", herd=HERD
            ),
            mp_lr    = wildlift_settings("mat.pen", herd=HERD
            ),
            pe    = wildlift_settings("pred.excl", herd=HERD
            ),
            pe_mr = wildlift_settings("pred.excl", herd=HERD
            ),
            pe_wr = wildlift_settings("pred.excl", herd=HERD
            ),
            pe_ld    = wildlift_settings("pred.excl", herd=HERD
            ),
            pe_lr    = wildlift_settings("pred.excl", herd=HERD
            )
        )

l <- list(area= 13119,
            lin=26154,
            seism=21235,
            young=25.7,
            cost=12,
            yr_deact=5,
            yr_restor=15)
wildlift_linear(area= 13119,
            lin=26154,
            seism=21235,
            young=25.7,
            cost=12,
            yr_deact=5,
            yr_restor=15)$pop
w=.wildlift_forecast(wildlift_settings(),
    tmax=20, pop.start=100, 0.35,
    linear=list(), deact=TRUE)
ww=.wildlift_forecast(wildlift_settings(),
    tmax=20, pop.start=100, 0.35,
    linear=l, deact=TRUE)
class(w) <- "wildlift_forecast"
class(ww) <- "wildlift_forecast"
summary(w)
summary(ww)
}
