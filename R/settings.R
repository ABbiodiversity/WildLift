## preset demography parameters
.get_demography <- function(
pen.type=c("mat.pen", "pred.excl", "moose.red", "wolf.red", "cons.breed"),
herd=NULL) {

    df <- structure(list(RecoveryAction = c("MP", "MP", "MP", "MP", "MP",
        "MP", "MP", "MP", "MP", "MP", "MP", "PE", "PE", "PE", "PE", "PE",
        "PE", "PE", "PE", "CB", "CB", "CB", "CB", "CB", "CB", "CB", "CB",
        "MR", "MR", "MR", "MR", "MR", "MR", "MR", "MR", "WR", "WR", "WR",
        "WR"), PenType = c("mat.pen", "mat.pen", "mat.pen", "mat.pen",
        "mat.pen", "mat.pen", "mat.pen", "mat.pen", "mat.pen", "mat.pen",
        "mat.pen", "pred.excl", "pred.excl", "pred.excl", "pred.excl",
        "pred.excl", "pred.excl", "pred.excl", "pred.excl", "cons.breed",
        "cons.breed", "cons.breed", "cons.breed", "cons.breed", "cons.breed",
        "cons.breed", "cons.breed", "moose.red", "moose.red", "moose.red",
        "moose.red", "moose.red", "moose.red", "moose.red", "moose.red",
        "wolf.red", "wolf.red", "wolf.red", "wolf.red"), PopID = c("EastSideAthabasca",
        "ColumbiaNorth", "ColumbiaSouth", "FrisbyQueest", "WellsGreySouth",
        "Groundhog", "Parsnip", "KennedySiding", "KlinsezaMoberly", "Quintette",
        "AverageSubpop", "EastSideAthabasca", "ColumbiaNorth", "ColumbiaSouth",
        "FrisbyQueest", "WellsGreySouth", "Groundhog", "Parsnip", "AverageSubpop",
        "EastSideAthabasca", "ColumbiaNorth", "ColumbiaSouth", "FrisbyQueest",
        "WellsGreySouth", "Groundhog", "Parsnip", "AverageSubpop", "EastSideAthabasca",
        "ColumbiaNorth", "ColumbiaSouth", "FrisbyQueest", "WellsGreySouth",
        "Groundhog", "Parsnip", "AverageSubpop", "KennedySiding", "KlinsezaMoberly",
        "Quintette", "AverageSubpop"), Subpopulation = c("East Side Athabasca",
        "Columbia North", "Columbia South", "Frisby-Queest", "Wells Grey South",
        "Groundhog", "Parsnip", "Kennedy Siding", "Klinse-za (Moberly)",
        "Quintette", "Average subpopulation", "East Side Athabasca",
        "Columbia North", "Columbia South", "Frisby-Queest", "Wells Grey South",
        "Groundhog", "Parsnip", "Average subpopulation", "East Side Athabasca",
        "Columbia North", "Columbia South", "Frisby-Queest", "Wells Grey South",
        "Groundhog", "Parsnip", "Average subpopulation", "East Side Athabasca",
        "Columbia North", "Columbia South", "Frisby-Queest", "Wells Grey South",
        "Groundhog", "Parsnip", "Average subpopulation", "Kennedy Siding",
        "Klinse-za (Moberly)", "Quintette", "Average subpopulation"),
            Scw = c(0.163, 0.217, 0.285, 0.363, 0.239, 0.234, 0.163,
            0.283, 0.308, 0.294, 0.259, 0.163, 0.217, 0.285, 0.363, 0.239,
            0.234, 0.163, 0.259, 0.163, 0.217, 0.285, 0.363, 0.239, 0.234,
            0.163, 0.259, 0.163, 0.217, 0.285, 0.363, 0.239, 0.234, 0.163,
            0.259, 0.283, 0.308, 0.294, 0.259), Scm = c(0.598, 0.598,
            0.598, 0.598, 0.598, 0.598, 0.598, 0.598, 0.598, 0.598, 0.598,
            0.72, 0.72, 0.72, 0.72, 0.72, 0.72, 0.72, 0.72, 0.72, 0.72,
            0.72, 0.72, 0.72, 0.72, 0.72, 0.72, 0.163, 0.233, 0.266,
            0.227, 0.313, 0.434, 0.163, 0.257, 0.554, 0.506, 0.489, 0.513
            ), Saw = c(0.853, 0.784, 0.767, 0.853, 0.868, 0.853, 0.875,
            0.844, 0.748, 0.81, 0.823, 0.853, 0.784, 0.767, 0.853, 0.868,
            0.853, 0.875, 0.823, 0.853, 0.784, 0.767, 0.853, 0.868, 0.853,
            0.875, 0.823, 0.853, 0.784, 0.767, 0.853, 0.868, 0.853, 0.875,
            0.823, 0.844, 0.748, 0.81, 0.823), Sam = c(0.903, 0.834,
            0.817, 0.903, 0.918, 0.903, 0.925, 0.894, 0.798, 0.86, 0.876,
            0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.95,
            0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.879, 0.879, 0.879,
            0.879, 0.879, 0.879, 0.879, 0.879, 0.962, 0.86, 0.917, 0.912
            ), Fw = c(0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92), Fm = c(0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92, 0.92,
            0.92)), row.names = c("EastSideAthabasca_MP", "ColumbiaNorth_MP",
        "ColumbiaSouth_MP", "FrisbyQueest_MP", "WellsGreySouth_MP", "Groundhog_MP",
        "Parsnip_MP", "KennedySiding_MP", "KlinsezaMoberly_MP", "Quintette_MP",
        "AverageSubpop_MP", "EastSideAthabasca_PE", "ColumbiaNorth_PE",
        "ColumbiaSouth_PE", "FrisbyQueest_PE", "WellsGreySouth_PE", "Groundhog_PE",
        "Parsnip_PE", "AverageSubpop_PE", "EastSideAthabasca_CB", "ColumbiaNorth_CB",
        "ColumbiaSouth_CB", "FrisbyQueest_CB", "WellsGreySouth_CB", "Groundhog_CB",
        "Parsnip_CB", "AverageSubpop_CB", "EastSideAthabasca_MR", "ColumbiaNorth_MR",
        "ColumbiaSouth_MR", "FrisbyQueest_MR", "WellsGreySouth_MR", "Groundhog_MR",
        "Parsnip_MR", "AverageSubpop_MR", "KennedySiding_WR", "KlinsezaMoberly_WR",
        "Quintette_WR", "AverageSubpop_WR"), class = "data.frame")
    pen.type <- match.arg(pen.type)
    if (pen.type == "wolf.red") {
        Herd <- if (is.null(herd))
            "KennedySiding" else as.character(herd)
        Herd <- match.arg(Herd, c("KennedySiding", "KlinsezaMoberly", "Quintette"))
    } else {
        Herd <- if (is.null(herd))
            "EastSideAthabasca" else as.character(herd)
        Herd <- match.arg(Herd, unique(df$PopID))
    }
    df1 <- df[df$PenType==pen.type & df$PopID == Herd,]
    if (nrow(df1) < 1)
        stop(sprintf("%s recovery action is not avilable for %s", pen.type, Herd))
    if (pen.type %in% c("mat.pen", "pred.excl", "cons.breed")) {
        list(
            c.surv.wild = df1$Scw,
            c.surv.capt = df1$Scm,
            f.surv.wild = df1$Saw,
            f.surv.capt = df1$Sam,
            f.preg.wild = df1$Fw,
            f.preg.capt =df1$Fm
        )
    } else {
        list(
            c.surv.wild = df1$Scm,
            c.surv.capt = df1$Scw,
            f.surv.wild = df1$Sam,
            f.surv.capt = df1$Saw,
            f.preg.wild = df1$Fm,
            f.preg.capt =df1$Fw
        )
    }
}


.get_demography_old <-
function(
pen.type=c("mat.pen", "pred.excl", "moose.red", "wolf.red", "cons.breed"),
herd=NULL) {
    pen.type <- match.arg(pen.type)
    ## cons breeding uses pred exclosure numbers according to RobS
    if (pen.type == "cons.breed")
        pen.type <- "pred.excl"
    parms <- list()
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
    HerdsW <- c(
        "KennedySiding",
        "KlinsezaMoberly",
        "Quintette")
    # default is based on Boreal/East Side Athabasca
    if (pen.type != "pred.excl") {
        parms$c.surv.wild <- 0.163       # calf survival rate in the wild, annual
        #c.surv1.capt <- 0.9       # calf survival rate when captive, 0-1 month
        #c.surv2.capt <- 0.6       # calf survival rate when captive, 1-12 months
        #parms$c.surv.capt <- c.surv1.capt*c.surv2.capt # calf survival rate when captive, annual
        parms$c.surv.capt <- 0.598
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
    if (pen.type == "wolf.red") {
        if (is.null(herd)) {
            #stop("Must specify a herd for wolf reduction.")
            parms$f.surv.wild <- 0.912 # 0.801 when no wolf reduction
            parms$c.surv.wild <- 0.513 # 0.295 when no wolf reduction
            parms$f.surv.capt <- parms$f.surv.wild
            parms$c.surv.capt <- parms$c.surv.wild
        } else {
            herd <- match.arg(herd, HerdsW)
            # these are herds from wolf reduction study: WITH wolf reduction
            # no penning (captive=wild) option is considered
            if (herd == "KennedySiding") {
                parms$f.surv.wild <- 0.962
                parms$c.surv.wild <- 0.554
                parms$f.surv.capt <- parms$f.surv.wild
                parms$c.surv.capt <- parms$c.surv.wild
            }
            if (herd == "KlinsezaMoberly") {
                parms$f.surv.wild <- 0.860
                parms$c.surv.wild <- 0.506
                parms$f.surv.capt <- parms$f.surv.wild
                parms$c.surv.capt <- parms$c.surv.wild
            }
            if (herd == "Quintette") {
                parms$f.surv.wild <- 0.917
                parms$c.surv.wild <- 0.489
                parms$f.surv.capt <- parms$f.surv.wild
                parms$c.surv.capt <- parms$c.surv.wild
            }
        }
    } else {
        if (!is.null(herd)) {
            herd <- match.arg(herd, c(Herds, HerdsW))
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
            # no penning (captive=wild) option is considered
            if (herd == "KennedySiding") {
                if (pen.type != "mat.pen")
                    stop(sprintf("%s herd is not available for pen.type=%s.",
                                 herd, pen.type))
                parms$f.surv.wild <- 0.844
                parms$c.surv.wild <- 0.283
                parms$f.surv.capt <- parms$f.surv.wild
                parms$c.surv.capt <- parms$c.surv.wild
            }
            if (herd == "KlinsezaMoberly") {
                if (pen.type != "mat.pen")
                    stop(sprintf("%s herd is not available for pen.type=%s.",
                                 herd, pen.type))
                parms$f.surv.wild <- 0.748
                parms$c.surv.wild <- 0.308
                parms$f.surv.capt <- parms$f.surv.wild
                parms$c.surv.capt <- parms$c.surv.wild
            }
            if (herd == "Quintette") {
                if (pen.type != "mat.pen")
                    stop(sprintf("%s herd is not available for pen.type=%s.",
                                 herd, pen.type))
                parms$f.surv.wild <- 0.810
                parms$c.surv.wild <- 0.294
                parms$f.surv.capt <- parms$f.surv.wild
                parms$c.surv.capt <- parms$c.surv.wild
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
function(pen.type=c("mat.pen", "pred.excl", "moose.red", "wolf.red", "cons.breed")) {
    pen.type <- match.arg(pen.type)
    parms <- list()
    if (pen.type == "mat.pen") {
        parms$pen.cap <- 35       # how many adult females can live in mat pen?
        parms$pen.cost.setup <- 500 # cost in thousands to set up pen
        parms$pen.cost.proj <- 80 # costs of project manager
        parms$pen.cost.maint <- 300 # cost in thousands for patrolling and repairing fence + shepherd + contingencies
        parms$pen.cost.capt <- 250 # cost in thousands to capture cows, monitor, survey, calf collar
        parms$pen.cost.pred <- 0 # cost in thousands for removing predators annually
    }
    if (pen.type == "pred.excl") {
        parms$pen.cap <- 35        # how many adult females can live in big pen?
        parms$pen.cost.setup <- (77*24) + 20 # cost in thousands to set up pen
        parms$pen.cost.proj <- 80 # costs of project manager
        parms$pen.cost.maint <- 600 # cost in thousands for patrolling and repairing fence + contingencies
#        parms$pen.cost.capt <- 200-80 # cost in thousands to capture cows, monitor, survey, calf collar - minus PM costs
        parms$pen.cost.capt <- 200 # cost in thousands to capture cows, monitor, survey, calf collar
        parms$pen.cost.pred <- 80 # cost in thousands for removing predators annually
    }

    if (pen.type == "cons.breed") {
        parms$pen.cap <- 35        # how many adult females can live in big pen?
        parms$pen.cost.setup <- 9000 # cost in thousands to set up pen
        parms$pen.cost.proj <- 80 # costs of project manager
        parms$pen.cost.maint <- 280 # cost in thousands for patrolling and repairing fence + contingencies
        parms$pen.cost.capt <- 50 # cost in thousands to capture cows, monitor, survey, calf collar
        parms$pen.cost.pred <- 0 # cost in thousands for removing predators annually
    }
    ## wolf reduction: 5.1/wolf

    if (!(pen.type %in% c("mat.pen", "pred.excl", "cons.breed"))) {
        parms$pen.cap <- 0
        parms$pen.cost.setup <- 0
        parms$pen.cost.proj <- 0
        parms$pen.cost.maint <- 0
        parms$pen.cost.capt <- 0
        parms$pen.cost.pred <- 0
    }
    parms
}

wildlift_settings <-
function(
pen.type=c("mat.pen", "pred.excl", "moose.red", "wolf.red", "cons.breed"),
herd=NULL,
...) {
    if (inherits(pen.type, "wildlift_settings")) {
        parms <- pen.type
    } else {
        parms <- c(.get_demography(pen.type, herd), .get_cost(pen.type))
        attr(parms, "pen.type") <- pen.type
        if (!is.null(herd))
            attr(parms, "herd") <- herd
        class(parms) <- "wildlift_settings"
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
    parms$call <- match.call()
    parms
}
