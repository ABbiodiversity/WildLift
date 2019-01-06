library(CaribouBC)

caribou_settings("mat.pen")
caribou_settings("pred.excl")
caribou_forecast(caribou_settings("mat.pen"))
x1 <- caribou_forecast(caribou_settings("mat.pen"), fpen.prop = 0.75)
x2 <- caribou_forecast(caribou_settings("pred.excl"), fpen.prop = 0.75)
b1 <- caribou_breakeven(x1)
b2 <- caribou_breakeven(x2)

x1
x2
b1
b2

plot(x2)
lines(x1,col=2)

## imitate Shiny


input <- list(
    tmax = 20,
    pop.start = 100,
    fpen.perc = 25, # note! this is percent, NOT proportion
    penningDemCsw = 0.16,
    penningDemCsc = 0.54,
    penningDemFsw = 0.85,
    penningDemFsc = 0.90,
    penningDemFpw = 0.92,
    penningDemFpc = 0.92,
    penningCostPencap = 35,
    penningCostSetup = 500,
    penningCostProj = 80,
    penningCostMaint = 250,
    penningCostCapt = 250,
    penningCostPred = 0)

s <- caribou_settings(input$penningType,
    ## cost
    pen.cap = input$penningCostPencap,
    pen.cost.setup = input$penningCostSetup,
    pen.cost.proj = input$penningCostProj,
    pen.cost.maint = input$penningCostMaint,
    pen.cost.capt = input$penningCostCapt,
    pen.cost.pred = input$penningCostPred,
    ## demography
    c.surv.wild = input$penningDemCsw,
    c.surv.capt = input$penningDemCsc,
    f.surv.wild = input$penningDemFsw,
    f.surv.capt = input$penningDemFsc,
    f.preg.wild = input$penningDemFpw,
    f.preg.capt = input$penningDemFpc)
f <- caribou_forecast(s,
    tmax = input$tmax,
    pop.start = input$pop.start,
    fpen.prop = input$fpen.perc / 100)
getF <- function() f
b <- caribou_breakeven(getF())
getB <- function() b
fb <- caribou_forecast(f$settings,
    tmax = input$tmax,
    pop.start = input$pop.start,
    fpen.prop = getB())
getFB <- function() fb

tab <- cbind(
    Results=unlist(summary(getF())),
    Breakeven=unlist(summary(getFB())))
data.frame(tab[c(
    "npens", "lam.pen", "lam.nopen",
    "Nend.nopen", "Nend.pen", "Nend.diff",
    "Cost.total", "Cost.percap"),])
