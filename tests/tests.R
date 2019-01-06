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
    fpen.perc = 25) # note! this is percent, NOT proportion

f <- caribou_forecast(caribou_settings(),
    tmax = input$tmax,
    pop.start = input$pop.start,
    fpen.prop = input$fpen.perc / 100)
getF <- function() f
b <- caribou_forecast(getF()$settings,
    tmax = input$tmax,
    pop.start = input$pop.start,
    fpen.prop = caribou_breakeven(getF()))
getB <- function() b

tab <- cbind(
    Results=unlist(summary(getF())),
    Breakeven=unlist(summary(getB())))
df <- data.frame(tab[c(
    "npens", "lam.pen", "lam.nopen",
    "Nend.nopen", "Nend.pen", "Nend.diff",
    "Cost.total", "Cost.percap"),])
#DT::datatable(df)


#library(plotly)
#df <- plot(getF(), plot=FALSE)
#p <- plot_ly(df, x = ~Years, y = ~Npen, name = 'Penned', type = 'scatter', mode = 'lines') %>%
#  add_trace(y = ~Nnopen, name = 'Baseline (no pen)', mode = 'lines')
#p

