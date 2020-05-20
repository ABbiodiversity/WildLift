#remotes::install_github("ABbiodiversity/WildLift")

library(WildLift)

## settings with different treatments, herds
wildlift_settings("mat.pen")
wildlift_settings("pred.excl")
wildlift_settings("moose.red")
wildlift_settings("wolf.red", "Quintette") # wolf reduction

## fpen.prop
wildlift_forecast(wildlift_settings("mat.pen"))
## fpen.inds: single value
wildlift_forecast(wildlift_settings("mat.pen"), fpen.inds = 5)
## fpen.inds: vector of values
wildlift_forecast(wildlift_settings("mat.pen"), fpen.inds = c(5, 4, 6))

## compare scenarios with props
x1 <- wildlift_forecast(wildlift_settings("mat.pen"), fpen.prop = 0.75)
x2 <- wildlift_forecast(wildlift_settings("pred.excl"), fpen.prop = 0.75)
b1 <- wildlift_breakeven(x1)
b2 <- wildlift_breakeven(x2)

x1
x2
b1
b2

plot(x2)
lines(x1,col=2)

x3 <- wildlift_forecast(wildlift_settings("mat.pen", c.surv.wild=0.64), fpen.prop = 0.5)

## breakeven with inds
s <- wildlift_settings("mat.pen")
x4 <- wildlift_forecast(s, fpen.inds = 50)
b4i <- wildlift_breakeven(x4, type="inds")
b4p <- wildlift_breakeven(x4, type="prop")
x5i <- wildlift_forecast(s, fpen.inds = b4i)
x5p <- wildlift_forecast(s, fpen.prop = b4p)
summary(x5i$Nend.pen)
summary(x5p$Nend.pen)
rbind(i=x5i$Npop[1,], p=x5p$Npop[1,]) # diff should be small

## imitate Shiny


input <- list(
    tmax = 20,
    pop.start = 100,
    fpen.perc = 25) # note! this is percent, NOT proportion

f <- wildlift_forecast(wildlift_settings(),
    tmax = input$tmax,
    pop.start = input$pop.start,
    fpen.prop = input$fpen.perc / 100)
getF <- function() f
b <- wildlift_forecast(getF()$settings,
    tmax = input$tmax,
    pop.start = input$pop.start,
    fpen.prop = wildlift_breakeven(getF()))
getB <- function() b

tab <- cbind(
    Results=unlist(summary(getF())),
    Breakeven=unlist(summary(getB())))
df <- data.frame(tab[c(
    "fpen.prop", "npens", "lam.pen", "lam.nopen",
    "Nend.nopen", "Nend.pen", "Nend.diff",
    "Cost.total", "Cost.percap"),])
rownames(df)[1L] <- "Percent.penned"
df[1L,] <- df[1L,]*100
#DT::datatable(df)


#library(plotly)
#df <- plot(getF(), plot=FALSE)
#p <- plot_ly(df, x = ~Years, y = ~Npen, name = 'Penned', type = 'scatter', mode = 'lines') %>%
#  add_trace(y = ~Nnopen, name = 'Baseline (no pen)', mode = 'lines')
#p
