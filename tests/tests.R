library(CaribouBC)

## settings with different treatments, herds
caribou_settings("mat.pen")
caribou_settings("pred.excl")
caribou_settings("moose.red")
caribou_settings("wolf.red", "Quintette") # wolf reduction

## fpen.prop
caribou_forecast(caribou_settings("mat.pen"))
## fpen.inds: single value
caribou_forecast(caribou_settings("mat.pen"), fpen.inds = 5)
## fpen.inds: vector of values
caribou_forecast(caribou_settings("mat.pen"), fpen.inds = c(5, 4, 6))

## compare scenarios with props
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

x3 <- caribou_forecast(caribou_settings("mat.pen", c.surv.wild=0.64), fpen.prop = 0.5)

## breakeven with inds
s <- caribou_settings("mat.pen")
x4 <- caribou_forecast(s, fpen.inds = 50)
b4i <- caribou_breakeven(x4, type="inds")
b4p <- caribou_breakeven(x4, type="prop")
x5i <- caribou_forecast(s, fpen.inds = b4i)
x5p <- caribou_forecast(s, fpen.prop = b4p)
summary(x5i$Nend.pen)
summary(x5p$Nend.pen)
rbind(i=x5i$Npop[1,], p=x5p$Npop[1,]) # diff should be small

