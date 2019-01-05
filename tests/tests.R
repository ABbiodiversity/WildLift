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
