#shiny::runApp("inst/shiny/matpen")

## install/update CaribouBC package as needed
## need to install from github for rsconnect to work properly
#devtools::install_github("psolymos/CaribouBC")

library(shiny)
library(shinydashboard)
library(plotly)
library(openxlsx)
library(CaribouBC)

## initialize sliders for the different pen types
inits <- list(
    penning = c(
        fpen.prop = 0.25,
        caribou_settings("mat.pen")),
    penning_compare = FALSE,
    predator = c(
        fpen.prop = 0.25,
        caribou_settings("pred.excl")),
    predator_compare = FALSE,
    moose = c(
        fpen.prop = 0.25,
        caribou_settings("moose.red")),
    moose_compare = FALSE)

get_settings <- function(x) {
    c(tmax = x$tmax,
        pop.start = x$pop.start,
        fpen.prop = x$fpen.prop,
        unlist(x$settings))
}

## TODO:
## OK - add static settings sliders
## OK - implement plot
## OK - implement summary
## OK - add download options? what to download?
## - remove pred cost slider where not relevant
## - need some kind of help page / info popups?
