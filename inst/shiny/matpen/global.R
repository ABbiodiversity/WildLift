#shiny::runApp("inst/shiny/matpen")

## install/update CaribouBC package as needed
## need to install from github for rsconnect to work properly
#devtools::install_github("psolymos/CaribouBC")

library(shiny)
library(shinydashboard)
library(plotly)
library(CaribouBC)

## TODO:
## OK - add static settings sliders
## OK - implement plot
## OK - implement summary
## - render dynamic settings sliders depending on observing pen type
## - need some kind of help page / info popups?
## - add download options? what to download?
