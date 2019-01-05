## list of required packages
pkgs <- c(
    "shiny",
    "shinydashboard")

## avoid compilation
op <- options("install.packages.compile.from.source"="never")
## list of installed packages
inst <- rownames(installed.packages())
## list of missing packages -> install these from binary
needed_pkgs <- setdiff(pkgs, inst)
if (length(needed_pkgs) > 0L)
    install.packages(needed_pkgs, repos="https://cloud.r-project.org/")

## load all required packages
for (i in pkgs)
    library(i, character.only=TRUE)
## restore options
options(op)

## install/update CaribouBC package as needed
#devtools::install_github("psolymos/CaribouBC")
