## parsing *.R files in Shiny apps

library(WildLift)

path <- system.file("shiny", package="WildLift")
#path <- "inst/shiny"
apps <- list.files(path)

for (app in apps) {
    cat("Parsing Shiny app:", app, "\n")
    files <- list.files(file.path(path, app), pattern="\\.R$")
    for (file in files) {
        cat(" * file:", file)
        tmp <- parse(file.path(path, app, file))
        cat(" -- OK\n")
    }
}
