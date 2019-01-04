.onAttach <- function(libname, pkgname){
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                    fields=c("Version", "Date"))
    packageStartupMessage(paste(pkgname, ver[1], "\t", ver[2]))
    #if (is.null(getOption("CaribouBC"))) {
    #    options("CaribouBC" = list()) # options can come here
    #}
    invisible(NULL)
}

.onUnload <- function(libpath){
    #options("CaribouBC" = NULL)
    invisible(NULL)
}
