run_app <-
function(app="CaribouBC") {
    app <- match.arg(app)
    cat("\nLauching Shiny app:", app, "\n")
    shiny::runApp(
        system.file(paste0("shiny/", app), package="CaribouBC"),
        display.mode = "normal")
}
