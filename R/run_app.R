run_app <-
function(app="WildLift") {
    app <- match.arg(app)
    cat("\nLauching Shiny app:", app, "\n")
    shiny::runApp(
        system.file(paste0("shiny/", app), package="WildLift"),
        display.mode = "normal")
}
