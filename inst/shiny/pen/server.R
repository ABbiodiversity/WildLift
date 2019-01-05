server <- function(input, output, session) {
    ## make colors depend on value displayed
    output$lambda <- renderValueBox({
        valueBox(
            input$tmax,
            "Growth rate",
            color = "aqua",
            icon = icon("divide")
        )
    })
    output$breakeven <- renderValueBox({
        valueBox(
            paste0(input$fpen.perc, "%"),
            "Breakeven percentage",
            color = "aqua",
            icon = icon("equals")
        )
    })
    output$popsize <- renderValueBox({
        valueBox(
            200,
            paste("Population size after", input$tmax, "years"),
            color = "aqua",
            icon = icon("chart-line")
        )
    })
    output$penningPlot <- renderPlot({plot(0)})
}
