server <- function(input, output, session) {
    if (FALSE) {
    ## make colors depend on value displayed
    output$lambda <- renderValueBox({
        req(getF())
        valueBox(
            round(getF()$Npop$lam.pen[1L], 2),
            "Growth rate",
            color = "aqua",
            icon = icon("divide")
        )
    })
    output$breakeven <- renderValueBox({
        req(getB())
        valueBox(
            paste0(round(getB() * 100), "%"),
            "Breakeven percentage",
            color = "aqua",
            icon = icon("equals")
        )
    })
    output$popsize <- renderValueBox({
        req(getF())
        valueBox(
            floor(rev(getF()$Npop$N.pen)[1L]),
            paste("Population size after", input$tmax, "years"),
            color = "aqua",
            icon = icon("chart-line")
        )
    })
    }
    ## apply settings and get forecast
    getF <- reactive({
        s <- caribou_settings(input$penningType,
            ## cost
            pen.cap = input$penningCostPencap,
            pen.cost.setup = input$penningCostSetup,
            pen.cost.proj = input$penningCostProj,
            pen.cost.maint = input$penningCostMaint,
            pen.cost.capt = input$penningCostCapt,
            pen.cost.pred = input$penningCostPred,
            ## demography
            c.surv.wild = input$penningDemCsw,
            c.surv.capt = input$penningDemCsc,
            f.surv.wild = input$penningDemFsw,
            f.surv.capt = input$penningDemFsc,
            f.preg.wild = input$penningDemFpw,
            f.preg.capt = input$penningDemFpc)
        f <- caribou_forecast(s,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = input$fpen.perc / 100)
        return(f)
    })
    ## calculate breakeven percentage
    getB <- reactive({
        req(getF())
        caribou_breakeven(getF())
    })
    ## make summary table
    getFB <- reactive({
        req(getB())
        caribou_forecast(getF()$settings,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = getB())
    })

    output$penningPlot <- renderPlot({
        req(getF())
        plot(getF())
    })
    output$penningTable <- renderTable({
        req(getFB())
        tab <- cbind(
            Results=unlist(summary(getF())),
            Breakeven=unlist(summary(getFB())))
        data.frame(tab[c(
            "npens", "lam.pen", "lam.nopen",
            "Nend.nopen", "Nend.pen", "Nend.diff",
            "Cost.total", "Cost.percap"),])
    }, rownames=TRUE, colnames=TRUE)
}
