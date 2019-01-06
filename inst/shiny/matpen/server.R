server <- function(input, output, session) {

#  output$rSelection <- renderUI({
#    if (!is.null(input$r_selected))
#      sourcelist$rsel <- as.integer(input$r_selected)
#    if (verbose)
#      print(paste('# sources:', input$r, '--- selected:', sourcelist$rsel))
#    selectInput("r_selected", "Factor to add sample to",
#                choices=seq_len(as.integer(input$r)),
#                selected = sourcelist$rsel)
#  })

    ## default settings to serve as prototype
    values <- reactiveValues(s = caribou_settings())

    ## this resets all settings when pen type changes
    observeEvent(input$penningType, {
        values$s <- caribou_settings(input$penningType)
    })

    ## dynamic rendering of demography sliders
    output$penningDemControls <- renderUI({
        tagList(
            sliderInput("penningDemCsw", "Calf survival, wild",
                min = 0, max = 1, value = values$s$c.surv.wild, step = 0.01),
            sliderInput("penningDemCsc", "Calf survival, captive",
                min = 0, max = 1, value = values$s$c.surv.capt, step = 0.01),
            sliderInput("penningDemFsw", "Maternal survival, wild",
                min = 0, max = 1, value = values$s$f.surv.wild, step = 0.01),
            sliderInput("penningDemFsc", "Maternal survival, captive",
                min = 0, max = 1, value = values$s$f.surv.capt, step = 0.01),
            sliderInput("penningDemFpw", "Pregnancy rate, wild",
                min = 0, max = 1, value = values$s$f.preg.wild, step = 0.01),
            sliderInput("penningDemFpc", "Pregnancy rate, captive",
                min = 0, max = 1, value = values$s$f.preg.capt, step = 0.01)
        )
    })
    ## observe every demography slider change
    observeEvent(input$penningDemCsw, {
        values$s$c.surv.wild <- input$penningDemCsw
    })
    observeEvent(input$penningDemCsc, {
        values$s$c.surv.capt <- input$penningDemCsc
    })
    observeEvent(input$penningDemFsw, {
        values$s$f.surv.wild <- input$penningDemFsw
    })
    observeEvent(input$penningDemFsc, {
        values$s$f.surv.capt <- input$penningDemFsc
    })
    observeEvent(input$penningDemFpw, {
        values$s$f.preg.wild <- input$penningDemFpw
    })
    observeEvent(input$penningDemFpc, {
        values$s$f.preg.capt <- input$penningDemFpc
    })

    ## dynamic redering of cost sliders
    output$penningCostControls <- renderUI({
        tagList(
            sliderInput("penningCostPencap", "Max in a single pen",
                min = 1, max = 50, value = values$s$pen.cap, step = 1),
            sliderInput("penningCostSetup", "Initial set up",
                min = 0, max = 500, value = values$s$pen.cost.setup, step = 10),
            sliderInput("penningCostProj", "Project manager",
                min = 0, max = 500, value = values$s$pen.cost.proj, step = 10),
            sliderInput("penningCostMaint", "Maintenance",
                min = 0, max = 500, value = values$s$pen.cost.maint, step = 10),
            sliderInput("penningCostCapt", "Capture/monitor",
                min = 0, max = 500, value = values$s$pen.cost.capt, step = 10),
            sliderInput("penningCostPred", "Removing predators",
                min = 0, max = 500, value = values$s$pen.cost.pred, step = 10)
        )
    })
    ## observe every cost slider change
    observeEvent(input$penningCostPencap, {
        values$s$pen.cap <- input$penningCostPencap
    })
    observeEvent(input$penningCostSetup, {
        values$s$pen.cost.setup <- input$penningCostSetup
    })
    observeEvent(input$penningCostProj, {
        values$s$pen.cost.proj <- input$penningCostProj
    })
    observeEvent(input$penningCostMaint, {
        values$s$pen.cost.maint <- input$penningCostMaint
    })
    observeEvent(input$penningCostCapt, {
        values$s$pen.cost.capt <- input$penningCostCapt
    })
    observeEvent(input$penningCostPred, {
        values$s$pen.cost.pred <- input$penningCostPred
    })


    ## apply settings and get forecast
    getF <- reactive({
        caribou_forecast(values$s,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = input$fpen.perc / 100)
    })
    ## make summary table
    getB <- reactive({
        req(getF())
        caribou_forecast(getF()$settings,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = caribou_breakeven(getF()))
    })

    output$penningPlot <- renderPlotly({
        req(getF())
        #plot(getF())
        df <- plot(getF(), plot=FALSE)
        colnames(df)[colnames(df) == "Npen"] <- "Individuals"
        p <- plot_ly(df, x = ~Years, y = ~Individuals,
            name = 'Pen', type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Nnopen, name = 'No pen',
                mode = 'lines') %>%
            layout(legend = list(x = 0.05, y = 0))
        p
    })
    output$penningTable <- renderTable({
        req(getB())
        tab <- cbind(
            Results=unlist(summary(getF())),
            Breakeven=unlist(summary(getB())))
        df <- data.frame(tab[c(
            "fpen.prop", "npens", "lam.pen", "lam.nopen",
            "Nend.nopen", "Nend.pen", "Nend.diff",
            "Cost.total", "Cost.percap"),])
        rownames(df)[1L] <- "Percent.penned"
        df[1L,] <- df[1L,]*100
        df
    }, rownames=TRUE, colnames=TRUE)
}
