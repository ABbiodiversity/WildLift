server <- function(input, output, session) {

    ## set values based inits
    values <- reactiveValues(
        penning = inits$penning,
        penning0 = NULL,
        penning_compare = inits$penning_compare)

    ## dynamically render button
    output$penning_button <- renderUI({
        tagList(
            actionButton("penning_button",
                if (values$penning_compare) "Reset" else "Compare")
        )
    })
    observeEvent(input$penning_button, {
        values$penning_compare <- !values$penning_compare
        if (values$penning_compare) {
            values$penning0 <- values$penning
        } else {
            values$penning0 <- NULL
        }
    })

    ## observe fpen perc slider change
    observeEvent(input$penning_FpenPerc, {
        values$penning$fpen.prop <- input$penning_FpenPerc / 100
    })

    ## observe every demography slider change
    observeEvent(input$penning_DemCsw, {
        values$penning$c.surv.wild <- input$penning_DemCsw
    })
    observeEvent(input$penning_DemCsc, {
        values$penning$c.surv.capt <- input$penning_DemCsc
    })
    observeEvent(input$penning_DemFsw, {
        values$penning$f.surv.wild <- input$penning_DemFsw
    })
    observeEvent(input$penning_DemFsc, {
        values$penning$f.surv.capt <- input$penning_DemFsc
    })
    observeEvent(input$penning_DemFpw, {
        values$penning$f.preg.wild <- input$penning_DemFpw
    })
    observeEvent(input$penning_DemFpc, {
        values$penning$f.preg.capt <- input$penning_DemFpc
    })

    ## observe every cost slider change
    observeEvent(input$penning_CostPencap, {
        values$penning$pen.cap <- input$penning_CostPencap
    })
    observeEvent(input$penning_CostSetup, {
        values$penning$pen.cost.setup <- input$penning_CostSetup
    })
    observeEvent(input$penning_CostProj, {
        values$penning$pen.cost.proj <- input$penning_CostProj
    })
    observeEvent(input$penning_CostMaint, {
        values$penning$pen.cost.maint <- input$penning_CostMaint
    })
    observeEvent(input$penning_CostCapt, {
        values$penning$pen.cost.capt <- input$penning_CostCapt
    })
    observeEvent(input$penning_CostPred, {
        values$penning$pen.cost.pred <- input$penning_CostPred
    })

    ## apply settings and get forecast
    penning_getF <- reactive({
        caribou_forecast(values$penning,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = values$penning$fpen.prop)
    })
    ## make summary table
    penning_getB <- reactive({
        req(penning_getF())
        p <- suppressWarnings(caribou_breakeven(penning_getF()))
        if (is.na(p))
            return(NULL)
        caribou_forecast(penning_getF()$settings,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = p)
    })
    penning_getF0 <- reactive({
        if (!values$penning_compare)
            return(NULL)
        caribou_forecast(values$penning0,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = values$penning0$fpen.prop)
    })
    ## make summary table
    penning_getB0 <- reactive({
        req(penning_getF0())
        p <- suppressWarnings(caribou_breakeven(penning_getF0()))
        if (is.na(p))
            return(NULL)
        caribou_forecast(penning_getF0()$settings,
            tmax = input$tmax,
            pop.start = input$pop.start,
            fpen.prop = p)
    })

    output$penning_Plot <- renderPlotly({
        req(penning_getF())
        df <- plot(penning_getF(), plot=FALSE)
        colnames(df)[colnames(df) == "Npen"] <- "Individuals"
        p <- plot_ly(df, x = ~Years, y = ~Individuals,
            name = 'Pen', type = 'scatter', mode = 'lines',
            color=I('red')) %>%
            add_trace(y = ~Nnopen, name = 'No pen',
                mode = 'lines', color=I('blue'))
        if (values$penning_compare) {
            df0 <- plot(penning_getF0(), plot=FALSE)
            p <- p %>% add_trace(y = ~Npen, name = 'Pen, reference', data = df0,
                    line=list(dash = 'dash', color='red')) %>%
                add_trace(y = ~Nnopen, name = 'No pen, reference', data = df0,
                    line=list(dash = 'dash', color='blue'))
        }
        p <- p %>% layout(legend = list(x = 0.05, y = 0))
        p
    })
    output$penning_Table <- renderTable({
        req(penning_getF())
        bev <- if (is.null(penning_getB()))
            NA else unlist(summary(penning_getB()))
        tab <- cbind(
            Results=unlist(summary(penning_getF())),
            Breakeven=bev)
        subs <- c("fpen.prop", "npens", "lam.pen", "lam.nopen",
            "Nend.pen", "Nend.nopen", "Nend.diff",
            "Cost.total", "Cost.percap")
        df <- tab[subs,,drop=FALSE]
        df[1L,] <- df[1L,]*100
        rownames(df) <- c("% penned",
            "# pens", "&lambda; (pen)", "&lambda; (no pen)",
            "N (end, pen)", "N (end, no pen)", "N (end, difference)",
            "Total cost (x $1000)", "Cost per capita (x $1000 / caribou)")
        if (values$penning_compare) {
            bev0 <- if (is.null(penning_getB0()))
                NA else unlist(summary(penning_getB0()))
            tab0 <- cbind(
                Results=unlist(summary(penning_getF0())),
                Breakeven=bev0)
            df0 <- tab0[subs,,drop=FALSE]
            df0[1L,] <- df0[1L,]*100
            rownames(df0) <- rownames(df)
            df <- cbind(df0, df)
            colnames(df) <- c("Results, reference", "Breakeven, reference",
                "Results", "Breakeven")
        }
        df
    }, rownames=TRUE, colnames=TRUE,
    striped=TRUE, bordered=TRUE, na="n/a",
    sanitize.text.function = function(x) x)

}
