server <- function(input, output, session) {

    ## >>> common part for all 3 tabs <<<====================

    ## set values based inits
    values <- reactiveValues(
        use_perc = TRUE,
        penning = inits$penning,
        penning0 = NULL,
        penning_compare = FALSE,
        predator = inits$predator,
        predator0 = NULL,
        predator_compare = FALSE,
        moose = inits$moose,
        moose0 = inits$moose0,
        wolf = inits$wolf,
        wolf0 = inits$wolf0,
        breeding = inits$breeding)
    ## set perc/inds
    observeEvent(input$use_perc, {
        values$use_perc <- input$use_perc == "perc"
    })

    ## >>> penning tab <<<=====================================

    ## dynamically render sliders
    output$penning_demogr_sliders <- renderUI({
        if (input$penning_herd != "Default")
            return(p("Demography settings not available for specific subpopulations."))
        tagList(
            sliderInput("penning_DemCsw", "Calf survival, wild",
                min = 0, max = 1, value = inits$penning$c.surv.wild, step = 0.001),
            sliderInput("penning_DemCsc", "Calf survival, captive",
                min = 0, max = 1, value = inits$penning$c.surv.capt, step = 0.001),
            sliderInput("penning_DemFsw", "Adult female survival, wild",
                min = 0, max = 1, value = inits$penning$f.surv.wild, step = 0.001),
            sliderInput("penning_DemFsc", "Adult female survival, captive",
                min = 0, max = 1, value = inits$penning$f.surv.capt, step = 0.001),
            sliderInput("penning_DemFpw", "Fecundity, wild",
                min = 0, max = 1, value = inits$penning$f.preg.wild, step = 0.001),
            sliderInput("penning_DemFpc", "Fecundity, captive",
                min = 0, max = 1, value = inits$penning$f.preg.capt, step = 0.001)
        )
    })
    ## dynamically render button
    output$penning_button <- renderUI({
        tagList(
            actionButton("penning_button",
                if (values$penning_compare)
                    "Single scenario" else "Compare scenarios",
                icon = icon(if (values$penning_compare)
                    "stop-circle" else "arrows-alt-h"))
        )
    })
    ## dynamically render subpopulation selector
    output$penning_herd <- renderUI({
        tagList(
            selectInput(
                "penning_herd", "Subpopulation",
                c("Default (East Side Athabasca)"="Default", Herds, HerdsWolf)
            )
        )
    })
    ## dynamically render perc or inds slider
    output$penning_perc_or_inds <- renderUI({
        if (values$use_perc) {
            tagList(
                sliderInput("penning_Fpen", "Percent of females penned",
                    min = 0, max = 100, value = round(100*inits$penning$fpen.prop),
                    step = 1),
                bsTooltip("penning_Fpen",
                    "Change the percent of female population in maternity penning. Default set, but the user can toggle.")
            )
        } else {
            tagList(
                sliderInput("penning_Fpen", "Number of females penned",
                    min = 0, max = input$popstart, value = inits$penning$fpen.inds,
                    step = 1),
                bsTooltip("penning_Fpen",
                    "Change the number of females in maternity penning. Default set, but the user can toggle.")
            )
        }
    })
    ## observers
    observeEvent(input$penning_herd, {
        values$penning <- c(
            fpen.prop = values$penning$fpen.prop,
            fpen.inds = values$penning$fpen.inds,
            caribou_settings("mat.pen",
                herd = if (input$penning_herd == "Default")
                    NULL else input$penning_herd))
        if (values$penning_compare) {
            values$penning0 <- values$penning
        } else {
            values$penning0 <- NULL
        }
    })
    observeEvent(input$penning_button, {
        values$penning_compare <- !values$penning_compare
        if (values$penning_compare) {
            values$penning0 <- values$penning
        } else {
            values$penning0 <- NULL
        }
    })
    observeEvent(input$penning_Fpen, {
        if (values$use_perc) {
            values$penning$fpen.prop <- input$penning_Fpen / 100
        } else {
            values$penning$fpen.inds <- input$penning_Fpen
        }
    })
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
    ## apply settings and get forecast
    penning_getF <- reactive({
        caribou_forecast(values$penning,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) values$penning$fpen.prop else NULL,
            fpen.inds = if (values$use_perc) NULL else values$penning$fpen.inds)
    })
    ## try to find breakeven point
    penning_getB <- reactive({
        req(penning_getF())
        p <- suppressWarnings(
            caribou_breakeven(penning_getF(),
                type = if (values$use_perc) "prop" else "inds")
        )
        if (is.na(p))
            return(NULL)
        caribou_forecast(penning_getF()$settings,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) p else NULL,
            fpen.inds = if (values$use_perc) NULL else p)
    })
    ## these are similar functions to the bechmark scenario
    penning_getF0 <- reactive({
        if (!values$penning_compare)
            return(NULL)
        caribou_forecast(values$penning0,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) values$penning0$fpen.prop else NULL,
            fpen.inds = if (values$use_perc) NULL else values$penning0$fpen.inds)
    })
    penning_getB0 <- reactive({
        req(penning_getF0())
        p <- suppressWarnings(
            caribou_breakeven(penning_getF0(),
                type = if (values$use_perc) "prop" else "inds")
        )
        if (is.na(p))
            return(NULL)
        caribou_forecast(penning_getF0()$settings,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) p else NULL,
            fpen.inds = if (values$use_perc) NULL else p)
    })
    ## making nice table of the results
    penning_getT <- reactive({
        req(penning_getF())
        bev <- if (is.null(penning_getB()))
            #NA else unlist(summary(penning_getB()))
            NA else get_summary(penning_getB(), values$use_perc)
        tab <- cbind(
            #Results=unlist(summary(penning_getF())),
            Results=get_summary(penning_getF(), values$use_perc),
            Breakeven=bev)
        subs <- c("fpen", "npens", "lam.pen", "lam.nopen",
            "Nend.pen", "Nend.nopen", "Nend.diff",
            "Cost.total", "Cost.percap")
        df <- tab[subs,,drop=FALSE]
        if (values$use_perc)
            df[1L,] <- df[1L,]*100
        rownames(df) <- c(if (values$use_perc) "% penned" else "# penned",
            "# pens", "&lambda; (maternity penning)", "&lambda; (no maternity penning)",
            "N (end, maternity penning)", "N (end, no maternity penning)", "N (new)",
            "Total cost (x $million)", "Cost per new caribou (x $million)")
        if (values$penning_compare) {
            bev0 <- if (is.null(penning_getB0()))
                #NA else unlist(summary(penning_getB0()))
                NA else get_summary(penning_getB0(), values$use_perc)
            tab0 <- cbind(
                #Results=unlist(summary(penning_getF0())),
                Results=get_summary(penning_getF0(), values$use_perc),
                Breakeven=bev0)
            df0 <- tab0[subs,,drop=FALSE]
            if (values$use_perc)
                df0[1L,] <- df0[1L,]*100
            rownames(df0) <- rownames(df)
            df <- cbind(df0, df)
            colnames(df) <- c("Results, reference", "Breakeven, reference",
                "Results", "Breakeven")
        }
        df
    })
    ## making nice table of the settings
    penning_getS <- reactive({
        req(penning_getF())
        bev <- if (is.null(penning_getB()))
            NA else get_settings(penning_getB(), values$use_perc)
        tab <- cbind(
            Results=get_settings(penning_getF(), values$use_perc),
            Breakeven=bev)
        SNAM <- c(
            "tmax" = "T max",
            "pop.start" = "N start",
            #"fpen.prop" = "% females penned",
            "fpen" = if (values$use_perc)
                "% females penned" else "# females penned",
            "c.surv.wild" = "Calf survival, wild",
            "c.surv.capt" = "Calf survival, captive",
            "f.surv.wild" = "Adult female survival, wild",
            "f.surv.capt" = "Adult female survival, captive",
            "f.preg.wild" = "Fecundity, wild",
            "f.preg.capt" = "Fecundity, captive",
            "pen.cap" = "Max in a single pen",
            "pen.cost.setup" = "Initial set up (x $1000)",
            "pen.cost.proj" = "Project manager (x $1000)",
            "pen.cost.maint" = "Maintenance (x $1000)",
            "pen.cost.capt" = "Capture/monitor (x $1000)",
            "pen.cost.pred" = "Removing predators (x $1000)")
        df <- tab[names(SNAM),,drop=FALSE]
        rownames(df) <- SNAM
        if (values$penning_compare) {
            bev0 <- if (is.null(penning_getB0()))
                NA else get_settings(penning_getB0(), values$use_perc)
            tab0 <- cbind(
                Results=get_settings(penning_getF0(), values$use_perc),
                Breakeven=bev0)
            df0 <- tab0[names(SNAM),,drop=FALSE]
            if (values$use_perc)
                df0["fpen",] <- df0["fpen",]*100
            rownames(df0) <- SNAM
            df <- cbind(df0, df)
            colnames(df) <- c("Results, reference", "Breakeven, reference",
                "Results", "Breakeven")
        }
        df
    })
    ## plot
    output$penning_Plot <- renderPlotly({
        req(penning_getF())
        df <- plot(penning_getF(), plot=FALSE)
        colnames(df)[colnames(df) == "Npen"] <- "Individuals"
        p <- plot_ly(df, x = ~Years, y = ~Individuals,
            name = 'Maternity penning', type = 'scatter', mode = 'lines',
            color=I('red')) %>%
            add_trace(y = ~Nnopen, name = 'No maternity penning',
                mode = 'lines', color=I('blue')) %>%
            config(displayModeBar = 'hover', displaylogo = FALSE)
        if (values$penning_compare) {
            df0 <- plot(penning_getF0(), plot=FALSE)
            p <- p %>% add_trace(y = ~Npen, name = 'Maternity penning, reference', data = df0,
                    line=list(dash = 'dash', color='red')) %>%
                add_trace(y = ~Nnopen, name = 'No maternity penning, reference', data = df0,
                    line=list(dash = 'dash', color='blue'))
        }
        p <- p %>% layout(legend = list(x = 100, y = 0))
        p
    })
    ## table
    output$penning_Table <- renderTable({
        req(penning_getT())
        penning_getT()
    }, rownames=TRUE, colnames=TRUE,
    striped=TRUE, bordered=TRUE, na="n/a",
    sanitize.text.function = function(x) x)
    ## dowload
    penning_xlslist <- reactive({
        req(penning_getF())
        req(penning_getT())
        TS <- plot(penning_getF(), plot=FALSE)
        if (values$penning_compare) {
            TS <- cbind(plot(penning_getF0(), plot=FALSE), TS[,-1])
            colnames(TS) <- c("Years",
                "N no maternity penning, reference", "N maternity penning, reference",
                "N no maternity penning", "N maternity penning")
        }
        df <- penning_getT()
        rownames(df) <- gsub("&lambda;", "lambda", rownames(df))
        ss <- penning_getS()
        out <- list(
            Info=data.frame(CaribouBC=paste0(
                c("R package version: ", "Date of analysis: ", "Caribou subpopulation: "),
                c(ver, format(Sys.time(), "%Y-%m-%d"), input$penning_herd))),
            Settings=as.data.frame(ss),
            TimeSeries=as.data.frame(TS),
            Summary=as.data.frame(df))
        out$Settings$Parameters <- rownames(ss)
        out$Settings <- out$Settings[,c(ncol(ss)+1, 1:ncol(ss))]
        out$Summary$Variables <- rownames(df)
        out$Summary <- out$Summary[,c(ncol(df)+1, 1:ncol(df))]
        out
    })
    output$penning_download <- downloadHandler(
        filename = function() {
            paste0("CaribouBC_maternity_pen_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(penning_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
    )


    ## >>> predator tab <<<=====================================

    ## dynamically render sliders
    output$predator_demogr_sliders <- renderUI({
        if (input$predator_herd != "Default")
            return(p("Demography settings not available for specific subpopulations."))
        tagList(
             sliderInput("predator_DemCsw", "Calf survival, wild",
                min = 0, max = 1, value = inits$predator$c.surv.wild, step = 0.001),
             sliderInput("predator_DemCsc", "Calf survival, captive",
                min = 0, max = 1, value = inits$predator$c.surv.capt, step = 0.001),
             sliderInput("predator_DemFsw", "Adult female survival, wild",
                min = 0, max = 1, value = inits$predator$f.surv.wild, step = 0.001),
             sliderInput("predator_DemFsc", "Adult female survival, captive",
                min = 0, max = 1, value = inits$predator$f.surv.capt, step = 0.001),
             sliderInput("predator_DemFpw", "Fecundity, wild",
                min = 0, max = 1, value = inits$predator$f.preg.wild, step = 0.001),
             sliderInput("predator_DemFpc", "Fecundity, captive",
                min = 0, max = 1, value = inits$predator$f.preg.capt, step = 0.001)
        )
    })

    ## dynamically render button
    output$predator_button <- renderUI({
        tagList(
            actionButton("predator_button",
                if (values$predator_compare)
                    "Single scenario" else "Compare scenarios",
                icon = icon(if (values$predator_compare)
                    "stop-circle" else "arrows-alt-h"))
        )
    })
    ## dynamically render subpopulation selector
    output$predator_herd <- renderUI({
        tagList(
            selectInput(
                "predator_herd", "Subpopulation",
                c("Default (East Side Athabasca)"="Default", Herds)
            )
        )
    })
    ## dynamically render perc or inds slider
    output$predator_perc_or_inds <- renderUI({
        if (values$use_perc) {
            tagList(
                sliderInput("predator_Fpen", "Percent of females penned",
                    min = 0, max = 100, value = round(100*inits$predator$fpen.prop),
                    step = 1),
                bsTooltip("predator_Fpen",
                    "Change the percent of female population in maternity penning. Default set, but the user can toggle.")
            )
        } else {
            tagList(
                sliderInput("predator_Fpen", "Number of females penned",
                    min = 0, max = input$popstart, value = inits$predator$fpen.inds,
                    step = 1),
                bsTooltip("predator_Fpen",
                    "Change the number of females in maternity penning. Default set, but the user can toggle.")
            )
        }
    })
    ## observers
    observeEvent(input$predator_herd, {
        values$predator <- c(
            fpen.prop = values$predator$fpen.prop,
            fpen.inds = values$predator$fpen.inds,
            caribou_settings("pred.excl",
                herd = if (input$predator_herd == "Default") NULL else input$predator_herd))
        if (values$predator_compare) {
            values$predator0 <- values$predator
        } else {
            values$predator0 <- NULL
        }
    })
    observeEvent(input$predator_button, {
        values$predator_compare <- !values$predator_compare
        if (values$predator_compare) {
            values$predator0 <- values$predator
        } else {
            values$predator0 <- NULL
        }
    })
    observeEvent(input$predator_Fpen, {
        if (values$use_perc) {
            values$predator$fpen.prop <- input$predator_Fpen / 100
        } else {
            values$predator$fpen.inds <- input$predator_Fpen
        }
    })
    observeEvent(input$predator_DemCsw, {
        values$predator$c.surv.wild <- input$predator_DemCsw
    })
    observeEvent(input$predator_DemCsc, {
        values$predator$c.surv.capt <- input$predator_DemCsc
    })
    observeEvent(input$predator_DemFsw, {
        values$predator$f.surv.wild <- input$predator_DemFsw
    })
    observeEvent(input$predator_DemFsc, {
        values$predator$f.surv.capt <- input$predator_DemFsc
    })
    observeEvent(input$predator_DemFpw, {
        values$predator$f.preg.wild <- input$predator_DemFpw
    })
    observeEvent(input$predator_DemFpc, {
        values$predator$f.preg.capt <- input$predator_DemFpc
    })
    observeEvent(input$predator_CostPencap, {
        values$predator$pen.cap <- input$predator_CostPencap
    })
    observeEvent(input$predator_CostSetup, {
        values$predator$pen.cost.setup <- input$predator_CostSetup
    })
    observeEvent(input$predator_CostProj, {
        values$predator$pen.cost.proj <- input$predator_CostProj
    })
    observeEvent(input$predator_CostMaint, {
        values$predator$pen.cost.maint <- input$predator_CostMaint
    })
    observeEvent(input$predator_CostCapt, {
        values$predator$pen.cost.capt <- input$predator_CostCapt
    })
    observeEvent(input$predator_CostPred, {
        values$predator$pen.cost.pred <- input$predator_CostPred
    })
    ## apply settings and get forecast
    predator_getF <- reactive({
        caribou_forecast(values$predator,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) values$predator$fpen.prop else NULL,
            fpen.inds = if (values$use_perc) NULL else values$predator$fpen.inds)
    })
    ## try to find breakeven point
    predator_getB <- reactive({
        req(predator_getF())
        p <- suppressWarnings(
            caribou_breakeven(predator_getF(),
                type = if (values$use_perc) "prop" else "inds")
        )
        if (is.na(p))
            return(NULL)
        caribou_forecast(predator_getF()$settings,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) p else NULL,
            fpen.inds = if (values$use_perc) NULL else p)
    })
    ## these are similar functions to the bechmark scenario
    predator_getF0 <- reactive({
        if (!values$predator_compare)
            return(NULL)
        caribou_forecast(values$predator0,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) values$predator0$fpen.prop else NULL,
            fpen.inds = if (values$use_perc) NULL else values$predator0$fpen.inds)
    })
    predator_getB0 <- reactive({
        req(predator_getF0())
        p <- suppressWarnings(
            caribou_breakeven(predator_getF0(),
                type = if (values$use_perc) "prop" else "inds")
        )
        if (is.na(p))
            return(NULL)
        caribou_forecast(predator_getF0()$settings,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) p else NULL,
            fpen.inds = if (values$use_perc) NULL else p)
    })
    ## making nice table of the results
    predator_getT <- reactive({
        req(predator_getF())
        bev <- if (is.null(predator_getB()))
            #NA else unlist(summary(predator_getB()))
            NA else get_summary(predator_getB(), values$use_perc)
        tab <- cbind(
            #Results=unlist(summary(predator_getF())),
            Results=get_summary(predator_getF(), values$use_perc),
            Breakeven=bev)
        subs <- c("fpen", "npens", "lam.pen", "lam.nopen",
            "Nend.pen", "Nend.nopen", "Nend.diff",
            "Cost.total", "Cost.percap")
        df <- tab[subs,,drop=FALSE]
        if (values$use_perc)
            df[1L,] <- df[1L,]*100
        rownames(df) <- c(if (values$use_perc) "% penned" else "# penned",
            "# pens", "&lambda; (predator exclosure)", "&lambda; (no predator exclosure)",
            "N (end, predator exclosure)", "N (end, no predator exclosure)", "N (new)",
            "Total cost (x $million)", "Cost per new caribou (x $million)")
        if (values$predator_compare) {
            bev0 <- if (is.null(predator_getB0()))
                #NA else unlist(summary(predator_getB0()))
                NA else get_summary(predator_getB0(), values$use_perc)
            tab0 <- cbind(
                #Results=unlist(summary(predator_getF0())),
                Results=get_summary(predator_getF0(), values$use_perc),
                Breakeven=bev0)
            df0 <- tab0[subs,,drop=FALSE]
            if (values$use_perc)
                df0[1L,] <- df0[1L,]*100
            rownames(df0) <- rownames(df)
            df <- cbind(df0, df)
            colnames(df) <- c("Results, reference", "Breakeven, reference",
                "Results", "Breakeven")
        }
        df
    })
    ## making nice table of the settings
    predator_getS <- reactive({
        req(predator_getF())
        bev <- if (is.null(predator_getB()))
            NA else get_settings(predator_getB(), values$use_perc)
        tab <- cbind(
            Results=get_settings(predator_getF(), values$use_perc),
            Breakeven=bev)
        SNAM <- c(
            "tmax" = "T max",
            "pop.start" = "N start",
            #"fpen.prop" = "% females penned",
            "fpen" = if (values$use_perc)
                "% females penned" else "# females penned",
            "c.surv.wild" = "Calf survival, wild",
            "c.surv.capt" = "Calf survival, captive",
            "f.surv.wild" = "Adult female survival, wild",
            "f.surv.capt" = "Adult female survival, captive",
            "f.preg.wild" = "Fecundity, wild",
            "f.preg.capt" = "Fecundity, captive",
            "pen.cap" = "Max in a single pen",
            "pen.cost.setup" = "Initial set up (x $1000)",
            "pen.cost.proj" = "Project manager (x $1000)",
            "pen.cost.maint" = "Maintenance (x $1000)",
            "pen.cost.capt" = "Capture/monitor (x $1000)",
            "pen.cost.pred" = "Removing predators (x $1000)")
        df <- tab[names(SNAM),,drop=FALSE]
        rownames(df) <- SNAM
        if (values$predator_compare) {
            bev0 <- if (is.null(predator_getB0()))
                NA else get_settings(predator_getB0(), values$use_perc)
            tab0 <- cbind(
                Results=get_settings(predator_getF0(), values$use_perc),
                Breakeven=bev0)
            df0 <- tab0[names(SNAM),,drop=FALSE]
            if (values$use_perc)
                df0["fpen",] <- df0["fpen",]*100
            rownames(df0) <- SNAM
            df <- cbind(df0, df)
            colnames(df) <- c("Results, reference", "Breakeven, reference",
                "Results", "Breakeven")
        }
        df
    })
    ## plot
    output$predator_Plot <- renderPlotly({
        req(predator_getF())
        df <- plot(predator_getF(), plot=FALSE)
        colnames(df)[colnames(df) == "Npen"] <- "Individuals"
        p <- plot_ly(df, x = ~Years, y = ~Individuals,
            name = 'Predator exclosure', type = 'scatter', mode = 'lines',
            color=I('red')) %>%
            add_trace(y = ~Nnopen, name = 'No predator exclosure',
                mode = 'lines', color=I('blue')) %>%
            config(displayModeBar = 'hover', displaylogo = FALSE)
        if (values$predator_compare) {
            df0 <- plot(predator_getF0(), plot=FALSE)
            p <- p %>% add_trace(y = ~Npen, name = 'Predator exclosure, reference', data = df0,
                    line=list(dash = 'dash', color='red')) %>%
                add_trace(y = ~Nnopen, name = 'No predator exclosure, reference', data = df0,
                    line=list(dash = 'dash', color='blue'))
        }
        p <- p %>% layout(legend = list(x = 100, y = 0))
        p
    })
    ## table
    output$predator_Table <- renderTable({
        req(predator_getT())
        predator_getT()
    }, rownames=TRUE, colnames=TRUE,
    striped=TRUE, bordered=TRUE, na="n/a",
    sanitize.text.function = function(x) x)
    ## dowload
    predator_xlslist <- reactive({
        req(predator_getF())
        req(predator_getT())
        TS <- plot(predator_getF(), plot=FALSE)
        if (values$predator_compare) {
            TS <- cbind(plot(predator_getF0(), plot=FALSE), TS[,-1])
            colnames(TS) <- c("Years",
                "N no predator exclosure, reference", "N predator exclosure, reference",
                "N no predator exclosure", "N predator exclosure")
        }
        df <- predator_getT()
        rownames(df) <- gsub("&lambda;", "lambda", rownames(df))
        ss <- predator_getS()
        out <- list(
            Info=data.frame(CaribouBC=paste0(
                c("R package version: ", "Date of analysis: ", "Caribou subpopulation: "),
                c(ver, format(Sys.time(), "%Y-%m-%d"), input$predator_herd))),
            Settings=as.data.frame(ss),
            TimeSeries=as.data.frame(TS),
            Summary=as.data.frame(df))
        out$Settings$Parameters <- rownames(ss)
        out$Settings <- out$Settings[,c(ncol(ss)+1, 1:ncol(ss))]
        out$Summary$Variables <- rownames(df)
        out$Summary <- out$Summary[,c(ncol(df)+1, 1:ncol(df))]
        out
    })
    output$predator_download <- downloadHandler(
        filename = function() {
            paste0("CaribouBC_predator_exclosure_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(predator_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
    )


    ## >>> moose tab <<<=====================================

    ## dynamically render sliders
    output$moose_demogr_sliders <- renderUI({
        if (input$moose_herd != "Default")
            return(p("Demography settings not available for specific subpopulations."))
        tagList(
            sliderInput("moose_DemCsw", "Calf survival, moose reduction",
                min = 0, max = 1, value = inits$moose$c.surv.wild, step = 0.001),
            sliderInput("moose_DemCsc", "Calf survival, no moose reduction",
                min = 0, max = 1, value = inits$moose0$c.surv.wild, step = 0.001),
            sliderInput("moose_DemFsw", "Adult female survival, moose reduction",
                min = 0, max = 1, value = inits$moose$f.surv.wild, step = 0.001),
            sliderInput("moose_DemFsc", "Adult female survival, no moose reduction",
                min = 0, max = 1, value = inits$moose0$f.surv.wild, step = 0.001),
            sliderInput("moose_DemFpw", "Fecundity, moose reduction",
                min = 0, max = 1, value = inits$moose$f.preg.wild, step = 0.001),
            sliderInput("moose_DemFpc", "Fecundity, no moose reduction",
                min = 0, max = 1, value = inits$moose0$f.preg.wild, step = 0.001)
        )
    })    ## dynamically render subpopulation selector
    output$moose_herd <- renderUI({
        tagList(
            selectInput(
                "moose_herd", "Subpopulation",
                c("Default (East Side Athabasca)"="Default", Herds)
            )
        )
    })
    ## dynamically render perc or inds slider
    output$moose_perc_or_inds <- renderUI({
        if (values$use_perc) {
            tagList(
                sliderInput("moose_Fpen", "Percent of females penned",
                    min = 0, max = 100, value = round(100*inits$moose$fpen.prop),
                    step = 1),
                bsTooltip("moose_Fpen",
                    "Change the percent of female population in maternity penning. Default set, but the user can toggle.")
            )
        } else {
            tagList(
                sliderInput("moose_Fpen", "Number of females penned",
                    min = 0, max = input$popstart, value = inits$moose$fpen.inds,
                    step = 1),
                bsTooltip("moose_Fpen",
                    "Change the number of females in maternity penning. Default set, but the user can toggle.")
            )
        }
    })
    ## observers
    observeEvent(input$moose_herd, {
        values$moose <- c(
            fpen.prop = values$moose$fpen.prop,
            fpen.inds = values$moose$fpen.inds,
            caribou_settings("moose.red",
                herd = if (input$moose_herd == "Default")
                    NULL else input$moose_herd))
        values$moose0 <- c(
            fpen.prop = values$moose0$fpen.prop,
            fpen.inds = values$moose0$fpen.inds,
            caribou_settings("mat.pen",
                herd = if (input$moose_herd == "Default")
                    NULL else input$moose_herd))
    })
    observeEvent(input$moose_DemCsw, {
        values$moose$c.surv.wild <- input$moose_DemCsw
    })
    observeEvent(input$moose_DemCsc, {
        values$moose0$c.surv.wild <- input$moose_DemCsc
    })
    observeEvent(input$moose_DemFsw, {
        values$moose$f.surv.wild <- input$moose_DemFsw
    })
    observeEvent(input$moose_DemFsc, {
        values$moose0$f.surv.wild <- input$moose_DemFsc
    })
    observeEvent(input$moose_DemFpw, {
        values$moose$f.preg.wild <- input$moose_DemFpw
    })
    observeEvent(input$moose_DemFpc, {
        values$moose0$f.preg.wild <- input$moose_DemFpc
    })
    observeEvent(input$moose_Fpen, {
        if (values$use_perc) {
            values$moose$fpen.prop <- input$moose_Fpen / 100
            values$moose0$fpen.prop <- input$moose_Fpen / 100
        } else {
            values$moose$fpen.inds <- input$moose_Fpen
            values$moose0$fpen.inds <- input$moose_Fpen
        }
    })
    ## moose reduction with penning
    moose_getF <- reactive({
        caribou_forecast(values$moose,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) values$moose$fpen.prop else NULL,
            fpen.inds = if (values$use_perc) NULL else values$moose$fpen.inds)
    })
    ## no moose reduction with penning
    moose_getB <- reactive({
        caribou_forecast(values$moose0,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = if (values$use_perc) values$moose0$fpen.prop else NULL,
            fpen.inds = if (values$use_perc) NULL else values$moose0$fpen.inds)
    })
    ## moose reduction without penning
    moose_getF0 <- reactive({
        caribou_forecast(values$moose,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = 0)
    })
    ## no moose reduction without penning
    moose_getB0 <- reactive({
        caribou_forecast(values$moose0,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = 0)
    })
    ## making nice table of the results
    moose_getT <- reactive({
        req(moose_getF(),
            moose_getB(),
            moose_getF0(),
            moose_getB0())
        subs <- c("lam.pen", "Nend.pen")
        df <- cbind(
            NoMooseNoPen=get_summary(moose_getB0(), values$use_perc)[subs],
            NoMoosePen=get_summary(moose_getB(), values$use_perc)[subs],
            MooseNoPen=get_summary(moose_getF0(), values$use_perc)[subs],
            MoosePen=get_summary(moose_getF(), values$use_perc)[subs]
        )
        Nnew <- pmax(0, df[2,]-df[2,1])
        df <- rbind(df,
            Nnew=Nnew,
            Cost=c(NA, NA, NA, NA),
            CostPerNew=c(NA, NA, NA, NA))
        rownames(df) <- c("&lambda;", "N (end)", "N (new)",
                          "Total cost (x $million)",
                          "Cost per new caribou (x $million)")
        colnames(df) <- c(
            "No moose reduction, no pen",
            "No moose reduction, penned",
            "Moose reduction, no pen",
            "Moose reduction, penned")
        df
    })

    ## making nice table of the settings
    moose_getS <- reactive({
        req(moose_getF(),
            moose_getB(),
            moose_getF0(),
            moose_getB0())
        tab <- cbind(
            MooseNoPen=get_settings(moose_getF0(), values$use_perc),
            MoosePen=get_settings(moose_getF(), values$use_perc),
            NoMooseNoPen=get_settings(moose_getB0(), values$use_perc),
            NoMoosePen=get_settings(moose_getB(), values$use_perc)
        )
        SNAM <- c(
            "tmax" = "T max",
            "pop.start" = "N start",
            #"fpen.prop" = "% females penned",
            "fpen" = if (values$use_perc)
                "% females penned" else "# females penned",
            "c.surv.wild" = "Calf survival, wild",
            "c.surv.capt" = "Calf survival, captive",
            "f.surv.wild" = "Adult female survival, wild",
            "f.surv.capt" = "Adult female survival, captive",
            "f.preg.wild" = "Fecundity, wild",
            "f.preg.capt" = "Fecundity, captive",
            "pen.cap" = "Max in a single pen")
        df <- tab[names(SNAM),,drop=FALSE]
        rownames(df) <- SNAM
        colnames(df) <- c(
            "Moose reduction, no pen",
            "Moose reduction, penned",
            "No moose reduction, no pen",
            "No moose reduction, penned")
        df
    })
    ## plot
    output$moose_Plot <- renderPlotly({
        req(moose_getF())
        dF0 <- plot(moose_getF0(), plot=FALSE)
        dF <- plot(moose_getF(), plot=FALSE)
        dB0 <- plot(moose_getB0(), plot=FALSE)
        dB <- plot(moose_getB(), plot=FALSE)
        colnames(dF0)[colnames(dF0) == "Npen"] <- "Individuals"
        p <- plot_ly(dF0, x = ~Years, y = ~Individuals,
            name = 'Moose reduction, no pen', type = 'scatter', mode = 'lines',
            color=I('red')) %>%
            add_trace(y = ~Npen, name = 'Moose reduction, penned', data = dF,
                mode = 'lines', color=I('blue')) %>%
            add_trace(y = ~Npen, name = 'No moose reduction, no pen', data = dB0,
                    line=list(dash = 'dash', color='red')) %>%
            add_trace(y = ~Npen, name = 'No moose reduction, penned', data = dB,
                line=list(dash = 'dash', color='blue')) %>%
            layout(legend = list(x = 100, y = 0)) %>%
            config(displayModeBar = 'hover', displaylogo = FALSE)
        p
    })
    ## table
    output$moose_Table <- renderTable({
        req(moose_getT())
        moose_getT()
    }, rownames=TRUE, colnames=TRUE,
    striped=TRUE, bordered=TRUE, na="n/a",
    sanitize.text.function = function(x) x)
    ## dowload
    moose_xlslist <- reactive({
        req(moose_getF(), moose_getF0(), moose_getB(), moose_getB0())
        req(moose_getT())
        TS <- cbind(
            plot(moose_getF0(), plot=FALSE)[,c("Years", "Npen")],
            plot(moose_getF(), plot=FALSE)[,"Npen"],
            plot(moose_getB0(), plot=FALSE)[,"Npen"],
            plot(moose_getB(), plot=FALSE)[,"Npen"])
        colnames(TS) <- c("Years",
            "N moose reduction, no pen",
            "N moose reduction, penned",
            "N no moose reduction, no pen",
            "N no moose reduction, penned")
        df <- moose_getT()
        rownames(df) <- gsub("&lambda;", "lambda", rownames(df))
        ss <- moose_getS()
        out <- list(
            Info=data.frame(CaribouBC=paste0(
                c("R package version: ", "Date of analysis: ", "Caribou subpopulation: "),
                c(ver, format(Sys.time(), "%Y-%m-%d"), input$moose_herd))),
            Settings=as.data.frame(ss),
            TimeSeries=as.data.frame(TS),
            Summary=as.data.frame(df))
        out$Settings$Parameters <- rownames(ss)
        out$Settings <- out$Settings[,c(ncol(ss)+1, 1:ncol(ss))]
        out$Summary$Variables <- rownames(df)
        out$Summary <- out$Summary[,c(ncol(df)+1, 1:ncol(df))]
        out
    })
    output$moose_download <- downloadHandler(
        filename = function() {
            paste0("CaribouBC_moose_reduction_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(moose_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
    )


    ## >>> wolf tab <<<=====================================

    ## dynamically render sliders
    output$wolf_demogr_sliders <- renderUI({
        if (input$wolf_herd != "Default")
            return(p("Demography settings not available for specific subpopulations."))
        tagList(
            sliderInput("wolf_DemCsw", "Calf survival, wolf reduction",
                min = 0, max = 1, value = inits$wolf$c.surv.wild, step = 0.001),
            sliderInput("wolf_DemCsc", "Calf survival, no wolf reduction",
                min = 0, max = 1, value = inits$wolf0$c.surv.wild, step = 0.001),
            sliderInput("wolf_DemFsw", "Adult female survival, wolf reduction",
                min = 0, max = 1, value = inits$wolf$f.surv.wild, step = 0.001),
            sliderInput("wolf_DemFsc", "Adult female survival, no wolf reduction",
                min = 0, max = 1, value = inits$wolf0$f.surv.wild, step = 0.001),
            sliderInput("wolf_DemFpw", "Fecundity, wolf reduction",
                min = 0, max = 1, value = inits$wolf$f.preg.wild, step = 0.001),
            sliderInput("wolf_DemFpc", "Fecundity, no wolf reduction",
                min = 0, max = 1, value = inits$wolf0$f.preg.wild, step = 0.001)
        )
    })
    ## dynamically render subpopulation selector
    output$wolf_herd <- renderUI({
        tagList(
            selectInput(
                "wolf_herd", "Subpopulation",
                c("Default (Average of all subpopulations)"="Default", HerdsWolf)
            )
        )
    })
    ## observers
    observeEvent(input$wolf_herd, {
        values$wolf <- caribou_settings("wolf.red",
                herd = if (input$wolf_herd == "Default")
                    NULL else input$wolf_herd)
        ## set AFS=0.801 CS=0.295 under no wolf option
        values$wolf0 <- caribou_settings("mat.pen",
                herd = if (input$wolf_herd == "Default")
                    NULL else input$wolf_herd,
                f.surv.capt=0.801,
                f.surv.wild=0.801,
                c.surv.capt=0.295,
                c.surv.wild=0.295)
    })
    observeEvent(input$wolf_DemCsw, {
        values$wolf$c.surv.wild <- input$wolf_DemCsw
    })
    observeEvent(input$wolf_DemCsc, {
        values$wolf0$c.surv.wild <- input$wolf_DemCsc
    })
    observeEvent(input$wolf_DemFsw, {
        values$wolf$f.surv.wild <- input$wolf_DemFsw
    })
    observeEvent(input$wolf_DemFsc, {
        values$wolf0$f.surv.wild <- input$wolf_DemFsc
    })
    observeEvent(input$wolf_DemFpw, {
        values$wolf$f.preg.wild <- input$wolf_DemFpw
    })
    observeEvent(input$wolf_DemFpc, {
        values$wolf0$f.preg.wild <- input$wolf_DemFpc
    })
    ## wolf reduction without penning
    wolf_getF0 <- reactive({
        caribou_forecast(values$wolf,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = 0)
    })
    ## no wolf reduction without penning
    wolf_getB0 <- reactive({
        caribou_forecast(values$wolf0,
            tmax = input$tmax,
            pop.start = input$popstart,
            fpen.prop = 0)
    })
    ## making nice table of the results
    wolf_getT <- reactive({
        req(wolf_getF0(),
            wolf_getB0())
        subs <- c("lam.pen", "Nend.pen")
        Cost <- input$wolf_nremove * input$tmax * input$wolf_cost1 / 1000
        df <- cbind(
            WolfNoPen=get_summary(wolf_getF0(), values$use_perc)[subs],
            NoWolfNoPen=get_summary(wolf_getB0(), values$use_perc)[subs])
        Nnew <- max(0, df[2,1] - df[2,2])
        CostPerNew <- if (Nnew <= 0) NA else Cost/Nnew
        df <- rbind(df,
            Nnew=c(Nnew,NA),
            Cost=c(Cost, NA),
            CostPerNew=c(CostPerNew, NA))
        rownames(df) <- c("&lambda;", "N (end)", "N (new)",
                          "Total cost (x $million)",
                          "Cost per new caribou (x $million)")
        colnames(df) <- c(
            "Wolf reduction",
            "No wolf reduction")
        df
    })
    ## making nice table of the settings
    wolf_getS <- reactive({
        req(wolf_getF0(),
            wolf_getB0())
        tab <- cbind(
            WolfNoPen=get_settings(wolf_getF0(), values$use_perc),
            NoWolfNoPen=get_settings(wolf_getB0(), values$use_perc))
        SNAM <- c(
            "tmax" = "T max",
            "pop.start" = "N start",
            #"fpen.prop" = "% females penned",
            "fpen" = if (values$use_perc)
                "% females penned" else "# females penned",
            "c.surv.wild" = "Calf survival, wild",
            "c.surv.capt" = "Calf survival, captive",
            "f.surv.wild" = "Adult female survival, wild",
            "f.surv.capt" = "Adult female survival, captive",
            "f.preg.wild" = "Fecundity, wild",
            "f.preg.capt" = "Fecundity, captive",
            "pen.cap" = "Max in a single pen")
        print("wolf_getS 3")
        df <- tab[names(SNAM),,drop=FALSE]
        rownames(df) <- SNAM
        colnames(df) <- c(
            "Wolf reduction",
            "No wolf reduction")
        df
    })
    ## plot
    output$wolf_Plot <- renderPlotly({
        req(wolf_getF0(),
            wolf_getB0())
        dF0 <- plot(wolf_getF0(), plot=FALSE)
        dB0 <- plot(wolf_getB0(), plot=FALSE)
        colnames(dF0)[colnames(dF0) == "Npen"] <- "Individuals"
        p <- plot_ly(dF0, x = ~Years, y = ~Individuals,
            name = 'Wolf reduction', type = 'scatter', mode = 'lines',
            color=I('red')) %>%
            add_trace(y = ~Npen, name = 'No wolf reduction', data = dB0,
                    mode = 'lines', color=I('blue')) %>%
            layout(legend = list(x = 100, y = 0)) %>%
            config(displayModeBar = 'hover', displaylogo = FALSE)
        p
    })
    ## table
    output$wolf_Table <- renderTable({
        req(wolf_getT())
        wolf_getT()
    }, rownames=TRUE, colnames=TRUE,
    striped=TRUE, bordered=TRUE, na="n/a",
    sanitize.text.function = function(x) x)
    ## dowload
    wolf_xlslist <- reactive({
        req(wolf_getF0(), wolf_getB0())
        print("req")
        req(wolf_getT())
        print("req getT")
        TS <- cbind(
            plot(wolf_getF0(), plot=FALSE)[,c("Years", "Npen")],
            plot(wolf_getB0(), plot=FALSE)[,"Npen"])
        print("TS")
        colnames(TS) <- c("Years",
            "N wolf reduction",
            "N no wolf reduction")
        df <- wolf_getT()
        print("getT")
        rownames(df) <- gsub("&lambda;", "lambda", rownames(df))
        ss <- wolf_getS()
        print("getS")
        out <- list(
            Info=data.frame(CaribouBC=paste0(
                c("R package version: ", "Date of analysis: ", "Caribou subpopulation: "),
                c(ver, format(Sys.time(), "%Y-%m-%d"), input$wolf_herd))),
            Settings=as.data.frame(ss),
            TimeSeries=as.data.frame(TS),
            Summary=as.data.frame(df))
        out$Settings$Parameters <- rownames(ss)
        out$Settings <- out$Settings[,c(ncol(ss)+1, 1:ncol(ss))]
        out$Summary$Variables <- rownames(df)
        out$Summary <- out$Summary[,c(ncol(df)+1, 1:ncol(df))]
        print("out")
        out
    })
    output$wolf_download <- downloadHandler(
        filename = function() {
            paste0("CaribouBC_wolf_reduction_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(wolf_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
    )


    ## >>> breeding tab <<<=====================================

    ## dynamically render sliders
    output$breeding_years <- renderUI({
        tagList(
            sliderInput("breeding_yrs",
                "Number of years that females are added to the facility",
                min = 0, max = input$tmax, value = 0, step = 1) #value = 1
        )
    })
    output$breeding_jyears <- renderUI({
        tagList(
            sliderInput("breeding_jyrs",
                "Number of years to delay juvenile transfer",
                min = 0, max = input$tmax, value = 0, step = 1)
        )
    })
    output$breeding_demogr_sliders <- renderUI({
        if (input$breeding_herd != "Default")
            return(p("Demography settings not available for specific subpopulations."))
        tagList(
            sliderInput("breeding_DemCsc", "Calf survival in facility",
                min = 0, max = 1,
                value = inits$breeding$c.surv.capt, step = 0.001),
            sliderInput("breeding_DemCsw", "Calf survival, recipient & status quo",
                min = 0, max = 1,
                value = inits$breeding$c.surv.wild, step = 0.001),
            sliderInput("breeding_DemFsc", "Adult female survival in facility",
                min = 0, max = 1,
                value = inits$breeding$f.surv.capt, step = 0.001),
            sliderInput("breeding_DemFsw", "Adult female survival, recipient & status quo",
                min = 0, max = 1,
                value = inits$breeding$f.surv.wild, step = 0.001),
            sliderInput("breeding_DemFpc", "Fecundity in facility",
                min = 0, max = 1,
                value = inits$breeding$f.preg.capt, step = 0.001),
            sliderInput("breeding_DemFpw", "Fecundity, recipient & status quo",
                min = 0, max = 1,
                value = inits$breeding$f.preg.wild, step = 0.001)
        )
    })
    ## dynamically render subpopulation selector
    output$breeding_herd <- renderUI({
        tagList(
            selectInput(
                "breeding_herd", "Subpopulation",
                c("Default (East Side Athabasca)"="Default", Herds)
            )
        )
    })
    ## observers
    observeEvent(input$breeding_herd, {
        values$breeding <- caribou_settings("cons.breed",
                herd = if (input$breeding_herd == "Default")
                    NULL else input$breeding_herd)
    })
    observeEvent(input$breeding_DemCsw, {
        values$breeding$c.surv.wild <- input$breeding_DemCsw
    })
    observeEvent(input$breeding_DemCsc, {
        values$breeding$c.surv.capt <- input$breeding_DemCsc
    })
    observeEvent(input$breeding_DemFsw, {
        values$breeding$f.surv.wild <- input$breeding_DemFsw
    })
    observeEvent(input$breeding_DemFsc, {
        values$breeding$f.surv.capt <- input$breeding_DemFsc
    })
    observeEvent(input$breeding_DemFpw, {
        values$breeding$f.preg.wild <- input$breeding_DemFpw
    })
    observeEvent(input$breeding_DemFpc, {
        values$breeding$f.preg.capt <- input$breeding_DemFpc
    })
    observeEvent(input$breeding_CostSetup, {
        values$breeding$pen.cost.setup <- input$breeding_CostSetup
    })
    observeEvent(input$breeding_CostProj, {
        values$breeding$pen.cost.proj <- input$breeding_CostProj
    })
    observeEvent(input$breeding_CostMaint, {
        values$breeding$pen.cost.maint <- input$breeding_CostMaint
    })
    observeEvent(input$breeding_CostCapt, {
        values$breeding$pen.cost.capt <- input$breeding_CostCapt
    })
    ## breeding reduction without penning
    breeding_getF <- reactive({
        req(input$breeding_yrs, input$breeding_ininds, input$breeding_jyrs)
        nn <- rep(input$breeding_ininds, input$breeding_yrs)
        op <- c(rep(0, input$breeding_jyrs), input$breeding_outprop)
        caribou_breeding(values$breeding,
            tmax = input$tmax,
            pop.start = input$popstart,
            f.surv.trans = input$breeding_ftrans,
            j.surv.trans = input$breeding_jtrans,
            j.surv.red = input$breeding_jsred,
            in.inds = nn,
            out.prop = op)
    })
    ## plot
    output$breeding_Plot <- renderPlotly({
        req(breeding_getF())
        bb <- breeding_getF()
        dF <- summary(bb)
        colnames(dF)[colnames(dF) == "Nrecip"] <- "Individuals"
        p <- plot_ly(dF, x = ~Years, y = ~Individuals,
            name = 'Recipient', type = 'scatter', mode = 'lines',
            text = hover(t(bb$Nrecip)),
            hoverinfo = 'text',
            color=I('red')) %>%
            add_trace(y = ~Nwild, name = 'Status quo', data = dF,
                    mode = 'lines', color=I('blue'),
                    text = hover(t(bb$Nwild))) %>%
            add_trace(y = ~Ncapt, name = 'Inside facility', data = dF,
                    mode = 'lines', color=I('black'),
                    text = hover(t(bb$Ncapt))) %>%
            add_trace(y = ~Nout, name = 'Juvenile females out', data = dF,
                    mode = 'lines', color=I('orange'),
                    text = hover(t(bb$Nout))) %>%
            add_trace(y = ~Nin, name = 'Adult females in', data = dF,
                line=list(color='grey'),
                text = hover(t(bb$Nin))) %>%
            layout(legend = list(x = 100, y = 0)) %>%
            config(displayModeBar = 'hover', displaylogo = FALSE)
        p
    })
    ## making nice table of the settings
    breeding_getS <- reactive({
        req(breeding_getF())
        x <- breeding_getF()
        s <- x$settings
        s$call <- NULL
        tab <- cbind(c(tmax = x$tmax,
            pop.start = x$pop.start,
            out.prop=x$out.prop,
            f.surv.trans=x$f.surv.trans,
            j.surv.trans=x$j.surv.trans,
            j.surv.red=x$j.surv.red,
            unlist(s)))
        SNAM <- c(
            "tmax" = "T max",
            "pop.start" = "N start",
            "c.surv.wild" = "Calf survival, wild",
            "c.surv.capt" = "Calf survival in facility",
            "f.surv.wild" = "Adult female survival, wild",
            "f.surv.capt" = "Adult female survival in facility",
            "f.preg.wild" = "Fecundity, wild",
            "f.preg.capt" = "Fecundity in facility",
            #"out.prop"="Proportion of calves transferred",
            "pen.cost.setup" = "Initial set up (x $1000)",
            "pen.cost.proj" = "Project manager (x $1000)",
            "pen.cost.maint" = "Maintenance (x $1000)",
            "pen.cost.capt" = "Capture/monitor (x $1000)",
            "pen.cost.pred" = "Removing predators (x $1000)",
            "f.surv.trans"="Adult female survival during capture/transport to facility",
            "j.surv.trans"="Juvenile female survival during capture/transport from facility to recipient subpopulation",
            "j.surv.red"="Relative reduction in survival of juvenile females transported to recipient subpopulation for 1 year after transport")
        df <- tab[names(SNAM),,drop=FALSE]
        rownames(df) <- SNAM
        colnames(df) <- "Breeding"
        #print(df)
        df
    })

    ## table
    output$breeding_Table <- renderTable({
        req(breeding_getF())
        zz <- breeding_getF()

        ## one time cost
        cost1 <- zz$settings$pen.cost.setup
        ## yearly costs
        cost2 <- zz$settings$pen.cost.proj +
            zz$settings$pen.cost.maint +
            zz$settings$pen.cost.capt +
            zz$settings$pen.cost.pred
        cost <- (cost1 + zz$tmax * cost2) / 1000
        #print(c(cost1/1000, cost2/1000, cost))

        dF <- summary(zz)[,-(1:3)]
        colnames(dF) <- c("In facility", "Recipient", "Status quo")
        N0 <- dF[1,,drop=FALSE]
        Ntmax1 <- dF[nrow(dF)-1L,,drop=FALSE]
        Ntmax <- dF[nrow(dF),,drop=FALSE]
        Nnew <- Ntmax[1,"Recipient"] - Ntmax[1,"Status quo"]
        df <- rbind(
            '&lambda;'=round(Ntmax/Ntmax1, 3),
            'N (end)'=Ntmax,
            'N (new)'=c(NA, max(0,Nnew),NA),
            "Total cost (x $million)"=c(NA, cost, NA),
            "Cost per new caribou (x $million)"=c(NA,
                ifelse(Nnew>0,cost/Nnew, NA), NA))
        df
    }, rownames=TRUE, colnames=TRUE,
    striped=TRUE, bordered=TRUE, na="n/a",
    sanitize.text.function = function(x) x)

    ## dowload
    breeding_xlslist <- reactive({
        req(breeding_getF())
        bb <- breeding_getF()
        dF <- summary(bb)
        ss <- breeding_getS()
        out <- list(
            Info=data.frame(CaribouBC=paste0(
                c("R package version: ", "Date of analysis: ", "Caribou subpopulation: "),
                c(ver, format(Sys.time(), "%Y-%m-%d"), input$breeding_herd))),
            Settings=as.data.frame(ss),
            TimeSeries=as.data.frame(dF),
            AgeClasses=stack_breeding(bb))
        out$Settings$Parameters <- rownames(ss)
        out$Settings <- out$Settings[,c(ncol(ss)+1, 1:ncol(ss))]
        out
    })
    output$breeding_download <- downloadHandler(
        filename = function() {
            paste0("CaribouBC_conservation_breeding_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(breeding_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
    )

    ## >>> linear features <<<=====================================

    output$seismic_sliders <- renderUI({
        area <- switch(input$seismic_herd,
            "coldlake"=6726,
            "esar"=13119,
            "wsar"=15707)
        linkm <- switch(input$seismic_herd,
            "coldlake"=11432,
            "esar"=26154,
            "wsar"=26620)
        lin2d <- switch(input$seismic_herd,
            "coldlake"=8012,
            "esar"=21235,
            "wsar"=21941)
        yng <- switch(input$seismic_herd,
            "coldlake"=13.85,
            "esar"=25.70,
            "wsar"=6.88)
        tagList(
            sliderInput("seismic_area",
                "Range area (sq km)",
                min = 0, max = 20000, value = area, step = 1),
            sliderInput("seismic_linkm",
                "Linear feature length (km)",
                min = 0, max = 40000, value = linkm, step = 1),
            sliderInput("seismic_lin2d",
                "Conventional seismic length (km)",
                min = 0, max = 40000, value = lin2d, step = 1),
            sliderInput("seismic_young",
                "Percent young forest (<30 yrs; %)",
                min = 0, max = 100, value = round(yng, 1), step = 0.11),
#            sliderInput("seismic_cost",
#                "Cost per km (x $1000)",
#                min = 0, max = 100, value = 12, step = 1),
            sliderInput("seismic_deact",
                "Years for 100% deactivation",
                min = 0, max = 50, value = 5, step = 1),
            sliderInput("seismic_restor",
                "Years for 100% restoration",
                min = 0, max = 50, value = 15, step = 1)
        )
    })
    seismic_all <- reactive({
        req(input$seismic_area,
            input$seismic_linkm,
            input$seismic_lin2d,
            input$seismic_young)
        if (input$seismic_linkm < input$seismic_lin2d) {
            showNotification("Conventional seismic cannot be more than total linear",
                             type="error")
            return(NULL)
        }
        caribou_seismic(
            tmax=input$tmax,
            pop.start=input$popstart,
            area=input$seismic_area,
            lin=input$seismic_linkm,
            seism=input$seismic_lin2d,
            young=input$seismic_young,
            cost=input$seismic_cost,
            yr_deact=input$seismic_deact,
            yr_restor=input$seismic_restor)
    })

    ## plot
    output$seismic_Plot <- renderPlotly({
        req(seismic_all())
        sm <- seismic_all()
        #print(sm)
        dF <- data.frame(sm$pop)
        colnames(dF)[1:2] <- c("Years", "Individuals")
        p <- plot_ly(dF, x = ~Years, y = ~Individuals,
            name = 'No linear features', type = 'scatter', mode = 'lines',
            color=I('red')) %>%
            add_trace(y = ~N1, name = 'Status quo', data = dF,
                    mode = 'lines', color=I('blue')) %>%
            add_trace(y = ~Ndeact, name = 'Deactivation', data = dF,
                    mode = 'lines', color=I('black')) %>%
            add_trace(y = ~Nrestor, name = 'Restoration', data = dF,
                    mode = 'lines', color=I('orange')) %>%
            layout(legend = list(x = 100, y = 0)) %>%
            config(displayModeBar = 'hover', displaylogo = FALSE)
        p
    })
    ## table
    seismic_getT <- reactive({
        req(seismic_all())
        sm <- seismic_all()
        dF <- data.frame(sm$pop)
        colnames(dF) <- c("Years", "No linear features", "No restoration",
            "Deactivation", "Restoration",
            "Linear density, deactivation", "Linear density, restoration",
            "Percent young forest")
        df <- dF[nrow(dF),2:5]
        rownames(df) <- "N (end)"
        cost <- c(NA, NA, sm$costdeact, sm$costrestor)
        Nnew <- c(NA, NA, pmax(0, dF[nrow(dF),4:5]-dF[nrow(dF),3]))
        df <- rbind(
            "&lambda;"=dF[nrow(dF),2:5]/dF[nrow(dF)-1,2:5],
            df,
            "N (new)"=Nnew,
            "Total cost (x $million)"=cost,
            "Cost per new caribou (x $million)"=c(ifelse(Nnew>0,cost/Nnew, NA), NA, NA))
        df
    })
    output$seismic_Table <- renderTable({
        req(seismic_getT())
        seismic_getT()
    }, rownames=TRUE, colnames=TRUE,
    striped=TRUE, bordered=TRUE, na="n/a",
    sanitize.text.function = function(x) x)

    ## dowload
    seismic_xlslist <- reactive({
        req(seismic_all(), seismic_getT())
        sm <- seismic_all()
        dF <- data.frame(sm$pop)
        colnames(dF) <- c("Years", "No linear features", "No restoration",
                          "Deactivation", "Restoration",
        "Linear density, deactivation", "Linear density, restoration",
        "Percent young forest")
        df <- seismic_getT()
        print("getT")
        rownames(df) <- gsub("&lambda;", "lambda", rownames(df))
        out <- list(
            Info=data.frame(CaribouBC=paste0(
                c("R package version: ", "Date of analysis: ", "Caribou subpopulation: "),
                c(ver, format(Sys.time(), "%Y-%m-%d")))),
            TimeSeries=as.data.frame(dF),
            Summary=as.data.frame(df))
        out$Summary$Variables <- rownames(df)
        out$Summary <- out$Summary[,c(ncol(df)+1, 1:ncol(df))]
        out
    })
    output$seismic_download <- downloadHandler(
        filename = function() {
            paste0("CaribouBC_linear_features_", format(Sys.time(), "%Y-%m-%d"), ".xlsx")
        },
        content = function(file) {
            write.xlsx(seismic_xlslist(), file=file, overwrite=TRUE)
        },
        contentType="application/octet-stream"
    )


}

