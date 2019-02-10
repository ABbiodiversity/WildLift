dashboardPage(
  dashboardHeader(title = "Caribou BC"),
  dashboardSidebar(
    sliderInput("tmax", "Number of years to forecast",
      min = 1, max = 100, value = 20, step = 1
    ),
    sliderInput("popstart", "Initial population size",
      min = 1, max = 200, value = 100, step = 1
    ),
    bsTooltip("tmax",
      "Number of years in which the caribou population is forecasted. Default set, but the user can change the value by slider."),
    bsTooltip("popstart",
      "Number of caribou in the starting population. Default set, but the user can change the value by slider."),
    sidebarMenu(
      menuItem("Maternity pen", tabName = "penning"),
      menuItem("Predator exclosure", tabName = "predator"),
      menuItem("Moose reduction", tabName = "moose")
    )
  ),
  dashboardBody(
    tabItems(

      tabItem("penning",
        fluidRow(
          column(width=8,
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Population forecast: Maternity pen",
              plotlyOutput("penning_Plot", width = "100%", height = 400)
            ),
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Summary: Maternity pen",
              tableOutput("penning_Table"),
              downloadButton("penning_download", "Download results as Excel file")
            )
          ),
          column(width=4,
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Penning",
              sliderInput("penning_FpenPerc", "Percent of females penned",
                min = 0, max = 100, value = round(100*inits$penning$fpen.prop),
                step = 1),
              uiOutput("penning_button")
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              title = "Demography",
              sliderInput("penning_DemCsw", "Calf survival, wild",
                min = 0, max = 1, value = inits$penning$c.surv.wild, step = 0.01),
              sliderInput("penning_DemCsc", "Calf survival, captive",
                min = 0, max = 1, value = inits$penning$c.surv.capt, step = 0.01),
              sliderInput("penning_DemFsw", "Adult female survival, wild",
                min = 0, max = 1, value = inits$penning$f.surv.wild, step = 0.01),
              sliderInput("penning_DemFsc", "Adult female survival, captive",
                min = 0, max = 1, value = inits$penning$f.surv.capt, step = 0.01),
              sliderInput("penning_DemFpw", "Pregnancy rate, wild",
                min = 0, max = 1, value = inits$penning$f.preg.wild, step = 0.01),
              sliderInput("penning_DemFpc", "Pregnancy rate, captive",
                min = 0, max = 1, value = inits$penning$f.preg.capt, step = 0.01)
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              title = "Cost (x $1000)",
              sliderInput("penning_CostPencap", "Max in a single pen",
                min = 1, max = 100, value = inits$penning$pen.cap, step = 1),
              sliderInput("penning_CostSetup", "Initial set up",
                min = 0, max = 2000, value = 100*round(inits$penning$pen.cost.setup/100),
                step = 100),
              sliderInput("penning_CostProj", "Project manager",
                min = 0, max = 500, value = inits$penning$pen.cost.proj, step = 10),
              sliderInput("penning_CostMaint", "Maintenance",
                min = 0, max = 1000, value = inits$penning$pen.cost.maint, step = 10),
              sliderInput("penning_CostCapt", "Capture/monitor",
                min = 0, max = 500, value = inits$penning$pen.cost.capt, step = 10)#,
              #sliderInput("penning_CostPred", "Removing predators",
              #  min = 0, max = 500, value = inits$penning$pen.cost.pred, step = 10)
            )
          )
        )
      ),

      tabItem("predator",
        fluidRow(
          column(width=8,
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Population forecast: Predator exclosure",
              plotlyOutput("predator_Plot", width = "100%", height = 400)
            ),
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Summary: Predator exclosure",
              tableOutput("predator_Table"),
              downloadButton("predator_download", "Download results as Excel file")
            )
          ),
          column(width=4,
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Penning",
              sliderInput("predator_FpenPerc", "Percent of females penned",
                min = 0, max = 100, value = round(100*inits$predator$fpen.prop),
                step = 1),
              uiOutput("predator_button")
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              title = "Demography",
              sliderInput("predator_DemCsw", "Calf survival, wild",
                min = 0, max = 1, value = inits$predator$c.surv.wild, step = 0.01),
              sliderInput("predator_DemCsc", "Calf survival, captive",
                min = 0, max = 1, value = inits$predator$c.surv.capt, step = 0.01),
              sliderInput("predator_DemFsw", "Adult female survival, wild",
                min = 0, max = 1, value = inits$predator$f.surv.wild, step = 0.01),
              sliderInput("predator_DemFsc", "Adult female survival, captive",
                min = 0, max = 1, value = inits$predator$f.surv.capt, step = 0.01),
              sliderInput("predator_DemFpw", "Pregnancy rate, wild",
                min = 0, max = 1, value = inits$predator$f.preg.wild, step = 0.01),
              sliderInput("predator_DemFpc", "Pregnancy rate, captive",
                min = 0, max = 1, value = inits$predator$f.preg.capt, step = 0.01)
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              title = "Cost (x $1000)",
              sliderInput("predator_CostPencap", "Max in a single pen",
                min = 1, max = 100, value = inits$predator$pen.cap, step = 1),
              sliderInput("predator_CostSetup", "Initial set up",
                min = 0, max = 2000, value = 100*round(inits$predator$pen.cost.setup/100),
                step = 100),
              sliderInput("predator_CostProj", "Project manager",
                min = 0, max = 500, value = inits$predator$pen.cost.proj, step = 10),
              sliderInput("predator_CostMaint", "Maintenance",
                min = 0, max = 1000, value = inits$predator$pen.cost.maint, step = 10),
              sliderInput("predator_CostCapt", "Capture/monitor",
                min = 0, max = 500, value = inits$predator$pen.cost.capt, step = 10),
              sliderInput("predator_CostPred", "Removing predators",
                min = 0, max = 500, value = inits$predator$pen.cost.pred, step = 10)
            )
          )
        )
      ),

      tabItem("moose",
        fluidRow(
          column(width=8,
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Population forecast: Moose reduction",
              plotlyOutput("moose_Plot", width = "100%", height = 400)
            ),
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Summary: Moose reduction",
              tableOutput("moose_Table"),
              downloadButton("moose_download", "Download results as Excel file")
            )
          )
        )
      )

    )
  )
)

