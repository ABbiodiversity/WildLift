dashboardPage(
  dashboardHeader(title = "Caribou BC"),
  dashboardSidebar(
    sliderInput("tmax", "Number of years to forecast",
      min = 1, max = 100, value = 20, step = 1
    ),
    sliderInput("pop.start", "Initial population size",
      min = 1, max = 200, value = 100, step = 1
    ),
    sidebarMenu(
      menuItem("Maternity pen", tabName = "penning")#,
      #menuItem("Safe haven", tabName = "haven")
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
              title = "Population forecast",
              plotlyOutput("penning_Plot", width = "100%", height = 400)
            ),
            box(
              width = NULL, status = "success", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Summary",
              tableOutput("penning_Table")
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
              sliderInput("penning_DemFsw", "Maternal survival, wild",
                min = 0, max = 1, value = inits$penning$f.surv.wild, step = 0.01),
              sliderInput("penning_DemFsc", "Maternal survival, captive",
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
                min = 1, max = 50, value = inits$penning$pen.cap, step = 1),
              sliderInput("penning_CostSetup", "Initial set up",
                min = 0, max = 500, value = inits$penning$pen.cost.setup, step = 10),
              sliderInput("penning_CostProj", "Project manager",
                min = 0, max = 500, value = inits$penning$pen.cost.proj, step = 10),
              sliderInput("penning_CostMaint", "Maintenance",
                min = 0, max = 500, value = inits$penning$pen.cost.maint, step = 10),
              sliderInput("penning_CostCapt", "Capture/monitor",
                min = 0, max = 500, value = inits$penning$pen.cost.capt, step = 10),
              sliderInput("penning_CostPred", "Removing predators",
                min = 0, max = 500, value = inits$penning$pen.cost.pred, step = 10)
            )
          )
        )
      )
    )
  )
)

