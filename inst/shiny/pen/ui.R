dashboardPage(
  dashboardHeader(title = "Caribou BC"),
  dashboardSidebar(
    sliderInput("tmax", "Number of years to forecast",
      min = 1, max = 100, value = 20, step = 1
    ),
    sliderInput("pop.start", "Initial population size",
      min = 1, max = 200, value = 100, step = 1
    ),
    sliderInput("fpen.perc", "Percent of females penned",
      min = 0, max = 100, value = 25, step = 1
    ),
    sidebarMenu(
      menuItem("Penning", tabName = "penning")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("penning",
#        fluidRow(
#          valueBoxOutput("lambda"),
#          valueBoxOutput("popsize"),
#          valueBoxOutput("breakeven")
#        ),
        fluidRow(
          column(width=8,
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Population forecast",
              plotOutput("penningPlot", width = "100%", height = 400)
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Summary",
              tableOutput("penningTable")
            )
          ),
          column(width=4,
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = FALSE,
              title = "Pen type",
              selectInput("penningType", label=NULL,
                list("Maternity pen"="mat.pen",
                     "Predator exclusion"="pred.excl"))
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              title = "Demography",
              sliderInput("penningDemCsw", "Calf survival, wild",
                min = 0, max = 1, value = 0.16, step = 0.01
              ),
              sliderInput("penningDemCsc", "Calf survival, captive",
                min = 0, max = 1, value = 0.54, step = 0.01
              ),
              sliderInput("penningDemFsw", "Maternal survival, wild",
                min = 0, max = 1, value = 0.85, step = 0.01
              ),
              sliderInput("penningDemFsc", "Maternal survival, captive",
                min = 0, max = 1, value = 0.90, step = 0.01
              ),
              sliderInput("penningDemFpw", "Pregnancy rate, wild",
                min = 0, max = 1, value = 0.92, step = 0.01
              ),
              sliderInput("penningDemFpc", "Pregnancy rate, captive",
                min = 0, max = 1, value = 0.92, step = 0.01
              )
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              title = "Cost (x $1000$)",
              sliderInput("penningCostPencap", "Max in a single pen",
                min = 1, max = 50, value = 35, step = 1
              ),
              sliderInput("penningCostSetup", "Initial set up",
                min = 0, max = 500, value = 500, step = 10
              ),
              sliderInput("penningCostProj", "Project manager",
                min = 0, max = 500, value = 80, step = 10
              ),
              sliderInput("penningCostMaint", "Maintenance",
                min = 0, max = 500, value = 250, step = 10
              ),
              sliderInput("penningCostCapt", "Capture/monitor",
                min = 0, max = 500, value = 250, step = 10
              ),
              sliderInput("penningCostPred", "Removing predators",
                min = 0, max = 500, value = 0, step = 10
              )
            )
          )
        )
      )
    )
  )
)

