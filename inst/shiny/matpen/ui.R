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
        fluidRow(
          column(width=8,
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = FALSE, collapsed = FALSE,
              title = "Population forecast",
              #plotOutput("penningPlot", width = "100%", height = 400)
              plotlyOutput("penningPlot", width = "100%", height = 400)
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
              uiOutput("penningDemControls")
            ),
            box(
              width = NULL, status = "info", solidHeader = TRUE,
              collapsible = TRUE, collapsed = TRUE,
              title = "Cost (x $1000$)",
              uiOutput("penningCostControls")
            )
          )
        )
      )
    )
  )
)

