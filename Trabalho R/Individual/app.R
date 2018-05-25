import = function(libName){
  result = tryCatch({
    library(libName, character.only = T)
  }, error = function(e) {
    install.packages(libName)
    library(libName, character.only = T)
  })
}

import("shiny")
import("here")
import("shinydashboard")

current.dir = here()

ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Introducao ao R", icon = icon("minus"), tabName = "intro")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
        includeHTML(paste0(current.dir, "/index.html"))
      ),
      tabItem(tabName = "intro",
              includeHTML(paste0(current.dir, "/intro.html"))
      )
    )
  ),
  title = "Portfolio"
)

server <- function(input, output) { }

shinyApp(ui = ui, server = server)
