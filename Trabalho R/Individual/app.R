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
import("magrittr")
import("knitr")
import("shinydashboard")

current.dir = here()

includeRmd <- function(path, r_env = parent.frame()) {
  encoding = getOption("shiny.site.encoding", default = "UTF-8")
  paste0(
    readLines(path, warn = FALSE, encoding = encoding),
    collapse = '\n'
  ) %>%
    knit2html(
      text = .,
      fragment.only = TRUE,
      envir = r_env,
      options = "",
      stylesheet = "",
      encoding = encoding,
      quiet = T
    ) %>%
    gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
    gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
    HTML
}

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
        includeRmd(paste0(current.dir, "/index.Rmd"))
      ),
      
      tabItem(tabName = "intro",
        includeRmd(paste0(current.dir, "/Aula01.Rmd"))
      )
    )
  ),
  title = "Portfolio"
)

server <- function(input, output) { }

shinyApp(ui = ui, server = server)
