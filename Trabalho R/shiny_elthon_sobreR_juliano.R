#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Verify if all needed packages are installed and active
mypkgdf <- data.frame(installed.packages())

#Verify if package shiny installed
if ('shiny' %in% mypkgdf$Package) {
  print("package shiny is installed")
  library(shiny)
} else {
  #install package shiny
  install.packages("shiny")
  library(shiny)
}

#Verify if package shinydashboard installed
if ('shinydashboard' %in% mypkgdf$Package) {
  print("package shinydashboard is installed")
  library(shinydashboard)
} else {
  #install package shinydashboard
  install.packages("shinydashboard")
  library(shinydashboard)
}

#Verify if package rmarkdown installed
if ('rmarkdown' %in% mypkgdf$Package) {
  print("package rmarkdown is installed")
  library(rmarkdown)
} else {
  #install package rmarkdown
  install.packages("rmarkdown")
  library(rmarkdown)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("O que falta colocar", tabName = "DashBoard", icon = icon("dashboard")),
      menuItem("Introducao ao R", icon = icon("th"), tabName = "introducao")
    )
    
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "DashBoard",

              h2("# O que falta colocar") ,
              p("+ plots") ,
              p("+ tamanho dos tipos de variaveis"),
              p("+ object size - para carregar os datasets"),
              p("+ lubridate - conversor de datas"),
              p("+ criar uma coluna no dataset"),
              p("+ matrizes"),
              p("+ colocar mais sobre o notebook"),
              p("+ falar mais sobre o R"),
              p("+ regressao"),
              p("+ tratamento de datas")


      ),
      
      tabItem(tabName = "introducao",
              h2("1.1 Sobre o R"),
              
              p("Uma das principal ferramenta dos cientista de dados"),
              p("Com mais de 2 milhoes de usuario ao redor do mundo. A cada ano, estima-se que o numero de usuarios cresce 40%")
      )
    )
  ),
  title = "Dashboard example"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

