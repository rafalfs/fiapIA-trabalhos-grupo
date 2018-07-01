#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

ui <- navbarPage(
  "Trabalho R",
  tabPanel("Grupo", includeHTML("./pages/grupo/groups.nb.html")),
  tabPanel("Descrição", includeHTML("./pages/descricao/descricao.nb.html")),
  tabPanel("Enriquecimento do Dataset", includeHTML("./pages/enriquecimento/EnriquecimentoDataset.html")),
  tabPanel("Analises", includeMarkdown("./pages/analises/analises.Rmd")),
  tabPanel("Subsets"),
  tabPanel("Analises Graficas"),
  tabPanel("Modelagem ML")
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)
