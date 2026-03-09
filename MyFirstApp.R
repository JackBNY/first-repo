#My First App

#Packages
library(shiny)
install.packages("shinythemes")
library(shinythemes)

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                  "My First App",
                  tabPanel("Name and Fave Player",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "What is your name?", ""),
                             textInput("txt2", "Who is your favorite basketball player?", "")
                             
                           ),
                           mainPanel(
                             h1("Student Info"),
                             
                             h3("Ouput 1"),
                             verbatimTextOutput("txtout"),
                           )
                           ),
                  tabPanel("Fave Color",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("color", "What is your favorite color?", ""),
                           ),
                           mainPanel(
                             h1("Student Info"),
                             
                             h3("Output1"),
                             verbatimTextOutput("colorOut")
                           ))
                )
                )

server <- function(input, output) {
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep = " ")
  })
  output$colorOut <- renderText({
    paste(input$color)
  })
  }


shinyApp(ui = ui, server = server)


