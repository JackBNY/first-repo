#Movies App

#Load Libraries
library(shiny)
library(shinythemes)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)

#Work for Director, Actor, and Actress Pages
Directors <- My_Movie_Data %>%
  group_by(Director) %>%
  summarise(avg_rating = mean(Rating),
            total_movies = n(),
            Total_Score = avg_rating * total_movies) %>%
  arrange(desc(Total_Score))

Actors <- My_Movie_Data %>%
  group_by(Actor) %>%
  summarise(avg_rating = mean(Rating),
            total_movies = n(),
            Total_Score = avg_rating * total_movies) %>%
  arrange(desc(Total_Score))


Actresses <- My_Movie_Data %>%
  group_by(Actress) %>%
  summarise(avg_rating = mean(Rating),
            total_movies = n(),
            Total_Score = avg_rating * total_movies) %>%
  arrange(desc(Total_Score))


Directors <- as.data.frame(Directors)
Actors <- as.data.frame(Actors)
Actresses <- as.data.frame(Actresses)
  

#Calculating Movie Rating -> Stars
get_stars <- function(rating){
  case_when(
    rating >= 8.75 ~ 5,
    rating >= 8    ~ 4.5,
    rating >= 7.25 ~ 4,
    rating >= 6.5  ~ 3.5,
    rating >= 5.75 ~ 3,
    rating >= 5    ~ 2.5,
    rating >= 4    ~ 2,
    rating >= 3    ~ 1.5,
    rating >= 2    ~ 1,
    TRUE ~ 0.5
  )
}

#UI



ui <- navbarPage("Movies!",
                 theme = shinytheme("cerulean"),
                 tabPanel("Rating Dist",
                          page_fluid(
                            sliderInput(
                              "slider",
                              label = "Number of Bins",
                              min = 1,
                              max = 10,
                              value = 5
                            ),
                            plotOutput("plot")
                          )
                 ),
                 tabPanel("Actors",
                          dataTableOutput("ActorTable")
                 ),
                 tabPanel("Add Moive",
                          textInput("Movie","What movie did you watch?"," "),
                          numericInput("Rating", "How would you rate this movie out of 10?",
                                       value = 5, min = 0, max = 10),
                          textInput("Director", "Who was the director?"," "),
                          textInput("Actor", "Who was the lead actor in this movie?", " "),
                          textInput("Actress", "Who was the lead actress in this movie?", " "),
                          numericInput("Year", "What year did the movie come out?", min = 1900, max = 2026, value = 2026),
                          textInput("Genre", "What is the genre?", " "),
                         actionButton("AddMovie", "Add Movie!"),
                         dataTableOutput("table")
                         
                          )
                 )

                

#Server

server <- function(input, output, session) {
  
  movie_data <- reactiveVal(My_Movie_Data)
  
  output$plot <- renderPlot({
    ggplot(movie_data(), aes(Star)) +
      geom_histogram(bins = input$slider)
  })
  
  output$ActorTable <- renderDataTable({
    datatable(data = Actors,
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  
  observeEvent(input$AddMovie, {
    new_row <- data.frame(
      Movie = input$Movie,
      Rating = input$Rating,
      Star = get_stars(input$Rating),
      Director = input$Director,
      Actor = input$Actor,
      Actress = input$Actress,
      Year = input$Year, 
      Genre = input$Genre
    )
    
    movie_data(rbind(movie_data(), new_row))
    
  })
  
  observeEvent(input$table_cell_edit, {
    
    info <- input$table_cell_edit
    data <- movie_data()
    
    data[info$row,info$col] <- info$value
    
    movie_data(data)
    
    
  })
    
    output$table <- renderDataTable({
      datatable(movie_data(),
                options = list(pageLength = 10),
                rownames = FALSE,
                editable = TRUE)
    })
    
    
  
}

shinyApp(ui = ui, server = server)
        