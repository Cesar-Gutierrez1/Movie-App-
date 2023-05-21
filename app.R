# step 1 - say what you want to make
# the app should have a few drop down menus, one for the x-axis, one for the y-axis, and the last one for movie genre - that determines which points show up
# the user is also given a slider based on their choice of previous drop down - they can choose the range between ratings, votes, and metascore
# the plot matches

# step 2 - make it
# importing all the libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)

# here is the raw data and it gets turned in 'movies' so that i can use it in the app
url = 'https://raw.githubusercontent.com/peetck/IMDB-Top1000-Movies/master/IMDB-Movie-Data.csv'
movies = read_csv(url)


# Define UI for application that draws a scatter plot
ui = fluidPage(
  navbarPage("IMDB Movies Ranked",
             tabPanel("Visualization",
                      titlePanel("IMDB Movies Ranked"),
                      sidebarLayout(
                        sidebarPanel(
                          # x axis - the only choice will be title so that the user can clearly see what movie gets what rating, votes, etc.
                          selectInput("x_axis", "X Axis:", choices = c("Title")),

                          # the y axis - three options the user can choose from - rating, votes, and metascore
                          selectInput("y_axis", "Y Axis:", choices = c("Rating", "Votes", "Metascore")),

                          # the last inout is genre
                          selectInput("genre", "Genre:", choices = unique(movies$Genre)),

                          # Of the minimum three input elements, the choices of one of the inputs must be conditionally dependent on another input.
                          # the inputs from the y axis will create sliders that are dependent on what category the user chooses

                          # condition panel for the metascore
                          conditionalPanel(
                            condition = "input.y_axis == 'Metascore'",
                            sliderInput("metascore_range", "Metascore Range:", min = 0, max = 100, value = c(0, 100))
                          ),
                          # condition panel for the rating
                          conditionalPanel(
                            condition = "input.y_axis == 'Rating'",
                            sliderInput("Rating_range", "Rating Range:", min = 0, max = 10, value = c(0, 10))
                          ),
                          # condition panel for the votes
                          conditionalPanel(
                            condition = "input.y_axis == 'Votes'",
                            sliderInput("Votes_range", "Votes Range:", min = 0, max = 100000000, value = c(0, 100000000))
                          )
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
             ),
             # table panel - will show the movies being sorted by either the rating, votes, or metascore - depending on the user
             tabPanel("Table",
                      sidebarLayout(
                        sidebarPanel(
                          #options for the user
                          selectInput("sort_by", "Sort By:", choices = c("Rating", "Votes", "Metascore")),
                          # the user can se the movies rated from increments of 10
                          selectInput("num_rows", "Number of Rows", choices = seq(10, nrow(movies), by = 10))
                        ),
                        mainPanel(
                          tableOutput("table")
                        )
                      )
             ),
             # the about page - markdown
             tabPanel("About",
                      includeMarkdown("about.Rmd")
             )
  )
)

# Define server logic required to draw a scatter
server = function(input, output) {

  # using the reactive
  movies_reactive = reactive({
    movies %>%
      # filtering and piping
      # filtering the genres into being the genres available from the dataset
      filter(Genre == input$genre) %>%

      # filtering the metascore to match the dataset
      filter(`Metascore` >= input$metascore_range[1] & `Metascore` <= input$metascore_range[2]) %>%

      # filtering the ratings to match the datset
      filter(`Rating` >= input$Rating_range[1] & `Rating` <= input$Rating_range[2]) %>%

      # filtering the votes to match the dataset
      filter(`Votes` >= input$Votes_range[1] & `Votes` <= input$Votes_range[2])
  })

  # the scatterplot output
  output$plot = renderPlot({a
    ggplot(movies_reactive(), aes_string(x = input$x_axis, y = input$y_axis)) +
      geom_point() +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })

  # the table/second panel
  output$table = renderTable({
    movies_reactive() %>%
      # !! means to 'unquote'
      select(Title, !!input$sort_by) %>%
      # arrange so the list goes from highest to lowest
      arrange(desc(!!input$sort_by)) %>%
      head(input$num_rows)
  })
}

# Run app
shinyApp(ui = ui, server = server)
