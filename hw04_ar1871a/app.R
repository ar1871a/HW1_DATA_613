library(shiny)
library(shinyWidgets)
library(ggplot2)

ui <- fluidPage(
  setBackgroundColor("yellow"),
  titlePanel(
    div(
      h2(style = "color: red;", "Homework Assignment"),
      p("Agustina M. Roffo Hernandez")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("discrete",   "Choose a discrete variable:",  choices = c("cyl","gear","carb")),
      selectInput("continuous", "Choose a continuous variable:", choices = c("mpg","hp","wt"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data",    tableOutput("data")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("BoxPlot", plotOutput("boxplot")),
        tabPanel("Bar",     plotOutput("frequency_barplot")),
        tabPanel("Histogram", plotOutput("histplot"))
      )
    )
  )
)

server <- function(input, output, session) {
  output$data <- renderTable({mtcars[, c(input$discrete, input$continuous)]
  })

  output$summary <- renderPrint({
    summary(samp())
  })

  output$boxplot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$discrete, y = input$continuous)) +
      geom_boxplot()
  })
  
  output$frequency_barplot <- renderPlot({
    req(input$discrete)+
      geom_bar()
  })
  
  output$histplot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$continuous)) +
      geom_histogram()
  })
}


shinyApp(ui, server)


