#Load Required Packages
library(shiny)
library(shinyWidgets) #I needed this one for the setBackgroundColor function
library(ggplot2)

#Define User Interface (set background color, title, name)
ui <- fluidPage(
  setBackgroundColor("yellow"),
  titlePanel(
    div(
      h2(style = "color: red;", "Homework Assignment"),
      p("Agustina M. Roffo Hernandez")
    )
  ),
  
#Layout with sidebar for inputs (used selectInput to define discrete and continuous variables usage) and main area for outputs (data table, summary,box plot, frequency barplot, histogram)  
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
        tabPanel("BarPlot",     plotOutput("frequency_barplot")),
        tabPanel("Histogram", plotOutput("histplot"))
      )
    )
  )
)

#Define the server logic to generate outputs based on inputs
server <- function(input, output, session) {
  output$data <- renderTable({mtcars[, c(input$discrete, input$continuous)]
  })
  
#Data table: subset mtcars to the selected columns
  output$summary <- renderPrint({
    cat("Summary: Discrete\n")
    print(summary(mtcars[[input$discrete]]))
    cat("\nSummary: Continuous\n")
    print(summary(mtcars[[input$continuous]]))
  })

#Summary statistics: shows summaries for both discrete and continuous variables selected
    output$boxplot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$discrete, y = input$continuous)) +
      geom_boxplot()
  })

#Boxplot: shows discrete variable on x-axis and continuous variable on y-axis 
  output$frequency_barplot <- renderPlot({
    req(input$discrete)
    
#Bar plot: shows frequency counts of the discrete variable (I converted the chosen discrete column to a factor for proper bar chart grouping, for this I duplicated mtcars to extract the data)
    mtcars2 <- mtcars
    mtcars2[[input$discrete]] <- as.factor(mtcars2[[input$discrete]])
    
    ggplot(mtcars2, aes_string(x = input$discrete)) +
      geom_bar()
  })
  
#Histogram: shows distribution of the continuous variable selected
  output$histplot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$continuous)) +
      geom_histogram()
  })
}


shinyApp(ui, server)


