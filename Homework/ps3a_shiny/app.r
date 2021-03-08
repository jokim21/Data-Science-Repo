library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  actionButton(inputId = "update", 
               label = "Update"),
  navlistPanel(              
    tabPanel(title = "Normal data",
             plotOutput("norm"),
    ),
    tabPanel(title = "Uniform data",
             plotOutput("unif"),
    ),
    tabPanel(title = "Chi Squared data",
             plotOutput("chisq"),
    ),
  plotOutput("hist"),
  verbatimTextOutput("stats"))
)


server <- function(input, output) {
  data <- eventReactive(input$update, {
    rnorm(input$num)
  })
  
  rv <- reactiveValues(
    norm = rnorm(100), 
    unif = runif(100),
    chisq = rchisq(100, 2))
  
  observeEvent(input$renorm, { rv$norm <- rnorm(100) })
  observeEvent(input$reunif, { rv$unif <- runif(100) })
  observeEvent(input$rechisq, { rv$chisq <- rchisq(100, 2) })
  
  observeEvent(input$update, {
    print(input$title)
  })
  output$hist <- renderPlot({
    hist(data(),
    main = isolate(input$title))
  })
  output$stats <- renderPrint({
    summary(data())
  })
  output$title <- renderText({
    input$title})
  output$norm <- renderPlot({
    hist(data(),
         main = isolate(input$title))
  })
  output$unif <- renderPlot({
    hist(rv$unif,
        main = "100 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
    hist(rv$chisq,
         main = "100 random draws from a random draws from a Chi Square distribution with two degree of freedom")
  })
}

shinyApp(ui = ui, server = server)