library(shiny)

ui <- fluidPage(
      navlistPanel(              
    tabPanel(title = "Normal data",
             plotOutput("norm"),
             actionButton("renorm", "Resample")
    ),
    tabPanel(title = "Uniform data",
             plotOutput("unif"),
             actionButton("reunif", "Resample")
    ),
    tabPanel(title = "Chi Squared data",
             plotOutput("chisq"),
             actionButton("rechisq", "Resample"))
    ),
    sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  actionButton(inputId = "update", 
               label = "Update"),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  data <- eventReactive(input$update, {
    rnorm(input$num)
  })
  rv <- reactiveValues(
    norm = rnorm(500), 
    unif = runif(500),
    chisq = rchisq(500, 2))
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
    hist(rv$norm, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard normal distribution")
  })
  output$unif <- renderPlot({
    hist(rv$unif, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a standard uniform distribution")
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, breaks = 30, col = "grey", border = "white",
         main = "500 random draws from a Chi Square distribution with two degree of freedom")
  })
}

shinyApp(ui = ui, server = server)