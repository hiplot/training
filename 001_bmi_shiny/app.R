library(shiny)

#ui 
ui <- fluidPage(
  titlePanel("Calculate Body Mass Index (BMI)"),
  
  sidebarLayout(
    sidebarPanel(
      p("This tool calculates the BMI by using fomular:"),
      br(),
      img(src="bmi.svg"),
      br(),
      br(),
      
      numericInput(inputId = "weight",label = "Enter body weight (kg)",value = 75,min = 0,max = 500,step = 1),
      numericInput(inputId = "height",label = "Enter body height (cm)",value = 180,min = 0,max = 300,step = 5)
      
    ),
    mainPanel(
      p("According to your input, the BMI is:"),
      br(),
      strong(textOutput(outputId = "bmi"),style="font-size:150%;")
    )
  )
)

#server 
server <- function(input, output, session) {
  output$bmi <- renderText({
    bmi = input$weight / ((input$height / 100) ^ 2)
    sprintf("%2.4f kg/m2",bmi)
  })
}

# run app
shinyApp(ui, server)