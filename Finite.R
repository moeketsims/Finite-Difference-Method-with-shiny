library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
# library(plotly)
# Hello papi...
# add kemelo instead

ui <- fluidPage(
  # use a gradient in background
  # setBackgroundColor(
  #   color = c("#F7FBFF", "#2171B5"),
  #   gradient = "linear",
  #   direction = "bottom"
  # ),
  # App title ----
  #titlePanel(),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      # Input: Simple integer interval ----
      numericInput("block", "Size of the Block:", 0.5, min = 0.1, max = 2,
                   step = 0.001),
      verbatimTextOutput("value1"),
      # Input: Decimal interval with step value ----
      sliderInput("delta_x", "Delta X:",
                  min = 0.001, max = 2,
                  value = 0.01, step = 0.001),
      # Input: Specification of range within an interval ----
      numericInput("t_space", "Time Space:", 300, min = 10, max = 400),
      verbatimTextOutput("value2"),
      
      sliderInput("delta_t", "Delta t:",
                  min = 0, max = 1,
                  value = 0.5, step = 0.1),
      # Input: Custom currency format for with basic animation ----
      sliderInput("rho", "Rho:",
                  min = 1, max = 10000,
                  value = 2000, step = 100,
                  animate = TRUE),
      
      sliderInput("cp", "cp:",
                  min = 1, max = 1000,
                  value = 500, step = 10,
                  animate = TRUE),
      
      sliderInput("lamda", "lamda:",
                  min = 1, max = 30,
                  value = 1, step = 1,
                  animate = TRUE),
      
      numericInput("IC", "Initial Condition:", 303, min = 10, max = 1000),
      verbatimTextOutput("value3")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel('Temperature Profile',
                 fluidRow(
                   column(width = 6,
                     plotOutput('dist'))
                   )
                 ),
        tabPanel('Nodes')
                
      )
    ))
)
server <- function(input, output) {
  
  nx <- reactive({
    A <- floor(input$block/input$delta_x)
  })
  nt <- reactive({
    B <- floor(input$t_space/input$delta_t)
  })
  T_i <- reactive({
    D <- rep(input$IC,nx())
  })
 TT <- reactive({
   T_ <- T_i()
   for (k in 1:nt()) {
     Tn <- T_
     T_[2:(nx()-1)] <- Tn[2:(nx()-1)] + input$delta_t*input$lamda/(input$rho*input$cp)*((Tn[3:nx()] - 2 * Tn[2:(nx()-1)] + Tn[1:(nx()-2)])/(input$delta_x)**2)
     T_[1] = 293.15
     T_[nx()] = T_[nx()-1]
   }
    T_ - 273.15
 })
 output$dist <- renderPlot({
   df <- data.frame(x = seq(0,input$block, length = nx()), y = TT())
   ggplot(data = df, aes(x, y ))+
     geom_line(size=0.9)+
     xlab('deltaX')+
     ylab('Temperature')+
     ggtitle("Temperature Profile") +
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5))
  })
}
shinyApp(ui, server)

