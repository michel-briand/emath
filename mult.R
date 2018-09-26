#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#
# This application is a small workshop for mathematics.
# It is inspired from the Micmaths video (https://youtu.be/-X49VQgi86E).
# It displays a representation of multiplication on the circle.
# Playing with the number and the modulo, one creates flower petals.
#
# (c) 2018 Michel Briand
# Creative Commons Attribution-ShareAlike 4.0 International License
# http://creativecommons.org/licenses/by-sa/4.0/

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Multiplication"),

  
  fluidRow(
    column(12,
        span(style="font-style: italic;font-size: 1em;",
          p(style="margin:0px", "Ce petit atelier mathématique est inspiré par la vidéo de Micmaths :"),
          tags$a(href="https://youtu.be/-X49VQgi86E", "La face cachée des tables de multiplication",
                 target="_blank",rel="noopener noreferrer", style="align: center;"),
          p(style="margin:0px", "En choisissant la table de multiplication et le modulo, vous pouvez visualiser des pétales de fleur...")
        )
    ),
    column(12, br()) # blank
  ),
  
  
  # Sidebar with a slider input for number and modulo
  sidebarLayout(
    sidebarPanel(
      sliderInput("nombre",
                  "Table de multiplication :",
                  min = 1,
                  max = 500,
                  value = 2),
      sliderInput("modulo",
                  "Modulo :",
                  min = 0,
                  max = 500,
                  value = 10)
    ),
    
    # Show a plot of the generated circle and lines
    mainPanel(
      plotOutput("distPlot", width = "50vw", height = "50vw")
    )
  ),
  
  fluidRow(
    column(12,
           br(),br(),
           p("(c) 2018 Michel Briand"),
           tags$a(rel="license", href="http://creativecommons.org/licenses/by-sa/4.0/",
                  "Creative Commons Attribution-ShareAlike 4.0 International License"),
           img(alt="Creative Commons License",style="border-width:0",src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png"),
           br(),br()
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({

    n <- input$nombre
    m <- input$modulo
    theta <- 2*pi/m
    
    N <- 100
    tt <- seq(0, 2*pi, length.out = N)
    i <- seq(0, m-1)
    
    circleFun <- function(center = c(0,0),diameter = 1){
      r = diameter / 2
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
    
    startFun <- function(a) {
      return(a*theta)
    }
    endFun <- function(a) {
      return(n*a*theta)
    }
    
    linesFun <- function(center = c(0,0),diameter = 1){
      r = diameter / 2
      # do not use lapply which returns a list, use this to have a vector:
      s <- sapply(i, startFun, simplify = TRUE)
      e <- sapply(i, endFun, simplify = TRUE)
      tt <- c(rbind(s, e))
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    } 
    
    dat <- circleFun(c(0,0), 2)
    dat2 <- linesFun(c(0,0), 2)
    g <- ggplot(asp = 1, dat,aes(x,y)) + geom_path() + geom_path(data=dat2)
    
    return(g)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
