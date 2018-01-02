library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Confidence Interval for proportion"),
  
  # Sidebar with slider inputs for sample size of true proportion
  sidebarLayout(
    sidebarPanel(
      sliderInput("prop",
                  "True proportion:",
                  min = 0.01,
                  max = 0.99,
                  value = 0.5),
      sliderInput("nsamp",
                  "Sample Size:",
                  min = 50,
                  max = 1000,
                  value = 50, 
                  step = 50)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)