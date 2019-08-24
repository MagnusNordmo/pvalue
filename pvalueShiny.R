


library(shiny)


ui <- fluidPage(
  
  
  titlePanel("P value intuition pump"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "sample",
                  label = "Sample Size",
                  min = 6,
                  max = 500,
                  value = 40),
      sliderInput(inputId = "sims",
                  label = "Simulations",
                  min = 1,
                  max = 500,
                  value = 5),
      sliderInput(inputId = "eff",
                  label = "Difference Between Groups",
                  min = 0,
                  max = 50,
                  value = 0),
      actionButton("go", "Re-run")
    ),
    
    mainPanel(
      "Imagine you have two groups of people with IQ mean = 100 and sd = 15. One group
      has received an intervention and you perform a t-test and assess the pvalue.",
      plotOutput(outputId = "distPlot")
      
    )
  )
)


server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    options(scipen = 10)
    
    input$go
    
    pvalues = 0
    for (i in 1:input$sims){
      x <- rnorm(input$sample/2, mean = 100,sd = 15)
      y <- rnorm(input$sample/2, mean = 100 + input$eff,sd = 15)
      z <- t.test(x,y)
      pvalues[i] <- z$p.value
    }
    isolate({hist(pvalues,breaks = 20)})
    abline(v = 0.05,col = "red")
    mtext(paste(round(((sum(pvalues < .05)/length(pvalues))*100),digits = 1), "% significant with p < .05", sep = " "), side = 3, line=0)
    
  })
}

shinyApp(ui, server)

