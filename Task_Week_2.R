# Task: Write a shiny program (and put it online) for a difference in proportions test:
# Hypothesis Test: Given the input, perform a hypothesis test for significance
# Sample Size Calculator: Given inputs, output t-stat and required sample sizes for experiment


# check out rbookdown? package

library(shiny)
library(ggplot2)


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Hypothesis Test", 
             numericInput(inputId = "p1", label = "Sample Proportion 1", value = 0.5),
             numericInput(inputId = "p2", label = "Sample Proportion 2", value = 0.5),
             numericInput(inputId = "n1", label = "Sample Size 1", value = 30),
             numericInput(inputId = "n2", label = "Sample Size 2", value = 30),
             actionButton(inputId = "test", label = "Calculate p Value"),
             textOutput("significance"),
             plotOutput("conf_int")),
    
    tabPanel("Sample Size Calculator",
             numericInput(inputId = "p1_2", label = "Sample Proportion 1", value = 0.5),
             numericInput(inputId = "p2_2", label = "Sample Proportion 2", value = 0.5),
             numericInput(inputId = "ratio", label = "Ratio between n1 and n2", value = 1),
             numericInput(inputId = "alpha", label = "Type 1 Error Rate", value = 0.05),
             numericInput(inputId = "power", label = "Power: 1 - Type 2 Error Rate", value = 0.95),
             actionButton(inputId = "calculate", label = "Calculate Sample Size"),
             htmlOutput("samplesize"),
             plotOutput("size_graph")))
)

###########################################################################################################

hyp_test <- function(input) {
  validate(need((input$p1 >= 0 && input$p1 <= 1), "Fix p1 such that: 0 <= p1 <= 1"),
           need((input$p2 >= 0 && input$p2 <= 1), "Fix p2 such that: 0 <= p2 <= 1"),
           need(input$n1 >= 30, "Fix n1 such that: n1 <= 30"),
           need(input$n2 >= 30, "Fix n2 such that: n2 <= 30"))
  
  success = c(input$p1 * input$n1, input$p2 * input$n2)
  sample = c(input$n1, input$n2)
  test = prop.test(success, sample)
  
  return(test)
}


get_size <- function(input) {
  validate(need((input$p1_2 >= 0 && input$p1_2 <= 1), "Fix p1 such that: 0 <= p1 <= 1"),
           need((input$p2_2 >= 0 && input$p2_2 <= 1), "Fix p2 such that: 0 <= p2 <= 1"),
           need(input$ratio > 0, "Fix power such that: ratio > 0"),
           need((input$alpha > 0 && input$alpha < 1), "Fix alpha such that: 0 <= alpha <= 1"),
           need((input$power > 0 && input$power < 1), "Fix power such that: 0 <= power <= 1"))
  
  n2 = ceiling((input$p1_2 * (1 - input$p1_2) / input$ratio +
                  input$p2_2 * (1 - input$p2_2)) *
                 ((qnorm(1 - input$alpha / 2)+ qnorm(input$power)) / (input$p1_2 - input$p2_2))^2)
  
  n1 = ceiling(n2 * input$ratio)
  
  return(c(n1,n2))
}


# get_fxn <- function(input) {
#   validate(need((input$p1_2 >= 0 && input$p1_2 <= 1), "Fix p1 such that: 0 <= p1 <= 1"),
#            need((input$p2_2 >= 0 && input$p2_2 <= 1), "Fix p2 such that: 0 <= p2 <= 1"),
#            need(input$ratio > 0, "Fix power such that: ratio > 0"),
#            need((input$alpha > 0 && input$alpha < 1), "Fix alpha such that: 0 <= alpha <= 1"),
#            need((input$power > 0 && input$power < 1), "Fix power such that: 0 <= power <= 1"))
#   
#   fxn = function(x) (input$p1_2 * (1 - input$p1_2) / input$ratio +
#                      input$p2_2 * (1 - input$p2_2)) *
#                     ((qnorm(1 - input$alpha / 2)+ qnorm(input$power)) / (input$p1_2 - input$p2_2))^2
# 
#   return(fxn)
# }

###########################################################################################################

server <- function(input, output) {
  
  
  p.value <- eventReactive(input$test, hyp_test(input)$p.value)
  conf.int <- eventReactive(input$test, hyp_test(input)$conf.int)
  sample.size <- eventReactive(input$calculate, get_size(input))
  
  # fxn <- eventReactive(input$calculate, get_fxn(input))
  
  output$significance <- renderText({
    paste("The p value is:", p.value())
  })
  
  
  output$conf_int <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
      geom_hline(yintercept = 0, color = "red") +
      geom_hline(yintercept = conf.int()[1], linetype = "dashed", color = "blue") +
      geom_hline(yintercept = conf.int()[2], linetype = "dashed", color = "blue") +
      labs(x = "", y = "Difference in Proportion") +
      ylim(-1, 1)
  })
  
  
  output$samplesize <- renderUI({
    HTML("The n1 required for significance is", sample.size()[1], "<br> The n2 required for significance is", sample.size()[2])
  })
  
  
#   output$graph <- renderPlot({
#     ggplot(mtcars, aes(x = wt, y = mpg, color = cyl)) +
#       geom_function(fun = fxn())
#   })
# }

###########################################################################################################

shinyApp(ui, server)