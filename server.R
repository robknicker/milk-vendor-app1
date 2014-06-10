# server.R
library(shiny)

shinyServer(
  function(input, output) {
    
    milk <- reactive({
      c(as.numeric(input$num1),
        as.numeric(input$num2),
        as.numeric(input$num3),
        as.numeric(input$num4),
        as.numeric(input$num5),
        as.numeric(input$num6),
        as.numeric(input$num7),
        as.numeric(input$num8),
        as.numeric(input$num9),
        as.numeric(input$num10),
        as.numeric(input$num11),
        as.numeric(input$num12),
        as.numeric(input$num13),
        as.numeric(input$num14),
        as.numeric(input$num15),
        as.numeric(input$num16),
        as.numeric(input$num17),
        as.numeric(input$num18),
        as.numeric(input$num19),
        as.numeric(input$num20),
        as.numeric(input$num21))  
    })
    
    mean <- reactive({
      sum(milk())/length(milk())
    })
    var <- reactive({
      sum((milk()-sum(milk())/length(milk()))^2)/(length(milk())-1) 
    })
    expi <- reactive({
      as.numeric(input$exp)
    })
    newmean <- reactive({
      expi()*mean()
    })
    newvar <- reactive({
      expi()^2*var()
    })
    cost <- reactive({
      as.numeric(input$cost)
    })
    sell <- reactive({
      as.numeric(input$sell)
    })
    overcost<- reactive({
      cost()
    })
    undercost<- reactive({
      sell() - cost()
    })
    q <- reactive({
      undercost()/(overcost()+undercost())
    })
    opt <- reactive({
      qnorm(q(), newmean(), sqrt(newvar()))
    })
    output$opt <- renderText({
      opt()
    })
    output$mean <- renderPrint({
      mean()
    })
    output$sd <- renderText({
      sqrt(var())
    })
    output$newmean <- renderText({
      newmean()
    })
    output$newsd <- renderText({
      sqrt(newvar())
    })
    output$hist <- renderPlot({
      hist(milk())
    })
  }
)