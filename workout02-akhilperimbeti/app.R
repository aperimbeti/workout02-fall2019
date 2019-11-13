###Workout2 R Script

library(shiny)
library(ggplot2)
library(tidyr)

ui <- fluidPage(
  h1("Saving Investment Scenarios"),
  fluidRow(
    column(3,
           sliderInput("initial",
                       "Initial amount",
                       min = 0,
                       max = 10000,
                       value = 1000,
                       step = 500), 
           sliderInput("contribution",
                       "Annual Contribution",
                       min = 0,
                       max = 5000,
                       value = 200,
                       step = 200),
           sliderInput("growth",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = .1)),
    
    
    column(3,
           sliderInput("rate1",
                       "High Yield Annual Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 2,
                       step = .1),
           sliderInput("rate2",
                       "Fixed Income Annual Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5,
                       step = .1),
           sliderInput("rate3",
                       "US Equity Annual Rate(in %)",
                       min = 0,
                       max = 20,
                       value = 10,
                       step = .1)),
    
    column(3,
           sliderInput("vol1",
                       "High Yield Volatility(in %)",
                       min = 0,
                       max = 20,
                       value = .1,
                       step = .1),
           sliderInput("vol2",
                       "Fixed Income Volatility(in %)",
                       min = 0,
                       max = 20,
                       value = 4.5,
                       step = .1),
           sliderInput("vol3",
                       "US Equity Volatility(in %)",
                       min = 0,
                       max = 20,
                       value = 15,
                       step = .1)),
    
    column(3,
           sliderInput("years", 
                       "Years",
                       min = 0, 
                       max = 50,
                       value = 20,
                       step = 1),
           numericInput("r_seed", 
                        "Random Seed", 
                        value = 12345),
           selectInput("facet",
                       "Facet?",
                       choices = list("Yes" = 1, "No" = 2),
                       selected = 1))
  ),
  hr(),
  h4("Timelines"),
  fluidRow(
    column(12, plotOutput("distPlot"))
  ))


server <- function(input, output){
  
  investment <- reactive({
    high_yield <- rep(0, input$years)
    us_bonds <- rep(0, input$years)
    us_stocks <- rep(0, input$years)
    year <- c(0:input$years)
    r_seed <- set.seed(input$r_seed)
    
    for(n in 0:input$years){
      rate_yield <- 0.1 
      vol_yield <- 0.15  
      r1<- rnorm(1, mean = input$rate1/100, sd = input$vol1/100)
      amt <- input$initial*(1 + r1) + input$contribution*(1+input$growth/100)^n
      high_yield[n+1] <- amt*(1 + r1) + input$contribution*(1+input$growth/100)^n
    }
    
    for(n in 0:input$years){
      rate_bonds <- 0.05 
      vol_bonds <- 0.045  
      r2<- rnorm(1, mean = input$rate2/100, sd = input$vol2/100)
      amt <- input$initial*(1 + r2) + input$contribution*(1+input$growth/100)^n
      us_bonds[n+1] <- amt*(1 + r2) + input$contribution*(1+input$growth/100)^n
    }
    for(n in 0:input$years){
      rate_stock <- 0.1 
      vol_stock <- 0.15  
      r3<- rnorm(1, mean = input$rate3/100, sd = input$vol3/100)
      amt <- input$initial*(1 + r3) + input$contribution*(1+input$growth/100)^n
      us_stocks[n+1] <- amt*(1 + r3) + input$contribution*(1+input$growth/100)^n
    }
    investment <- data.frame(
      year = 0:input$years,
      us_bonds = us_bonds,
      high_yield = high_yield,
      us_stocks = us_stocks
    )
    return(investment)
  })  
  
  plot1 <- reactive({
    if(input$facet == 1){
      facet_data <- gather(investment(), key = "savings", value = "value", c("high_yield", "us_bonds", "us_stocks"))
      ggplot(facet_data, aes(x = year, y = value, color = savings, fill = savings)) +
        geom_area(stat = 'identity', alpha = 0.2) +
        facet_wrap( ~ savings) +
        labs(title = "Three Indices", x = "Year", y = "US Dollars") +
        theme_bw()
    }else{
      ggplot(investment(), aes(x = year)) +
        geom_line(aes(y = high_yield, color = "high_yield")) +
        geom_line(aes(y = us_bonds, color = "us_bonds")) +
        geom_line(aes(y = us_stocks, color = "us_stocks")) +
        labs(title = "Saving Timelines", x = "Year", y = "Amount") +
        scale_color_manual(name = "index", values = c("blue", "red", "green")) +
        theme_bw()
    }
  })
  output$distPlot <- renderPlot({
    plot1()
  })
}

shinyApp(ui = ui, server = server)