
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#setwd("C:\\R\\shinyTest1")

library(shiny)
library(ggplot2)
library(reshape2)
library(scales)

annualGrowth <- function(startValue, 
                         growth, 
                         years, 
                         fixedGrowth = 0, 
                         reduct = 0,
                         firstYear = 0){
  #year n = year(n-1) * (1+gr)
  #how to keep year one?
  years_vect = c(2:years)
  values = startValue
  for (year in years_vect){
    value = values[year-1]
    value = value*(1+(growth-reduct)) + fixedGrowth
    values = c(values,value)
  }
  values[1] = values[1] + firstYear
  return(values)
}

cumGrowth <- function(x){
  values = x[1]
  for (i in (2:length(x)) ){
    value = x[i] + values[i-1]
    values = c(values,value)
  }
  return(values)
}

pv <- function(dr, initalCost,netProfit, nn=length(netProfit)) {
  
  netProfit[1] = netProfit[1] + initalCost
  
  sum(netProfit * (1/(1+dr))^(0:(nn-1))) }


paybackDiscounted <- function(costProject,netProfit,dr=0){
  remainingCost = costProject
  returns = netProfit
  returns[1] = returns[1] + remainingCost
  year = 1
  

  while (remainingCost > 0){
    
    remainingCost = remainingCost - (netProfit[year]*((1/(1+dr))^year))
    
    year = year + 1
    
    if(is.na(netProfit[year])& remainingCost > 0){
      return(-1)
    }
    
  }
  
  if(year==2){
    payback = (netProfit[year-1]*((1/(1+dr))^(year-1))) / costProject
    return(payback)
  }
  
  res = (netProfit[year-1]*((1/(1+dr))^(year-1))) / (remainingCost +(netProfit[year-1]*((1/(1+dr))^(year-1)))+ (netProfit[year-2]*((1/(1+dr))^(year-2))))
  
  if(res>1){
    res = 1
  }
  
  if(res < 0){
    res = 0
  }
  
  payback = year - 1 + res
  return(payback)
}

shinyServer(function(input, output) {
  
  forecast <- reactive({
  
    revBegin <- as.numeric(input$revBegin)
    
    #ongoing percentage of revenue increase per year
    revGrowthPerc <- as.numeric(input$revGrowthPerc)
    
    #Fixed revenue increased from project
    revGrowth <- as.numeric(input$revGrowth)
    
    #Additonal Percent revenue increase from more predictive capabilities
    revGrowthPercNew <- as.numeric(input$revGrowthPercNew)
    
    costBegin <- as.numeric(input$costBegin)
    
    costProject <- as.numeric(input$costProject)
    
    costGrowthPerc <- as.numeric(input$costGrowthPerc)
    
    #fixed cost increase for salaries and license costs
    costGrowth <- as.numeric(input$costGrowth)
    
    #Percent per year reduction in cost from new project
    #Typically from increased efficiency
    costRedPerc <- as.numeric(input$costRedPerc)
    
    savingYear <- as.numeric(input$savingYear)
    
    years <- as.numeric(input$years)
    
    forecast <- data.frame("year" = c(1:years),
                           "revenue" = annualGrowth(
                             revBegin,
                             revGrowthPerc,
                             years,
                             #fixedGrowth = revGrowth
                             ),
                           "revenueNew" = annualGrowth(
                             revBegin,
                             revGrowthPerc+revGrowthPercNew,
                             years,
                             fixedGrowth=revGrowth
                             ),
                           "cost" = annualGrowth(
                             costBegin,
                             costGrowthPerc,
                             years
                             ),
                           "costNew" = annualGrowth(
                             costBegin - savingYear,
                             costGrowthPerc,
                             years ,
                             fixedGrowth=costGrowth,
                             reduct=costRedPerc,
                             firstYear=costProject
                             )
    )
    

    
    forecast$profit <- forecast$revenue - forecast$cost
    
    forecast$profitNew <- forecast$revenueNew - forecast$costNew
    
    forecast$cumRevenue <- cumGrowth(forecast$revenue)
    
    forecast$cumRevenueNew <- cumGrowth(forecast$revenueNew)
    
    forecast$cumCost <- cumGrowth(forecast$cost)
    
    forecast$cumCostNew <- cumGrowth(forecast$costNew)
    
    forecast$cumProfit <- cumGrowth(forecast$profit)
    
    forecast$cumProfitNew <- cumGrowth(forecast$profitNew)
    
    forecast$savingsTotal <- forecast$cost - forecast$costNew
    
    forecast
  
  })
  
  output$summarize <- renderPrint( {summary(forecast())}) # Display summary of data table 'forecast'
  
  forecastLong <- reactive({
    
    forecast <- forecast()
    
    if (input$chartType == "Cost"){
      
      forecastLong <- melt(data.frame("year" = forecast$year,
                                      "Cost Org" = forecast$cost,
                                      "Cost New" = forecast$costNew), id = "year")
    }
    
    if (input$chartType == "Revenue"){
      
      forecastLong <- melt(data.frame("year" = forecast$year,
                                      "Revenue Org" = forecast$revenue,
                                      "Revenue New" = forecast$revenueNew), id = "year")
    }
    
    if (input$chartType == "Profit"){
      
      forecastLong <- melt(data.frame("year" = forecast$year,
                                      "Profit Org" = forecast$profit,
                                      "Profit New" = forecast$profitNew), id = "year")
    }
    
    forecastLong
    })

  forecastLongCum <- reactive({
    
    forecast <- forecast()
    
    if (input$chartType == "Cost"){
      
      forecastLongCum <- melt(data.frame("year" = forecast$year,
                                      "Cuml Cost Org" = forecast$cumCost,
                                      "Cuml Cost New" = forecast$cumCostNew), id = "year")
    }
    
    if (input$chartType == "Revenue"){
      
      forecastLongCum <- melt(data.frame("year" = forecast$year,
                                      "Cuml Revenue Org" = forecast$cumRevenue,
                                      "Cuml Revenue New" = forecast$cumRevenueNew), id = "year")
    }
    
    if (input$chartType == "Profit"){
      
      forecastLongCum <- melt(data.frame("year" = forecast$year,
                                      "Cuml Profit Org" = forecast$cumProfit,
                                      "Cuml Profit New" = forecast$cumProfitNew), id = "year")
    }
    
    forecastLongCum
  })

    output$costPlot <- renderPlot({
    title <- paste("Annual ", input$chartType, " Forecast")
    forecastLong <- forecastLong()
    forecastLongDF <- data.frame(
                                  "year" = forecastLong$year,
                                  "value" = forecastLong$value,
                                  "variable" = forecastLong$variable)
    ggplot(forecastLongDF, 
       aes(x = year, y = value, color = variable)) +
      geom_line(size=1.5) +
      ggtitle(title) +
      scale_y_continuous(name=input$chartType, labels=comma)


  })
  
  output$cumCostPlot <- renderPlot({
    title <- paste("Cummulative ", input$chartType, " Forecast")
    forecastLong <- forecastLongCum()
    forecastLongDF <- data.frame(
      "year" = forecastLong$year,
      "value" = forecastLong$value,
      "variable" = forecastLong$variable)
    ggplot(forecastLongDF, 
           aes(x = year, y = value, color = variable)) +
      geom_line(size=1.5) +
      ggtitle(title) +
      scale_y_continuous(name=input$chartType, labels=comma)
    
    
  })
  
  output$forecast <- renderDataTable({
    forecast <- forecast()
    forecast[,1:8]
  })
  
  output$npv <- renderText({
    forecast <- forecast()
    discountRate <- as.numeric(input$discountRate)
    costProject <- as.numeric(input$costProject)
    npv <- pv(discountRate,costProject,(forecast$profitNew - forecast$profit)) - costProject
    text <- paste("Net Present Value of Project: ","$",formatC(round(npv,2), format="d", big.mark=','))
    text
  })
  
  output$payback <- renderText({
    forecast <- forecast()
    costProject <- as.numeric(input$costProject)
    payback <- paybackDiscounted(costProject, (forecast$profitNew - forecast$profit))
    text <- paste("Payback Period: ", round(payback,2))
    text
  })
  
  output$paybackDiscounted <- renderText({
    forecast <- forecast()
    costProject <- as.numeric(input$costProject)
    dr <- as.numeric(input$discountRate)
    payback <- paybackDiscounted(costProject, (forecast$profitNew - forecast$profit), dr=dr)
    text <- paste("Discounted Payback Period: ", round(payback,2))
    text
  })

})


