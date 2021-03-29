library(reshape)
library(reshape2)
library(psych)
library(shiny)
library(plotly)

simulator_days <- function(So, r, d, sigma, num_simulations, numDays){
  
  simul <- as.matrix(seq(numDays))
  
  for(i in 1:num_simulations){
    simul <- cbind(simul, rlnorm(numDays, (r - d - .5*sigma*sigma)/365, sigma*sqrt(1/365)))
  }
  
  simul <- apply(simul[,-1], 2, cumprod)
  simul <- apply(simul, 2, FUN = function(x){x*So})
  
  dates <- as.matrix(seq(numDays))
  
  simul <- cbind(dates, simul)
  return(simul)
}

BS_Call <- function(So, K, r, d, sigma, numDays){
  d1 <- (log(So/K)+(r-d+.5*sigma*sigma)*(numDays/365))/(sigma*sqrt(numDays/365)) 
  d2 <- (log(So/K)+(r-d-.5*sigma*sigma)*(numDays/365))/(sigma*sqrt(numDays/365)) 
  So*exp(-d*(numDays/365))*pnorm(d1, mean = 0, sd = 1) - K*exp(-r*(numDays/365))*pnorm(d2, mean = 0, sd = 1)
}
BS_Put <- function(So, K, r, d, sigma, numDays){
  d1 <- (log(So/K)+(r-d+.5*sigma*sigma)*(numDays/365))/(sigma*sqrt(numDays/365)) 
  d2 <- (log(So/K)+(r-d-.5*sigma*sigma)*(numDays/365))/(sigma*sqrt(numDays/365)) 
  K*exp(-r*(numDays/365))*pnorm(-d2, mean = 0, sd = 1) - So*exp(-d*(numDays/365))*pnorm(-d1, mean = 0, sd = 1)
}  

MC_Call <- function(simul, K, r, numDays){
  Call <- simul[numDays, -1]
  Call <- apply(t(Call), 2, function(x){max(0, x-K)})
  Call <- mean(Call)*exp(-r*numDays/365)
  return(Call)
}
MC_Put <- function(simul, K, r, numDays){
  Call <- simul[numDays, -1]
  Call <- apply(t(Call), 2, function(x){max(0, K-x)})
  Call <- mean(Call)*exp(-r*numDays/365)
  return(Call)
}

# Asian Options
MC_Call_A_priceAverage <- function(simul, K, r, numDays){
  Call <- simul[, -1]
  Call <- apply(Call, 2, function(x){max(0, mean(x)-K)} )
  Call <- mean(Call)*exp(-r*numDays/365)
  return(Call)
}
MC_Call_A_strikeAverage <- function(simul, K, r, numDays){
  Call <- simul[, -1]
  AvgPrice <- apply(Call, 2, function(x){max(0, mean(x))} )
  Call <- mean(pmax(0, Call[numDays, ]-AvgPrice))
  return(Call)
}
MC_Put_A_priceAverage <- function(simul, K, r, numDays){
  Put <- simul[, -1]
  Put <- apply(Put, 2, function(x){max(0, K-mean(x))} )
  Put <- mean(Put)*exp(-r*numDays/365)
  return(Put)
}
MC_Put_A_strikeAverage <- function(simul, K, r, numDays){
  Put <- simul[, -1]
  AvgPrice <- apply(Put, 2, function(x){max(0, mean(x))} )
  Put <- mean(pmax(0, AvgPrice-Put[numDays, ]))
  return(Put)
}

# Binary Options
Bin_Call <- function(simul, K, r, numDays){
  Call <- simul[numDays, -1]
  Call <- apply(t(Call), 2, function(x){if(x>K){100}else{0}})
  Call <- mean(Call)*exp(-r*numDays/365)
  return(Call)
}
Bin_Put <- function(simul, K, r, numDays){
  Put <- simul[numDays, -1]
  Put <- apply(t(Put), 2, function(x){if(x<K){100}else{0}})
  Put <- mean(Put)*exp(-r*numDays/365)
  return(Put)
}

# Barrier Options
Bar_Call_knockIn <-  function(simul, So, K, Bar, r, numDays){
  Call <- simul[numDays, -1]
  if(Bar>So){plantilla <- apply(simul[, -1], 2, function(x){if(max(x)>Bar){1}else{0}})}else{plantilla <- apply(simul[, -1], 2, function(x){if(max(x)<Bar){1}else{0}}) }
  Call <- apply(t(Call), 2, function(x){max(0, x-K)})
  Call <- mean(Call*plantilla)*exp(-r*numDays/365)
  return(Call)
}
Bar_Call_knockOut <-  function(simul, So, K, Bar, r, numDays){
  Call <- simul[numDays, -1]
  if(Bar>So){plantilla <- apply(simul[, -1], 2, function(x){if(max(x)>Bar){0}else{1}})}else{plantilla <- apply(simul[, -1], 2, function(x){if(max(x)<Bar){0}else{1}}) }
  Call <- apply(t(Call), 2, function(x){max(0, x-K)})
  Call <- mean(Call*plantilla)*exp(-r*numDays/365)
  return(Call)
}

Bar_Put_knockIn <-  function(simul, So, K, Bar, r, numDays){
  Call <- simul[numDays, -1]
  if(Bar>So){plantilla <- apply(simul[, -1], 2, function(x){if(max(x)>Bar){1}else{0}})}else{plantilla <- apply(simul[, -1], 2, function(x){if(max(x)<Bar){1}else{0}}) }
  Call <- apply(t(Call), 2, function(x){max(0, K-x)})
  Call <- mean(Call*plantilla)*exp(-r*numDays/365)
  return(Call)
}
Bar_Put_knockOut <-  function(simul, So, K, Bar, r, numDays){
  Call <- simul[numDays, -1]
  if(Bar>So){plantilla <- apply(simul[, -1], 2, function(x){if(max(x)>Bar){0}else{1}})}else{plantilla <- apply(simul[, -1], 2, function(x){if(max(x)<Bar){0}else{1}}) }
  Call <- apply(t(Call), 2, function(x){max(0, K-x)})
  Call <- mean(Call*plantilla)*exp(-r*numDays/365)
  return(Call)
}

############################################################################################################

shinyServer(function(input, output, session) {
  
  simul <- reactive({ simul <- simulator_days(input$So, (input$r/100), (input$d/100), (input$sigma/100), input$numSim, input$numDays)
                    })
  
  
  output$stock_plot <- renderPlot({
      data <- simul()
      data <- data[, -1]
      palette(c("blue4", "blue3", "dodgerblue4", "darkslategray", "lightsteelblue3", "royalblue3", "steelblue4"))
      matplot(data, type = 'l', lwd = 0.5, lty = 1, col = 1:5,
              xlab = 'Days', ylab = 'Stock Price',
              main = 'Stock Price Simulation (Log-Normal random walks)')
      grid()
      abline(h=input$strikeO, col = "black", lwd=2); abline(h=input$barrier, col = "darkgreen", lwd=2)
      legend( x="bottomright", 
              legend=c("Strike Price","Barrier"), 
              col=c("black","darkgreen"), lwd=2, lty=c(1,1), 
              pch=c(NA,NA), merge=FALSE )
      
  })
  
  output$option_plot <- renderPlotly({
    
    simul <- simul(); So = input$So; r = (input$r/100); d = (input$d/100); 
    sigma = (input$sigma/100); num_simulations = input$numSim; numDays = input$numDays
    Bar = input$barrier; lowerR = So-20; upperR = So+20
    
    option_plot <- data.frame(c(lowerR:upperR))
    names(option_plot) <- "Strike_Price"
    
    option_plot$CallKI <- lapply(lowerR:upperR, function(x){Bar_Call_knockIn(simul, So, x, Bar, r, numDays)} )
    option_plot$CallKI <- round(as.numeric(option_plot$CallKI), 4)
    
    option_plot$CallKO <- lapply(lowerR:upperR, function(x){Bar_Call_knockOut(simul, So, x, Bar, r, numDays)} )
    option_plot$CallKO <- round(as.numeric(option_plot$CallKO), 4)
    
    option_plot$PutKI <- lapply(lowerR:upperR, function(x){Bar_Put_knockIn(simul, So, x, Bar, r, numDays)} )
    option_plot$PutKI <- round(as.numeric(option_plot$PutKI), 4)
    
    option_plot$PutKO <- lapply(lowerR:upperR, function(x){Bar_Put_knockOut(simul, So, x, Bar, r, numDays)} )
    option_plot$PutKO <- round(as.numeric(option_plot$PutKO), 4)
    
    option_plot$MC_Call_A_priceAverage <- lapply(lowerR:upperR, function(x){MC_Call_A_priceAverage(simul, x, r, numDays)} )
    option_plot$MC_Call_A_priceAverage <- round(as.numeric(option_plot$MC_Call_A_priceAverage), 4)

    option_plot$MC_Put_A_priceAverage <- lapply(lowerR:upperR, function(x){MC_Put_A_priceAverage(simul, x, r, numDays)} )
    option_plot$MC_Put_A_priceAverage <- round(as.numeric(option_plot$MC_Put_A_priceAverage), 4)

    option_plot$MC_Call <- lapply(lowerR:upperR, function(x){MC_Call(simul, x, r, numDays)} )
    option_plot$MC_Call <- round(as.numeric(option_plot$MC_Call), 4)
    
    option_plot$MC_Put <- lapply(lowerR:upperR, function(x){MC_Put(simul, x, r, numDays)} )
    option_plot$MC_Put <- round(as.numeric(option_plot$MC_Put), 4)
    
    option_plot$BS_Call <- lapply(lowerR:upperR, function(x){BS_Call(So, x, r, d, sigma, numDays)} )
    option_plot$BS_Call <- round(as.numeric(option_plot$BS_Call), 4)
    
    option_plot$BS_Put <- lapply(lowerR:upperR, function(x){BS_Put(So, x, r, d, sigma, numDays)} )
    option_plot$BS_Put <- round(as.numeric(option_plot$BS_Put), 4)
    
    colnames(option_plot) <- c("Strike Price", 
                               "Call (Knock In)", "Call (Knock Out)", "Put (Knock In)", "Put (Knock Out)",
                               "Call (Asian)", "Put (Asian)", 
                               "Call (Monte Carlo)", "Put (Monte Carlo)",
                               "Call (Black-Scholes)", "Put (Black-Scholes)")
    
    option_plot_melt <- melt(option_plot, id = c("Strike Price"))
    names(option_plot_melt) <- c("Strike Price", "variable", "Premium")
    
    p <- plot_ly(option_plot_melt, x = `Strike Price`, y = Premium,
                color = variable, colors = c("darkblue", "darkgreen", "deepskyblue3", "darkolivegreen2",
                                             "darkorchid4", "indianred", 
                                             "yellow4", "yellow2", "lightsalmon4", "lightsalmon2")
                 )
    p
  })
  
  output$callT <- renderText({round(BS_Call(input$So, input$strikeO, (input$r/100), (input$d/100), (input$sigma/100), input$numDays), 4)})  
  output$putT <- renderText({ round(BS_Put(input$So, input$strikeO, (input$r/100), (input$d/100), (input$sigma/100), input$numDays), 4)})  
  output$CallMC <- renderText({ round(MC_Call(simul(), input$strikeO, (input$r/100), input$numDays), 4)})  
  output$PutMC <- renderText({ round(MC_Put(simul(), input$strikeO, (input$r/100), input$numDays), 4)})
  
  output$CallAP <- renderText({ round(MC_Call_A_priceAverage(simul(), input$strikeO, (input$r/100), input$numDays), 4)})
  output$CallAS <- renderText({ round(MC_Call_A_strikeAverage(simul(), input$strikeO, (input$r/100), input$numDays), 4)})
  output$PutAP <- renderText({ round(MC_Put_A_priceAverage(simul(), input$strikeO, (input$r/100), input$numDays), 4)})
  output$PutAS <- renderText({ round(MC_Put_A_strikeAverage(simul(), input$strikeO, (input$r/100), input$numDays), 4)})
  
  output$CallBin <- renderText({ round(Bin_Call(simul(), input$strikeO, (input$r/100), input$numDays), 4)})
  output$PutBin <- renderText({ round(Bin_Put(simul(), input$strikeO, (input$r/100), input$numDays), 4)})
  
  output$CallKI <- renderText({ round(Bar_Call_knockIn(simul(), input$So, input$strikeO, input$barrier, (input$r/100), input$numDays), 4)})
  output$PutKI <- renderText({ round(Bar_Put_knockIn(simul(), input$So, input$strikeO, input$barrier, (input$r/100), input$numDays), 4)})
  output$CallKO <- renderText({ round(Bar_Call_knockOut(simul(), input$So, input$strikeO, input$barrier, (input$r/100), input$numDays), 4)})
  output$PutKO <- renderText({ round(Bar_Put_knockOut(simul(), input$So, input$strikeO, input$barrier, (input$r/100), input$numDays), 4)})
})
############################################################################################################


