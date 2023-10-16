library(quantmod)
library(forecast)
library(ggplot2)

#Part 3.1
# Getting the 5 years of Stock data of COST from Yahoo Finance
getSymbols("KO",
           src="yahoo",
           from = format(Sys.time() - 365*5*24 * 60 * 60, "%Y-%m-%d %H:%M:%00"),
           to = Sys.time(),
           periodicity = 'monthly'
) 

getSymbols("cost",
           src="yahoo",
           from = format(Sys.time() - 365*5*24 * 60 * 60, "%Y-%m-%d %H:%M:%00"),
           to = Sys.time(),
           periodicity = 'monthly'
) 

#Plotting Individual graphs
plot(KO$KO.Close,
     main = "Closed Price of Coco-Cola (KO)",
     ylab = "Price (in $)")
plot(COST$COST.Close,
     main = "Closed Price of Costco (COST)",
     ylab = "Price (in $)")


# Storing the stocks data
Cola_StockData = as.data.frame(KO)
Costco_StockData = as.data.frame(COST)
StockData <- cbind(Cola_StockData,Costco_StockData)
StockData$Dates <- as.Date(rownames(StockData))
#View(StockData)


#Converting into time series
ko.ts = ts(StockData$KO.Close, 
            start = c(2017,5), 
            end = c(2022,5),
            frequency = 12)
cost.ts = ts(StockData$COST.Close, 
            start = c(2017,5), 
            end = c(2022,5),
            frequency = 12)

#Part 3.2
# Calculating AR(1)
StockData$KO_AR1 = TTR::SMA(StockData$KO.Close, n = 2)
StockData$COST_AR1 = TTR::SMA(StockData$COST.Close, n = 2)


# Coco-cola
# Plotting the original and AR(1) series
ggplot(StockData, aes(x= StockData$Dates)) + 
  geom_line(aes(y = StockData$KO.Close,
                color = "Actual Price"),
            group = 1) + 
  geom_line(aes(y = StockData$KO_AR1,
                color = "Predicted price using AR(1)"),
            group = 1)+
  theme_light(base_size = 13)+ 
  labs(title ="Actual prices vs ARIMA(1) for KO",
       x= "Time Period",
       y= "Close Price (in $)") + 
  legend(color="Legend")



# Plotting the original and AR(1) series
ggplot(StockData, aes(x= StockData$Dates)) + 
  geom_line(aes(y = StockData$KO.Close,
                color = "Actual Price"),
            group = 1) + 
  geom_line(aes(y = StockData$KO_AR1,
                color = "Predicted price using AR(1)"),
            group = 1)+
  theme_light(base_size = 13, 
              legend.position="top",legend.justification="right")+ 
  labs(title ="Actual prices vs ARIMA(1) for KO",
       x= "Time Period",
       y= "Close Price (in $)") + 
  labs(color="Legend")


# Creating exponential smoothing
esr_KO = ses(ko.ts,
             h = 24, 
             level = c(85,95),
             alpha = 0.55)

autoplot(esr_KO) +
  theme_light(base_size = 14) +
  labs(title ="Exponential smoothing for Coco Cola",
       x= "Time Period",
       y= "Close price (in $)")


# Costco
# Plotting the original and AR(1) series
ggplot(StockData, aes(x= StockData$Dates)) + 
  geom_line(aes(y = StockData$COST.Close,
                color = "Actual Price"),
            group = 1) + 
  geom_line(aes(y = StockData$COST_AR1,
                color = "Predicted price using AR(1)"),
            group = 1)+
  theme_light(base_size = 14)+ 
  labs(title ="Actual prices vs ARIMA(1) for COSTCO",
       x= "Time Period",
       y= "Close Price (in $)") + 
  labs(color="Legend")

# Creating exponential smoothing
esr_COST = ses(cost.ts,
             h = 24, 
             level = c(85,95),
             alpha = 0.55)

autoplot(esr_COST) +
  theme_light(base_size = 14) +
  labs(title ="Exponential smoothing for Costco",
       x= "Time Period",
       y= "Close price (in $)")



# PART 3.3
#cola
cola_autoarima <- auto.arima(ko.ts)
forecast_cola <- forecast(cola_autoarima)
#forecast_cola
plot(forecast_cola,
     xlab = "Time Period",
     ylab = "Close price of KO (in $)")

#costco
costco_autoarima <- auto.arima(cost.ts)
forecast_costco <- forecast(costco_autoarima)
#forecast_costco
plot(forecast_costco,
     xlab = "Time Period",
     ylab = "Close price of COST (in $)")



# PART 3.4
# opening the data source
file_path = 'C:/Users/narut/Documents/Northeastern University/Q2_Apr2022/ALY 6050/Week 3/Project/dry_wine.csv'
wine_sales <- read.csv(file_path)
#View(wine_sales)

# using auto arima
wine_sales.ts = ts(wine_sales[2], 
            start = c(1980,1), 
            end = c(1995,7), 
            frequency = 12)
wine_autoarima = auto.arima(wine_sales.ts)
summary(wine_autoarima)
plot(forecast(wine_autoarima, h = 25),
     col = "#C70039",
     xlab = "Time Period",
     ylab = "Dry Wine Price (in $)")
 