# DSAN 5600 Time Series Midterm Exam Review

## Reference: Professor Purna's Code in Lab 4's demonstration
## Code by code example:

---
title: "Review Midterm"
author: "Hermann Junhao Fan"
format: 
  html:
    embed-resources: true
toc: true
---

The questions will be the same as the sample exam, but the dataset is eggs_price data-set in lab4's folder.

```{r, echo=FALSE,message=FALSE,warning=FALSE}
library(tidyverse)
library(ggplot2)
library(forecast)
library(astsa) 
library(xts)
library(tseries)
library(fpp2)
library(fma)
library(lubridate)
library(tidyverse)
library(TSstudio)
library(quantmod)
library(tidyquant)
library(plotly)
library(ggplot2)
library(dplyr)
```

```{r}
egg_data <- read.csv("eggs_price_2025.csv")
colnames(egg_data) <- c("Date", "Price")
plot_ly(egg_data, x = ~as.Date(Date)) %>%
  add_lines(y = ~Price, name = 'Egg prices (USD)', line = list(color = 'blue')) %>%
  layout(title = "Time series plot in Egg Prices",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Egg Prices"))
```

```{r}
ts_egg <- ts(egg_data$Price, 
                start = decimal_date(as.Date(min(egg_data$Date))), 
                frequency = 12)  #Monthly dataa


plot(ts_egg, main = "Time Series plot")
```


## Checking for stationary

```{r}
f1 <- ggAcf(ts_egg, 50) + 
  ggtitle("ACF plot without differencing") + 
  theme_minimal()

f1
```
The ACF decays slowly to 0, which is a typical sign of non-stationary. The ADF Test below indicating the same.

```{r}
tseries::adf.test(ts_egg)
```
## Lag plot:

```{r}
gglagplot(ts_egg, do.lines = FALSE) +
  ggtitle("Lag Plot of Original Egg Prices") +
  theme_minimal()
```
From lag 1-4, we are able to see a clear autocorrelation.The lag plot does not show a consistent of autocorrelation.

```{r}
require(gridExtra)

plot1<-autoplot(ts_egg, main="without the log transformation") 
plot2<-autoplot(log(ts_egg), main="log transformed data") 

grid.arrange(plot1, plot2,nrow=2)
```
There is no such big difference, so we do not need log transformation in this case.

```{r}
tseries::adf.test(ts_egg)
```
Not stationary, so take the differencing

```{r}
fig1 <- ggAcf(ts_egg, 50) +
  ggtitle("ACF of egg's price time series") + 
  theme_minimal()

fig1
```
Non-stationary

```{r}
library(patchwork)
diff <- diff(ts_egg)
fig_diff1 <- ggAcf(diff, 50) + 
  ggtitle("ACF For differenced egg price") + 
  theme_minimal() # q = 0, 1, 2
fig_diff2_pacf <- ggPacf(diff, 50) + 
  ggtitle("PACF For differenced egg price")+ 
  theme_minimal() # p = 0, 1, 2, 3

diff_2 <- diff(diff, lag = 12) # D = 1
fig_diff2_acf <- ggAcf(diff_2, 50) + 
  ggtitle("ACF for seasonal differencing")+
  theme_minimal() # Q = 1 or Q = 0

fig_diff2_pacf <- ggPacf(diff_2, 50) + 
  ggtitle("PACF for seasonal differencing") + 
  theme_minimal() # P = 1, 2, 3, 4
  
  
(fig_diff1 | fig_diff2_pacf) / (fig_diff2_acf | fig_diff2_pacf)
```

q = 0, 1, 2 = 1:2
p = 0, 1, 2, 3 = 1:3
d = 1
D = 1
Q = 0, 1 = 1:2
P = 1, 2, 3, 4 = 1:4



```{r}
######################## Check for different combinations ########


#write a funtion
SARIMA.c=function(p1,p2,q1,q2,P1,P2,Q1,Q2,data){
  
  #K=(p2+1)*(q2+1)*(P2+1)*(Q2+1)
  
  temp=c()
  d=1
  D=1
  s=12
  
  i=1
  temp= data.frame()
  ls=matrix(rep(NA,9*35),nrow=35)
  
  
  for (p in p1:p2)
  {
    for(q in q1:q2)
    {
      for(P in P1:P2)
      {
        for(Q in Q1:Q2)
        {
          if(p+d+q+P+D+Q<=9)
          {
            
            model<- Arima(data,order=c(p-1,d,q-1),seasonal=c(P-1,D,Q-1))
            ls[i,]= c(p-1,d,q-1,P-1,D,Q-1,model$aic,model$bic,model$aicc)
            i=i+1
            #print(i)
            
          }
          
        }
      }
    }
    
  }
  
  
  temp= as.data.frame(ls)
  names(temp)= c("p","d","q","P","D","Q","AIC","BIC","AICc")
  
  temp
  
}

```

q = 0, 1, 2 = 1:2
p = 0, 1, 2, 3 = 1:3
d = 1
D = 1
Q = 0, 1 = 1:2
P = 1, 2, 3, 4 = 1:4


```{r}

output=SARIMA.c(p1=1,p2=3,q1=1,q2=2,P1=1,P2=4,Q1=1,Q2=2,data=ts_egg)
#output

#knitr::kable(output)
output[which.min(output$AIC),] 
output[which.min(output$BIC),]
output[which.min(output$AICc),]

```

It find out ARIMA(2, 1, 0)(0, 1, 1)[12] is the best model.

```{r}
auto.arima(ts_egg)
```
Auto ARIMA THINKs ARIMA(4, 1, 1)(0, 0, 2)[12] is the best model.

## Diagnose:

```{r}
# Set seed for reproducibility
set.seed(123)

# ------------------- Fit SARIMA Models & Extract Key Output Dynamically -------------------

# Model 1: SARIMA(2,1,0)(0,1,1)[12]
model_output1 <- capture.output(sarima(ts_egg, 2,1,0, 0,1,1,12))

# Find the relevant line numbers dynamically based on the keyword "Coefficients"
start_line1 <- grep("Coefficients", model_output1)
end_line1 <- length(model_output1)  

# Print the relevant section automatically
cat(model_output1[start_line1:end_line1], sep = "\n")

# ------------------- Model 2: SARIMA(4,1,1)(0,0,2)[12] -------------------

model_output2 <- capture.output(sarima(ts_egg, 4,1,1, 0,0,2,12))

# Find the relevant line numbers dynamically based on the keyword "Coefficients"
start_line2 <- grep("Coefficients", model_output2)
end_line2 <- length(model_output2)  

# Print the relevant section automatically
cat(model_output2[start_line2:end_line2], sep = "\n")

```
ARIMA(2, 1, 0)(0, 1, 1)[12] is the best, since: 

- The **Residual Plot** shows nearly consistent fluctuation around zero, however, after 2020, the residuals fluctate more than the previous years, but still around 0, we could say that the residual is stationary.

- The **Autocorrelation Function (ACF)** of the residuals reveals significant autocorrelations around lag 1.5 and high order lags, which are too high to include in the model.

- The **Q-Q Plot** indicates that the residuals follow a near-normal distribution, with minor deviations at the tails, which is typical in time series data.

- The **P-value** plots showing lots of insignificance from lag = 0 to lag = 16. AR2, AR3 are not significant in this case.

```{r}
# ------------------- Fit SARIMA Models & Extract Key Output Dynamically -------------------

# Model 1: SARIMA(2,1,0)(0,1,1)[12]
model_output1 <- capture.output(sarima(ts_egg, 2,1,0, 0,1,1,12))

# Find the relevant line numbers dynamically based on the keyword "Coefficients"
start_line1 <- grep("Coefficients", model_output1)
end_line1 <- length(model_output1)  

# Print the relevant section automatically
cat(model_output1[start_line1:end_line1], sep = "\n")
```

## Fit the model:


- The **Residual Plot** shows nearly consistent fluctuation around zero, however, after 2020, the residuals fluctate more than the previous years, but still around 0, we could say that the residual is stationary.

- The **Autocorrelation Function (ACF)** of the residuals reveals significant autocorrelations around lag 0 and high order lags, which are too high to include in the model.

- The **Q-Q Plot** indicates that the residuals follow a near-normal distribution, with minor deviations at the tails, which is typical in time series data.

- except ar1 is not significant, all other parameters are significant.

```{r}
fit <- Arima(ts_egg, order=c(2,1,0), seasonal=list(order=c(0,1,1),period=12))
summary(fit)
```

```{r}
library(ggplot2)
library(forecast)
library(tsibble)
library(dplyr)

# Convert ts_egg to a data frame with proper dates
start_date <- as.Date("1980-01-01")  # Adjust based on actual data
date_seq <- seq(start_date, by = "month", length.out = length(ts_egg))

ts_egg_df <- data.frame(
  Date = date_seq,
  Price = as.numeric(ts_egg)
)

# Fit ARIMA model
fit <- auto.arima(ts_egg)

# Generate forecasts
mean_forecast <- forecast(meanf(ts_egg, h = 12))
naive_forecast <- forecast(naive(ts_egg, h = 12))
snaive_forecast <- forecast(snaive(ts_egg, h = 12))
drift_forecast <- forecast(rwf(ts_egg, h = 12, drift = TRUE))
arima_forecast <- forecast(fit, h = 12)

# Convert forecasts to data frame
# Extract forecasted values properly
forecast_df <- data.frame(
  Date = seq(max(ts_egg_df$Date) %m+% months(1), by = "month", length.out = 12),
  Mean = as.numeric(mean_forecast$mean),    # Extract mean forecast
  Naive = as.numeric(naive_forecast$mean),
  SNaive = as.numeric(snaive_forecast$mean),
  Drift = as.numeric(drift_forecast$mean),
  ARIMA = as.numeric(arima_forecast$mean)
)
# Plot with ggplot2
ggplot(ts_egg_df, aes(x = Date, y = Price)) +
  geom_line(color = "black", size = 1) +  # Actual Data
  geom_line(data = forecast_df, aes(x = Date, y = Mean, color = "Mean"), size = 1, linetype = "dashed") +
  geom_line(data = forecast_df, aes(x = Date, y = Naive, color = "Naïve"), size = 1, linetype = "dotted") +
  geom_line(data = forecast_df, aes(x = Date, y = SNaive, color = "Seasonal Naïve"), size = 1, linetype = "dotdash") +
  geom_line(data = forecast_df, aes(x = Date, y = Drift, color = "Drift"), size = 1, linetype = "twodash") +
  geom_line(data = forecast_df, aes(x = Date, y = ARIMA, color = "ARIMA Fit"), size = 1) +
  ggtitle("Egg Price Forecast") +
  xlab("Year") + ylab("Egg Price") +
  scale_color_manual(values = c("Mean" = "blue", "Naïve" = "red", 
                                "Seasonal Naïve" = "green", "Drift" = "orange", 
                                "ARIMA Fit" = "purple")) +
  guides(colour = guide_legend(title = "Forecast Methods")) +
  theme_minimal()
```
```{r}
accuracy(snaive(ts_egg, h=12))
accuracy(fit) # Fit is better
```
```{r}
fit %>% forecast(h=36) %>% autoplot() #next 3 years

sarima.for(ts_egg, 36, 2,1,0,0,1,1,12)
```


