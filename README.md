# DSAN 5600 Time Series Midterm Exam Review

## Code by code example:

1. Step 1:

```{r}
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

Those above are possible libraries.

2. Step 2:

```{r}
egg_data <- read.csv("eggs_price_2025.csv")
colnames(egg_data) <- c("Date", "Price")
plot_ly(egg_data, x = ~as.Date(Date)) %>%
  add_lines(y = ~Price, name = 'Egg prices (USD)', line = list(color = 'blue')) %>%
  layout(title = "Time series plot in Egg Prices",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Egg Prices"))
```

So the function above is a plotting method using Plotly. Basically, this chunk of code is an example of data cleaning, renaming, and so on, nothing special here.

3. Step 3:

```{r}
ts_egg <- ts(egg_data$Price, 
                start = decimal_date(as.Date(min(egg_data$Date))), 
                frequency = 12)  #Monthly dataa


plot(ts_egg, main = "Time Series plot")
```

This is a really important part. This kind of coding is good for all data, but it is important to figure our the frequencies. For example, there are three types of frequencies:

- Monthly, f = 12
- Daily, f = 365.25
- Quarterly, f = 4

If you do not write the frequency right, the autoplot or plot() function will look really weird.

4. Step 4: Find ACF, PACF, and conduct ADF test 

```{r}
f1 <- ggAcf(ts_egg, 50) + 
  ggtitle("ACF plot without differencing") + 
  theme_minimal()

f1
```

```{r}
tseries::adf.test(ts_egg)
```

```{r}
gglagplot(ts_egg, do.lines = FALSE) +
  ggtitle("Lag Plot of Original Egg Prices") +
  theme_minimal()
```

The code below is something to judge, whether we need to use log transformation or not

```{r}
require(gridExtra)

plot1<-autoplot(ts_egg, main="without the log transformation") 
plot2<-autoplot(log(ts_egg), main="log transformed data") 

grid.arrange(plot1, plot2,nrow=2)
```

If non-stationary, take the difference:

```{r}
diff <- diff(ts_egg)
fig_diff1 <- ggAcf(diff, 50) + 
  ggtitle("ACF For differenced egg price") + 
  theme_minimal()

fig_diff1
```

5. Step 5: Find the best p, q, and d.

**Keys**

p --> PACF
q --> ACF
d --> Differencing

```{r}
# now we have d = 1
# for q -> ACF, acf has 2 significant spikes, so maybe q = 0, 1, 2
fig_diff_P <- ggPacf(diff, 50) + 
  ggtitle("PACF for differenced data") + 
  theme_minimal()

fig_diff_P # in this case, p = 0, 1, 2
```

```{r}

# Load necessary libraries
library(knitr)
library(kableExtra)
library(forecast)

# Create an empty matrix to store results
results_matrix <- matrix(rep(NA, 6 * 20), nrow = 20)

# Initialize index for matrix row
i <- 1

# Loop through ARIMA model parameters: d = 1,2; p = 0:4; q = 1,2
for (d in 1:2) {
  for (p in 0:2) {
    for (q in 0:2) {
      
      # Ensure 'ts_indeed' is a univariate time series by selecting the correct column
      model <- Arima(ts_egg, order = c(p, d, q), include.drift = TRUE)
      
      # Store model parameters and AIC/BIC/AICc values in matrix
      results_matrix[i, ] <- c(p, d, q, model$aic, model$bic, model$aicc)
      
      # Increment row index
      i <- i + 1
    }
  }
}

# Convert matrix to data frame
results_df <- as.data.frame(results_matrix)
colnames(results_df) <- c("p", "d", "q", "AIC", "BIC", "AICc")

# Find the row with the minimum AIC
highlight_row <- which.min(results_df$AIC)

# Generate kable table with highlighting for the row with the minimum AIC
knitr::kable(results_df, align = 'c', caption = "Comparison of ARIMA Models") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(highlight_row, bold = TRUE, background = "#FFFF99")  # Highlight row in yellow
```

6. Auto ARiMA:

```{r}
auto.arima(ts_egg)

```

7. Loop again

```{r}
# Loop aagin 


# Load necessary libraries
library(knitr)
library(kableExtra)
library(forecast)

# Create an empty matrix to store results
results_matrix <- matrix(rep(NA, 6 * 20), nrow = 20)

# Initialize index for matrix row
i <- 1

# Loop through ARIMA model parameters: d = 1,2; p = 0:4; q = 1,2
for (d in 1) {
  for (p in 0:4) {
    for (q in 0:2) {
      
      # Ensure 'ts_indeed' is a univariate time series by selecting the correct column
      model <- Arima(ts_egg, order = c(p, d, q), include.drift = TRUE)
      
      # Store model parameters and AIC/BIC/AICc values in matrix
      results_matrix[i, ] <- c(p, d, q, model$aic, model$bic, model$aicc)
      
      # Increment row index
      i <- i + 1
    }
  }
}

# Convert matrix to data frame
results_df <- as.data.frame(results_matrix)
colnames(results_df) <- c("p", "d", "q", "AIC", "BIC", "AICc")

# Find the row with the minimum AIC
highlight_row <- which.min(results_df$AIC)

# Generate kable table with highlighting for the row with the minimum AIC
knitr::kable(results_df, align = 'c', caption = "Comparison of ARIMA Models") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(highlight_row, bold = TRUE, background = "#FFFF99")  # Highlight row in yellow

```



