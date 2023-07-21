## author: Philip Turk (philip.turk@gmail.com)
## date: 2023_02_24

library(tidyverse)
library(forecast)
library(lubridate)
source("My_Theme.R")
set.seed(20210708)

temp01 <- read_csv("2021_02_13_CRI.csv")
temp01$Date <- as.Date(temp01$Date, "%m/%d/%y")
temp01 <- temp01 %>% filter(Date >= "2020-04-06")

y <- ts(temp01$CumDeaths, start = c(week("2020-04-06"), wday("2020-04-06")), frequency = 7) 

## SARIMA
fit01 <- auto.arima(y) 
autoplot(fit01)

checkresiduals(fit01)
rstand <- fit01$residuals/fit01$sigma2^.5
plot(rstand)
abline(h = 0)
hist(rstand)
shapiro.test(rstand)

my_arima <- fit01 %>% forecast(h = 7, bootstrap = TRUE, level = c(80))
all(diff(my_arima$lower) >= 0)
all(diff(my_arima$upper) >= 0)
print(last(y) <= my_arima$lower[7]) 

## Exponential Smoothing State Space Model
fit02 <- ets(y)
autoplot(fit02)

checkresiduals(fit02)
rstand2 <- fit02$residuals/fit02$sigma2^.5
plot(rstand2)
abline(h = 0)
hist(rstand2)
shapiro.test(rstand2)

my_ets <- fit02 %>% forecast(h = 7, bootstrap = TRUE, level = c(80))
all(diff(my_ets$lower) >= 0)
all(diff(my_ets$upper) >= 0)
print(last(y) <= my_ets$lower[7])

## Seasonal and Trend decomposition using Loess
fit03 <- stl(y, s.window = "periodic")
fit03_v2 <- stlf(y, s.window = "periodic") ## Need for residuals
autoplot(fit03)

checkresiduals(fit03_v2)

my_stl <- fit03 %>% forecast(h = 7, level = c(80))
all(diff(my_stl$lower) >= 0)
all(diff(my_stl$upper) >= 0)
print(last(y) <= my_stl$lower[7])

## NNAR
fit04 <- nnetar(y)

checkresiduals(fit04)

my_nnar <- fit04 %>% forecast(h = 7, bootstrap = TRUE, PI = TRUE, level = c(80))
all(diff(my_nnar$lower) >= 0)
all(diff(my_nnar$upper) >= 0)
print(last(y) <= my_nnar$lower[7])

## TBATS 
fit05 <- tbats(y) 
plot(fit05)

checkresiduals(fit05)
rstand5 <- fit05$errors/sqrt(fit05$variance)
plot(rstand5)
abline(h = 0)
hist(rstand5)
shapiro.test(rstand5)

my_tbats <- fit05 %>% forecast(h = 7, level = c(80))
all(diff(my_tbats$lower) >= 0)
all(diff(my_tbats$upper) >= 0)
print(last(y) <= my_tbats$lower[7])

## Forecast and PI for Ensemble
temp_low <- cbind(my_arima$lower, my_ets$lower, my_nnar$lower, my_stl$lower, my_tbats$lower) 
PIlo <- apply(temp_low, 1, min)
PIlo <- ifelse(PIlo < last(y), last(y), PIlo)
temp_up <- cbind(my_arima$upper, my_ets$upper, my_nnar$upper, my_stl$upper, my_tbats$upper) 
PIup <- apply(temp_up, 1, max)
my_ensemble <- (my_ets[["mean"]] + my_arima[["mean"]] + my_stl[["mean"]] + 
                my_nnar[["mean"]] + my_tbats[["mean"]])/5
                    
poly01 <- tibble(Date = c(last(temp01$Date) + 1:7, rev(last(temp01$Date) + 1:7)), 
                 Bounds = c(PIup, rev(PIlo)))
p01_graph <- tibble(Date = c(temp01$Date, last(temp01$Date) + 1:7),
                    Deaths = c(y, rep(NA, 7)),
                    Fit = c(rep(NA, length(y)), my_ensemble))

p01 <- ggplot(p01_graph, aes(x = Date))
p01 <- p01 + geom_polygon(data = poly01, aes(x = Date, y = Bounds), fill = "#F8766D", alpha = 0.5)
p01 <- p01 + geom_line(aes(y = Deaths), colour = "black", linetype = "dashed")
p01 <- p01 + geom_line(aes(y = Fit), colour = "red")
p01 <- p01 + geom_point(aes(y = Deaths), size = 0.75)
p01 <- p01 + scale_y_continuous(labels = scales::comma)
# p01 <- p01 + labs(y = "Cumulative Deaths")
p01 <- p01 + labs(y = "Cumulative Deaths",
                  title = "COVID-19 Cumulative Deaths Time Series, Ensemble of Models 7-Day Forecast
CRI Region as of February 13, 2021",
                  subtitle = "Observed time series (black), approximate 80% forecast interval for out-of-sample forecasts (pink)
Predicted deaths for the upcoming week: 116 with 80% forecast interval of (54, 167)",
caption = "Models: 1.) SARIMA; 2.) exponential smoothing state space model; 3.) seasonal-trend decomposition loess; 4.) feed-forward neural network SAR;
5.) exponential smoothing state space model (Box-Cox transformation, ARMA errors, trend, and trigonometric seasonal components)

MAPE [Feb 7 - Feb 13]: 0.64%")
p01 <- p01 + scale_x_date(date_breaks = "1 month", minor_breaks = NULL,  
                          date_labels = "%b %y",
                          limits = as.Date(c(NA, "2021-02-20")))
p01 + My_Theme

## Performance check; 7-day-ahead out-of-sample MAPE
Obs <- c(2597, 2607, 2635, 2656, 2684, 2697, 2705) ## Previous 7 days' observations
Fcst <- c(2607.337, 2626.342, 2650.282, 2673.428, 2694.352, 2716.156, 2732.103)
cbind(Obs, Fcst) ## Previous 7 days' ensemble forecasts from last model (2021/02/06)
100*mean(abs((Obs - Fcst)/Obs)) ## 0.64%

ceiling(my_ensemble[7] - last(temp01$CumDeaths))
ceiling(PIlo[7] - last(temp01$CumDeaths))
ceiling(PIup[7] - last(temp01$CumDeaths))
