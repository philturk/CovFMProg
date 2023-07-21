## author: Philip Turk (philip.turk@gmail.com)
## date: 2023_04_03

library(tidyverse)
library(forecast)
source("My_Theme.R")

temp01 <- read_csv("Feb13_Hospital_Stats.csv")
temp01$date <- as.Date(temp01$date, "%m/%d/%y")
temp01 <- temp01 %>% mutate(Date = date, Hosp = all_bed + vacute_census)

temp01 <- temp01 %>% filter(Date >= "2020-03-26") ## March 26 was the first date for virtual hospital encounters.
temp01_red <- temp01 %>% filter(date <= "2021-02-06")

## Save for later
Sevenday_Obs <- tail(temp01$Hosp, n = 7)

library(EnvCpt)
fit_envcpt <- envcpt(temp01$Hosp)
fit_envcpt$trendcpt@cpts 	## 2020-12-13, 2021-01-14	
fit_envcpt$trendcpt@param.est
# 100*(-13.424249 - (9.251833))/(abs(9.251833)) ## Relative change of -245%

## Acknowledge interval constraint
a <- 0
b <- 1250
fit <- auto.arima(log((temp01_red$Hosp - a)/(b - temp01_red$Hosp)), seasonal = FALSE, stepwise = FALSE, 
           approximation = FALSE, trace = TRUE)

checkresiduals(fit)
rstand <- fit$residuals/fit$sigma2^.5 
plot(rstand)
abline(h = 0)
shapiro.test(rstand)
autoplot(fit)

fc <- forecast(fit, h = 7, level = c(95)) ## Bootstrap option exists

back_tran <- function(y){1250*exp(y)/(1 + exp(y))}
fc[["mean"]] <- back_tran(fc[["mean"]]) # (b*exp(fc[["mean"]]) + a)/(1 + exp(fc[["mean"]])) 
fc[["lower"]] <- back_tran(fc[["lower"]])
fc[["upper"]] <- back_tran(fc[["upper"]])

## Hospital Census Plot

upper <- fitted(fit) + 2*sqrt(fit$sigma2)
lower <- fitted(fit) - 2*sqrt(fit$sigma2)
upper <- back_tran(upper)
lower <- back_tran(lower)
poly01 <- tibble(Date = c(temp01_red$Date, rev(temp01_red$Date)), 
                 Bounds = c(upper, rev(lower)))
poly02 <- tibble(Date = c(last(temp01_red$Date) + 1:7, rev(last(temp01_red$Date) + 1:7)), 
                 Bounds = c(fc[["upper"]], rev(fc[["lower"]])))
p01_graph <- tibble(Date = c(temp01_red$Date, last(temp01_red$Date) + 1:7),
                    Hosp = c(temp01_red$Hosp, rep(NA, 7)),
                    Fit = c(back_tran(fitted(fit)), fc[["mean"]]),
                    SDO = c(rep(NA, nrow(temp01_red)), Sevenday_Obs))

p01 <- ggplot(p01_graph, aes(x = Date))
p01 <- p01 + geom_polygon(data = poly01, aes(x = Date, y = Bounds), fill = "#00BFC4", alpha = 0.5)
p01 <- p01 + geom_polygon(data = poly02, aes(x = Date, y = Bounds), fill = "#F8766D", alpha = 0.5)
p01 <- p01 + geom_line(aes(y = Fit), color = "red")
p01 <- p01 + geom_point(aes(y = Hosp), size = 0.5)
p01 <- p01 + geom_point(aes(y = SDO), size = 0.5)
p01 <- p01 + geom_hline(yintercept = 1250, linetype = "dotted", color = "blue", size = 1.0)
p01 <- p01 + geom_vline(xintercept = as.Date("2021-01-14"), linetype = "dashed",
                        color = "green", size = 0.5)
p01 <- p01 + scale_y_continuous(labels = scales::comma, limits = c(0, 1250))
p01 <- p01 + scale_x_date(date_breaks = "1 month", minor_breaks = NULL,  
                          date_labels = "%b %y",
                          limits = as.Date(c(NA, "2021-02-20")))
# p01 <- p01 + labs(y = "Persons")
p01 <- p01 + labs(y = "Persons", 
                    title = "COVID-19 Hospital Census Time Series Model with Constraint, 7-Day Look-Back
CRI Region as of February 13, 2021",
                    subtitle = "95% prediction interval for in-sample one-step-ahead forecasts (teal), fitted values (red),
95% forecast interval for out-of-sample forecasts (pink), 1250 carrying capacity (blue line)
Green line denotes most recent changepoint for local linear trend",
caption = "MAPE [Feb 7 - Feb 13]: 6.79%; (2020/12/13, 2021/01/14] slope = +9.3; (2021/01/14, 2021/02/13] slope = -13.4")
p01 <- p01 + theme_bw()
p01 + My_Theme

## Performance check; 7-day-ahead out-of-sample MAPE and trend check
Fcst <- tail(p01_graph$Fit, n = 7) ## 7 days' forecasts from model (2021/02/06)
cbind(Sevenday_Obs, Fcst) 
100*mean(abs((Sevenday_Obs - Fcst)/Sevenday_Obs)) ## 6.79%
trend_check(Sevenday_Obs, Fcst) ## Pass
