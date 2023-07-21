## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_07_28

## Requires JAGS to run

library(tidyverse)
library(eSIR)
library(lubridate) 

source("trend_check.R")
source("My_Theme.R")

temp01 <- read_csv("2021_02_13_CRI.csv")
temp01$Date <- as.Date(temp01$Date, "%m/%d/%y")
N <- 2544041 

temp01_Mar25 <- temp01 %>% filter(Date >= "2020-03-25") %>%
  mutate(Time = Time - 14)

library(EnvCpt)
fit_envcpt <- envcpt(temp01_Mar25$Prevalence)
plot(fit_envcpt$trendcpt, ylab = "Infection Prevalence", xlab = "Date", xaxt = "n")
new_x <- seq(1, length(fit_envcpt$trendcpt@data.set[,3]), 14)
new_x_labs <- as.Date("2020-03-24") + days(new_x)
axis(1, new_x, format(new_x_labs, "%b %d"), cex.axis = 0.9)
cpts <- fit_envcpt$trendcpt@cpts
abline(v = cpts[-length(cpts)])
temp01_Mar25[cpts[-length(cpts)], "Date"]
fit_envcpt$trendcpt@param.est
## slope = -2885.74286; (Jan 16, Jan 21]
## slope = -5264.21146; (Jan 21, Feb 13]
100*(-5264.21146 - (-2885.74286))/(abs(-2885.74286)) ## Relative change -82%
detach(package:EnvCpt)
detach(package:MASS)

## Begin eSIR model fit

R <- temp01_Mar25$Removed/N
Y <- temp01_Mar25$Prevalence/N
death_prop <- tibble(Date = temp01_Mar25$Date, death_prop = (temp01_Mar25$CumDeaths/N)/R)

change_time01 <- c("03/25/2020", "09/23/2020", "12/01/2020", "01/21/2021")
pi01 <- c(1.0, 0.50, 0.70, 1.00, 0.90) ## Estimated from observed data 
                                       ## using other methods (not shown) 

ymd("2021-03-30") - ymd("2020-03-25") + 1 ## 371 days
ymd("2021-03-30") - ymd("2021-02-13") ## 45 days

## Data end on 02/13/2021; 45-day forecast
set.seed(20202021)
res.step02 <- tvt.eSIR(Y, R, begin_str = "03/25/2020", death_in_R = 0.001921730, T_fin = 371, 
                       nadapt = 10000, beta0 = 0.7020037, gamma0 = 0.2979969, 
                       add_death = TRUE, R0_sd = 0.15, 
                       pi0 = pi01, change_time = change_time01, dic = T, 
                       casename = "Forecast_Extra/CRI01",
                       save_files = T, save_mcmc = T, save_plot_data = T, 
                       M = 5e3, nburnin = 2e3) 
## Increase M and nburnin in practice 

load("Forecast_Extra/CRI01_plot_data.Rdata")

## Graph

p09_data_master_01 <- plot_data_ls$infection_plot_ls$data_comp
p09_data_master_01 <- p09_data_master_01 %>% select(-mean) %>%
  mutate(median = round(N*median, 0), 
         lower = round(N*lower, 0),
         upper = round(N*upper, 0),
         Date = ymd("2020-03-24") + days(time))
p09_data_master_01 <- full_join(p09_data_master_01, plot_data_ls$infection_plot_ls$data_pre) %>%
  mutate(Obs_Prev = N*Y)

p09_data_master_02 <- plot_data_ls$infection_plot_ls$data_poly 
p09_data_master_02 <- p09_data_master_02 %>% 
  mutate(Bounds = round(N*y, 0),
         Date = ymd("2020-03-24") + days(x))

p09 <- ggplot(p09_data_master_01, aes(x = Date)) 
p09 <- p09 + geom_line(aes(y = median), colour = "red")
p09 <- p09 + geom_polygon(data = p09_data_master_02, 
                          aes(x = Date, y = Bounds, fill = value, group = phase), alpha = 0.5)
p09 <- p09 + geom_point(aes(y = Obs_Prev), size = 0.5)
p09 <- p09 + geom_vline(xintercept = as.Date("2021-01-21"), linetype = "dashed",
                        color = "green", size = 0.5)
p09 <- p09 + scale_y_continuous(labels = scales::comma, limits = c(0, 350000))
p09 <- p09 + scale_x_date(date_breaks = "1 month", minor_breaks = NULL,  
                          date_labels = "%b %y",
                          limits = as.Date(c(NA, "2021-03-30")))
# p09 <- p09 + labs(y = "Persons")
p09 <- p09 + labs(y = "Persons",
                  title = "COVID-19 Infection Prevalence Prediction Curve, 45-Day Forecast
CRI Region as of February 13, 2021",
subtitle = "95% credible interval for predicted prevalence (pink),
95% credible interval for unknown prevalence (blue),
Green line denotes most recent changepoint for local linear trend",
caption = "MAPE [Feb 7 - Feb 13]: 9.19%; (2021/01/16, 2021/01/21] slope = -2,886; (2021/01/21, 2021/02/13] slope = -5,264
Forecasted infection prevalence for March 30, 2021: 103,820 (95% CrI: [82,152, 127,872])")
p09 <- p09 + theme_bw()
p09 <- p09 + theme(legend.position = "none")
p09 + My_Theme

## Performance check; 7-day-ahead out-of-sample MAPE and trend check
Obs <- c(220085, 206910, 201940, 194214, 186471, 181515, 177389) ## Previous 7 days' observations
Fcst <- c(219094, 216884, 214753, 212551, 210285, 207971, 205696) ## Previous 7 days' forecasts from last model (2021/02/06)
cbind(Obs, Fcst) 
100*mean(abs((Obs - Fcst)/Obs)) ## 9.19%
trend_check(Obs, Fcst) ## Favorable trend