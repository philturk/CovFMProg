## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_10_31
## note: Isolate the forecast to look only at the Winter 2020 surge

## Requires JAGS to run
# library(devtools)
# install_github("lilywang1988/eSIR")

library(tidyverse)
library(eSIR)
library(lubridate) 

source("trend_check.R")
source("My_Theme.R")

temp01 <- read_csv("2021_02_13_CRI.csv")
temp01$Date <- as.Date(temp01$Date, "%m/%d/%y")
N <- 2544041 

temp01_Sept24 <- temp01 %>% filter(Date >= "2020-09-24") %>%
  mutate(Time = Time - 197)

library(EnvCpt)
fit_envcpt <- envcpt(temp01_Sept24$Prevalence)
plot(fit_envcpt$trendcpt, ylab = "Infection Prevalence", xlab = "Date", xaxt = "n")
new_x <- seq(1, length(fit_envcpt$trendcpt@data.set[,3]), 14)
new_x_labs <- as.Date("2020-09-23") + days(new_x)
axis(1, new_x, format(new_x_labs, "%b %d"), cex.axis = 0.9)
cpts <- fit_envcpt$trendcpt@cpts
abline(v = cpts[-length(cpts)])
temp01_Sept24[cpts[-length(cpts)], "Date"]
fit_envcpt$trendcpt@param.est
## slope = -2885.74286; (Jan 16, Jan 21]
## slope = -5264.21146; (Jan 21, Feb 13]
100*(-5264.21146 - (-2885.74286))/(abs(-2885.74286)) ## Relative change -82%
detach(package:EnvCpt)
detach(package:MASS)

## Begin eSIR model fit

R <- temp01_Sept24$Removed/N
Y <- temp01_Sept24$Prevalence/N
death_prop <- tibble(Date = temp01_Sept24$Date, death_prop = (temp01_Sept24$CumDeaths/N)/R)

## Can tune model here if desired
change_time01 <- NULL ## add dates
pi01 <- NULL ## add transmission rate modifier values

ymd("2021-03-30") - ymd("2020-09-24") + 1 ## 188 days
ymd("2021-03-30") - ymd("2021-02-13") ## 45 days

## Data end on 02/13/2021; 45-day forecast
set.seed(20202021)
res.step02 <- tvt.eSIR(Y, R, begin_str = "09/24/2020", death_in_R = 0.001921730, T_fin = 188, 
                       nadapt = 10000, beta0 = 0.194, gamma0 = 0.0821, 
                       add_death = TRUE, R0_sd = 0.15, 
                       pi0 = pi01, change_time = change_time01, dic = T, 
                       casename = "Winter2020_Forecast_Extra/CRI01",
                       save_files = T, save_mcmc = T, save_plot_data = T, 
                       M = 5e3, nburnin = 2e3) 
## Increase M and nburnin in practice

res.step02