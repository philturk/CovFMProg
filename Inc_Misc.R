## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_08_20

library(tidyverse)
library(incidence)
library(lubridate)
source("My_Theme.R")

temp01 <- read_csv("2021_02_13_CRI.csv")
temp01$Date <- as.Date(temp01$Date, "%m/%d/%y")

## Infection incidence

CRI_incidence_function_data <- temp01 %>% select(Date, Incidence) %>% uncount(Incidence)

CRI_incidence_object <- incidence(CRI_incidence_function_data$Date)
peak_data <- estimate_peak(CRI_incidence_object) ## See help page for details
peak_data ## "2021-01-09"

library(EnvCpt)
fit_envcpt <- envcpt(temp01$Incidence)
plot(fit_envcpt$trendcpt, ylab = "Infection Incidence", xlab = "Date", xaxt = "n")
new_x <- seq(1, length(fit_envcpt$trendcpt@data.set[,3]), 14)
new_x_labs <- as.Date("2020-03-10") + days(new_x)
axis(1, new_x, format(new_x_labs, "%b %d"), cex.axis = 0.9)
cpts <- fit_envcpt$trendcpt@cpts
abline(v = cpts[-length(cpts)])
temp01[cpts[-length(cpts)], "Date"]
fit_envcpt$trendcpt@param.est
## slope = -32.151341; (Dec 1, Dec 29]
## slope = -240.730928; (Dec 29, Feb 13]
## Dec 30 is changepoint
detach(package:EnvCpt)
detach(package:MASS)

## Most recent local fit
fit_01 <- fit(subset(CRI_incidence_object, from = "2020-12-30", to = "2021-02-13"))
fit_01
# summary(fit_01$model) ## R^2 = 0.5476
## Halving time in days
## 32.35701; (25.46541, 44.36271)
## Daily growth rate
## -0.021; (-0.027, -0.016)
## Or do it this way
-log(2)/fit_01$info$r
-log(2)/fit_01$info$r.conf

CRI_incidence_object.7 <- incidence(CRI_incidence_function_data$Date, interval = "Sunday week")
CRI_df <- data.frame(counts = CRI_incidence_object.7$counts, dates = CRI_incidence_object.7$dates)

p05b <- ggplot(CRI_df, aes(x = dates, y = counts))
p05b <- p05b + geom_bar(stat = "identity", fill = "blue", alpha = 0.7, width = 6.75,
                        position = position_nudge(x = 3.5))
p05b <- p05b + stat_smooth(aes(x = dates + 3.5, y = counts), method = "loess", formula = y ~ x, size = 0.5, se = FALSE, color = "red", span = 0.3)
p05b <- p05b + scale_y_continuous(labels = scales::comma, limits = c(0, NA))
p05b <- p05b + scale_x_date(date_breaks = "1 month", minor_breaks = NULL,  
                            date_labels = "%b %y",
                            limits = as.Date(c(NA, "2021-02-20")))
# p05b <- p05b + labs(y = "Persons", x = "Date")
p05b <- p05b + labs(title = "COVID-19 Observed Weekly (Sun-Sat) Infection Incidence,
CRI Region as of February 13, 2021",
subtitle = "LOESS Non-Parametric Regression Line (red)", y = "Persons", x = "Date",
caption = "Estimated Daily Incidence Peak: January 9, 2021; Most Recent Local Neighborhood [December 30, 2020 - February 13, 2021]
Estimated Local Halving Time for Daily Incidence (days): 32 (95% CI: [25, 44])")
p05b + My_Theme

## Infection prevalence

library(EnvCpt)
fit_envcpt <- envcpt(log10(temp01$Prevalence))
plot(fit_envcpt$trendcpt, ylab = "Log_10 Infection Prevalence", xlab = "Date", xaxt = "n")
new_x <- seq(1, length(fit_envcpt$trendcpt@data.set[,3]), 14)
new_x_labs <- as.Date("2020-03-10") + days(new_x)
axis(1, new_x, format(new_x_labs, "%b %d"), cex.axis = 0.9)
cpts <- fit_envcpt$trendcpt@cpts
abline(v = cpts[-length(cpts)])
temp01[cpts[-length(cpts)], "Date"]
fit_envcpt$trendcpt@param.est
## slope = -0.0020905488; (Jan 17, Jan 25]
## slope = -0.0100627181; (Jan 25, Feb 13]
## Jan 25 is changepoint

## 95% CI on halving time
temp_data <- temp01 %>% filter(Date >= "2021-01-26") %>%
  mutate(Time = Time - 321, Prevalence = log10(Prevalence), .keep = "used")
local_mod <- lm(Prevalence ~ Time, data = temp_data)
r <- coefficients(summary(local_mod))["Time", "Estimate"]
se <- coefficients(summary(local_mod))["Time", "Std. Error"]
-log10(2)/(r + c(-1, 1)*1.96*se)

detach(package:EnvCpt)
detach(package:MASS)

## Calculations for arrow in plot
-log10(2)/(-0.0100627181) ## 29.91538, using r above
ymd("2021-03-15") - ymd("2021-02-13") ## 30 days
10^(log10(177389) + (-0.0100627181)*29.915380) ## ~88,694

## Doubling time reference line (30 days)
ymd("2021-03-15") - ymd("2020-03-11") ## 369 days
10^(log10(7) + (log10(2)/30)*369) ## ~ 35,299

## Generate plot
p07 <- ggplot(temp01, aes(x = Date, y = Prevalence))
p07 <- p07 + geom_point()
p07 <- p07 + geom_line()
p07 <- p07 + geom_vline(xintercept = as.Date("2021-1-25"), linetype = "dotted", 
                        color = "blue", size = 1.5)
p07 <- p07 + geom_segment(aes(x = as.Date("2021-02-13"), y = 177389, xend = as.Date("2021-03-15"), yend = 88694), color = "red", arrow = arrow(length = unit(0.5, "cm")))
p07 <- p07 + geom_segment(aes(x = as.Date("2020-03-11"), y = 7, xend = as.Date("2021-03-15"), yend = 35299.37), color = "green", linetype = "dotted", size = 1.0)
p07 <- p07 + scale_y_log10(limits = c(7, NA), labels = scales::comma)
p07 <- p07 + scale_x_date(date_breaks = "1 month", minor_breaks = NULL,  
                          date_labels = "%b %y",  
                          limits = as.Date(c("2020-03-11", "2021-03-22"))) ## Set upper limit by eyeball 
# p07 <- p07 + labs(y = "Persons (Log Scale)")
p07 <- p07 + labs(y = "Persons (Log Scale)",
                  title = "Covid-19 Infection Prevalence,
CRI Region as of February 13, 2021",
                  subtitle = "Blue line denotes most recent changepoint in local linear trend (January 25, 2021),
Green dotted reference line denotes 30-day doubling time",
caption = "Horizontal extent of red arrow is estimated halving time of 30 days (95% CI: [28, 32])
Vertical extent of the red arrow is half the current daily infection prevalence (from 177,389 to 88,694)")
p07 + My_Theme
