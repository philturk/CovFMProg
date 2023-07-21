## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_07_08

## Requires Stan software to run
library(tidyverse)
library(EpiNow2)
source("My_Theme.R")

reporting_delay <- list(mean = convert_to_logmean(2, 0.5),
                        mean_sd = 0.1,
                        sd = convert_to_logsd(2, 0.5),
                        sd_sd = 0.1,
                        max = 7)

generation_time <- get_generation_time(disease = "SARS-CoV-2", source = "ganyani")

incubation_period <- get_incubation_period(disease = "SARS-CoV-2", source = "lauer")

temp01 <- read_csv("2021_02_13_CRI.csv")
temp01$Date <- as.Date(temp01$Date, "%m/%d/%y")

reported_cases <- temp01 %>% select(date = Date, confirm = Incidence)

estimates <- epinow(reported_cases = reported_cases, 
                    generation_time = generation_time,
                    delays = delay_opts(incubation_period, reporting_delay),
                    rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
                    stan = stan_opts(cores = 4),
                    CrIs = c(0.8))
summary(estimates)
tail(summary(estimates, type = "parameters", params = "R"), n = 8)
R_that <- summary(estimates, type = "parameters", params = "R")$median
tail(summary(estimates, output = "estimated_reported_cases"), n = 8)

p01 <- plot(estimates, type = "R")
p01 <- p01 + geom_line(aes(y = R_that), colour = "red")
p01 <- p01 + scale_x_date(date_breaks = "2 week",  
                          date_labels = "%b %d") 
# p01 <- p01 + labs(y = "Effective
# Reproduction Number")
p01 <- p01 + labs(title = "COVID-19 Time-Varying Reproduction Number, 7-Day Forecast
CRI Region as of February 13, 2021", subtitle = "Feb 20 forecast (80% CrI) for R_t (with associated latent daily infection incidence):
nominal: 0.84 (3,996); worst case: 1.00 (7,233); best case: 0.71 (2,135)",
y = "Effective
Reproduction Number",
caption = "Lognormal prior on reporting delay (mean 2 days, SD 0.5 days); Gamma prior on generation time (mean 3.6 days, SD 3.1 days);
Lognormal prior on incubation period (mean 5.5 days, SD 2.4 days)")
p01 + My_Theme

# 30-Day Zoom
# p01a <- plot(estimates, type = "R")
# p01a <- p01a + geom_line(aes(y = R_that), colour = "red")
# p01a <- p01a + scale_x_date(date_breaks = "1 week",  
#                             date_labels = "%b %d",
#                             limits = as.Date(c("2021-01-22", "2021-02-20")))
# p01a <- p01a + ylim(0.5, 1.5)
# p01a <- p01a + labs(title = "COVID-19 Time-Varying Reproduction Number, 7-Day Forecast
# CRI Region as of February 13, 2021", subtitle = "Feb 20 forecast (80% CrI) for R_t (with associated daily infection incidence): 
# nominal: 0.84 (3,996); worst case: 1.00 (7,233); best case: 0.71 (2,135)",
# y = "Effective
# Reproduction Number",
# caption = "Lognormal prior on reporting delay (mean 2 days, SD 0.5 days); Gamma prior on serial interval (mean 3.6 days, SD 3.1 days);
# Lognormal prior on incubation period (mean 5.5 days, SD 2.4 days)")
# p01a + My_Theme
