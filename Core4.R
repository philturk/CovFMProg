## author: Philip Turk (philip.turk@gmail.com)
## date: 2021_07_08

library(tidyverse)

source("ma_3pt.R")
source("ma_5pt.R")
source("My_Theme.R")

temp01 <- read_csv("2021_02_13_CRI.csv")
temp01$Date <- as.Date(temp01$Date, "%m/%d/%y")

temp01_ma <- temp01 %>% transmute(Date, ## Keep Date to be safe
                                  Prev_ma = round(ma_3pt(Prevalence), 0),
                                  CumDeaths_ma = round(ma_3pt(CumDeaths), 0),
                                  Incidence_ma = round(ma_5pt(Incidence), 0),
                                  Deaths_t_ma = round(ma_5pt(Deaths_t), 0))

temp01_Long_a <- temp01 %>% 
  pivot_longer(c(Prevalence, CumDeaths, Incidence, Deaths_t), 
               names_to = "Status", values_to = "Count") %>%
  transmute(Date, Time, 
            Status = factor(Status, 
                            levels = c("Prevalence",
                                       "CumDeaths",
                                       "Incidence",
                                       "Deaths_t"),
                            labels = c("Prevalence\n(est latent)",
                                       "Deaths\n(cumulative)",
                                       "Incidence\n(est latent)",
                                       "Deaths\n(daily)")), Count)

temp01_Long_b <- temp01_ma %>%
  pivot_longer(c(Prev_ma, CumDeaths_ma, Incidence_ma, Deaths_t_ma),
               names_to = "Status_ma", values_to = "Count_ma")

temp01_Long <- temp01_Long_a %>% mutate(Count_ma = temp01_Long_b$Count_ma) %>%
  mutate(Count = ifelse(Time <= 13 & Status %in% c("Deaths\n(cumulative)", "Deaths\n(daily)"), NA, Count), 
         Count_ma = ifelse(Time <= 13 & Status %in% c("Deaths\n(cumulative)", "Deaths\n(daily)"), NA, Count_ma))

p01 <- ggplot(temp01_Long, aes(x = Date, y = Count, colour = "red"))
p01 <- p01 + facet_grid(Status~., scales = "free_y")
p01 <- p01 + geom_point() 
p01 <- p01 + geom_line()
p01 <- p01 + geom_line(aes(x = Date, y = Count_ma), colour = "black")
# p01 <- p01 + labs(x = "Date", y = "Persons")
p01 <- p01 + labs(x = "Date", y = "Persons", title = "COVID-19 Pandemic Data,
CRI Region as of February 13, 2021",
                  subtitle = "Black lines are moving averages (3-pt for top two graphs; 5-pt for bottom two graphs)",
caption = "Source: NCDHHS at https://www.ncdhhs.gov/covid-19-case-count-nc")
p01 <- p01 + scale_x_date(date_breaks = "1 month", minor_breaks = NULL,  
                          date_labels = "%b %y",
                          limits = as.Date(c(NA, "2021-02-20")))
p01 <- p01 + scale_y_continuous(labels = scales::comma)
p01 <- p01 + theme_minimal() 
p01 <- p01 + theme(legend.position = "none")
p01 <- p01 + theme(panel.background = element_rect(fill = NA, color = "black"))
p01 <- p01 + theme(strip.text.y = element_text(size = 14))
p01 + My_Theme
