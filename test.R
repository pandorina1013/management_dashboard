library(flexdashboard)
library(ggplot2)
library(data.table)
library(formattable)
library(dplyr)
library(plotly)

output.dt <- fread("data.csv", encoding = "UTF-8")
outcome.dt <- output.dt %>% filter(amount < 0) %>% select(ds,amount)
outcome.dt <- outcome.dt[as.character(substr(Sys.Date(),1,4)) == as.character(substr(outcome.dt$ds,1,4)),]
outcome.dt$ds <- substr(outcome.dt$ds,6,7)
outcome.dt <- outcome.dt %>% group_by(ds) %>% summarise(total = sum(amount))

income.dt <- output.dt %>% filter(amount >= 0) %>% select(ds,amount)
income.dt <- income.dt[as.character(substr(Sys.Date(),1,4)) == as.character(substr(income.dt$ds,1,4)),]
income.dt$ds <- substr(income.dt$ds,6,7)
income.dt <- income.dt %>% group_by(ds) %>% summarise(total = sum(amount))

i_o.dt <- data.table(income = income.dt, outcome = -outcome.dt$total, diff = income.dt$total+outcome.dt$total)
colnames(i_o.dt) <- c("ds","income","outcome","diff")