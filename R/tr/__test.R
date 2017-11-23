

# remove(list = ls())
source("tr_functions.R")

library(dplyr)

df <- data.frame(date = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), "day"))

df %>% mutate(month = months(date), weekday = weekdays(date)) %>% group_by(month) %>% filter(!weekday %in% c("Saturday", "Sunday")) %>% summarise(last_weekday = weekdays(max(date)))


df %>% mutate(mm = format(date, format = "%m"), dd = format(date, format = "%d"), yyyy = format(date, format = "%Y"), wk = format(date, format = "%U"), 
    weekday = format(date, format = "%A")) %>% mutate(wk_mm = floor(as.integer(wk)/as.integer(mm))) %>% group_by(yyyy, mm, weekday, dd) %>% 
    filter(weekday == "Thursday")

