library(tidyverse)
library(psych)
library(lavaan)
library(lubridate)

ema <- read.csv("EMA data_2023-1 bk _time_event.csv")
data <- read.csv("EMA data_bk_TE.csv")
ema$time %>% strptime(,format = "%Y-%m-%d %H:%M:%S") -> ema$time
data$time %>% strptime(,format = "%Y-%m-%d %H:%M") -> data$time
table(ema$STR_jump)
summary(ema)

ema %>% mutate(days = day(ema$time)) -> ema_d
data %>% mutate(days = day(data$time), DinY = yday(time), AMPM = ifelse(am(time)==T, 1, 0)) -> dat_
                
                
##하루 스트레스 빈도
data %>% filter(STR_YN==1)) %>%
  group_by(ID, DinY) %>% summarise(count=n()) -> da_

model <- 'level: 1
            STR_YN ~ CM_RUM
            level: 2
            AM_EXV ~ STR_YN
            '
fit <- sem(model = model, cluster = "ID", data = ema)
summary(fit)

model2 <- 'level: 1
            STR_YN ~ M_RUM
            M_EXV ~ STR_YN
            level: 2
            M_RUM ~ T1_DEP + T1_RUM + T1_EXV
            '
fit2 <- sem(model = model2, cluster = "ID", data = ema)
summary(fit2)




