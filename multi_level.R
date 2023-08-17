library(tidyverse)
library(psych)
library(lavaan)
library(lubridate)

##[dealing with data]##
#우선 데이터명 ema만 사용. data는 csv파일에서 변수명 설명을 지워 정리한 파일
ema <- read.csv("EMA data_2023-1 bk _time_event.csv")
# data <- read.csv("EMA data_bk_TE.csv")
ema$time %>% strptime(,format = "%Y-%m-%d %H:%M:%S") -> ema$time #시간변수로
# data$time %>% strptime(,format = "%Y-%m-%d %H:%M") -> data$time
table(ema$STR_jump)
summary(ema)

ema %>% mutate(days = day(ema$time), week=week(ema$time)) -> ema_d #날짜만 열로 산출
#오전오후까지 나눠서 열로 추가 DinY = 1년 단위 일수(필요없으므로 나중에 지울 것)
# data %>% mutate(days = day(data$time), DinY = yday(time), AMPM = ifelse(am(time)==T, 1, 0)) -> dat_
                
##하루 스트레스 빈도
data %>% group_by(ID, DinY) %>% mutate(rsp=summarise(count=n())) -> da_
data %>% group_by(ID, DinY) %>% summarise(Mean=mean(M_DEP)) -> da_2
data %>% group_by(ID, DinY) %>% summarise(count=n(), rate=n()/5,
                    MD=mean(M_DEP), MNA=mean(M_NA)) -> smmr_D
ema_d %>% group_by(ID) %>% summarise(count=n(), rate=n()/35,
                                          MD=mean(M_DEP), MNA=mean(M_NA)) -> dd




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




