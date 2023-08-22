library(tidyverse)
library(psych)
library(lavaan)
library(lubridate)

##[dealing with data]##


#data는 csv파일에서 변수명 설명을 지워 정리한 파일
ema <- read.csv("EMA data_2023-1 bk _time_event.csv")
data <- read.csv("EMAdata_bk_TE.csv")
ema$time %>% strptime(,format = "%Y-%m-%d %H:%M:%S") -> ema$time #시간변수로
data$time %>% strptime(,format = "%Y-%m-%d %H:%M") -> data$time
table(data$STR_jump)
summary(data)
t.test(M_DEP ~ STR_YN, data, var.equal=T) #스트레스 여부에 따른 EMA 우울 점수 차이
t.test(M_RUM ~ STR_YN, data, var.equal=T) #스트레스 여부에 따른 EMA 반추 점수 차이
t.test(M_EXV ~ STR_YN, data, var.equal=T) #스트레스 여부에 따른 EMA 경험회피 점수 차이
t.test(M_NA ~ STR_YN, data, var.equal=T) #스트레스 여부에 따른 EMA 부정정서 점수 차이
## 위는 개인차를 모두 제외한 차이로 하루 단위 스트레스 발생 이전과 이후


ema %>% mutate(days = day(ema$time), week=week(ema$time)) -> ema_d #날짜만 열로 산출
#DinY = 1년 단위 일수
data %>% mutate(days = day(data$time), DinY = yday(time), AMPM = ifelse(am(time)==T, 1, 0)) -> dat_
# write_csv(dat_,"EMA_time_var.csv", na='NA')

#하루 스트레스 빈도
dat_ %>% group_by(ID, DinY) %>% summarise(count=n()) -> da_
dat_ %>% group_by(ID) %>% summarise(count=n())
dat_ %>% group_by(ID, DinY) %>% summarise(Mean=mean(M_DEP)) -> da_2
dat_ %>% group_by(ID, DinY) %>% summarise(count=n(), rate=n()/5,
                    MD=mean(M_DEP), MNA=mean(M_NA)) -> smmr_D
ema_d %>% group_by(ID) %>% summarise(count=n(), rate=n()/35,
                                          MD=mean(M_DEP), MNA=mean(M_NA)) -> dd
#시간변수 만들기: pre-stress slope, post-stress slope, time vari
dat_



##[modeling]##


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




