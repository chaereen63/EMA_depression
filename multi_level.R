library(tidyverse);library(psych)
library(lavaan);library(lubridate)
library(data.table);library(ggplot2)

##[dealing with data]##
#data는 csv파일에서 변수명 설명을 지워 정리한 파일
#ema <- read.csv("EMA data_2023-1 bk _time_event.csv")
data <- read.csv("EMAdata_bk_TE.csv")
#ema$time %>% strptime(,format = "%Y-%m-%d %H:%M:%S") -> ema$time #시간변수로
data$time %>% strptime(,format = "%Y-%m-%d %H:%M") -> data$time
table(data$STR_jump)
summary(data)
#pre-stress and post-stress in day
t.test(M_DEP ~ STR_jump, data, var.equal=F) #EMA 우울
t.test(M_RUM ~ STR_jump, data, var.equal=F) #EMA 반추
t.test(M_EXV ~ STR_jump, data, var.equal=F) #EMA 경험회피
t.test(M_NA ~ STR_jump, data, var.equal=F) #EMA 부정정서
## 위는 개인차를 모두 제외한 차이


#ema %>% mutate(days = day(ema$time), week=week(ema$time)) -> ema_d #날짜만 열로 산출
#DinY = 1년 단위 일수
data %>% mutate(days = day(data$time), DinY = yday(time), AMPM = ifelse(am(time)==T, 1, 0), Hour=hour(time)) -> dat_
#write_csv(dat_,"EMA_time_var.csv", na='NA')

#하루 스트레스 빈도 및 변수 별 평균 (우울, 부정정서, 반추, 경험회피)
dat_ %>% group_by(ID, DinY) %>% summarise(count=n(), rate=n()/5, 
                    dMDEP=mean(M_DEP), dMNA=mean(M_NA), dMRUM=mean(M_RUM), dMEXV=mean(M_EXV)) -> smmr_D
dat_ %>% group_by(ID, DinY, STR_jump) %>% summarise(count=n(), rate=n()/5,
                                          djMD=mean(M_DEP), djMNA=mean(M_NA), djMRUM=mean(M_RUM), djMEXV=mean(M_EXV)) -> smmr_J
dat_ %>% group_by(ID) %>% summarise(count=n(), rate=n()/35,
                                          MD=mean(M_DEP), MNA=mean(M_NA)) -> dd #총 응답비율

# 시간변수 만들기: pre-stress slope, post-stress slope, time vari
# 개인의 사건 발생 시점을 기준으로 시간변수를 만들어야 함.
EMAtb <- data.table::fread("EMA_time_var.csv")
dat_ <- read_csv("EMA_time_var.csv")
#group: 개인, 개인 내 하루, 하루 내 시간, 하루 내 스트레스 점프
EMAtb[,.(ID, DinY, Hour, STR_jump)]
EMAtb[,]
dat_ %>% group_by(ID) %>% scale()

library(data.table)

#time scale
dat_ %>%  mutate(C_Hour = scale(Hour, center = F, scale = T)) -> fin

print(dt)

#하루단위로 볼 필요도 있을 것 같음.
# 회귀
summary(lm(M_RUM ~ DinY, data = dat_)) #다만 여기서는 개인차가 모두 제외됨.
summary(lm(M_EXV ~ DinY, data = dat_))
summary(lm(M_EXV ~ DinY*ID, data = dat_))
#개인, 하루단위 별로 보았을 때: 상호작용으로 보는 게 맞는지는 모르겠다.

##[modeling]##

#간접효과만 있는 모형
model1 <- 'level: 1
            STR_YN ~ M_RUM
            M_EXV ~ STR_YN
            level: 2
            M_RUM ~ T1_DEP + T1_RUM + T1_EXV
            '
fit1 <- sem(model = model1, cluster = "ID", data = dat_)
summary(fit1)
#model2: 간접, 직접효과 모두 있는 모형
model2 <- 'level: 1
            STR_YN ~ M_RUM
            M_EXV ~ STR_YN
            M_EXV ~ M_RUM
            level: 2
            M_RUM ~ T1_DEP + T1_RUM + T1_EXV
            '
fit2 <- sem(model = model2, cluster = "ID", data = dat_)
summary(fit2)

#model3: 이론에 따라 매개 순서 변경, 사전시점과 스트레스 유무
model3 <- 'level: 1
            STR_YN ~ M_EXV
            M_RUM ~ STR_YN
            level: 2
            M_EXV ~ T1_DEP + T1_RUM + T1_EXV
            M_RUM ~ T1_DEP + T1_RUM + T1_EXV
            '
fit3 <- sem(model = model3, cluster = "ID", data = dat_)
summary(fit3)
#model4: 간접, 작접효과
model4 <- 'level: 1
            STR_YN ~ M_EXV 
            M_RUM ~ STR_YN
            M_RUM ~ M_EXV 
            M_RUM ~~ + M_DEP
            level: 2
            STR_YN ~ M_EXV 
            M_RUM ~ STR_YN
            M_RUM ~ M_EXV + AM_DEP
            '
fit4 <- sem(model = model4, cluster = "ID", data = dat_)
summary(fit4)
