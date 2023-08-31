## plot
library(tidyverse);library(psych)
library(lavaan);library(lubridate)
library(data.table);library(ggplot2)
#산점도(스트레스 사건 발생 유무 별)
plot(dat_$M_EXV, dat_$M_RUM,
     col = ifelse(dat_$STR_YN == "1", "green", "red"), 
     pch = ifelse(dat_$STR_YN == "1", 17, 19), main = "스트레스 경험 유무")
legend("topleft", pch = c(17, 19),
       c("stress_YES", "stress_NO"),
       col = c("green", "red"))
stress_Y <- subset(dat_, STR_YN =="1")
stress_N <- subset(dat_, STR_YN =="0")
abline(lm(stress_Y$M_RUM ~ stress_Y$M_EXV), col = "green")
abline(lm(stress_N$M_RUM ~ stress_N$M_EXV), col = "red")

#시간에 따라
plot(dat_$tseq, dat_$M_RUM,
     col = ifelse(dat_$STR_YN == "1", "green", "red"), 
     pch = ifelse(dat_$STR_YN == "1", 17, 19), main = "스트레스 경험 유무",
     xlab = "응답순서", ylab = "시간 별 반추")
legend("topleft", pch = c(17, 19),
       c("stress_YES", "stress_NO"),
       col = c("green", "red"))
stress_Y <- subset(dat_, STR_YN =="1")
stress_N <- subset(dat_, STR_YN =="0")
abline(lm(stress_Y$M_RUM ~ stress_Y$tseq), col = "green")
abline(lm(stress_N$M_RUM ~ stress_N$tseq), col = "red")
#jump_시간변수 추가해야함
plot(dat_$DinY, dat_$M_RUM,
     col = ifelse(dat_$STR_jump == "1", "green", "orange"), 
     pch = ifelse(dat_$STR_jump == "1", 17, 19), main = "스트레스 경험 전후",
     xlab = "하루 별 응답", ylab = "시간 별 반추")
legend("topleft", pch = c(17, 19),
       c("스트레스 발생 이후", "스트레스 발생 이전"),
       col = c("green", "orange"))
stress_Y <- subset(dat_, STR_jump =="1")
stress_N <- subset(dat_, STR_jump =="0")
abline(lm(stress_Y$M_RUM ~ stress_Y$tseq), col = "green")
abline(lm(stress_N$M_RUM ~ stress_N$tseq), col = "orange")
#자료의 표현
plot(dat_$DinY, dat_$M_RUM,
     col = dat_$ID,
     xlab = "하루 별 응답", ylab = "시간 별 반추")
abline(lm(M_RUM ~ DinY*ID, data = dat_), col = dat_$ID)
abline(lm(M_RUM ~ DinY*ID, data = dat_), col = dat_$ID)
#visualizing
linep <- ggplot(data=dat_, aes(y=CM_RUM, x=tseq, color=ID, group=ID)) + 
  geom_point() + geom_line() +
  ggtitle("개인별  하루 간격 반추의 변화") + 
  theme(legend.position = "none")
linep
#visualizing_응답 순서에 따라(반추)
linep <- ggplot(data=smmr_test, aes(y=dMRUM, x=tseq, color=ID, group=ID)) + 
  geom_point() + geom_line() +
  labs(x = "응답순서", y = "반추", 
                 title = "개인별  하루 간격 반추의 변화_하루 평균치") + 
  theme(legend.position = "none")
linep
#visualizing_응답순서를 시간(hour)에 따라 색으로 표현했을 때_반추
linep <- ggplot(data=dat_, aes(y=CM_RUM, x=tseq, color=Hour, group=ID)) + 
  geom_point() + geom_line() +
  labs(x = "Time", y = "반추 (person-mean centering)", 
       title = "개인 별 EMA응답_반추")  + 
  theme(legend.position = "none")
linep
#응답순서를 시간(hour)에 따라 색으로 표현했을 때_경험회피
linep <- ggplot(data=dat_, aes(y=CM_EXV, x=tseq, color=Hour, group=ID)) + 
  geom_point() + geom_line() +
  labs(x = "Time", y = "경험회피(person-mean centering)", 
       title = "개인 별 EMA응답_경험회피") + 
  theme(legend.position = "none") + scale_x_continuous(breaks = c(1,10,20,30))
linep
#우울
linep <- ggplot(data=dat_, aes(y=CM_DEP, x=tseq, color=Hour, group=ID)) + 
  geom_point() + geom_line() +
  labs(x = "Time", y = "우울(person-mean centering)", 
       title = "개인 별 응답_우울") + 
  theme(legend.position = "none") + scale_x_continuous(breaks = c(1,10,20,30))
linep
#부정정서
linep <- ggplot(data=dat_, aes(y=CM_NA, x=tseq, color=Hour, group=ID)) + 
  geom_point() + geom_line() +
  labs(x = "Time", y = "부정정서(person-mean centering)", 
       title = "개인 별 응답_부정정서") + 
  theme(legend.position = "none") + scale_x_continuous(breaks = c(1,10,20,30))
linep
#visualizing_hours
linep <- ggplot(data=dat_, aes(y=CM_RUM, x=Hour, color=factor(STR_YN), group=ID)) + 
  geom_point() + geom_line() + 
  labs(x = "Time(단위: 시)", y = "반추(person-mean centering)", 
       title = "하루 내 스트레스 유무에 따른 차이_반추") + 
  theme(legend.position = "right") + scale_x_continuous(breaks = c(9,12,15,18,21,24))
linep

#visualizing_hours: 이전 스트레스와 다음 스트레스
linep <- ggplot(data=dat_, aes(y=CM_RUM, x=Hour, color=factor(STR_YN), group=ID)) + 
  geom_point() + geom_line() + 
  labs(x = "Time(단위: 시)", y = "반추(person-mean centering)", 
       title = "하루 내 스트레스 유무에 따른 차이_반추") + 
  theme(legend.position = "right") + scale_x_continuous(breaks = c(9,12,15,18,21,24))
linep
linep_EXV <- ggplot(data=dat_, aes(y=CM_EXV, x=Hour, color=factor(STR_YN), group=STR_YN)) + 
  geom_point() + geom_line() + 
  labs(x = "Time(단위: 시)", y = "경험회피(person-mean centering)", 
       title = "하루 내 스트레스 유무에 따른 차이_경험회피") + 
  theme(legend.position = "right") + scale_x_continuous(breaks = c(9,12,15,18,21,24))
linep_EXV
linep_DEP <- ggplot(data=dat_, aes(y=CM_DEP, x=Hour, color=factor(STR_YN), group=STR_YN)) + 
  geom_point() + geom_line() + 
  labs(x = "Time(단위: 시)", y = "우울(person-mean centering)", 
       title = "하루 내 스트레스 유무에 따른 차이_우울") + 
  theme(legend.position = "right") + scale_x_continuous(breaks = c(9,12,15,18,21,24))
linep_DEP
linep_NA <- ggplot(data=dat_, aes(y=CM_NA, x=Hour, color=factor(STR_YN), group=STR_YN)) + 
  geom_point() + geom_line() + 
  labs(x = "Time(단위: 시)", y = "부정정서(person-mean centering)", 
       title = "하루 내 스트레스 유무에 따른 차이_부정정서") + 
  theme(legend.position = "right") + scale_x_continuous(breaks = c(9,12,15,18,21,24))
linep_NA
#visualizing_hours: 스트레스 발생
linep <- ggplot(data=dat_, aes(y=CM_RUM, x=Hour, color=factor(STR_jump), group=STR_jump, shape=factor(STR_YN))) + 
  geom_point() + geom_line() + 
  labs(x = "Time(단위: 시)", y = "반추(person-mean centering)", 
       title = "하루 내 스트레스 최초발생 전후 구분_반추") + 
  theme(legend.position = "right") + scale_x_continuous(breaks = c(9,12,15,18,21,24))
linep
