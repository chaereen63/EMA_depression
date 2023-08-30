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
#visualizing
linep <- ggplot(data=smmr_test, aes(y=dMRUM, x=tseq, color=ID, group=ID)) + 
  geom_point() + geom_line() +
  ggtitle("개인별  하루 간격 반추의 변화_하루 평균치") + 
  theme(legend.position = "none")
linep
#visualizing
linep <- ggplot(data=dat_, aes(y=CM_RUM, x=tseq, color=Hour, group=ID)) + 
  geom_point() + geom_line() +
  ggtitle("") + 
  theme(legend.position = "none")
linep
#visualizing
linep <- ggplot(data=dat_, aes(y=CM_EXV, x=tseq, color=Hour, group=ID)) + 
  geom_point() + geom_line() +
  labs(x = "응답순서", y = "경험회피", 
       title = "") + 
  theme(legend.position = "none")
linep
#visualizing_hours
linep <- ggplot(data=dat_, aes(y=CM_RUM, x=Hour, color=STR_YN, group=STR_YN)) + 
  geom_point() + geom_line() + 
  ggtitle("") + 
  theme(legend.position = "none") + scale_x_continuous(breaks = c(9,12,15,18,21,24))
linep
