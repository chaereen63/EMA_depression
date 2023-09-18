### new one_순차 매개 모형
#level2설정을 위해 주관적 스트레스의 시간적 변화 살펴보기_산점도
plot(dat_$tseq, dat_$STR_L,
     col = ifelse(dat_$STR_YN == "1", "green", "red"), 
     pch = ifelse(dat_$STR_YN == "1", 17, 19), main = "스트레스 경험 유무",
     xlab = "응답순서", ylab = "시간 별 스트레스 강도")
legend("topleft", pch = c(17, 19),
       c("stress_YES", "stress_NO"),
       col = c("green", "red"))
stress_Y <- subset(dat_, STR_YN =="1")
stress_N <- subset(dat_, STR_YN =="0")
abline(lm(stress_Y$STR_L ~ stress_Y$tseq), col = "green")
abline(lm(stress_N$STR_L ~ stress_N$tseq), col = "red")
#일단 스트레스 강도는 level 2에서 보면 약간의 차이는 있겠지만 잘 모르겠다.. 
#Yes or No로 구분하는 것은 의미 있을 수 있으나, 개인 내로 보는 게 더 적절
#그림으로 더 볼 것:더 뭘...

# analyses

