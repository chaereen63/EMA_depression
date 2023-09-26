library(tidyverse);library(psych)
library(lavaan);library(lubridate)
library(nlme);library(lme4);library(lmerTest);library(ggplot2)
### new one_순차 매개 모형
dat_ <- read_csv("EMAdata_bk_TE.csv")
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
## mod_SM: 제약 없는 모형 Lv.1) 순차매개 Lv.2) 평균적인 변화
mod_SM <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN
           level: 2
            AM_DEP ~ AM_EXV + AM_RUM
            AM_RUM ~ AM_EXV
          '
fitsm <- sem(model = mod_SM, cluster = "ID", data = dat_)
summary(fitsm)
lavInspect(fitsm, "icc") #mplus?
lavInspect(fitsm, "h1")
#clear!!
#ICC가 0에 가까운데, MLM을 해야하는가? 그냥 sem
lmodel <- 'CM_EXV ~ STR_YN
          CM_RUM ~ CM_EXV + STR_YN
          CM_DEP ~ CM_RUM + CM_EXV + STR_YN
          '
fitL <- sem(model = lmodel, data = dat_)
summary(fitL) #level1에서의 결과가 같다.
lmodel2 <- 'AM_DEP ~ AM_EXV + AM_RUM
            AM_RUM ~ AM_EXV
          '
fitL2 <- sem(model = lmodel2, data = dat_)
summary(fitL2) #level2의 결과와 조금 다름

#스트레스 강도로 조절 (조절효과 항 추가)
subjectM <- 'level: 1
            CM_EXV ~ STR_YN  + STR_YN*STR_L
            CM_RUM ~ CM_EXV + STR_YN + STR_YN*STR_L
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN + STR_YN*STR_L
           level: 2
            AM_DEP ~ AM_RUM
            AM_RUM ~ AM_EXV
          '
fitsub <- sem(model = subjectM, data = dat_, cluster = "ID")
summary(fitsub)
lavInspect(fitsub, "icc")
lavInspect(fitsub, 'h1')


#lme function으로 구하기
intercept.only <- lme(CM_DEP ~ 1, 
                      random = ~1 | ID,
                      dat_, method = "ML", na.action = na.omit)
summary(intercept.only)
VarCorr(intercept.only)
random.intercept <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + STR_YN*STR_L,
                        random = ~1 | ID,
                        dat_, method = "ML", na.action = na.omit)
summary(random.intercept)
VarCorr(random.intercept)
random.coefficients <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + STR_YN*STR_L,
                           random = ~ CM_RUM + CM_EXV | ID,
                           dat_, method = "ML", na.action = na.omit)
summary(random.coefficients)
lv1_model <- 'CM_EXV ~ STR_YN  + STR_YN*STR_L
            CM_RUM ~ CM_EXV + STR_YN + STR_YN*STR_L
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN + STR_YN*STR_L'
intercept.slop.model <- lme(lv1_model,
                      random = CM_EXV + CM_RUM + CM_DEP | ID,
                      dat_, mothod = "ML", na.action = na.omit)