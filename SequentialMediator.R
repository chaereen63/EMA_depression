library(tidyverse);library(psych)
library(lavaan);library(lubridate)
library(nlme);library(lme4);library(lmerTest);library(ggplot2);library(multilevel)

### new one_순차 매개 모형
ema <- read_csv("EMAdata_bk_TE.csv")
#level2설정을 위해 주관적 스트레스의 시간적 변화 살펴보기_산점도
plot(ema$tseq, ema$STR_L,
     col = ifelse(ema$STR_YN == "1", "green", "red"), 
     pch = ifelse(ema$STR_YN == "1", 17, 19), main = "스트레스 경험 유무",
     xlab = "응답순서", ylab = "시간 별 스트레스 강도")
legend("topleft", pch = c(17, 19),
       c("stress_YES", "stress_NO"),
       col = c("green", "red"))
stress_Y <- subset(ema, STR_YN =="1")
stress_N <- subset(ema, STR_YN =="0")
abline(lm(stress_Y$STR_L ~ stress_Y$tseq), col = "green")
abline(lm(stress_N$STR_L ~ stress_N$tseq), col = "red")
#일단 스트레스 강도는 level 2에서 보면 약간의 차이

# analyses
## ICCs
hrs.DEP <- aov(M_DEP~as.factor(ID),data=ema)
hrs.RUM <- aov(M_RUM~as.factor(ID),data=ema)
hrs.EXV <- aov(M_EXV~as.factor(ID),data=ema)
hrs.STR_L <- aov(STR_L~as.factor(ID),data=ema)
hrs.STR_YN <- aov(STR_YN~as.factor(ID),data=ema)
ICC1(hrs.DEP);ICC2(hrs.DEP)
ICC1(hrs.RUM);ICC2(hrs.RUM)
ICC1(hrs.EXV);ICC2(hrs.EXV)
ICC1(hrs.STR_L);ICC2(hrs.STR_L)
ICC1(hrs.STR_YN);ICC2(hrs.STR_YN)

## mod_SM: 제약 없는 모형 Lv.1) 순차매개 Lv.2) 평균적인 변화
mod_SM <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN
           level: 2
            AM_DEP ~ AM_EXV + AM_RUM
            AM_RUM ~ AM_EXV
          '
fitsm <- sem(model = mod_SM, cluster = "ID", data = ema)
summary(fitsm)
#clear!!
#multi-level?
lmodel <- 'CM_EXV ~ STR_YN
          CM_RUM ~ CM_EXV + STR_YN
          CM_DEP ~ CM_RUM + CM_EXV + STR_YN
          '
fitL <- sem(model = lmodel, data = dat_)
summary(fitL) #level1에서의 결과가 같다.
lmodel2 <- 'AM_DEP ~ AM_EXV + AM_RUM
            AM_RUM ~ AM_EXV
          '
fitL2 <- sem(model = lmodel2, data = ema)
summary(fitL2) #level2의 결과와 조금 다름

#스트레스 강도로 조절 (조절효과 항 추가)
subjectM <- 'level: 1
            CM_EXV ~ STR_YN  + STR_L + STR_YN*STR_L
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV
           level: 2
            AM_DEP ~ AM_EXV + AM_RUM
            AM_RUM ~ AM_EXV
          '
fitsub <- sem(model = subjectM, data = ema, cluster = "ID")
summary(fitsub)
lavInspect(fitsub, 'icc') #적합 이후의 icc
lavInspect(fitsub, 'h1') #비제약 모델에 대한 평균, 공분산


#스트레스 강도 조절효과_수준간 상호작용?
subjectM <- 'level: 1
            CM_EXV ~ STR_YN  + STR_L + STR_YN*STR_L
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV
           level: 2
            AM_DEP ~ AM_EXV + AM_RUM
            AM_RUM ~ AM_EXV
          '
fitsub <- sem(model = subjectM, data = ema, cluster = "ID")
summary(fitsub)

#lme function으로 구하기
intercept.only <- lme(CM_DEP ~ 1, 
                      random = ~1 | ID,
                      ema, method = "ML", na.action = na.omit)
summary(intercept.only)
VarCorr(intercept.only) #비표준화 점수
random.intercept <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + STR_L + STR_YN*STR_L,
                        random = ~1 | ID,
                        ema, method = "ML", na.action = na.omit)
summary(random.intercept)
VarCorr(random.intercept)
random.coefficients <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + STR_YN*STR_L,
                           random = ~ CM_RUM + CM_EXV | ID,
                           ema, method = "ML", na.action = na.omit)
summary(random.coefficients)
VarCorr(random.coefficients)
#main model
