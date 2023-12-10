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

#스트레스 강도가 2수준에서 조절
#개인 별 스트레스 평균 구하기
subjectM2 <- 'level: 1
            CM_EXV ~ STR_YN  
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN
           level: 2
            STR_L ~~ STR_L
          '
fitsub <- sem(model = subjectM2, data = ema, cluster = "ID")
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
  # model2
  ## lme로는 순차매개가 안됨(복잡한 모형을 못봄)
intercept.as.outcome <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + M_STRL, 
                            random = ~ 1 | ID,
                            M_EMA, method = "ML", na.action = na.omit)
summary(intercept.as.outcome)
VarCorr(intercept.as.outcome)
intercept.slope.as.outcome <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + M_STRL,
                            random = ~ CM_EXV | ID,
                            M_EMA, method = "ML", na.action = na.omit)
summary(intercept.slope.as.outcome)
VarCorr(intercept.slope.as.outcome)
anova(intercept.slope.as.outcome, intercept.as.outcome)
#interaction.intercept.outcome <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + M_STRL + CM_EXV*M_STRL,
#                                  random = ~ 1 | ID,
#                                  M_EMA, method = "ML", na.action = na.omit)
#summary(interaction.intercept.outcome)
#anova(inercept.as.outcome, interaction.intercept.outcome)
#VarCorr(interaction.intercept.outcome)
ctrl <- lmeControl(opt='optim')
interaction.slope.outcome <- lme(CM_DEP ~ CM_RUM + CM_EXV + STR_YN + M_STRL + CM_EXV*M_STRL,
                                     random = ~ CM_EXV | ID,
                                     M_EMA, method = "ML", na.action = na.omit, control = ctrl)
summary(interaction.slope.outcome)
#VarCorr(interaction.slope.outcome)
#anova(intercept.as.outcome, intercept.only)

 #차이가 없음. 평균적인 스트레스 강도에 따라 경험회피가 조절된다는 설명은 의미가 없을지도?
  ##SEM: 순차매개를 lme로는 볼 수가 없으므로 multi-level SEM으로 분석
  # 단 multi level sem은 모형 적합도를 산출해주지 않으므로 별도의 계산이 필요해 보임.
subjectS <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN
           level: 2
            M_STRL ~ M_STRL
          '
fitsub_s <- sem(model = subjectS, data = M_EMA, cluster = "ID")
summary(fitsub_s)

subjectSN <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV
           level: 2
            i =~ CM_EXV + STR_YN
            i ~ M_STRL
          '
fitsub_sn <- sem(model = subjectSN, data = M_EMA, cluster = "ID")
summary(fitsub_sn)

subjectest <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN
           level: 2
           AM_EXV ~ STR_YN + M_STRL + STR_YN*M_STRL
          '
test <- sem(model = subjectest, data = M_EMA, cluster = "ID")
summary(test)

subjectest2 <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV
           level: 2
           AM_EXV ~ STR_YN + AM_NA + STR_YN*AM_NA
          '
test2 <- sem(model = subjectest2, data = M_EMA, cluster = "ID")
summary(test2)
####final###
subjectS <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV
           level: 2
            AM_EXV ~ M_STRL
          '
fitsub_s <- sem(model = subjectS, data = M_EMA, cluster = "ID")
summary(fitsub_s)
subjectF <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN
           level: 2
            AM_EXV ~ M_STRL + STR_YN + M_STRL*STR_YN
          '
fitsub_f <- sem(model = subjectF, data = M_EMA, cluster = "ID")
summary(fitsub_f)






#stress level에 따른 변화 (다만 해석이 애매... 평소 스트레스가 아니라 스트레스 사건에 대한 강도라서)
subjectS2 <- 'level: 1
            CM_EXV ~ STR_L
            CM_RUM ~ CM_EXV + STR_L
            CM_DEP ~ CM_RUM + CM_EXV + STR_L
           level: 2
            AM_EXV ~ M_STRL
          '
fitsub_s2 <- sem(model = subjectS2, data = M_EMA, cluster = "ID")
summary(fitsub_s2)
###random slope in sem###
subjectRS <- 'level: 1
            CM_EXV ~ a*STR_L
            CM_RUM ~ CM_EXV + STR_L
            CM_DEP ~ CM_RUM + CM_EXV + STR_L
           level: 2
            a ~ M_STRL
          '
fitsub_rs <- sem(model = subjectRS, data = M_EMA, cluster = "ID")
summary(fitsub_rs)
#부정정서 (nagative emotion)
subjectNE <- 'level: 1
            CM_EXV ~ STR_YN
            CM_RUM ~ CM_EXV + STR_YN
            CM_DEP ~ CM_RUM + CM_EXV + STR_YN
           level: 2
            AM_EXV ~ AM_NA
          '
fitsub_NE <- sem(model = subjectNE, data = M_EMA, cluster = "ID")
summary(fitsub_NE)
