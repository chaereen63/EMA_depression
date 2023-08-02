library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)
ema <- read_csv("EMAdata_2023-1bk.csv")

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
