## data cleaning

library(tidyverse);library(psych)
library(lavaan);library(lubridate)

### new one_순차 매개 모형
ema <- read_csv("EMAdata_bk_TE.csv")
as.data.frame(ema)

ema %>% select("tseq", "STR_YN", "ID", "gender", "age", "T1_DEP", "T1_RUM",
               "T1_EXV", "M_DEP", "M_EXV", "M_NA", "AM_EXV","AM_DEP", "AM_RUM", "AM_NA",
               "CM_DEP", "CM_RUM", "CM_EXV", "CM_NA", "STR_L") -> mlema

mlema %>% group_by(ID) %>% mutate(M_STRL= mean(STR_L, na.rm = TRUE)) -> M_EMA
M_EMA[,c("ID", "STR_L","M_STRL")]

write.csv(M_EMA, "mean_STR.csv")
