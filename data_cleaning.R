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
#Not Appropriate Approach
#M_EMA  %>% mutate(STR_L = ifelse(is.na(STR_L), 0, STR_L)) %>% as.data.frame() -> tests 
#tests %>% group_by(ID) %>% mutate(M_STRL= mean(STR_L, na.rm = TRUE)) -> M_EMAtest
data <- read.csv("EMAdata_2023-1bk.csv", header = F)
data[-1,]-> data2

write.csv(data2, "BK_23_pos.csv")

table(M_EMA$ID, M_EMA$gender) -> ad
if_else(ad==0, count())
mean(M_EMA$age)
sqrt(var(M_EMA$age))

# Assuming your data frame is called 'your_data'
# Assuming 'level_2' is the between-person grouping variable
# Assuming 'level_1' is the within-person grouping variable
# Assuming 'value' is the variable for which you want to create a lagged vector

library(dplyr)
your_data <- read.csv("mean_STR.csv")
# Sort the data by level_2 and level_1
your_data <- your_data %>%
  arrange(ID, tseq)

# Add a lagged vector for 'value' within each level_2 group
your_data <- your_data %>%
  group_by(ID) %>%
  mutate(lagged_CM_DEP = lag(CM_DEP), lagged_CM_RUM = lag(CM_RUM),
         lagged_CM_EXV = lag(CM_EXV))

# Print the first few rows to check
head(your_data)
write.csv(your_data, "lagged_timeEMA.csv", na="999999999")
