# 다중선형회귀

setwd('C:/R/BigDataTeamProject data')

crime <- read.csv('서울시 구별 5대범죄 발생건수_2018.csv',  stringsAsFactors = FALSE, header = TRUE)
View(crime)
str(crime)

crime$crime = gsub(",", "", crime$crime)
crime$crime = as.integer(as.character(crime$crime))

crime$theft = gsub(",", "", crime$theft)
crime$theft = as.integer(as.character(crime$theft))

crime$assault = gsub(",", "", crime$assault)
crime$assault = as.integer(as.character(crime$assault))
str(crime)

write.csv(crime, file = "crime.csv")

variables <- read.csv("variables.csv", stringsAsFactors = F)
View(variables)
str(variables)

variables$cctv = gsub(",", "", variables$cctv)
variables$cctv = as.integer(as.character(variables$cctv))
str(variables)

write.csv(variables, file = "variables.csv")

data = merge(crime, variables, all.x=TRUE)
View(data)
str(data)

row.names(data) <- data$gu
data <- data[-1]
View(data)
str(data)

write.csv(data, file = "data.csv")

summary(data)

# scale
data_n <- as.data.frame(lapply(data, scale))
summary(data_n)

write.csv(data_n, file = "data_scale.csv")

# crime 
View(data_n)
crime.lm = lm(crime ~ game + play + elem + sing + video + police + alcohol + midpop + highpop + cctv, data = data_n)
crime.lm
summary(crime.lm)

# 살인(murder) 
crime.lm = lm(murder ~ game + play + elem + sing + video + police + alcohol + midpop + highpop + cctv, data = data_n)
crime.lm
summary(crime.lm)

# 강도(robbery)
crime.lm = lm(robbery ~ game + play + elem + sing + video + police + alcohol + midpop + highpop + cctv, data = data_n)
crime.lm
summary(crime.lm)

# 강간강제추행(rape)
crime.lm = lm(rape ~ game + play + elem + sing + video + police + alcohol + midpop + highpop + cctv, data = data_n)
crime.lm
summary(crime.lm)

# 절도(theft)
crime.lm = lm(theft ~ game + play + elem + sing + video + police + alcohol + midpop + highpop + cctv, data = data_n)
crime.lm
summary(crime.lm)

# 폭행(assault)
crime.lm = lm(assault ~ game + play + elem + sing + video + police + alcohol + midpop + highpop + cctv, data = data_n)
crime.lm
summary(crime.lm)

?lm

library(dplyr)

# elem
data_elem <- data_n %>% select(-elem)
#View(data_elem)
crime.lm_elem = lm(crime ~ game + play + sing + video + police + alcohol + midpop + highpop + cctv, data = data_elem)
crime.lm_elem
summary(crime.lm_elem)

# video
data_video <- data_n %>% select(-video)
#View(data_video)
crime.lm_video = lm(crime ~ game + play + elem + sing + police + alcohol + midpop + highpop + cctv, data = data_video)
crime.lm_video
summary(crime.lm_video)

# police
data_police <- data_n %>% select(-police)
#View(data_police)
crime.lm_police = lm(crime ~ game + play + elem + sing + video + alcohol + midpop + highpop + cctv, data = data_police)
crime.lm_police
summary(crime.lm_police)

# midpop
data_midpop <- data_n %>% select(-midpop)
#View(data_midpop)
crime.lm_midpop = lm(crime ~ game + play + elem + sing + video + police + alcohol + highpop + cctv, data = data_midpop)
crime.lm_midpop
summary(crime.lm_midpop)

# highpop
data_highpop <- data_n %>% select(-highpop)
#View(data_highpop)
crime.lm_highpop = lm(crime ~ game + play + elem + sing + video + police + alcohol + midpop + cctv, data = data_highpop)
crime.lm_highpop
summary(crime.lm_highpop)

# cctv
data_cctv <- data_n %>% select(-cctv)
#View(data_cctv)
crime.lm_cctv = lm(crime ~ game + play + elem + sing + video + police + alcohol + midpop + highpop, data = data_cctv)
crime.lm_cctv
summary(crime.lm_cctv)

AIC(crime.lm, crime.lm_video) # video 넣는 것이 좋음
AIC(crime.lm, crime.lm_elem) # elem 빼는 것이 좋음
AIC(crime.lm, crime.lm_police) # police 넣는  것이 좋음
AIC(crime.lm, crime.lm_midpop) # midpop 빼는 것이 좋음
AIC(crime.lm, crime.lm_highpop) # highpop 빼는 것이 좋음
AIC(crime.lm, crime.lm_cctv) # cctv 넣는 것이 좋음

AIC(crime.lm, crime.lm_video, crime.lm_elem, crime.lm_police, crime.lm_midpop, crime.lm_highpop, crime.lm_cctv)

data_n2 <- data_n %>% select(-c(video, elem, police, midpop, highpop, cctv))
View(data_n2)
crime.lm2 = lm(crime ~ game + play + sing + alcohol, data = data_n2)
crime.lm2
summary(crime.lm2)

data_n3 <- data_n %>% select(-c(elem, midpop, highpop))
View(data_n3)
crime.lm3 = lm(crime ~ game + play + sing + video + police + alcohol + cctv, data = data_n3)
crime.lm3
summary(crime.lm3)

AIC(crime.lm, crime.lm2, crime.lm3)
write.csv(data_n3, file = "data_scale_final.csv")

# 잔차가 정규분포를 따르는지 확인
hist(data_n3$crime)
qqnorm(data_n3$crime)
qqline(data_n3$crime)

