install.packages("foreign")
install.packages("reshape")
library(foreign)
library(dplyr)
library(ggplot2)
library(reshape)
raw_welfare <- read.spss("data_spss_Koweps2014.sav",to.data.frame = T)
welfare <- raw_welfare
dim(welfare)
str(welfare)
head(welfare)
summary(welfare)
View(welfare)
welfare <- rename(welfare,
                  c(h0901_4="sex", #성별
                  h0901_5="birth", #태어난 연도
                  h09_din="income")) #소득 
#변수 검토 및 정제 - 성별
class(welfare$sex)
summary(welfare$sex)                  

#이상치 확인
table(welfare $sex)

#이상치 결측 처리
welfare$sex <- ifelse(welfare$sex==9,NA,welfare$sex)

#결측치 확인
table(is.na(welfare$sex))

#항목이름 부여
welfare$sex <- ifelse(welfare$sex==1,"male","female")
table(welfare$sex)
qplot(welfare$sex)

#변수 검토 및 정제 -소득

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income)+xlim(0,10000) #x축 설정
table(is.na(welfare$income))

#성별 소득 평균표
sex_income <- welfare %>%
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income  

ggplot(data=sex_income,aes(x=sex,y=mean_income))+geom_col()

#변수 검토 및 정제 - 나이
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#이상치 확인
summary(welfare$birth)

#이상치 결측처리
welfare$birth <- ifelse(welfare$birth ==9999,NA,welfare$birth)

#결측치 확인
table(is.na(welfare$birth))

#나이 변수 생성
welfare$age <- 2014-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

#나이별 소득 평균표 생성
age_income <- welfare %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
age_income

#나이별 소득 평균 분석
#그래프 생성-산점도
ggplot(data=age_income,aes(x=age,y=mean_income))+geom_point()

#연령대 변수 생성
#번주  기준
#-----------
#초년  30세미만
#중년  30-59세
#노년  60세 이상

welfare <- welfare %>% 
  mutate(ageg = ifelse(age<30,"young",
                       ifelse(age<=59,"middle","old")))
table(welfare$ageg)
qplot(welfare$ageg)

#연령대별 소득 평균표 생성
#초년 빈도 적으므로 제외

welfare_income <- welfare %>% 
  filter(ageg!="young") %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))
welfare_income

#연령대별 소득 그래프
ggplot(data=welfare_income,aes(x=ageg,y=mean_income))+geom_col()

#연령대 및 성별 소득 평균표 생성
sex_income <- welfare %>% 
  filter(ageg!="young") %>% 
  group_by(ageg,sex) %>% 
  summarise(mean_income=mean(income))
sex_income

#연령대 및 성별 소득 평균그래프 생성1
ggplot(data = sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col()

#연령대 및 성별 소득 평균그래프 생성2
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex))+
  geom_col(position="dodge") #position 변경(기본값 = "stack")
