# R의 장점
# 1. 무료 배포
# 2. 모든 통계 기법이 이미 어딘가에 패키지 형태로 구형되어 있다.
# 3. 그래픽 관련 패키지 설치를 통해 다양한 그래프 활용 가능
# 4. 데이터 마이닝 및 기계 학습

# R 기초 사용법
# 1. 주석 : 프로그램 전반적인 내용, 명령어 내용이 무엇을 의미하는지 알 수 있게 하는 설명 기능
# 2. Ctrl + Enter : 명령어 실행하는 단축키
# 3. R은 대소문자 구분을 한다.


install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)






height<-c(190, 180, 170, 160)
height
weight<-c(80, 70, 60, 50)
weight

# 키 몸무게 데이터 프레임 만들기
df<-data.frame(height,weight)
df

# 성별 변수 추가
sex<-c("male", "female", "male", "female")

df1<-data.frame(sex, height, weight)
df1

# 데이터 프레임 연산
mean(df1$height) # $ : 변수명을 선택할 때 많이 쓰임

# Alt + shift + 아래방향키 : 똑같이 복사 붙여넣기

# 외부파일 불러오기
# 엑셀파일 불러오기
install.packages("readxl")
library(readxl)


# 해당경로 입력 방법 : 해당 파일 주소창에 텍스트 복사 (\을 /로 변경하기)
sampleDF<-read_excel("C:/Users/hongbi/Desktop/R 실습파일/sample_data.xlsx")
sampleDF
head(sampleDF) #앞부분 6개 확인
View(sampleDF) #전체 창 확인

# csv 파일 불러오기
# 범용 데이터 형식
# 용량 작아서 좋음
# 다양한 소프트웨어 사용

read.csv("C:/Users/hongbi/Desktop/R 실습파일/2. 기술통계,요인&신뢰도분석,카이제곱(항공).csv")
dep<-read.csv("C:/Users/hongbi/Desktop/R 실습파일/2. 기술통계,요인&신뢰도분석,카이제곱(항공).csv")
attach(dep)
head(dep)
View(dep)

# 카이제곱 검정
# csv 데이터 불러오기생략

# 테이블형성 및 관찰
dmt = xtabs(~성별+항공사)
dmt

install.packages("MASS")
library(MASS)

crosstab<-table(dep$성별, dep$항공사)
crosstab

addmargins(crosstab)


dmt1<-addmargins(prop.table)
(addmargins(crosstab,1),1))
dmt1

#반올림
round(dmt1, digits=3)

# 카이제곱 검정
chisq.test(dmt)
detach(dep) #함수를 붙이는 것



# 독립표본 t-검정
# 자료입력 및 관찰
dep<-read.csv("C:/Users/hongbi/Desktop/R 실습파일/3-1. t-test(사전사후).csv")
attach(dep)
head(dep)
boxplot(사전영어점수~집단,col="red")

# 정규성 검정
summary(사전영어점수[집단==1])
summary(사전영어점수[집단==2])
shapiro.test(사전영어점수[집단==1])
shapiro.test(사전영어점수[집단==2])

# 등분산 검정 및 독립표본 t-검정 시행
var.test(사전영어점수~집단, data=dep)

t.test(사전영어점수~집단, var.equal=TRUE) # 등분산

t.test(사전영어점수~집단) # 이분산

# 표본크기 및 평균과 표준편차
N<-tapply(사전영어점수, 집단, length)
Mean1<-tapply(사전영어점수, 집단, mean)
sd1<-tapply(사전영어점수, 집단, sd)


# 가로 결합
ttest<-cbind(N, Mean1, sd1)
ttest

# 반올림
round(ttest, digits=3)
detach(dep)

# 대응표본 t-검정
# 자료입력 생략(위 데이터)

# %>% : ctrl + shift + m : 함수를 연결하는 파이프
library(dplyr)
dep1<-dep %>%
  filter(dep$집단==1)
dep1

# 표본크기 및 평균과 표준편차
N<-sapply(dep1[c("사전영어점수", "사후영어점수")], length)
Mean1<-sapply(dep1[c("사전영어점수", "사후영어점수")], mean)
sd1<-sapply(dep1[c("사전영어점수", "사후영어점수")],sd)

data.frame(N, Mean1, sd1)

# paired t-test
t.test(dep1$사전영어점수, dep1$사후영어점수,paired=TRUE)
detach(dep1)

# 일원배치 분산분석
# 자료입력 및 관찰
dep<-read.csv("C:/Users/hongbi/Desktop/R 실습파일/4-1. 분산분석(음악미술치료).csv")
attach(dep)
head(dep)

# 정규성 검정
shapiro.test(사전우울[집단==1])
shapiro.test(사전우울[집단==2])
shapiro.test(사전우울[집단==3])

# 표본 크기 및 평균 표준 편차
N<-tapply(사전우울, 집단, length)
Mean1<-tapply(사전우울, 집단, mean)
sd1<-tapply(사전우울, 집단 , sd)

anova<-cbind(N, Mean1, sd1)
round(anova, digits=3)

# 일원배치 분산분석 시행
anova1<-aov(사전우울~factor(집단), data=dep)


summary(anova1)



#집단간 분산의 동질성 여부 파악
bartlett.test(사전우울~factor(집단), data=dep)

# 등분산 및 이분산 분산분석
oneway.test(사후우울~factor(집단), data=dep, var.equal=T) #등분산
oneway.test(사후우울~factor(집단), data=dep) #이분산

# Tukey HSD 사후검증
anova2<aov(사후우울~factor(집단),data=dep)
TukeyHSD(aov(사후우울~factor(집단),data=dep))

# 이원배치 분산분석
# 변수정리 factor

dep$집단<-factor(dep$집단, levels = c(1.0, 2.0, 3.0), labels = c("음악", "미술", "방치"))
dep$성별<-factor(dep$성별, levels = c(1.0, 2.0, 3.0), labels = c("남", "여"))
View(dep)


#이원배치 분산분석
dep.aov<-aov(dep$사후우울~dep$집단*dep$성별, data=dep)
summary(dep.aov)

# 기술통계량 그룹과 성별에 따른 평균 확인
model.tables(dep.aov, type="mean", se=T)

# 상호작용 효과
interaction.plot(x.factor=집단, trace.factor = 성별, response = 사후우울, fun = mean, type = "b", col = c("red", "blue", "green"))

#주효과 상호작용 효과
install.packages("HH")
library(HH)
interaction2wt(dep$사후우울~dep$집단*dep$성별, data=dep)

# 반복층정 분산분석
# 자료입력 및 관찰
dep<-read.csv("C:/Users/hongbi/Desktop/R 실습파일/4-2. 반복측정 분산분석(잇몸약).csv")
attach(dep)
head(dep)
View(dep)

#wide 자료로 변환
colnames(dep)[2]<-"score0"
colnames(dep)[3]<-"score1"
colnames(dep)[4]<-"score2"
colnames(dep)[5]<-"score3"

dep=reshape(dep,direction = "long", varying = 2,5, sep="")

View(dep)

# 숫자로 코딩된 변수를 범주형으로 변환

dep<-within(dep,{
group<-factor(집단)
time<-factor(time)
id<-factor(id)})


# 반복측정 분산분석
dep.aov1<-aov(score~집단*time+Error(id/time), data=dep)


summary(dep.aov1)

# 상관분석
install.packages("lattice")
library(lattice)
head(dep)
View(dep)

dep<-dep %>%
  select(쾌적성, 청결성, 시각성, 싸인에이지, 엔터테인먼트, 좌석이용편의성, 객실승무원, 지각된가치)

hd<-read.csv("C:/Users/hongbi/Desktop/R 실습파일/5. 상관&회귀분석(항공).csv")
attach(hd)
head(hd)
View(hd)

library(dplyr)
pairs(hd)

# 상관분석
cor.test(~쾌적성+청결성, data=hd)

install.packages("psych")
library(psych)


lowerCor(hd)


# 다중회귀분석
myresult<-lm(지각된가치~쾌적성+청결성+시각성+싸인에이지+엔터테인먼트+객실승무원, data=hd)
summary(myresult)

install.packages("car")
library(car)


# 자료입력 및 관찰
bd<-read.csv("C:/Users/hongbi/Desktop/R 실습파일/5. 상관&회귀분석(항공).csv")
attach(bd)
bd[(1:20),]
library(dplyr)
bd<-bd %>%
  select(쾌적성, 시각성, 싸인에이지, 엔터테인먼트, )






