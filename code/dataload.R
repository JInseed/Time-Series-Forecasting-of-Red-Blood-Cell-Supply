library(tidyverse)
library(lubridate)
library(gridExtra)
rm(list=ls())

select=dplyr::select

blood = read.csv('월별_헌혈통계.csv', fileEncoding = 'cp949',header=T,skip=2)

pat = read.csv('중증외상_환자_수_월별.csv', fileEncoding = 'cp949')

acc = read.csv('시도별_교통사고.csv', fileEncoding = 'cp949',header=T,skip=1)

colnames(acc)=c('년','월',1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)


acc$사고건수=c(rep(0,nrow(acc)))

for(i in 3:19){
  acc$사고건수=acc$사고건수+acc[,i]
}

acc=acc %>% 
  mutate(시점=ym(str_c(년,월))) %>% 
  filter(시점 >= ym('2016.01') & 시점 <= ym('2020.12') ) %>% 
  select(-년,-월)

colnames(blood)=c('년','월','헌혈건수')
colnames(pat)=c('년','월','환자수합계')


blood=blood %>% 
  mutate(시점=ym(str_c(년,월))) %>% 
  select(-년,-월)

pat=pat %>% 
  mutate(시점=ym(str_c(년,월))) %>% 
  select(-년,-월)

#혈액공급실적
년=c(2015:2020)
헌혈실적=c(2872156,2645181,2714819,2681611,2613901,2435210)
공급계=c(6278079,5891875,6255460,6395455,6387301,6008400)
수혈용=c(4048199,3920575,4136518,4277762,5354132,4129790)
적혈구제제=c(1948099,1901715,1906369,1904380,1913168,1789999)

sup=data.frame(년,헌혈실적,공급계,수혈용,적혈구제제)


sup=sup %>% 
  mutate(적혈구상수=적혈구제제/헌혈실적,
         cons=lag(적혈구상수)) %>% 
  filter(년 %in% c(2016:2020)) %>% 
  select(cons)

bcons=rep(sup[[1]],each=12)

blood$cons=bcons

blood=blood %>% 
  mutate(적혈구제제공급량=헌혈건수*cons)


#응급의료센터에서 수혈을 시행한 성인 외상환자~ 논문 발췌
a=((2.5*14)+(4.65*20))/34
pat=pat %>% 
  mutate(적혈구제제수요량=환자수합계*(a)) #3.76

blood$적혈구제제공급량=ts(blood$적혈구제제공급량,start=c(2016,1),frequency=12)
pat$적혈구제제수요량=ts(pat$적혈구제제수요량,start=c(2016,1),frequency=12)
acc$사고건수=ts(acc$사고건수,start=c(2016,1),frequency=12)


blood$t=1:nrow(blood)
pat$t=1:nrow(pat)


ts.bl=as_tsibble(blood$적혈구제제공급량)
colnames(ts.bl)=c('시점','적혈구제제공급량')
ts.bl$I=c(rep(0,49), rep(1,11))

ts.pat=as_tsibble(pat$적혈구제제수요량)
colnames(ts.pat)=c('시점','적혈구제제수요량')
ts.pat$I=c(rep(0,49), rep(1,11))


ts.acc=as_tsibble(acc$사고건수)
colnames(ts.acc)=c('시점','사고건수')


cor(ts.bl$적혈구제제공급량, ts.acc$사고건수)
cor(ts.pat$적혈구제제수요량, ts.acc$사고건수)
cor(ts.bl$적혈구제제공급량, ts.pat$적혈구제제수요량)

ccf(ts.bl$적혈구제제공급량, ts.acc$사고건수)


#boxcox
lambda=BoxCox.lambda(blood$적혈구제제공급량)
blood$box.적혈구제제공급량=BoxCox(blood$적혈구제제공급량,lambda = lambda)






