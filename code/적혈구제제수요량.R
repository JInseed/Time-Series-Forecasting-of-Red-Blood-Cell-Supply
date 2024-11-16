library(feasts)
library(fable)
library(forecast)
library(fpp3)
library(lubridate)
library(TSA)
library(lmtest)

#적혈구제제수요량
#원변수 그래프 및 ACF, PACF 확인
ts.pat %>% 
  autoplot()

ts.pat %>% 
  ggAcf()

ts.pat %>% 
  ggPacf()


#계절성 확인
ts.pat %>% 
  gg_season(적혈구제제수요량, period = 12)

ts.pat %>% 
  gg_subseries(적혈구제제수요량, period=12)

#lag plot으로 시차별 상관정도 확인
ts.pat %>% 
  gg_lag(적혈구제제수요량, geom='point', lags = 1:12)

#ACF 여러 정보 확인
ts.pat %>% 
  features(
    .var = 적혈구제제수요량,
    features = feat_acf
  )


###############################################################
#분해법 1
#지시함수 이용
ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ trend()+season())) %>% 
  report()

ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ trend() + I(trend()^2)+season())) %>% 
  report()
#가장 유의함

ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ trend() + I(trend()^2) + I(trend()^3)+season())) %>% 
  report()




fit_dec=ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ trend() + I(trend()^2)+season()))

fit_dec %>% 
  report()


#그래프
fit_dec %>% 
  augment() %>% 
  ggplot(aes(x = 시점)) +
  geom_line(aes(y = 적혈구제제수요량, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "적혈구제제수요량", title = "실제값과 예측값 비교") +
  guides(colour = guide_legend(title = "Series"))


#잔차항 체크
#ACF plot of residuals & Histogram of residuals
fit_dec %>% 
  residuals() %>% 
  ggplot()+
  geom_histogram(aes(.resid), bins = 40)

fit_dec %>% 
  residuals() %>% 
  ggplot() + 
  geom_point(aes(시점,.resid))

fit_dec %>% 
  residuals() %>% 
  ggAcf()

fit_dec %>% 
  residuals() %>% 
  ggPacf()


#Ljung test
#ljung_box를 적용하여 포트멘토 검정(portmanteau)도 해볼 수 있습니다.
#lag는 계산에 쓰일 시차 자기상관 계수의 수
#dof는 적합된 모형의 자유도
#pvalue가 유의수준보다 크면 백색잡음 인것. 기각을 안해야함

ptest=data.frame(matrix(nrow=25,ncol=2))
colnames(ptest)=c('lag','pvalue')
ptest$lag=c(1:25)

for (i in 1:25){
  ptest[i,'pvalue']=round(fit_dec %>% 
    augment() %>% 
    features(
      .var = .innov,
      features = ljung_box,
      lag=i
    ) %>% 
    select(lb_pvalue),5)
}

ptest

dec_resid=fit_dec %>% 
  residuals() %>% 
  select(.resid) %>% 
  mutate(resid=.resid) %>% 
  select(-.resid)

#lag plot으로 시차별 상관정도 확인
dec_resid %>% 
  gg_lag(resid, geom='point', lags = 1:12)




#모형별 AIC 확인, 회귀 모형을 이용했으므로 상수항을 포함하지 않은 모형으로 적합
caf_fit <- dec_resid %>% 
  model(
    arima000_000 = ARIMA(resid ~ 0+pdq(0,0,0)+PDQ(0,0,0)),
    arima000_100 = ARIMA(resid ~ 0+pdq(0, 0, 0)+PDQ(1,0,0)),
    arima100_100 = ARIMA(resid ~ 0+pdq(1, 0, 0)+PDQ(1,0,0)),
    arima001_101 = ARIMA(resid ~ 0+pdq(0, 0, 1)+PDQ(1,0,0)),
    arima101_100 = ARIMA(resid ~ 0+pdq(1, 0, 1)+PDQ(1,0,0)),
    arima100_000 = ARIMA(resid ~ 0+pdq(1, 0, 0)+PDQ(0,0,0)),
    arima001_000 = ARIMA(resid ~ 0+pdq(0, 0, 1)+PDQ(0,0,0)),
    arima100_000 = ARIMA(resid ~ 0+pdq(1, 0, 0)+PDQ(0,0,0)),
    arima101_000 = ARIMA(resid ~ 0+pdq(1, 0, 1)+PDQ(0,0,0)),
    step=ARIMA(resid)
  )


caf_fit %>% 
  pivot_longer(
    cols = 1:9,
    names_to = "ModelName",
    values_to = "Orders"
  )

caf_fit %>% 
  glance() %>% 
  arrange(AICc) %>% 
  select(.model, AICc)


caf_fit %>% 
  glance() %>% 
  arrange(AICc) %>% 
  select(AICc) %>% 
  as.vector()
  


#ARIMA 적합
fit_decArima <- dec_resid %>% 
  model(ARIMA(resid ~ 0+pdq(0,0,1)+PDQ(1,0,1)))

fit_decArima %>% 
  report()

fit_decArima %>% 
  residuals() %>% 
  ggAcf()

fit_decArima %>% 
  residuals() %>% 
  ggPacf()


ptest2=data.frame(matrix(nrow=25,ncol=2))
colnames(ptest2)=c('lag','pvalue')
ptest2$lag=c(1:25)

for (i in 1:25){
  ptest2[i,'pvalue']=round(fit_decArima %>% 
                            augment() %>% 
                            features(
                              .var = .innov,
                              features = ljung_box,
                              lag=i
                            ) %>% 
                            select(lb_pvalue),5)
}

ptest2  

decArima_res=fit_decArima %>% 
  residuals() %>% 
  select(.resid) %>% 
  mutate(resid=.resid) %>% 
  select(-.resid)
  
decArima_res

# modified Levene's test (잔차의 등분산 확인)
# suffle <- sample(irregular3,72)
# e1 <-suffle[1:36]; e2 <-suffle[37:72]
e1 <-decArima_res[1:30,'resid'][[1]]
e2 <-decArima_res[31:60,'resid'][[1]]
e <-c(e1,e2)
d1 <- abs(e1-median(e1)); d2 <- abs(e2-median(e2))
d <- c(d1,d2)
grp <- c(rep(1,30),rep(2,30))
data1 <- cbind(grp,e)
data2 <- cbind(grp,d)

t.test(d ~ grp, data=data2, var.equal=TRUE, conf.level = 0.95) 

# anova test (잔차의 평균이 동일한지 확인)
data1<-data.frame(data1)
model = aov(e ~ grp, data = data1)
summary(model)

#최종 모형 평가(AIC, AICc, BIC 등)
fit_decArima %>% 
  glance()

fit_decArima %>% 
  fabletools::forecast() %>% 
  autoplot()


###########################################3
#분해법 2, 코로나 이후 지시함수 포함

ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ I+trend()+season())) %>% 
  report()

ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ I+trend() + I(trend()^2)+season())) %>% 
  report()
#가장 유의함

ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ I+trend() + I(trend()^2) + I(trend()^3)+season())) %>% 
  report()


fit_dec=ts.pat %>% 
  model(TSLM(적혈구제제수요량 ~ I + trend() + I(trend()^2)+season()))

fit_dec %>% 
  report()

#그래프
fit_dec %>% 
  augment() %>% 
  ggplot(aes(x = 시점)) +
  geom_line(aes(y = 적혈구제제수요량, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "적혈구제제수요량", title = "실제값과 예측값 비교") +
  guides(colour = guide_legend(title = "Series"))


#잔차항 체크
#ACF plot of residuals & Histogram of residuals
fit_dec %>% 
  residuals() %>% 
  ggplot()+
  geom_histogram(aes(.resid), bins = 10)

fit_dec %>% 
  residuals() %>% 
  ggplot() + 
  geom_point(aes(시점,.resid))

fit_dec %>% 
  residuals() %>% 
  ggAcf()

fit_dec %>% 
  residuals() %>% 
  ggPacf()

#Ljung test
#ljung_box를 적용하여 포트멘토 검정(portmanteau)도 해볼 수 있습니다.
#lag는 계산에 쓰일 시차 자기상관 계수의 수
#dof는 적합된 모형의 자유도
#pvalue가 유의수준보다 크면 백색잡음 인것. 기각을 안해야함

ptest=data.frame(matrix(nrow=25,ncol=2))
colnames(ptest)=c('lag','pvalue')
ptest$lag=c(1:25)

for (i in 1:25){
  ptest[i,'pvalue']=round(fit_dec %>% 
                            augment() %>% 
                            features(
                              .var = .innov,
                              features = ljung_box,
                              lag=i
                            ) %>% 
                            select(lb_pvalue),5)
}

ptest

fit_dec %>% 
  glance() %>% 
  view()

dec_resid=fit_dec %>% 
  residuals() %>% 
  select(.resid) %>% 
  mutate(resid=.resid) %>% 
  select(-.resid)

#lag plot으로 시차별 상관정도 확인
dec_resid %>% 
  gg_lag(resid, geom='point', lags = 1:12)



# modified Levene's test (잔차의 등분산 확인)
# suffle <- sample(irregular3,72)
# e1 <-suffle[1:36]; e2 <-suffle[37:72]
set.seed(1225)
suffle = sample(dec_resid[,'resid'][[1]])

e1 <-suffle[1:30]
e2 <-suffle[31:60]
e <-c(e1,e2)
d1 <- abs(e1-median(e1)); d2 <- abs(e2-median(e2))
d <- c(d1,d2)
grp <- c(rep(1,30),rep(2,30))
data1 <- cbind(grp,e)
data2 <- cbind(grp,d)

t.test(d ~ grp, data=data2, var.equal=TRUE, conf.level = 0.95) 


# anova test (잔차의 평균이 동일한지 확인)
data1<-data.frame(data1)
model = aov(e ~ grp, data = data1)
summary(model)

t.test(dec_resid$resid, mu=0)


sqrt(mean((ts.pat$적혈구제제수요량-fitted(fit_dec)$.fitted)^2))


#최종 모형 평가(AIC, AICc, BIC 등)
fit_dec %>% 
  glance()

fit_dec %>% 
  fabletools::forecast(h=5) %>% 
  autoplot()

fit_dec %>% 
  forecast::forecast(h=5) %>% 
  autoplot()


fit_dec %>% 
  predict(ts(data=c(1:50),start=c(2016,1),frequency=12))

fit_dec %>% 
  predict(5)

?predict
############################################################
#Holt Winters 계절지수평활
fit_HW <- ts.pat %>% 
  model(ETS(적혈구제제수요량 ~ error("A") + trend("A") + season("A")))

fit_HW %>% 
  report()

#그래프
fit_HW %>% 
  augment() %>% 
  ggplot(aes(x = 시점)) +
  geom_line(aes(y = 적혈구제제수요량, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "적혈구제제수요량", title = "Holt winter's 계절 지수평활") +
  guides(colour = guide_legend(title = "Series"))




#잔차항 체크
#ACF plot of residuals & Histogram of residuals
fit_HW %>% 
  residuals() %>% 
  ggplot()+
  geom_histogram(aes(.resid), bins = 10)

fit_HW %>% 
  residuals() %>% 
  ggplot() + 
  geom_point(aes(시점,.resid))

fit_HW %>% 
  residuals() %>% 
  ggAcf()

fit_HW %>% 
  residuals() %>% 
  ggPacf()



#Ljung test
#ljung_box를 적용하여 포트멘토 검정(portmanteau)도 해볼 수 있습니다.
#lag는 계산에 쓰일 시차 자기상관 계수의 수
#dof는 적합된 모형의 자유도
#pvalue가 유의수준보다 크면 백색잡음 인것. 기각을 안해야함

ptest3=data.frame(matrix(nrow=25,ncol=2))
colnames(ptest3)=c('lag','pvalue')
ptest3$lag=c(1:25)

for (i in 1:25){
  ptest3[i,'pvalue']=round(fit_HW %>% 
                            augment() %>% 
                            features(
                              .var = .innov,
                              features = ljung_box,
                              lag=i
                            ) %>% 
                            select(lb_pvalue),5)
}

ptest3

HW_resid=fit_HW %>% 
  residuals() %>% 
  select(.resid) %>% 
  mutate(resid=.resid) %>% 
  select(-.resid)

#lag plot으로 시차별 상관정도 확인
HW_resid %>% 
  gg_lag(resid, geom='point', lags = 1:12)



# modified Levene's test (잔차의 등분산 확인)
# suffle <- sample(irregular3,72)
# e1 <-suffle[1:36]; e2 <-suffle[37:72]
set.seed(1225)
suffle = sample(HW_resid[,'resid'][[1]])
e1 <-suffle[1:30]
e2 <-suffle[31:60]
e <-c(e1,e2)
d1 <- abs(e1-median(e1)); d2 <- abs(e2-median(e2))
d <- c(d1,d2)
grp <- c(rep(1,30),rep(2,30))
data1 <- cbind(grp,e)
data2 <- cbind(grp,d)

t.test(d ~ grp, data=data2, var.equal=TRUE, conf.level = 0.95) 

# anova test (잔차의 평균이 동일한지 확인)
data1<-data.frame(data1)
model = aov(e ~ grp, data = data1)
summary(model)

t.test(HW_resid$resid, mu=0)

#RMSE
sqrt(mean((ts.pat$적혈구제제수요량-fitted(fit_HW)$.fitted)^2))



##############################################################
#SARIMA

#차분 따른 그래프
ts.pat %>% 
  transmute(
    `원변수` = 적혈구제제수요량,
    `dif` = difference(적혈구제제수요량,1),
    `season` = difference(적혈구제제수요량, 12), # Annual data이기에 12
    `sdif` = difference(difference(적혈구제제수요량, 12), 1)
  ) %>% 
  pivot_longer(
    cols = -시점,
    names_to = "Type",
    values_to = "적혈구제제수요량"
  ) %>% 
  mutate(Type = factor(Type, levels = c("원변수", 
                                        "dif",
                                        "season", 
                                        "sdif"))) %>% 
  ggplot(aes(x = 시점, y = 적혈구제제수요량)) +
  geom_line(size = 0.7) +
  facet_wrap(. ~ Type, scales = "free_y", nrow = 4) +
  labs(title = "차분 따른 그래프 개형", y = NULL)





dif_pat=ts.pat %>% 
  transmute(
    `원변수` = 적혈구제제수요량,
    `dif` = difference(적혈구제제수요량,1),
    `season` = difference(적혈구제제수요량, 12), # Annual data이기에 12
    `sdif` = difference(difference(적혈구제제수요량, 12), 1)) %>% as.data.frame() %>% 
  select(-시점)

ts.pat %>% 
  mutate(dif=difference(ts.pat$적혈구제제수요량,1)) %>% 
  ggplot()+
  geom_line(aes(시점,dif))

dif_pat %>% 
  select(dif) %>% 
  ggAcf()

dif_pat %>% 
  select(dif) %>% 
  ggPacf()

ts.pat %>% 
  mutate(season=difference(ts.pat$적혈구제제수요량,12)) %>% 
  ggplot()+
  geom_line(aes(시점,season))

dif_pat %>% 
  select(season) %>% 
  ggAcf()

dif_pat %>% 
  select(season) %>% 
  ggPacf()

#시계열 그림에서는 추세가 안보이나 서서히 감소하는 형태를 보이므로 확률적 추세를 갖는다고 판단 가능


ts.pat %>% 
  mutate(sdif= difference(difference(적혈구제제수요량, 12), 1)) %>% 
  ggplot()+
  geom_line(aes(시점,sdif))

dif_pat %>% 
  select(sdif) %>% 
  ggAcf()

dif_pat %>% 
  select(sdif) %>% 
  ggPacf()


sd(dif_pat$dif, na.rm = T)
sd(dif_pat$season, na.rm = T)
sd(dif_pat$sdif, na.rm = T)

t.test(dif_pat$sdif, mu=0)
    


#Ljung test
ptest4=data.frame(matrix(nrow=25,ncol=2))
colnames(ptest4)=c('lag','pvalue')
ptest4$lag=c(1:25)

for (i in c(1:25)){
  ptest4[i,'pvalue']=round(Box.test(dif_pat$sdif, type = 'Ljung', lag = i)$p.value,5)
}
ptest4



# modified Levene's test (잔차의 등분산 확인)
# suffle <- sample(irregular3,72)
# e1 <-suffle[1:36]; e2 <-suffle[37:72]
set.seed(1225)
suffle = sample(dif_pat$sdif[14:59])
e1 <-suffle[1:23]
e2 <-suffle[24:46]
e <-c(e1,e2)
d1 <- abs(e1-median(e1)); d2 <- abs(e2-median(e2))
d <- c(d1,d2)
grp <- c(rep(1,23),rep(2,23))
data1 <- cbind(grp,e)
data2 <- cbind(grp,d)

t.test(d ~ grp, data=data2, var.equal=TRUE, conf.level = 0.95) 

# anova test (잔차의 평균이 동일한지 확인)
data1<-data.frame(data1)
model = aov(e ~ grp, data = data1)
summary(model)

t.test(dif_pat$sdif, mu=0)




###################################################
ptest4
a=ts(dif_pat$sdif,start=c(2016,1),frequency=12)
test=as_tsibble(a)

test_fit=test %>% 
  model(step=ARIMA(value))

test_fit %>% 
  residuals() %>% 
  ggAcf()

test_fit %>% 
  residuals() %>% 
  ggPacf()

test_fit %>% 
  report()

ptest5=data.frame(matrix(nrow=25,ncol=2))
colnames(ptest5)=c('lag','pvalue')
ptest5$lag=c(1:25)

for (i in c(1:25)){
  ptest5[i,'pvalue']=round(test_fit %>% 
                             augment() %>% 
                             features(
                               .var = .innov,
                               features = ljung_box,
                               lag=i
                             ) %>% 
                             select(lb_pvalue),5)
}

ptest5
###############################################3


#모형별 AIC 확인, 회귀 모형을 이용했으므로 상수항을 포함하지 않은 모형으로 적합
caf_fit <- ts.pat %>% 
  model(
    arima010_010 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,0)+PDQ(0,1,0)),
    arima010_011 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,0)+PDQ(0,1,1)),
    arima010_110 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,0)+PDQ(1,1,0)),
    arima010_111 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,0)+PDQ(1,1,1)),
    
    arima011_010 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,1)+PDQ(0,1,0)),
    arima011_011 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,1)+PDQ(0,1,1)),
    arima011_110 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,1)+PDQ(1,1,0)),
    arima011_111 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,1)+PDQ(1,1,1)),
    
    arima013_010 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,3)+PDQ(0,1,0)),
    arima013_011 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,3)+PDQ(0,1,1)),
    arima013_110 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,3)+PDQ(1,1,0)),
    arima013_111 = ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,3)+PDQ(1,1,1)),
    
    arima110_010 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,0)+PDQ(0,1,0)),
    arima110_011 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,0)+PDQ(0,1,1)),
    arima110_110 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,0)+PDQ(1,1,0)),
    arima110_111 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,0)+PDQ(1,1,1)),
    
    arima113_010 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,3)+PDQ(0,1,0)),
    arima113_011 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,3)+PDQ(0,1,1)),
    arima113_110 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,3)+PDQ(1,1,0)),
    arima113_111 = ARIMA(적혈구제제수요량 ~ 0+pdq(1,1,3)+PDQ(1,1,1)),
    step010_110=ARIMA(적혈구제제수요량)
  )


caf_fit %>% 
  pivot_longer(
    cols = 1:21,
    names_to = "ModelName",
    values_to = "Orders"
  ) %>% 
  view()

caf_fit %>% 
  glance() %>% 
  arrange(AICc) %>% 
  select(.model, AICc) %>% 
  view()


caf_fit %>% 
  glance() %>% 
  arrange(AICc) %>% 
  select(AICc) %>% 
  as.vector()




fit_sarima_f= ts.pat %>% 
  model(ARIMA(적혈구제제수요량 ~ 0+pdq(0,1,0)+PDQ(0,1,1)))


fit_sarima_f %>% 
  report()

fit_sarima_f %>% 
  residuals() %>% 
  ggplot()+
  geom_histogram(aes(.resid), bins = 10)

fit_sarima_f %>% 
  residuals() %>% 
  ggplot() + 
  geom_point(aes(시점,.resid))

fit_sarima_f %>% 
  residuals() %>% 
  ggAcf()

fit_sarima_f %>% 
  residuals() %>% 
  ggPacf()

ptest5=data.frame(matrix(nrow=25,ncol=2))
colnames(ptest5)=c('lag','pvalue')
ptest5$lag=c(1:25)

for (i in c(1:25)){
  ptest5[i,'pvalue']=round(fit_sarima_f %>% 
                             augment() %>% 
                             features(
                               .var = .innov,
                               features = ljung_box,
                               lag=i
                             ) %>% 
                             select(lb_pvalue),5)
}

ptest5


sarima_resid=fit_sarima_f %>% 
  residuals() %>% 
  mutate(resid=.resid) %>% 
  select(resid) %>% 
  as.vector()



for (i in c(1:25)){
  ptest5[i,'pvalue']=round(Box.test(sarima_resid[[1]], type = 'B', lag = i)$p.value,5)
}

ptest5
library(tseries)

adf.test(sarima_resid[[1]])


# modified Levene's test (잔차의 등분산 확인)
# suffle <- sample(irregular3,72)
# e1 <-suffle[1:36]; e2 <-suffle[37:72]
set.seed(1225)
suffle = sample(sarima_resid[[1]])
e1 <-suffle[1:30]
e2 <-suffle[31:60]
e <-c(e1,e2)
d1 <- abs(e1-median(e1)); d2 <- abs(e2-median(e2))
d <- c(d1,d2)
grp <- c(rep(1,30),rep(2,30))
data1 <- cbind(grp,e)
data2 <- cbind(grp,d)

t.test(d ~ grp, data=data2, var.equal=TRUE, conf.level = 0.95) 

# anova test (잔차의 평균이 동일한지 확인)
data1<-data.frame(data1)
model = aov(e ~ grp, data = data1)
summary(model)

t.test(sarima_resid[[1]], mu=0)




#RMSE
sqrt(mean((ts.pat$적혈구제제수요량-fitted(fit_sarima_f)$.fitted)^2))




#그래프
fit_sarima_f %>% 
  augment() %>% 
  ggplot(aes(x = 시점)) +
  geom_line(aes(y = 적혈구제제수요량, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "적혈구제제수요량", title = "실제값과 예측값 비교") +
  guides(colour = guide_legend(title = "Series"))
