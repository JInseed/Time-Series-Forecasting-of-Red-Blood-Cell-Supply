library(feasts)
library(fable)
library(forecast)
library(fpp3)
library(lubridate)

ts.bl=as_tsibble(blood$적혈구제제공급량)

colnames(ts.bl)=c('시점','적혈구제제공급량')

#계절성 확인
ts.bl %>% 
  gg_season(적혈구제제공급량, period = 12)

ts.bl %>% 
  gg_subseries(적혈구제제공급량, period=12)

#lag plot으로 시차별 상관정도 확인
ts.bl %>% 
  gg_lag(적혈구제제공급량, geom='point', lags = 5:15)

?gg_lag

#원변수 ACF 확인
ts.bl %>% 
  ACF() %>% 
  autoplot()


#Decompositions(분해법)
dcmp = ts.bl %>% 
  model(classical_decomposition(적혈구제제공급량, type='additive'))

dcmp %>% 
  components()

dcmp %>% 
  components() %>% 
  autoplot()


#ACF 여러 정보 확인
ts.bl %>% 
  features(
    .var = 적혈구제제공급량,
    features = feat_acf
  )

#lm 이용
lm=ts.bl %>% 
  model(TSLM(적혈구제제공급량 ~ ts.acc$사고건수))

lm %>% 
  report()

lm %>% 
  augment()

lm %>% 
  augment() %>% 
  ggplot(aes(x = 시점)) +
  geom_line(aes(y = 적혈구제제공급량, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL, title = "Percent change in US consumption expenditure") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = NULL))

#잔차항 체크
#ACF plot of residuals & Histogram of residuals
lm %>% 
  gg_tsresiduals()

lm %>% 
  residuals() %>% 
  ggAcf()


lm %>% 
  residuals() %>% 
  ggPacf()


ts.bl %>% 
  model(ARIMA(적혈구제제공급량~pdq(0,1,1)+PDQ(0,1,1))) %>% 
  residuals() %>% 
  ggAcf()





#Ljung test
#ljung_box를 적용하여 포트멘토 검정(portmanteau)도 해볼 수 있습니다.
#lag는 계산에 쓰일 시차 자기상관 계수의 수
#dof는 적합된 모형의 자유도
#pvalue가 유의수준보다 크면 백색잡음 인것. 기각을 안해야함

lm %>% 
  augment() %>% 
  features(
    .var = .innov,
    features = ljung_box,
    lag=2
  )

#잔차 적합값 산점도
#무작위성이라면 등분산성 만족할 가능성 높음
lm %>% 
  augment() %>% 
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point(size = 1.2) +
  labs(x = "Fitted", y = "Residuals")

#기본적인 분해법

#지시함수 이용
fit = ts.bl %>% 
  model(TSLM(적혈구제제공급량 ~ trend()+season()))


fit = ts.bl %>% 
  model(TSLM(적혈구제제공급량 ~ trend() + I(trend()^2) + I(trend()^3)+season()))



fit %>% 
  augment() %>% 
  ggplot(aes(x = 시점)) +
  geom_line(aes(y = 적혈구제제공급량, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "Megalitres", title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))


#푸리에 변환
fit2 = ts.bl %>% 
  model(a=TSLM(적혈구제제공급량 ~ trend() + I(trend()^2) + I(trend()^3)+fourier(K = 6)))


fit2 %>% 
  report()

fit2 %>% 
  augment() %>% 
  ggplot(aes(x = 시점)) +
  geom_line(aes(y = 적혈구제제공급량, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  labs(y = "Megalitres", title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))


#최종 모형 평가(AIC, AICc, BIC 등)
fit2 %>% 
  glance() %>% 
  select(adj_r_squared, CV, AIC, AICc, BIC)


#미래 예측

fit = ets(ts.bl$적혈구제제공급량)

fit2 = TSLM(ts.bl$적혈구제제공급량 ~ trend() + I(trend()^2) + I(trend()^3)+fourier(K = 6))

summary(fit)
summary(fit2)


fit %>% 
  fabletools::forecast() %>% 
  autoplot()



fit_beer = fit %>% 
  model(TSLM(formula = 적혈구제제공급량 ~ trend() + season()))

fit_beer %>% 
  report()

fc_beer = fit_beer %>% 
  fabletools::forecast()

fc_beer %>% 
  autoplot(ts.bl)


summary(fit2)

















