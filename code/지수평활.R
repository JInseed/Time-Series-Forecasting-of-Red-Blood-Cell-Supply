library(TSA)



eacf(residuals(fit)$.resid)

eacf(ts.bl$적혈구제제공급량, ar.max = 7, ma.max = 13)


######
#지수평활
fit <- ts.bl %>% 
  model(ETS(적혈구제제공급량 ~ error("A") + trend("A") + season("A")))

fit %>% 
  report()

fc <- fit %>% 
  fabletools::forecast(h = 5)

fc %>% 
  autoplot(ts.bl) +
  geom_line(data = fit %>% augment(), aes(y = .fitted), color = "#D55E00") +
  labs(y = "적혈구제제공급량", title = "Holt's 계절지수평활")


#시계열 교차검증
ts.bl %>% 
#  stretch_tsibble(.init = 10) %>% #관측치 여러 조각으로 rolling
  model(
    `분해법` = TSLM(적혈구제제공급량 ~ trend() + I(trend()^2) + I(trend()^3)+season()),
    `Holt` = ETS(적혈구제제공급량 ~ error("A") + trend("A") + season("A"))) %>% 
  fabletools::forecast(k = 1) %>% 
  fabletools::accuracy(ts.bl)

ts.bl %>% 
  #  stretch_tsibble(.init = 10) %>% #관측치 여러 조각으로 rolling
  model(
    `분해법` = TSLM(적혈구제제공급량 ~ trend() + I(trend()^2) + I(trend()^3)+season()),
    `Holt` = ETS(적혈구제제공급량 ~ error("A") + trend("A") + season("A"))) %>% 
  glance()%>% 
  select(.model,adj_r_squared, CV, AIC, AICc, BIC)











