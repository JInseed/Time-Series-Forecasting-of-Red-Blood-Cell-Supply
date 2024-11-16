#차분 따른 그래프
ts.bl %>% 
  transmute(
    `원변수` = 적혈구제제공급량,
    `Log sales` = log(적혈구제제공급량),
    `Annual change in log sales` = difference(적혈구제제공급량, 12), # Annual data이기에 12
    `Doubly differenced log sales` = difference(difference(log(적혈구제제공급량), 12), 1)
  ) %>% 
  pivot_longer(
    cols = -시점,
    names_to = "Type",
    values_to = "적혈구제제공급량"
  ) %>% 
  mutate(Type = factor(Type, levels = c("원변수", 
                                        "Log sales",
                                        "Annual change in log sales", 
                                        "Doubly differenced log sales"))) %>% 
  ggplot(aes(x = 시점, y = 적혈구제제공급량)) +
  geom_line(size = 0.7) +
  facet_wrap(. ~ Type, scales = "free_y", nrow = 4) +
  labs(title = "Coricosteroid durg 적혈구제제공급량", y = NULL)


#Unit root tests
ts.bl %>%
  features(.var = 적혈구제제공급량, feature = unitroot_kpss)
#p value 0.1 로 기각안하므로 차분 할 필요 없음

ts.bl %>% 
 features(.var = 적혈구제제공급량, feature = c(unitroot_nsdiffs,unitroot_ndiffs))
#계절 차분은 필요해보임

ts.bl %>% 
  mutate(season_공급 = difference(적혈구제제공급량, 12)) %>%
  features(.var = season_공급, feature = c(unitroot_nsdiffs,unitroot_ndiffs))

ts.bl %>% 
  mutate(d_season_공급 = difference(difference(적혈구제제공급량, 12),1)) %>%
  features(.var = d_season_공급, feature = c(unitroot_nsdiffs,unitroot_ndiffs))
#계절 차분 후 1차 차분까지 필요할 것임을 확인



#ARIMA
#ARIMA() : 각 차수 자동으로 선택
#ARIMA()에서 자동이 아닌 수동으로 차수를 지정하여 모형을 적합시킬 때에는 pdq()를 활용
fit <- ts.bl %>% 
  model(ARIMA(적혈구제제공급량))

fit %>% 
  report()

fit %>% 
  residuals() %>% 
  ggAcf()

fit %>% 
  residuals() %>% 
  ggPacf()

#한번에 그리기
fit %>% 
  residuals() %>% 
  gg_tsdisplay(plot_type = 'partial')


#탐색
caf_fit <- ts.bl %>% 
  model(
    arima013 = ARIMA(적혈구제제공급량 ~ pdq(0, 1, 3)),
    arima210 = ARIMA(적혈구제제공급량 ~ pdq(2, 1, 0)),
    stepwise = ARIMA(적혈구제제공급량),
    search = ARIMA(적혈구제제공급량, stepwise = FALSE)
  )


caf_fit %>% 
  pivot_longer(
    cols = 1:4,
    names_to = "ModelName",
    values_to = "Orders"
  )

caf_fit %>% 
  glance() %>% 
  arrange(AICc) %>% 
  select(.model, AICc)






