library(feasts)
library(fable)
library(forecast)
library(fpp3)
library(lubridate)
library(TSA)


#공급-수요
ts.remain %>% 
  autoplot()

#분해법
ts.remain=as_tsibble(blood$적혈구제제공급량-pat$적혈구제제수요량)

colnames(ts.remain)=c('시점','remain')


#원변수 ACF, PACF 확인
ts.remain %>% 
  gg_tsdisplay(plot_type = 'partial', lag_max = 25)


#계절성 확인
ts.remain %>% 
  gg_season(remain, period = 12)

ts.remain %>% 
  gg_subseries(remain, period=12)

#lag plot으로 시차별 상관정도 확인
ts.remain %>% 
  gg_lag(remain, geom='point', lags = 1:12)

#ACF 여러 정보 확인
ts.remain %>% 
  features(
    .var = remain,
    features = feat_acf
  )

#차분 따른 그래프
ts.remain %>% 
  transmute(
    `원변수` = remain,
    `dif` = difference(remain,1),
    `season` = difference(remain, 12), # Annual data이기에 12
    `season_dif` = difference(difference(log(remain), 12), 1)
  ) %>% 
  pivot_longer(
    cols = -시점,
    names_to = "Type",
    values_to = "remain"
  ) %>% 
  mutate(Type = factor(Type, levels = c("원변수", 
                                        "dif",
                                        "season", 
                                        "season_dif"))) %>% 
  ggplot(aes(x = 시점, y = remain)) +
  geom_line(size = 0.7) +
  facet_wrap(. ~ Type, scales = "free_y", nrow = 4) +
  labs(title = "차분 따른 그래프 개형", y = NULL)


#단위근 검정
ts.remain %>%
  features(.var = remain, feature = unitroot_kpss)
#p value 0.0438 로 기각하여 차분 필요

ts.remain %>% 
  features(.var = remain, feature = c(unitroot_nsdiffs,unitroot_ndiffs))
#1차차분 필요

ts.remain %>% 
  mutate(dif_remain = difference(remain, 1)) %>%
  features(.var = dif_remain, feature = c(unitroot_nsdiffs,unitroot_ndiffs))
#1차 차분만 해서보니 계절차분 필요하다고 나옴

ts.remain %>% 
  mutate(dif_remain = difference(remain, 1)) %>% 
  select(dif_remain) %>% 
  gg_tsdisplay(plot_type = 'partial', lag_max = 25)


#계절 차분 후 1차 차분
ts.remain %>% 
  mutate(sdif_remain = difference(difference(remain, 12),1)) %>%
  features(.var = sdif_remain, feature = c(unitroot_nsdiffs,unitroot_ndiffs))
#1차 차분만 해서보니 계절차분 필요하다고 나옴

ts.remain %>% 
  mutate(sdif_remain = difference(difference(remain, 12),1)) %>% 
  select(sdif_remain) %>% 
  gg_tsdisplay(plot_type = 'partial', lag_max = 25)










