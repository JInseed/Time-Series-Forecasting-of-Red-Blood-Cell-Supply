library(tidyverse)
library(lubridate)
library(gridExtra)

acc = read.csv('서울시월별교통사고.csv', header=T, skip=1)

blood = read.csv('서울시월별헌혈현황.csv')

colnames(acc)=c('시점','합계')



blood

p1=acc %>% 
  filter(합계 > 1000) %>% 
  mutate(시점=ym(시점)) %>% 
  filter(시점 > ym('2015.01')) %>%
  ggplot()+
  geom_point(aes(시점,합계))+
  geom_line(aes(시점,합계),
            size=1,
            col='red',
            alpha=0.3,
            stat = 'identity')


p2=blood %>% 
  filter(합계 > 1000) %>% 
  mutate(시점=ym(시점)) %>%
  filter(시점 > ym('2015.01')) %>% 
  ggplot()+
  geom_point(aes(시점,합계))+
  geom_line(aes(시점,합계),
            size=1,
            col='blue',
            alpha=0.3,
            stat = 'identity')


grid.arrange(p1, p2, ncol=2)

cor(acc['합계'],blood['합계'])




acc2=acc %>% 
  filter(합계 > 1000) %>% 
  mutate(acc시점=ym(시점)) %>% 
  mutate(acc합계=scale(합계)) %>% 
  filter(acc시점 > ym('2015.01')) %>% 
  select(-합계,-시점)
  


blood2=blood %>% 
  filter(합계 > 1000) %>% 
  mutate(b시점=ym(시점)) %>%
  mutate(b합계=scale(합계)) %>% 
  filter(b시점 > ym('2015.01'))%>% 
  select(-합계,-시점)




ab=cbind(acc2,blood2)

ab
ab %>% ggplot()+
  geom_point(aes(acc시점,acc합계))+
  geom_line(aes(acc시점,acc합계),
            size=1,
            col='red',
            alpha=0.3,
            stat = 'identity')+
  geom_point(aes(b시점,b합계))+
  geom_line(aes(b시점,b합계),
            size=1,
            col='blue',
            alpha=0.3,
            stat = 'identity')



