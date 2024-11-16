library(tidyverse)


#헌혈건수 및 외상환자수 시각화
p1=blood %>% 
  ggplot()+
  geom_point(aes(시점,헌혈건수))+
  geom_line(aes(시점,헌혈건수),
            size=1,
            col='red',
            alpha=0.3,
            stat = 'identity')+
  ggtitle("월별헌혈건수")+ theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))
  

p2=pat %>%
  ggplot()+
  geom_point(aes(시점,환자수합계))+
  geom_line(aes(시점,환자수합계),
            size=1,
            col='blue',
            alpha=0.3,
            stat = 'identity')+
  ggtitle("월별 중증외상환자 수 합계")+theme(
              plot.title = element_text(size=20, face="bold"),
              axis.title.x = element_text(size=14, face="bold"),
              axis.title.y = element_text(size=14, face="bold"))

grid.arrange(p1, p2, ncol=2)


#적혈구제제 수요량 및 공급량 시각화
p3=blood %>% 
  ggplot()+
  geom_point(aes(시점,적혈구제제공급량))+
  geom_line(aes(시점,적혈구제제공급량),
            size=1,
            col='red',
            alpha=0.3,
            stat = 'identity')+
  ggtitle("월별 적혈구제제 공급량")+theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))

p4=pat %>%
  ggplot()+
  geom_point(aes(시점,적혈구제제수요량))+
  geom_line(aes(시점,적혈구제제수요량),
            size=1,
            col='blue',
            alpha=0.3,
            stat = 'identity')+
  ggtitle("월별 적혈구제제 수요량")+theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))


grid.arrange(p3, p4, ncol=2)


#적혈구제제 공급 및 수요 그래프 겹쳐 그리기
blood2 = blood %>% 
  mutate(b시점=시점)

pat2 = pat %>% 
  mutate(p시점=시점) %>% 
  select(-시점, -t)

df=cbind(blood2,pat2)


df %>% ggplot()+
  geom_point(aes(b시점,적혈구제제공급량))+
  geom_line(aes(b시점,적혈구제제공급량),
            size=1,
            col='red',
            alpha=0.3,
            stat = 'identity')+
  geom_point(aes(p시점,적혈구제제수요량))+
  geom_line(aes(p시점,적혈구제제수요량),
            size=1,
            col='blue',
            alpha=0.3,
            stat = 'identity')


#적혈구제제 공급-수요 그래프
df %>% 
  mutate(gap=적혈구제제공급량-적혈구제제수요량)%>% 
  ggplot()+
  geom_point(aes(시점,gap))+
  geom_line(aes(시점,gap),
            size=1,
            col='purple',
            alpha=0.3,
            stat = 'identity')+
  ggtitle("월별 적혈구제제 공급량-수요량")+theme(
    plot.title = element_text(size=20, face="bold"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))



view(df)
colnames(df)




########################################################
ts.plot(ts.bl$적혈구제제공급량-ts.pat$적혈구제제수요량)
ts.plot(ts.bl$적혈구제제공급량)


head(ts.bl$적혈구제제공급량)
head(ts.pat$적혈구제제수요량)


