library(tidyverse)
library(quantmod)
library(lubridate)

# Extract data from Yahoo ----
getSymbols('^MXX', src='yahoo', from='2006-01-01',
           to=Sys.Date(), periodicity='daily',
           format='xts')
IPC <- MXX$MXX.Adjusted

xts_to_dataframe <- function(data_xts){
  df_t <- data.frame(date=(index(data_xts)),
                     value=coredata(data_xts))
  colnames(df_t)<-c('date', 'value')
  df_t
}

IPC<-na.omit(IPC)
IPC_df<-xts_to_dataframe(IPC)

#View(IPC)
#View(IPC_df)

# Data visualization ----
ggplot(IPC_df, aes(y=value, x=date))+
  geom_line(color='#00AFEB') +
  geom_area(alpha=0.6)+
  scale_x_date(date_breaks = '1 year',
               date_labels='%Y')+
  theme_minimal()+
  labs(title='IPC of BMV',
       subtitle='From 2006 to 2021',
       caption='Yahoo data',
       x='Date',
       y='IPC/BMV')


# Time decomposition ----
IPC_ts<-ts(IPC_df$value, frequency=252,
           start=c(2006, 01))
plot(decompose(IPC_ts))

IPC_component<-data.frame(date=time(IPC_ts),
                          IPC_ts=IPC_ts,
                          seasonal=decompose(IPC_ts)$seasonal,
                          trend=decompose(IPC_ts)$trend,
                          random=decompose(IPC_ts)$random)

#head(IPC_component)

IPC_component %>%
  gather('id_var', 'values', -date) %>%
  ggplot(aes(x=date, y=values))+
  geom_line(aes(color=id_var))+
  facet_wrap(.~id_var, scales='free_y')+
  theme_minimal()+xlab('')+ylab('')+
  theme(legend.position='none')+
  labs(title='Time decomposition')

# Lags and differences ----
lags<-IPC_df %>%
  mutate(date=ymd(date),
         lag1=lag(value, k=1),
         d1=diff(IPC, differences=1),
         d2=diff(IPC, differences=2),
         d1log=log(value)-lag(log(value)),
         relative_tc=((value/lag(value, 1))-1)*100)

# Time components ----
IPC_d<-IPC_df %>%
  mutate(year=format(date, '%Y'),
         week_day=format(date, '%A'),
         week_day_num=wday(date),
         day_month=format(date, '%W'),
         month_text=format(date, '%M'),
         month_num=format(date, '%m'),
         quarter=as.yearqtr(date, '%Q'),
         tc_IPC=((value/lag(value, 1))-1)*100)

# Stats grouped by temporality ----
IPC_d %>%
  group_by(year) %>%
  summarise(pos_average=mean(tc_IPC[tc_IPC>0], na.rm=T),
           neg_average=mean(tc_IPC[tc_IPC<0], na.rm=T),
           total_average=mean(tc_IPC, na.rm=T))

IPC_d %>%
  filter(year==2008) %>%
  group_by(week_day) %>%
  summarise(day_avg=mean(tc_IPC, na.rm=T))


# Variation rate during 2008 ----
IPC_d %>% 
  mutate(month_name=month(date, label=T)) %>% 
  filter(year==2008) %>% 
  ggplot(aes(x=month_name, y=tc_IPC))+
  geom_boxplot(fill='blue')+
  theme_minimal()

# Historical distribution of the variables ----
IPC_d %>%
  group_by(year) %>% 
  ggplot(aes(x=year, y=tc_IPC))+
  geom_boxplot(fill='white', color='#3366FF',
               outlier.color='red')+
  theme_minimal()


# Histogram ----
ggplot(IPC_d, aes(tc_IPC))+
  geom_histogram(col='black',
                 fill='red',
                 alpha=0.4)+
  theme_minimal()+
  geom_vline(xintercept=0, color='black', size=0.7)+
  labs(title='IPC Returns',
       x='Returns',
       y='Frequency')

# QQplot ----
ggplot(IPC_d, aes(sample=tc_IPC))+
  stat_qq(color='red')+
  stat_qq_line(color='black')+
  theme_minimal()
