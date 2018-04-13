# 6240-hw1
---
title: "6240 hw1"
author: "YY"
date: "2/1/2018"
output:
  html_document: default
  pdf_document: default
---
You can find the full assignment at https://github.com/wikimedia-research/ Discovery-Hiring-Analyst-2016. This is a fairly standard data wrangling assignment.

```{r}
library(tidyverse)
```
```{r}
#import dataset and name data as event
event<-read.csv('/Users/yang/Desktop/graduate/6240/hw1/events_log.csv')
head(event)
```
```{r}
#create new variable daily
event$day<-substr(event$timestamp,7,8)
```


```{r}
a=event %>% filter(day!='30') %>% group_by(group,day)  %>% filter(action=='visitPage') %>% summarise( count=n_distinct(session_id))
b=event %>% filter(day!='30') %>% group_by(group,day)  %>% filter(action=='searchResultPage') %>% summarise( count=n_distinct(session_id))
a$clickthrough_rate=rate=a$count/b$count
```
```{r}
library(ggplot2)
a %>% group_by(day) %>% summarise(daily_average_clickthrough=mean(clickthrough_rate)) %>% ggplot(aes(x=day, y=daily_average_clickthrough,fill=day))+geom_bar(stat = 'identity')
a %>% group_by(group) %>% summarise(group_average_clickthrough=mean(clickthrough_rate)) %>% ggplot(aes(x=group, y=group_average_clickthrough,fill=group))+geom_bar(stat = 'identity')
```





#question2: Which results do people tend to try first? How does it change day-to-day?
```{r}
first_try=event %>% group_by(day,session_id) %>% filter(action!='searchResultPage',day!=30) %>% arrange(timestamp) %>% summarise(first=first(result_position)) %>%  group_by(day,first) %>% summarise(count=n()) %>% mutate(first_proportion=count/sum(count))
```
```{r}
first_try %>%filter(first<20) %>%  ggplot(aes(x=first, y=first_proportion, color=day))+geom_point()+geom_line()
```




#question3: What is their daily overall zero results rate? How does it vary between the groups?
```{r}
zero_rate=event %>% filter(day!='30') %>%group_by(group,day) %>%  filter(action=='searchResultPage') %>% summarise(zerorate=mean(n_results==0)) 

zero_rate %>% group_by(day) %>% summarise(average_zerorate=mean(zerorate)) %>% ggplot(aes(x=day, y=average_zerorate,fill=day))+geom_bar(stat = 'identity')
zero_rate %>% group_by(group) %>% summarise(average_zerorate=mean(zerorate)) %>% ggplot(aes(x=group, y=average_zerorate,fill=group))+geom_bar(stat = 'identity')

```
#question4

```{r}
library(lubridate)
#explore the relationship between group and session time interval
event %>% mutate(timestamp=ymd_hms(timestamp)) %>% group_by(group,session_id) %>% arrange(timestamp) %>% summarise(session_timeinterval=last(timestamp)-first(timestamp)) %>% summarise(session_length=mean(session_timeinterval)) %>% ggplot(aes(x=group, y=session_length,fill=group))+geom_bar(stat = 'identity')
```

summary: 
The daily overall clickthrough rate is almost same. Group a has higher clickthrogh rate compared with group b. 
People tend to try first result. The proportatin of trying first position is up to 65%. The daily proportation rate of first try result is almost same in eight days. 
The average daily zero rate is equal to 0.1848. In terms of group and daily, zero rate is almost same. 
The average session lenght is 12.79 seconds in groupa and 3.746 in group b. Hence, the group a has much larger average session lenght
