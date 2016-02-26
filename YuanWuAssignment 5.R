#Econ 294A2
#Assignment 5
#Yuan Wu
#1307193

library(ggplot2)
library(scales)
library(dplyr)

#Question 1
#a.
plot1 <- ggplot(
  data = diamonds, 
  aes(x = x*y*z,    #x axis=x*y*z
      y = price)) + #y axis=price
  scale_x_log10() + #set axis sclaes to log10
  scale_y_log10() + 
  geom_point(aes(color = clarity), alpha = 0.2) + #dot plot color coded according to clarity, transparancy(alpha)=0.2
  aes(size = carat) + #dot plot size coded according to carat
  scale_size(range=c(5,10)) #set dots size 5 in smallest, 10 in largest
plot1

#b.
plot2<- ggplot(
  data = diamonds,
  aes(x=carat, #x axis=carat, 
      y=..density.., #y axis= density,
      fill = clarity)) +  #color coded fill by clarity
  geom_histogram(binwidth = 0.2) + #bindwidth=0.2 historgram
  facet_grid(cut ~ .) #vertical separation by cut
plot2

#c.
plot3<- ggplot(
  data = diamonds,
  aes(y = price, #x axis = price
      x = cut)) + #y axis = cut
  geom_violin() + #violin plot
  geom_jitter(alpha = 0.01) #jitter dot fill
plot3

#Question 2
#a.
load(url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.RData"))
org_example2<-subset(org_example, !is.na(rw))
bymonth<-org_example2 %>% 
  group_by(year,month) %>% 
  mutate(
educ=educ,
median.rm=median(rw),
quar1st=quantile(rw, probs = 0.25, na.rm = T, names = TRUE, type = 7),
quar3rd=quantile(rw, probs = 0.75 , na.rm = T, names = TRUE, type = 7), 
decl1st=quantile(rw, probs = 0.1, na.rm = T, names = TRUE, type = 5) ,
decl9th=quantile(rw, probs = 0.9, na.rm = T, names = TRUE, type = 5),
total = n()
) %>% select(year, month, median.rm, quar1st, quar3rd, decl1st, decl9th, educ) %>%
  arrange(year,month)
#Create data frame that contains monthly median and percentiles, grouped by year

bymonth<-bymonth %>%
  mutate(
date = paste(year, month, "01", sep = "-"),
date = as.Date(date, format = "%Y-%m-%d")
)
#Convert year and month into date variable, filling days with "1".

plot4<-ggplot(
  data=bymonth,
  aes(date)) + #x axis=date
  geom_line(aes(y = median.rm)) + #line of median real wage, y axis=median real wage
  geom_ribbon(aes(ymin = quar1st, ymax = quar3rd), alpha=0.4) + #ribbon for quartiles
  geom_ribbon(aes(ymin = decl1st, ymax = decl9th), alpha=0.2) + #ribbon for deciles
  ylim(0, 50) # y axis from 0 to 50
plot4

#b.
bymonth2<-subset(org_example2, !is.na(educ))
bymonth2<-org_example2 %>% 
  group_by(year,month,educ) %>% 
  mutate(
    median.rm=median(rw),
    total = n()
  ) %>% select(year, month, median.rm, educ) %>%
  arrange(year,month)
#Create data frame that contains monthly median, grouped by year and education

bymonth2<-bymonth2 %>%
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  )
#Convert year and month into date variable, filling days with "1".

plot5<-ggplot(
  data=bymonth2,
  aes(x=date,y=median.rm)) +  
  geom_line(aes(color=educ)) #line of median real wage, colored coded by education groups
plot5
