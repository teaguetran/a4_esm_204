library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(kableExtra)
options(scipen=0)
damagesdata=read.csv(here("Data", "damages.csv"))
warmingdata=read.csv(here("Data", "warming.csv"))
#-----------------------------------------
### QUESTION 1
#ggplot to check it out
ggplot(damagesdata, aes(x=warming, y=damages))+
  geom_point()
#fitting a quadratic formula to data
attach(damagesdata)
plot(warming, damages, main="Scatterplot", pch=19)
fit2<-lm(damages~poly(warming,2,raw=TRUE))
fit2$coefficient[1]
fit2$coefficient[2]
fit2$coefficient[3]
quadratic = fit2$coefficient[3]*warming^2 + fit2$coefficient[2]*warming
quadratic
plot(warming, damages, main="Scatterplot")
par(new = TRUE)
lines(warming,quadratic, col="red")
ggplot(damagesdata, aes(x=warming, y=damages))+
  geom_point(col="grey54", size=1)+
  geom_line(aes(x=warming, y=quadratic), col="indianred3")+
  theme_bw()+
  labs(x="Warming (deg C)", y="Damage ($)", title="Annual Total Damages from Climate Change")
#-----------------------------------------
### QUESTION 2
damages_baseline=fit2$coefficient[3]*warmingdata$warming_baseline^2 + fit2$coefficient[2]*warmingdata$warming_baseline
damages_pulse=fit2$coefficient[3]*warmingdata$warming_pulse^2 + fit2$coefficient[2]*warmingdata$warming_pulse
appended <- warmingdata %>% 
  mutate(damages_baseline = damages_baseline) %>% 
  mutate(damages_pulse = damages_pulse) %>% 
  mutate(damages_diff= damages_pulse - damages_baseline) %>% 
  mutate(diff_co2=damages_diff/35000000000)
#plot 1: damages baseline
ggplot(appended, aes(x=year, y=damages_baseline))+
  labs(y="Damage ($)", x="Year", title="Baseline Damages without Pulse") +
  geom_point(color="indianred3")+
  theme_bw()
#plot 2: damages w pulse
ggplot(appended, aes(x=year, y=damages_pulse))+
  labs(y="Damage ($)", x="Year", title="Damages with Pulse") +
  geom_point(color="skyblue3")+
  theme_bw()
#plot 3
ggplot(appended, aes(x=year, y=damages_diff))+
  labs(y="Damage ($)", x="Year", title="Difference in Damages") +
  geom_point(color="darkseagreen4")+
  theme_bw()
#plot 4:
ggplot(appended, aes(x=year, y=diff_co2))+
  labs(y="Damage ($)", x="Year", title="Difference in Damages per ton of CO2") +
  geom_point(color="plum4")+
  theme_bw()
#-----------------------------------------
#QUESTION 3
pv <- function(num, dr = .02) {
  sum <- 0;
  r <- dr
  for(t in c(1:length(num))){
    present <- num[t]/(1+r)^t
    sum <- sum+present
  }
  return(sum)
}
discount_rate <- seq(.01, .1, .005)
pv(appended$diff_co2, discount_rate)
scc <- data.frame(discount_rate<-discount_rate, scc<-pv(appended$diff_co2, discount_rate))
colnames(scc) = c("discount_rate", "scc")
#plot
ggplot(scc, aes(x=discount_rate, y=scc))+
  labs(y="Cost ($)", x="Discount Rate", title="Social Cost of Carbon") +
  geom_line(color="salmon2")+
  theme_bw()