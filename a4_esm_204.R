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
damages_baseline=fit2$coefficient[3]*warmingdata$warming_baseline^2 + fit2$coefficient[2]*warmingdata$warming_baseline #use equation found in #1 to estimate new curve
damages_pulse=fit2$coefficient[3]*warmingdata$warming_pulse^2 + fit2$coefficient[2]*warmingdata$warming_pulse #use equation found in #1 to estimate new curve with pulse
appended <- warmingdata %>% 
  mutate(damages_baseline = damages_baseline) %>% 
  mutate(damages_pulse = damages_pulse) %>% 
  mutate(damages_diff= damages_pulse - damages_baseline) %>% 
  mutate(diff_co2=damages_diff/35000000000)
#plot 1: damages baseline
ggplot(appended, aes(x=year, y=damages_baseline))+
  labs(y="Damage ($)", x="Year", title="Baseline Damages without Pulse") +
  geom_point(color="indianred3")+
  theme_minimal()
#plot 2: damages w pulse
ggplot(appended, aes(x=year, y=damages_pulse))+
  labs(y="Damage ($)", x="Year", title="Damages with Pulse") +
  geom_point(color="skyblue3")+
  theme_minimal()
#plot 3
ggplot(appended, aes(x=year, y=damages_diff))+
  labs(y="Damage ($)", x="Year", title="Difference in Damages") +
  geom_point(color="darkseagreen4")+
  theme_minimal()
#plot 4:
ggplot(appended, aes(x=year, y=diff_co2))+
  labs(y="Damage ($)", x="Year", title="Difference in Damages per ton of CO2") +
  geom_point(color="plum4")+
  theme_minimal()
#-----------------------------------------
#QUESTION 3
pv <- function(num, dr) {
  sum <- 0;
  r <- dr
  for(t in c(1:length(num))){
    present <- num[t]/(1+r)^t
    sum <- sum+present
  }
  return(sum)
}
discount_rate <- seq(.01, .2, .001)
pv(appended$diff_co2, discount_rate)
scc <- data.frame(discount_rate<-discount_rate, scc<-pv(appended$diff_co2, discount_rate))
colnames(scc) = c("discount_rate", "scc")
#plot
ggplot(scc, aes(x=discount_rate, y=scc)) +
  labs(y="Cost ($)", x="Discount Rate", title="Social Cost of Carbon") +
  geom_line(color="salmon2") +
  theme_bw()
#-----------------------------------------
#QUESTION 4
#create function to find ramsey DR
find_dr <- function(p, n, g){
  r <- p + n*g
}
return(r)

dr_optimum <- find_dr(.001, 2, .01)

dr_optimum

pv(num, dr_optimum)

#create function to find cost at ramsey DR

present_val <- function(V, r, t)



#from Q3
discount_rate <- seq(.01, .2, .001)
pv(appended$diff_co2, discount_rate)
scc <- data.frame(discount_rate<-discount_rate, scc<-pv(appended$diff_co2, discount_rate))
colnames(scc) = c("discount_rate", "scc")
#plot
ggplot(scc, aes(x=discount_rate, y=scc))+
  geom_vline(xintercept = dr_optimum, 
             linetype = "dashed", 
             color = "red", 
             size=1, 
             alpha = 0.6)+
  labs(y="Cost ($)", x="Discount Rate", title="Social Cost of Carbon") +
  geom_line(color="salmon2")+
  theme_bw()+
  geom_point(aes(x=0.021, y=68.378453), shape=19, size=3, colour = "black")+
  annotate(geom="text", x=0.033, y=75, label="(0.021, 68.378)")

#------------------------------------------
#QUESTION 5

quadratic = fit2$coefficient[3]*warming^2 + fit2$coefficient[2]*warming

baseline <- pv(appended$damages_baseline, discount_rate)

#baseline * 1.5
baseline_15 <- appended$warming_baseline*1.5
damages_15 = fit2$coefficient[3] * baseline_15^2 + fit2$coefficient[2] * baseline_15

warmingx15 =data.frame(appended$year, appended$warming_baseline, damages_15)

baseline_15_damages <- pv(warmingx15$damages_15, discount_rate) 

policyA = data.frame(discount_rate, baseline, baseline_15_damages)

#Policy B

stable = rep(1.29, 50)
warming30years=appended[1:30, 3]
polbwarming=c(warming30years,stable)
#damage function
damagepolb=fit2$coefficient[3]*polbwarming^2 + fit2$coefficient[2]*polbwarming

pv_B = pv(damagepolb, discount_rate)
PolicyB = data.frame(discount_rate, pv_B)
colnames(PolicyB) = c("Discount Rate", "Present Value")

