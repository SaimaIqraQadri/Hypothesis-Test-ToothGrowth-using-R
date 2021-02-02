library(UsingR)
library(tidyverse)
data("ToothGrowth")
View(ToothGrowth)

str(ToothGrowth)

summary(ToothGrowth$len)
sd(ToothGrowth$len)

unique(ToothGrowth$supp)
unique(ToothGrowth$dose)
table(ToothGrowth$dose)
table(ToothGrowth$dose, ToothGrowth$supp)

#let's find average length/growth of tooth, grouped by supp nature
ToothGrowth %>% 
  group_by(supp) %>% 
  summarise(mean(len))

#Tooth Growth BY Dose
ggplot(ToothGrowth, aes(x = dose , y = len)) +
  geom_point()

ggplot(ToothGrowth, aes(x = dose , y = len)) +
  geom_point() +
  facet_wrap(~supp)


#Tooth Growth BY supp
ggplot(ToothGrowth, aes(x=supp, y=len)) + 
  geom_boxplot()
##tooth growth due to OJ supp is greater than the VC supp

#HYPOTHESIS TEST
##Null Hypothesis: There is no difference between the supp, the length remains
##the same
##Alternative Hypothesis: Tooth Growth by OJ supp is > VC supp
###Assupmtion: sample is random & independent, and noramally distributed
                
t.test(len ~ supp, data = ToothGrowth)
##p-value is greater than 0.05, so FAIL TO REJECT THE NULL HYPOTHESIS


#Tooth Growth by supp and dose
ggplot(ToothGrowth, aes(x=supp, y=len)) + 
  geom_boxplot() + 
  facet_wrap(~ dose)
#tooth growth due to OJ supp is greater than the VC supp when the quantity of
#dose is 0.5 and 1 
#When quantity of dose is 2, there is no difference


#HYPOTHESIS TEST FOR EACH DOSE

##when giving 0.5 dose, impact of supp on len
###Ho: no difference b/w the supp
###Ha: OJ supp has greater impact on growth and VC supp has lower impact on growth
####Assupmtion: sample is random & independent, and noramally distributed


dose1 <- filter(ToothGrowth, dose==0.5)
view(dose1)
t.test(len ~ supp, data = dose1)
### p-value is very low so REJECT NULL HYPOTHESIS in the fevor of alternatice hypothesis

dose2 <- filter(ToothGrowth, dose == 1)
view(dose2)
t.test(len ~ supp, data = dose2)
### again p-value is low so REJECT NULL HYPOTHESIS in favor of alternative hypothesis

dose3 <- filter(ToothGrowth, dose == 2)
view(dose3)
t.test(len ~ supp, data = dose3)
### p-value is greater then 0.05 so Fail TO REJECT NULL HYPOTHESIS


#CONCLUSION:
# 1; Tooth Growth due to 'OJ' supp is greater than the 'VC' supp
# 2; when we compare the tooth growth according to different doses of supp,
#    we see that In 0.5 and 1 quantity of dose the OJ supp has greater impact
#    rather than VC supp on Growth. but when give 2 quantity of dose the 
#    supplements effect is same on Growth