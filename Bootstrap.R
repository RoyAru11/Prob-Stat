install.packages("mosaic")
library(tidyverse)
library(mosaic)
NHANES_sleep <- read.csv("C:/Users/Arunabho Roy/Downloads/NHANES_sleep.csv")
View(NHANES_sleep)
head(NHANES_sleep, 5)
ggplot(NHANES_sleep) + 
  geom_histogram(aes(x = SleepHrsNight), binwidth=1)
mean(~SleepHrsNight, data=NHANES_sleep)
NHANES_sleep_bootstrap = mosaic::resample(NHANES_sleep)
mean(~SleepHrsNight, data=NHANES_sleep_bootstrap)

boot_sleep = do(10000)*mean(~SleepHrsNight, data=mosaic::resample(NHANES_sleep))
head(boot_sleep)
ggplot(boot_sleep) + 
  geom_histogram(aes(x=mean))

boot_sleep %>%
  summarize(std_err_sleep = sd(mean))
boot_sleep %>%
  summarize(prob_within_1se = sum(mean > 6.88 - 0.03 & mean < 6.88 + 0.03))

confint(boot_sleep, level=0.95)
boot_sleep = do(10000)*mean(~SleepHrsNight, data=mosaic::resample(NHANES_sleep))

ggplot(boot_sleep) + 
  geom_histogram(aes(x=mean))

#Original Data Histogram Plot
confint(boot_sleep, level = 0.95)
ggplot(NHANES_sleep) + 
  geom_histogram(aes(x = SleepHrsNight), binwidth=1)
NHANES_sleep = NHANES_sleep %>%
  mutate(DepressedAny = ifelse(Depressed != "None", yes=TRUE, no=FALSE))

prop(~DepressedAny, data=NHANES_sleep)
boot_depression = do(10000)*prop(~DepressedAny, data=mosaic::resample(NHANES_sleep))
head(boot_depression)

ggplot(boot_depression) + 
  geom_histogram(aes(x=prop_TRUE))
confint(boot_depression, level = 0.95)

# Sleep Hours by gender
mean(SleepHrsNight ~ Gender, data=NHANES_sleep)
diffmean(SleepHrsNight ~ Gender, data=NHANES_sleep)
boot_sleep_gender = do(10000)*diffmean(SleepHrsNight ~ Gender, data=mosaic::resample(NHANES_sleep))
head(boot_sleep_gender)
ggplot(boot_sleep_gender) +
  geom_histogram(aes(x=diffmean))
confint(boot_sleep_gender, level = 0.95)

ggplot(NHANES_sleep) +
  geom_jitter(aes(x=Age, y=SleepHrsNight), alpha=0.1)

lm_sleep_age = lm(SleepHrsNight ~ Age, data = NHANES_sleep)
coef(lm_sleep_age)
boot_sleep_age = do(10000)*lm(SleepHrsNight ~ Age, data=mosaic::resample(NHANES_sleep))
confint(boot_sleep_age, level=0.95)
ggplot(NHANES_sleep) + 
  geom_jitter(aes(x=Age, y=SleepHrsNight), alpha=0.1) + 
  geom_smooth(aes(x=Age, y=SleepHrsNight), method='lm')

#Statistical vs. practical significance
confint(boot_sleep_age, level = 0.95) %>% filter(name == 'Age')
sd(~SleepHrsNight, data = NHANES_sleep)

boot_sleep = do(10000)*mean(~SleepHrsNight, data=mosaic::resample(NHANES_sleep))
confint(boot_sleep, level = 0.95)

# Does Bootstrapping always work
rapidcity <- read.csv("C:/Users/Arunabho Roy/Downloads/rapidcity.csv")
view(rapidcity)

# Example 1 Sample Mean
mean(~Temp, data=rapidcity)

rapidcity_sample = sample_n(rapidcity, 50)
mean(rapidcity_sample$Temp, na.rm = TRUE)

rapidcity_sample_boot = do(10000)*mean(~Temp, data=mosaic::resample(rapidcity_sample))
confint(rapidcity_sample_boot, level = 0.95)

rapidcity_sample2 = sample_n(rapidcity, 50)
mean(~Temp, data=rapidcity_sample2)
rapidcity_sample_boot2 = do(10000)*mean(~Temp, data=mosaic::resample(rapidcity_sample2))
confint(rapidcity_sample_boot2, level = 0.95)

# Example 2 Sample Minimum
min(~Temp, data=rapidcity)
rapidcity_sample = sample_n(rapidcity,50)
min(~Temp, data = rapidcity_sample)
rapidcity_sample_boot_min = do(1000)*min(~Temp, data=mosaic::resample(rapidcity_sample))
confint(rapidcity_sample_boot_min, level = 0.95)

rapidcity_sample2 = sample_n(rapidcity, 50)
min(~Temp, data=rapidcity_sample2)
rapidcity_sample_boot_min2 = do(1000)*min(~Temp, data=mosaic::resample(rapidcity_sample2))
confint(rapidcity_sample_boot_min2, level = 0.95)
# Conclusion Bootstrap does not work on Min and Max 



