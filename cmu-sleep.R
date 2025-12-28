library(infer)
library(tidyverse)
library(broom)
library(dplyr)
sleep <- read_csv("~/Downloads/cmu-sleep.csv") %>% 
  mutate(demo_gender = recode(demo_gender, "1" = "female", "0" = "male"),
         demo_race = recode(demo_race, "1" = "non-under", "0" = "under"),
         demo_firstgen = recode(demo_firstgen, "1" = "first", "0" = "non-first"),
         study = as.factor(study))
firstgen_gpa <- sleep %>% filter(!is.na(demo_firstgen))
gender_gpa <- sleep %>% filter(!is.na(demo_gender))
race_gpa <- sleep %>% filter(!is.na(demo_race))
firstgen_gpa %>% ggplot(aes(x = demo_firstgen, y = cum_gpa)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(width = 0.2)
alpha <- 0.01
obsdiff_firstgen <- firstgen_gpa %>% specify(formula = cum_gpa~demo_firstgen) %>% 
  calculate(stat = "diff in means", order = c("first", "non-first"))
#construct a null distribution of 10000 students, take the 634 data as sample
null_dist_firstgen_gpa <- firstgen_gpa %>% 
  specify(formula = cum_gpa~demo_firstgen) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 10000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("first", "non-first"))
null_dist_firstgen_gpa %>% visualise() + shade_p_value(obsdiff_firstgen, direction = "less")
p_val <- null_dist_firstgen_gpa %>% get_p_value(obs_stat = obsdiff_firstgen, direction = "less")
#first-generation of uni students are likely to achieve lower overall gpa than those whose parents 
#once received the higher education, with significance level of less than 0.05
tidy(t.test(x=(firstgen_gpa %>% filter(demo_gender == "female"))$cum_gpa,
            y=(firstgen_gpa %>% filter(demo_gender == "male"))$cum_gpa,
            alternative = "two.sided"))
#The gender difference has no great influence on cumulative gpa
tidy(t.test(x=(race_gpa %>% filter(demo_race == "non-under"))$cum_gpa,
            y=(race_gpa %>% filter(demo_race == "under"))$cum_gpa,
            alternative = "greater"))
#students from the underrepresented race are less likely to achieve higher cumulative gpa than
#those from the non-under race.
alpha <- 0.05
sd_diff <- sleep %>% select(study, TotalSleepTime) %>% group_by(study) %>% 
  summarize(sd = sd(TotalSleepTime))
aov_result <- tidy(aov(TotalSleepTime ~ study, data = sleep))
aov_result
aov_judge<- aov_result$statistic[1] > qf(1-alpha, 4, nrow(sleep) - 5)
#Since the F-ratio is greater than the F critical value when alpha is 0.05, we reject
#H0 and claim that at least the total sleep time of one school is different from the others
#posthoc analysis using Bonferroni Method
pw_result <- pairwise.t.test(sleep$TotalSleepTime, sleep$study, 
                             p.adjust.method = "bonferroni") $ p.value
sleep %>% group_by(study) %>% 
  summarize(mean_sleep = mean(TotalSleepTime),
            sd_sleep = sd(TotalSleepTime),
            n = n())
#We concluded that students in categories 2 and 3 (uw students) have significantly 
#longer sleep time on average compared with those from the other categories, 
#with a significance level of 0.05
#Since parants' education backgrounds, race and course load do have impacts on gpa, 
#we control the first two variables and extract the 80% quantile of term units number
#to see if there is a correlation between total sleep time and term gpa. Since the non-first
#group accounts for the majority of the students, we only focus on the non-first group
maj <- sleep %>% filter(!is.na(term_units)) %>%
  summarize(lower = quantile(term_units, 0.1), 
            upper = quantile(term_units, 0.9))
sleep %>% filter(demo_firstgen == "non-first" & demo_race == "non-under" &
           between(term_units, maj$lower, maj$upper)) %>% 
  group_by(study) %>% ggplot(aes(x = TotalSleepTime, y = term_gpa)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ study, ncol = 3) + theme_minimal()

sleep %>% filter(demo_firstgen == "non-first" & demo_race == "under" &
                   between(term_units, maj$lower, maj$upper)) %>% 
  group_by(study) %>% ggplot(aes(x = TotalSleepTime, y = term_gpa)) +
  geom_point() + geom_smooth(method = "lm", se = TRUE, color = "brown") +
  facet_wrap(~ study, ncol = 3) + theme_minimal()

#As we see from the five categories, all of them indicate a positive correlation
#between total sleep time and students' gpa with the exclusion of disturbing factors.
#In this sense, are uw students more likely to ace their gpa
alpha = 0.05
sd_diff_uni <- sleep %>% select(study, term_gpa) %>% group_by(study) %>% 
  summarize(sd = sd(term_gpa))
aov_gpa_uni <- aov(term_gpa~study, data = sleep) %>% tidy()
pw_result_uni <- pairwise.t.test(sleep$term_gpa, sleep$study, 
                             p.adjust.method = "bonferroni") $ p.value
sleep %>% group_by(study) %>% 
  summarize(mean_gpa = mean(term_gpa),
            sd_gpa = sd(term_gpa))
#Notre Dame University students have significant higher gpa than UW students, which
#might indicate some other factors(learning difficulty, prof resources) that can affect gpa,
#but sleep time do improve students' academic performance in some way
#Prediction 
#A 2017 CMU students with a Total sleep time of 360h/month
filtered <- sleep %>% filter(!is.na(term_gpa) & !is.na(TotalSleepTime))
model <- lm(term_gpa~study+TotalSleepTime, data = filtered)
new <- data.frame(study = factor("1", levels = levels(filtered$study)),
                  TotalSleepTime = 360)
predicted <- predict(model, new, interval = "confidence")
