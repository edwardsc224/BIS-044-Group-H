#BIS 044 Group H Script
#GroupH.R
library(tidyverse)

results <-
  read.csv("healthcare-dataset-stroke-data.csv")

# Hypothesis: Married women are the most likely group to have a stroke.

results1 <- results %>%select(gender,age,ever_married,stroke)

# Find four groups (numerical): unmarried female, married female,
# unmarried male, married male, and compare to prove/disprove hypothesis
Male_Unmarried <- results1 %>% filter(gender=="Male") %>% filter(ever_married =="No")
  
stroke_male_unmarried <- Male_Unmarried %>% group_by(age) %>% summarize(sum(stroke))

Female_Unmarried <- results1 %>% filter(gender=="Female") %>% filter(ever_married =="No") %>% 
  summarize(stroketotal =sum(stroke))

Male_Married <-results1 %>% filter(gender=="Male") %>% filter(ever_married =="Yes") %>% 
  summarize(stroketotal =sum(stroke))

Female_Married <- results1 %>% filter(gender=="Female") %>% filter(ever_married =="Yes") %>% 
  summarize(stroketotal =sum(stroke))

Married <- results1 %>% filter(ever_married=="Yes") %>% group_by(gender,age) %>% summarize(stroketotal = sum(stroke))

library(ggplot2)
ggplot(Married, aes(x=age , y = stroketotal, color=gender)) + geom_bar(stat="identity")

Unmarried <- results1 %>% filter(ever_married=="No") %>% group_by(gender,age) %>% summarize(stroketotal = sum(stroke))

library(ggplot2)
ggplot(Unmarried, aes(x=age , y = stroketotal, color=gender)) + geom_bar(stat="identity")

#labs(title="Covid-19 Cases and Deaths in Somerset County, NJ", y="Counts", x="Time") + 
scale_colour_discrete(name="Legend", breaks=c("blue","red"), labels=c("cases", "deaths"))
