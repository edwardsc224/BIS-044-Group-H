#BIS 044 Group H Script
#GroupH.R
library(tidyverse)

results <-
  read.csv("healthcare-dataset-stroke-data.csv")

# Hypothesis: Married women above age 60 are the most likely group to have a stroke.

results1 <- results %>%select(gender,age,ever_married,stroke)

# Find four groups (numerical): unmarried female, married female,
# unmarried male, married male, and compare to prove/disprove hypothesis
Males <- 249
Females <- 141

Male_Unmarried <- results1 %>% filter(gender=="Male") %>% filter(ever_married =="No") %>% 
  summarize(stroketotal =sum(stroke)*100/Males)

Female_Unmarried <- results1 %>% filter(gender=="Female") %>% filter(ever_married =="No") %>% 
  summarize(stroketotal =sum(stroke)*100/Females)

Male_Married <-results1 %>% filter(gender=="Male") %>% filter(ever_married =="Yes") %>% 
  summarize(stroketotal =sum(stroke)*100/Males)

Female_Married <- results1 %>% filter(gender=="Female") %>% filter(ever_married =="Yes") %>% 
  summarize(stroketotal =sum(stroke)*100/Females)

Married <- results1 %>% filter(ever_married=="Yes") %>% group_by(gender,age) %>% 
  summarize(stroketotal = sum(stroke)/128)

library(ggplot2)
ggplot(Married, aes(x=age , y = stroketotal, color=gender)) + geom_bar(stat="identity", position = "dodge") +
  
  ggtitle("Total Fare Distribution") + 
  xlab("Fare") + 
  ylab("Frequency") +
  
  
  Unmarried <- results1 %>% filter(ever_married=="No") %>% group_by(gender,age) %>% summarize(stroketotal = sum(stroke))

library(ggplot2)
ggplot(Unmarried, aes(x=age , y = stroketotal, color=gender)) + geom_bar(stat="identity")

#5-year categories for x axis
#convert numbers to percentages -> even out data so males/females are measured 
#the same


