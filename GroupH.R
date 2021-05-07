#BIS 044 Group H Script
#GroupH.R
library(tidyverse)

results <-
  read.csv("healthcare-dataset-stroke-data.csv")

# Hypothesis: Married women above age 60 are the most likely group to have a stroke.

results1 <- results %>%select(gender,age,ever_married,stroke)

#Find the total number of males and females who have has a stroke
femnum <-results1 %>% filter(gender =="Female") %>% 
  summarize(stroketotal =sum(stroke))

malenum <-results1 %>% filter(gender =="Male")  %>% 
  summarize(stroketotal =sum(stroke))

# Find four groups (numerical): unmarried female, married female,
# unmarried male, married male, and compare to prove/disprove hypothesis
Males <- 108
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

  ggtitle("Total Stroke Distribution for Married") + 
  xlab("Age") + 
  ylab("Frequency") +
  scale_x_continuous(breaks = seq(15, 100, 5))

Unmarried <- results1 %>% filter(ever_married=="No") %>% group_by(gender,age) %>% 
  summarize(stroketotal = sum(stroke)/196)

library(ggplot2)
ggplot(Unmarried, aes(x=age , y = stroketotal, color=gender)) + geom_bar(stat="identity", position = "dodge") +

  ggtitle("Total Stroke Distribution for Unmarried Data") + 
  xlab("Age") + scale_x_continuous(breaks = seq(0, 100, 5)) +
  ylab("Frequency")



