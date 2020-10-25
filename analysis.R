# Start by setting your working directory
setwd("~/Desktop/CoronaKids/Data")

# Install necessary packages
install.packages("tm")  # For text mining
install.packages("SnowballC") # For text stemming
install.packages("wordcloud") # Word-cloud generator 
install.packages("RColorBrewer") # Color palettes
install.packages("tidyverse") # For organization
install.packages("emojifont") # For emojis ?
install.packages("ggrepel") # For fun
install.packages("gridExtra") # For cross-comparsion charts
install.packages("grid") # For comparison charts
install.packages("gt") # Idk
install.packages("dplyr") # For organization part 2 
install.packages('tidytext') # For 
install.packages("ggpubr") # For 

# Load necessary packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(emojifont)
library(ggrepel)
library(gridExtra)
library(grid)
library(gt)
library(dplyr)
library(tidytext)
library(ggpubr)

# Get dataset
dFull <- read.csv("Search_aggregate+!dup+!?+Subject-ID_final.csv")

# Remove NA values from dataset
d <- dFull %>%
  na.omit(dFull)

# Filter dataset by gender
m <- d %>%
  filter(str_detect(Sex, "m"))
f <- d %>%
  filter(str_detect(Sex, "f"))
u <- d %>%
  filter(str_detect(Sex, fixed("?")))

# Filter data by question number
q1 <- d %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "13."))) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
q2 <- d %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
q3 <- d %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
q4 <- d %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
q5 <- d %>%
  filter(str_detect(Question, "hug"))
q6 <- d %>%
  filter(str_detect(Question, "Where is the|first place"))
q7 <- d %>%
  filter(str_detect(Question, "rid|stop|go away"))
q8 <- d %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
q9 <- d %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
q10 <- d %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
q11 <- d %>%
  filter(str_detect(Question, "enjoying|enjoy"))


### Below is a sample of the varability within questions that str_detect took care of ###
### The key words above are the unique and/or most common words below for each question ###
# filter(Question=="1. What is the Coronavirus?" | Question=="1. What is the Corona virus?" | Question=="1. What is the corona virus?" | Question=="1. What is the Coronavirus?" | Question=="1. What is the Corona Virus?")
# filter(Question=="2. Who is the president?" | Question=="2. Who is the President?" | Question=="2. Who is the president of the United States?" | Question=="2. Who is the President of the United States?" | Question=="2. Who is the prime minister?" | Question=="3. Who is the prime minister?")
# filter(Question=="3. How many days have we been in lockdown?" | Question=="3. How many days have we been in quarantine?" | Question=="3. How many days we been in lockdown?" | Question=="3. How many days have we been in lock down?" | Question=="3. How many days have we been in lock down?")
# filter(Question=="4. Do you want to go to school?" | Question=="4. Do you want to go back to school?" | Question=="Do you want school to start back up?" | Question=="4. Do you want to go back to school/daycare?" | Question=="4. Do you miss your friends?" | Question=="5. Do you want to go back to school/nursery?")
# filter(Question=="5. Who is the first person you are going to hug when lockdown is over?" | Question=="5. Who is the first person you are going to hug when you get out of lockdown?" | Question=="5. Who is the first person you are going to hug when the virus is over?")
# filter(Question=="6. Where is the first place you want to go?" | Question=="6. Where is the first place you want to go after this?" | Question=="6. Where is the first place you want to go when the lockdown is over?" | Question=="6. Where is the first place you want to Go?")
# filter(Question=="7. What do you think we can do to get rid of the Coronavirus?" | Question=="7. What do you think we can do to get rid of the coronavirus?"| Question=="7. What do you think we can do to get rid of the corona virus?" | Question=="7. What do you think we can do to get rid of the Corona virus?") 
# filter(Question=="8. Are mommy and daddy good teachers?" | Question=="8. Is Momma a good teacher?" | Question=="8. Is mommy a good teacher?" | Question=="8. Is mom a good teacher?" | Question=="8. Is mommy a good substitute teacher?" | Question=="8. Who has become your new teacher?"	| Question=="8. Are your parents good teachers?" | Question=="8. Is mommy a good teacher ?" | Question=="8. Is momma a good teacher?" | Question=="8. Are mom and dad good teachers?" | Question=="8. Are your dad & I good teachers?" | Question=="8. Do you like when I help you with flash cards?" | Question=="8. Is mommy/daddy a good teacher?")
# filter(Question=="9. How did the Coronavirus start?" | Question=="9. How did the Corona virus start?" | Question=="9. How did the coronavirus start?" | Question=="9. How did the corona virus start?" | Question=="9. How did the Corona Virus start?" | Question=="9. How did coronavirus start?" | Question=="9. How did Coronavirus start?")
# filter(Question=="10. If you had to wear protective clothing to help you what would it be?" | Question=="10. If you had to wear protective clothing to help you, what would it be?" | Question=="12. If you had to wear protective clothing, what would you wear?" | Question=="9.If you had to wear protective clothing to help you what would it be?")
# filter(Question=="11. Are you enjoying the lockdown?" | Question=="11. Are you enjoying lockdown?" | Question=="9. Are you enjoying the lockdown?" | Question=="10. Are you enjoying the lockdown?" | Question=="11. Are you enjoying lockdown?" | Question=="11. Are you enjoying staying home with mommy and daddy?" | Question=="11. Are you enjoying quarantine?"| Question=="10. Are you enjoying lockdown?" | Question=="11. Are you enjoying staying at home?")

# View question answers
textq1 = paste(q1$Answer)
ggplot()+ 
  ggtitle("Question 1: What is the coronavirus?", subtitle = "n = 371") +
  annotate("text", x=4, y = 371:1, size=.5, label = textq1) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq2 = paste(q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister?", subtitle = "n = 331")+
  annotate("text", x = 4, y = 331:1, size=.5, label = textq2) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq3 = paste(q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown?", subtitle = "n = 383")+
  annotate("text", x = 4, y = 383:1, size=.5, label = textq3) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq4 = paste(q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school?", subtitle = "n = 353")+
  annotate("text", x = 4, y = 353:1, size=.5, label = textq4) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq5 = paste(q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 377")+
  annotate("text", x = 4, y = 377:1, size=.5, label = textq5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq6 = paste(q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go?", subtitle = "n = 381")+
  annotate("text", x = 4, y = 381:1, size=.5, label = textq6) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq7 = paste(q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 385")+
  annotate("text", x = 4, y = 385:1, size=.5, label = textq7) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq8 = paste(q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers?", subtitle = "n = 360")+
  annotate("text", x = 4, y = 360:1, size=.5, label = textq8) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq9 = paste(q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start?", subtitle = "n = 384")+
  annotate("text", x = 4, y = 384:1, size=.5, label = textq9) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq10 = paste(q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 356")+
  annotate("text", x = 4, y = 356:1, size=.5, label = textq10)+
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textq11 = paste(q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying the lockdown?", subtitle = "n = 339")+
  annotate("text", x = 4, y = 339:1, size=.5, label = textq11)+
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, color = "dark blue"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


# View answers with age and subject_id included in textstring
# textq1 = paste(q1$Subject_ID, q1$Answer, q1$Age)
# ggplot()+
#   ggtitle("Question 1: What is the coronavirus?") +
#   annotate("text", x=4, y = 371:1, size=.5, label = textq1) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq2 = paste(q2$Subject_ID, q2$Answer, q2$Age)
# ggplot() +
#   ggtitle("Question 2: Who is the president/prime minister?")+
#   annotate("text", x = 4, y = 331:1, size=.5, label = textq2) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq3 = paste(q3$Subject_ID, q3$Answer, q3$Age)
# ggplot() +
#   ggtitle("Question 3: How many days have we been in lockdown?")+
#   annotate("text", x = 4, y = 383:1, size=.5, label = textq3) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq4 = paste(q4$Subject_ID, q4$Answer, q4$Age)
# ggplot() +
#   ggtitle("Question 4: Do you want to go back to school?")+
#   annotate("text", x = 4, y = 353:1, size=.5, label = textq4) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq5 = paste(q5$Subject_ID, q5$Answer, q5$Age)
# ggplot() +
#   ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over?")+
#   annotate("text", x = 4, y = 377:1, size=.5, label = textq5) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq6 = paste(q6$Subject_ID, q6$Answer, q6$Age)
# ggplot() +
#   ggtitle("Question 6: Where is the first place you want to go?")+
#   annotate("text", x = 4, y = 381:1, size=.5, label = textq6) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq7 = paste(q7$Subject_ID, q7$Answer, q7$Age)
# ggplot() +
#   ggtitle("Question 7: What do you think we can do to get rid of the coronavirus?")+
#   annotate("text", x = 4, y = 385:1, size=.5, label = textq7) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq8 = paste(q8$Subject_ID, q8$Answer, q8$Age)
# ggplot() +
#   ggtitle("Question 8: Are your parents good teachers?")+
#   annotate("text", x = 4, y = 360:1, size=.5, label = textq8) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq9 = paste(q9$Subject_ID, q9$Answer, q9$Age)
# ggplot() +
#   ggtitle("Question 9: How did the coronavirus start?")+
#   annotate("text", x = 4, y = 386:1, size=.5, label = textq9) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq10 = paste(q10$Subject_ID, q10$Answer, q10$Age)
# ggplot() +
#   ggtitle("Question 10: If you had to wear protective clothing to help you what would it be?")+
#   annotate("text", x = 4, y = 356:1, size=.5, label = textq10)+
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())
# 
# textq11 = paste(q11$Subject_ID, q11$Answer, q11$Age)
# ggplot() +
#   ggtitle("Question 11. Are you enjoying the lockdown?")+
#   annotate("text", x = 4, y = 339:1, size=.5, label = textq11)+
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.line.x = element_blank(),
#         axis.line.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor=element_blank())


# Filter dataset by age
# Age 1
x101 <- d %>%
  filter(Age=="1")
# Age 2
x102 <- d %>%
  filter(Age %in% c("2", "2.5"))
# Age 3
x103 <- d %>%
  filter(Age %in% c("3", "3.5"))
# Age 4
x104 <- d %>%
  filter(Age %in% c("4", "4.5"))
# Age 5
x105 <- d %>%
  filter(Age %in% c("5", "5.5"))
# Age 6
x106 <- d %>%
  filter(Age %in% c("6", "6.5"))
# Age 7
x107 <- d %>%
  filter(Age=="7")
# Age 8
x108 <- d %>%
  filter(Age=="8")
# Age 9
x109 <- d %>%
  filter(Age=="9")
# Age 10
x110 <- d %>%
  filter(Age=="10")
# Age 11
x111 <- d %>%
  filter(Age=="11")
# Age 12
x112 <- d %>%
  filter(Age=="12")
# Age 13
x113 <- d %>%
  filter(Age=="13")
# Age 14
x114 <- d %>%
  filter(Age=="14")
# Age unknown
xUnknown <- d %>%
  filter(Age=="999")


# Filter age data.frames by question
# Question 1
x101q1<- x101 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
             
x102q1 <- x102 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x103q1 <- x103 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x104q1 <- x104 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x105q1 <- x105 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x106q1 <- x106 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x107q1 <- x107 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x108q1 <- x108 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x109q1 <- x109 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x110q1 <- x110 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x111q1 <- x111 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x112q1 <- x112 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x113q1 <- x113 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
x114q1 <- x114 %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))
xUnknownq1 <- xUnknown %>%
  filter(str_detect(Question, "What is the")) %>%
  filter(!(str_detect(Question, "12."))) %>%
  filter(!(str_detect(Question, "11."))) %>%
  filter(!(str_detect(Question, "10.")))

# Question 2
x101q2 <- x101 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x102q2 <- x102 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x103q2 <- x103 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x104q2 <- x104 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x105q2 <- x105 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x106q2 <- x106 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x107q2 <- x107 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x108q2 <- x108 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x109q2 <- x109 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x110q2 <- x110 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x111q2 <- x111 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x112q2 <- x112 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x113q2 <- x113 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
x114q2 <- x114 %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))
xUnknownq2 <- xUnknown %>%
  filter(str_detect(Question, "President|president|Prime Minister|prime minister|Prime minister|prime Minister"))

# Question 3
x101q3 <- x101 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x102q3 <- x102 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x103q3 <- x103 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x104q3 <- x104 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x105q3 <- x105 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x106q3 <- x106 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x107q3 <- x107 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x108q3 <- x108 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x109q3 <- x109 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x110q3 <- x110 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x111q3 <- x111 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x112q3 <- x112 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x113q3 <- x113 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
x114q3 <- x114 %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))
xUnknownq3 <- xUnknown %>%
  filter(str_detect(Question, "days")) %>%
  filter(!(str_detect(Question, "4. What does mummy wear most days")))

# Question 4
x101q4 <- x101 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x102q4 <- x102 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x103q4 <- x103 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x104q4 <- x104 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x105q4 <- x105 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x106q4 <- x106 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x107q4 <- x107 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x108q4 <- x108 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x109q4 <- x109 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x110q4 <- x110 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x111q4 <- x111 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x112q4 <- x112 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x113q4 <- x113 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
x114q4 <- x114 %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))
xUnknownq4 <- xUnknown %>%
  filter(str_detect(Question, "school|daycare|day care|nursery"))

# Question 5
x101q5 <- x101 %>%
  filter(str_detect(Question, "hug"))
x102q5 <- x102 %>%
  filter(str_detect(Question, "hug"))
x103q5 <- x103 %>%
  filter(str_detect(Question, "hug"))
x104q5 <- x104 %>%
  filter(str_detect(Question, "hug"))
x105q5 <- x105 %>%
  filter(str_detect(Question, "hug"))
x106q5 <- x106 %>%
  filter(str_detect(Question, "hug"))
x107q5 <- x107 %>%
  filter(str_detect(Question, "hug"))
x108q5 <- x108 %>%
  filter(str_detect(Question, "hug"))
x109q5 <- x109 %>%
  filter(str_detect(Question, "hug"))
x110q5 <- x110 %>%
  filter(str_detect(Question, "hug"))
x111q5 <- x111 %>%
  filter(str_detect(Question, "hug"))
x112q5 <- x112 %>%
  filter(str_detect(Question, "hug"))
x113q5 <- x113 %>%
  filter(str_detect(Question, "hug"))
x114q5 <- x114 %>%
  filter(str_detect(Question, "hug"))
xUnknownq5 <- xUnknown %>%
  filter(str_detect(Question, "hug"))

# Question 6
x101q6 <- x101 %>%
  filter(str_detect(Question, "Where is the|first place"))
x102q6 <- x102 %>%
  filter(str_detect(Question, "Where is the|first place"))
x103q6 <- x103 %>%
  filter(str_detect(Question, "Where is the|first place"))
x104q6 <- x104 %>%
  filter(str_detect(Question, "Where is the|first place"))
x105q6 <- x105 %>%
  filter(str_detect(Question, "Where is the|first place"))
x106q6 <- x106 %>%
  filter(str_detect(Question, "Where is the|first place"))
x107q6 <- x107 %>%
  filter(str_detect(Question, "Where is the|first place"))
x108q6 <- x108 %>%
  filter(str_detect(Question, "Where is the|first place"))
x109q6 <- x109 %>%
  filter(str_detect(Question, "Where is the|first place"))
x110q6 <- x110 %>%
  filter(str_detect(Question, "Where is the|first place"))
x111q6 <- x111 %>%
  filter(str_detect(Question, "Where is the|first place"))
x112q6 <- x112 %>%
  filter(str_detect(Question, "Where is the|first place"))
x113q6 <- x113 %>%
  filter(str_detect(Question, "Where is the|first place"))
x114q6 <- x114 %>%
  filter(str_detect(Question, "Where is the|first place"))
xUnknownq6 <- xUnknown %>%
  filter(str_detect(Question, "Where is the|first place"))

# Question 7
x101q7 <- x101 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x102q7 <- x102 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x103q7 <- x103 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x104q7 <- x104 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x105q7 <- x105 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x106q7 <- x106 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x107q7 <- x107 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x108q7 <- x108 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x109q7 <- x109 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x110q7 <- x110 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x111q7 <- x111 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x112q7 <- x112 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x113q7 <- x113 %>%
  filter(str_detect(Question, "rid|stop|go away"))
x114q7 <- x114 %>%
  filter(str_detect(Question, "rid|stop|go away"))
xUnknownq7 <- xUnknown %>%
  filter(str_detect(Question, "rid|stop|go away"))

# Question 8
x101q8 <- x101 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x102q8 <- x102 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x103q8 <- x103 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x104q8 <- x104 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x105q8 <- x105 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x106q8 <- x106 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x107q8 <- x107 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x108q8 <- x108 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x109q8 <- x109 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x110q8 <- x110 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x111q8 <- x111 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x112q8 <- x112 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x113q8 <- x113 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
x114q8 <- x114 %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))
xUnknownq8 <- xUnknown %>%
  filter(str_detect(Question, "teacher|teachers|flash cards"))

# Question 9
x101q9 <- x101 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x102q9 <- x102 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x103q9 <- x103 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x104q9 <- x104 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x105q9 <- x105 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x106q9 <- x106 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x107q9 <- x107 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x108q9 <- x108 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x109q9 <- x109 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x110q9 <- x110 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x111q9 <- x111 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x112q9 <- x112 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x113q9 <- x113 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
x114q9 <- x114 %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))
xUnknownq9 <- xUnknown %>%
  filter(str_detect(Question, "start")) %>%
  filter(!(str_detect(Question, "miss")))

# Question 10
x101q10 <- x101 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x102q10 <- x102 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x103q10 <- x103 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x104q10 <- x104 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x105q10 <- x105 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x106q10 <- x106 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x107q10 <- x107 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x108q10 <- x108 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x109q10 <- x109 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x110q10 <- x110 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x111q10 <- x111 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x112q10 <- x112 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x113q10 <- x113 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
x114q10 <- x114 %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))
xUnknownq10 <- xUnknown %>%
  filter(str_detect(Question, "if|protective")) %>%
  filter(!(str_detect(Question, "20. How")))

# Question 11
x101q11 <- x101 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x102q11 <- x102 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x103q11 <- x103 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x104q11 <- x104 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x105q11 <- x105 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x106q11 <- x106 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x107q11 <- x107 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x108q11 <- x108 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x109q11 <- x109 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x110q11 <- x110 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x111q11 <- x111 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x112q11 <- x112 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x113q11 <- x113 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
x114q11 <- x114 %>%
  filter(str_detect(Question, "enjoying|enjoy"))
xUnknownq11 <- xUnknown %>%
  filter(str_detect(Question, "enjoying|enjoy"))


# View answers
# Age 1
### No subjs ###

# Age 2 single graphs
text102q1 = paste(x102q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 2)", subtitle = "n = 5")+
  annotate("text", x = 4, y = 1:5, size=2, label = text102q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5), 
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q2 = paste(x102q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 2)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text102q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q3 = paste(x102q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 2)", subtitle = "n = 5")+
  annotate("text", x = 4, y = 1:5, size=2, label = text102q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q4 = paste(x102q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 2)", subtitle = "n = 5")+
  annotate("text", x = 4, y = 1:5, size=2, label = text102q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q5 = paste(x102q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 2)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text102q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q6 = paste(x102q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 2)", subtitle = "n = 5")+
  annotate("text", x = 4, y = 1:5, size=2, label = text102q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q7 = paste(x102q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 2)", subtitle = "n = 5")+
  annotate("text", x = 4, y = 1:5, size=2, label = text102q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q8 = paste(x102q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 2)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text102q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q9 = paste(x102q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 2)", subtitle = "n = 5")+
  annotate("text", x = 4, y = 1:5, size=2, label = text102q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q10 = paste(x102q10$Answer)
ggplot() +
  ggtitle("Question 10: If you had to wear protective clothing what would it be? (Age 2)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text102q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q11 = paste(x102q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 2)", subtitle = "n = 4") +
  annotate("text", x = 4, y = 1:4, size=2, label = text102q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


# Age 2 multiple graphs
text102q1graph = paste(x102q1$Answer)
x102q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:5, size=2, label = text102q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q2graph = paste(x102q2$Answer)
x102q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text102q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q3graph = paste(x102q3$Answer)
x102q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:5, size=2, label = text102q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q4graph = paste(x102q4$Answer)
x102q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:5, size=2, label = text102q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q5graph = paste(x102q5$Answer)
x102q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text102q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q6graph = paste(x102q6$Answer)
x102q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:5, size=2, label = text102q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q7graph = paste(x102q7$Answer)
x102q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:5, size=2, label = text102q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q8graph = paste(x102q8$Answer)
x102q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text102q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q9graph = paste(x102q9$Answer)
x102q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:5, size=2, label = text102q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q10graph = paste(x102q10$Answer)
x102q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text102q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102q11graph = paste(x102q11$Answer)
x102q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text102q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text102qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x102qKey <- ggplot() + 
  ggtitle("Question Key (Age 2)", subtitle = "n = 4.545455") + #(5+4+4+4+5+5+4+5+5+4+5)/11
  annotate("text", x = 0, y = 11:1, size=2, label = text102qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x102q1graph, x102q2graph, x102q3graph, x102q4graph, x102q5graph, x102q6graph, x102q7graph, x102q8graph, x102q9graph, x102q10graph, x102q11graph, x102qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

# Age 3
text103q1 = paste(x103q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 3)", subtitle = "n = 19")+
  annotate("text", x = 4, y = 1:19, size=2, label = text103q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q2 = paste(x103q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 3)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text103q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q3 = paste(x103q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 3)", subtitle = "n = 20")+
  annotate("text", x = 4, y = 1:20, size=2, label = text103q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q4 = paste(x103q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 3)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text103q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q5 = paste(x103q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 3)", subtitle = "n = 19")+
  annotate("text", x = 4, y = 1:19, size=2, label = text103q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q6 = paste(x103q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 3)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text103q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q7 = paste(x103q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 3)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text103q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q8 = paste(x103q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 3)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text103q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q9 = paste(x103q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 3)", subtitle = "n = 22")+
  annotate("text", x = 4, y = 1:22, size=2, label = text103q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q10 = paste(x103q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 3)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text103q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q11 = paste(x103q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 3)", subtitle = "n = 18")+
  annotate("text", x = 4, y = 1:18, size=2, label = text103q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


#Age 3 grid graph
text103q1graph = paste(x103q1$Answer)
x103q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:19, size=1.5, label = text103q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q2graph = paste(x103q2$Answer)
x103q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text103q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q3graph = paste(x103q3$Answer)
x103q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:20, size=2, label = text103q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q4graph = paste(x103q4$Answer)
x103q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text103q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q5graph = paste(x103q5$Answer)
x103q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:19, size=2, label = text103q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q6graph = paste(x103q6$Answer)
x103q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text103q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q7graph = paste(x103q7$Answer)
x103q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text103q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q8graph = paste(x103q8$Answer)
x103q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text103q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q9graph = paste(x103q9$Answer)
x103q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:22, size=2, label = text103q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q10graph = paste(x103q10$Answer)
x103q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text103q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103q11graph = paste(x103q11$Answer)
x103q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:18, size=2, label = text103q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text103qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x103qKey <- ggplot() + 
  ggtitle("Question Key (Age 3)", subtitle = "n = 20.36364") + #(19+21+18+21+20+21+19+21+21+21+22)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text103qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x103q1graph, x103q2graph, x103q3graph, x103q4graph, x103q5graph, x103q6graph, x103q7graph, x103q8graph, x103q9graph, x103q10graph, x103q11graph, x103qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

# Age 4
text104q1 = paste(x104q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 4)", subtitle = "n = 53")+
  annotate("text", x = 4, y = 1:53, size=2, label = text104q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q2 = paste(x104q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 4)", subtitle = "n = 53")+
  annotate("text", x = 4, y = 1:53, size=2, label = text104q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q3 = paste(x104q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 4)", subtitle = "n = 55")+
  annotate("text", x = 4, y = 1:55, size=2, label = text104q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q4 = paste(x104q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 4)", subtitle = "n = 50")+
  annotate("text", x = 4, y = 1:50, size=2, label = text104q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q5 = paste(x104q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 4)", subtitle = "n = 51")+
  annotate("text", x = 4, y = 1:51, size=2, label = text104q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q6 = paste(x104q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 4)", subtitle = "n = 53")+
  annotate("text", x = 4, y = 1:53, size=2, label = text104q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q7 = paste(x104q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 4)", subtitle = "n = 54")+
  annotate("text", x = 4, y = 1:54, size=2, label = text104q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q8 = paste(x104q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 4)", subtitle = "n = 50")+
  annotate("text", x = 4, y = 1:50, size=2, label = text104q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q9 = paste(x104q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 4)", subtitle = "n = 56")+
  annotate("text", x = 4, y = 1:56, size=2, label = text104q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q10 = paste(x104q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 4)", subtitle = "n = 49")+
  annotate("text", x = 4, y = 1:49, size=2, label = text104q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q11 = paste(x104q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 4)", subtitle = "n = 44")+
  annotate("text", x = 4, y = 1:44, size=2, label = text104q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


#Age 4 grid graph
text104q1graph = paste(x104q1$Answer)
x104q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:53, size=.75, label = text104q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q2graph = paste(x104q2$Answer)
x104q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:53, size=.75, label = text104q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q3graph = paste(x104q3$Answer)
x104q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:55, size=.75, label = text104q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q4graph = paste(x104q4$Answer)
x104q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:50, size=.75, label = text104q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q5graph = paste(x104q5$Answer)
x104q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:51, size=.75, label = text104q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q6graph = paste(x104q6$Answer)
x104q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:53, size=.75, label = text104q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q7graph = paste(x104q7$Answer)
x104q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:54, size=.75, label = text104q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q8graph = paste(x104q8$Answer)
x104q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:50, size=.75, label = text104q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q9graph = paste(x104q9$Answer)
x104q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:56, size=.75, label = text104q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q10graph = paste(x104q10$Answer)
x104q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:49, size=.75, label = text104q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104q11graph = paste(x104q11$Answer)
x104q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:44, size=.75, label = text104q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text104qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x104qKey <- ggplot() + 
  ggtitle("Question Key (Age 4)", subtitle = "n = 51.63636") + #(53+49+44+53+55+50+51+53+54+50+56)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text104qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x104q1graph, x104q2graph, x104q3graph, x104q4graph, x104q5graph, x104q6graph, x104q7graph, x104q8graph, x104q9graph, x104q10graph, x104q11graph, x104qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

#Age 5
text105q1 = paste(x105q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 5)", subtitle = "n = 64")+
  annotate("text", x = 4, y = 1:64, size=2, label = text105q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q2 = paste(x105q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 5)", subtitle = "n = 60")+
  annotate("text", x = 4, y = 1:60, size=2, label = text105q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q3 = paste(x105q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 5)", subtitle = "n = 64")+
  annotate("text", x = 4, y = 1:64, size=2, label = text105q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q4 = paste(x105q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 5)", subtitle = "n = 58")+
  annotate("text", x = 4, y = 1:58, size=2, label = text105q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q5 = paste(x105q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 5)", subtitle = "n = 65")+
  annotate("text", x = 4, y = 1:65, size=2, label = text105q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q6 = paste(x105q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 5)", subtitle = "n = 63")+
  annotate("text", x = 4, y = 1:63, size=2, label = text105q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q7 = paste(x105q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 5)", subtitle = "n = 64")+
  annotate("text", x = 4, y = 1:64, size=2, label = text105q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q8 = paste(x105q8$Answer) 
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 5)", subtitle = "n = 60")+
  annotate("text", x = 4, y = 1:60, size=2, label = text105q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q9 = paste(x105q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 5)", subtitle="n = 65")+
  annotate("text", x = 4, y = 1:65,  size=2, label = text105q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q10 = paste(x105q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 5)", subtitle = "n = 58")+
  annotate("text", x = 4, y = 1:58, size=2, label = text105q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q11 = paste(x105q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 5)", subtitle = "n = 59")+
  annotate("text", x = 4, y = 1:59, size=2, label = text105q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


#Age 5 grid graph
text105q1graph = paste(x105q1$Answer)
x105q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:64, size=.65, label = text105q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q2graph = paste(x105q2$Answer)
x105q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:60, size=.65, label = text105q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q3graph = paste(x105q3$Answer)
x105q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:64, size=.65, label = text105q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q4graph = paste(x105q4$Answer)
x105q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:58, size=.65, label = text105q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q5graph = paste(x105q5$Answer)
x105q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:65, size=.65, label = text105q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q6graph = paste(x105q6$Answer)
x105q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:63, size=.65, label = text105q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q7graph = paste(x105q7$Answer)
x105q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:64, size=.65, label = text105q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q8graph = paste(x105q8$Answer)
x105q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:60, size=.65, label = text105q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q9graph = paste(x105q9$Answer)
x105q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:65, size=.65, label = text105q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q10graph = paste(x105q10$Answer)
x105q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:58, size=.65, label = text105q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105q11graph = paste(x105q11$Answer)
x105q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:59, size=.65, label = text105q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text105qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x105qKey <- ggplot() + 
  ggtitle("Question Key (Age 5)", subtitle = "n = 61.81818") + #(64+58+59+60+64+58+65+63+64+60+65)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text105qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color = "dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x105q1graph, x105q2graph, x105q3graph, x105q4graph, x105q5graph, x105q6graph, x105q7graph, x105q8graph, x105q9graph, x105q10graph, x105q11graph, x105qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

#Age 6
text106q1 = paste(x106q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 6)", subtitle = "n = 45")+
  annotate("text", x = 4, y = 1:45, size=2, label = text106q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q2 = paste(x106q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 6)", subtitle = "n = 37")+
  annotate("text", x = 4, y = 1:37, size=2, label = text106q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q3 = paste(x106q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 6)", subtitle = "n = 49")+
  annotate("text", x = 4, y = 1:49, size=2, label = text106q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q4 = paste(x106q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 6)", subtitle = "n = 41")+
  annotate("text", x = 4, y = 1:41, size=2, label = text106q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q5 = paste(x106q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 6)", subtitle = "n = 49")+
  annotate("text", x = 4, y = 1:49, size=2, label = text106q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q6 = paste(x106q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 6)", subtitle = "n = 50")+
  annotate("text", x = 4, y = 1:50, size=2, label = text106q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q7 = paste(x106q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 6)", subtitle = "n = 50")+
  annotate("text", x = 4, y = 1:50, size=2, label = text106q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q8 = paste(x106q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 6)", subtitle = "n = 45")+
  annotate("text", x = 4, y = 1:45, size=2, label = text106q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q9 = paste(x106q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 6)", subtitle = "n = 50")+
  annotate("text", x = 4, y = 1:50, size=2, label = text106q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q10 = paste(x106q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 6)", subtitle = "n = 45")+
  annotate("text", x = 4, y = 1:45, size=2, label = text106q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q11 = paste(x106q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 6)", subtitle = "n = 41")+
  annotate("text", x = 4, y = 1:41, size=2, label = text106q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

#Age 6 grid graph
text106q1graph = paste(x106q1$Answer)
x106q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:45, size=.75, label = text106q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q2graph = paste(x106q2$Answer)
x106q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:37, size=.75, label = text106q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q3graph = paste(x106q3$Answer)
x106q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:47, size=.75, label = text106q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q4graph = paste(x106q4$Answer)
x106q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:41, size=.75, label = text106q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q5graph = paste(x106q5$Answer)
x106q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:49, size=.75, label = text106q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q6graph = paste(x106q6$Answer)
x106q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:50, size=.75, label = text106q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q7graph = paste(x106q7$Answer)
x106q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:50, size=.75, label = text106q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q8graph = paste(x106q8$Answer)
x106q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:45, size=.75, label = text106q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q9graph = paste(x106q9$Answer)
x106q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:50, size=.75, label = text106q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q10graph = paste(x106q10$Answer)
x106q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:45, size=.75, label = text106q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106q11graph = paste(x106q11$Answer)
x106q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:41, size=.75, label = text106q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text106qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x106qKey <- ggplot() + 
  ggtitle("Question Key (Age 6)", subtitle = "n = 45.45455") + #(45+45+41+37+47+41+49+50+50+45+50)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text106qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle=element_text(size=10, color="dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x106q1graph, x106q2graph, x106q3graph, x106q4graph, x106q5graph, x106q6graph, x106q7graph, x106q8graph, x106q9graph, x106q10graph, x106q11graph, x106qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

#Age 7
text107q1 = paste(x107q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 7)", subtitle = "n = 43")+
  annotate("text", x = 4, y = 1:43, size=2, label = text107q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q2 = paste(x107q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 7)", subtitle = "n = 33")+
  annotate("text", x = 4, y = 1:33, size=2, label = text107q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q3 = paste(x107q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 7)", subtitle = "n = 43")+
  annotate("text", x = 4, y = 1:43, size=2, label = text107q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q4 = paste(x107q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 7)", subtitle = "n = 41")+
  annotate("text", x = 4, y = 1:41, size=2, label = text107q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q5 = paste(x107q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 7)", subtitle = "n = 43")+
  annotate("text", x = 4, y = 1:43, size=2, label = text107q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q6 = paste(x107q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 7)", subtitle = "n = 44")+
  annotate("text", x = 4, y = 1:44, size=2, label = text107q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q7 = paste(x107q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 7)", subtitle = "n = 43")+
  annotate("text", x = 4, y = 1:43, size=2, label = text107q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q8 = paste(x107q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 7)", subtitle = "n = 39")+
  annotate("text", x = 4, y = 1:39, size=2, label = text107q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q9 = paste(x107q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 7)", subtitle = "n = 43")+
  annotate("text", x = 4, y = 1:43, size=2, label = text107q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q10 = paste(x107q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 7)", subtitle = "n = 39")+
  annotate("text", x = 4, y = 1:39, size=2, label = text107q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q11 = paste(x107q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 7)", subtitle = "n = 40")+
  annotate("text", x = 4, y = 1:40, size=2, label = text107q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

#Age 7 grid graph
text107q1graph = paste(x107q1$Answer)
x107q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:43, size=1, label = text107q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q2graph = paste(x107q2$Answer)
x107q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:33, size=1, label = text107q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q3graph = paste(x107q3$Answer)
x107q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:43, size=1, label = text107q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q4graph = paste(x107q4$Answer)
x107q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:41, size=1, label = text107q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q5graph = paste(x107q5$Answer)
x107q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:43, size=1, label = text107q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q6graph = paste(x107q6$Answer)
x107q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:44, size=1, label = text107q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q7graph = paste(x107q7$Answer)
x107q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:43, size=1, label = text107q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q8graph = paste(x107q8$Answer)
x107q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:39, size=1, label = text107q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q9graph = paste(x107q9$Answer)
x107q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:43, size=1, label = text107q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q10graph = paste(x107q10$Answer)
x107q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:39, size=1, label = text107q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107q11graph = paste(x107q11$Answer)
x107q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:40, size=1, label = text107q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text107qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x107qKey <- ggplot() + 
  ggtitle("Question Key (Age 7)", subtitle = "n = 21") + #(21+21+19+18+23+21+21+22+22+22+21)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text107qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x107q1graph, x107q2graph, x107q3graph, x107q4graph, x107q5graph, x107q6graph, x107q7graph, x107q8graph, x107q9graph, x107q10graph, x107q11graph, x107qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

#Age 8
text108q1 = paste(x108q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 8)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text108q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q2 = paste(x108q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 8)", subtitle = "n = 18")+
  annotate("text", x = 4, y = 1:18, size=2, label = text108q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q3 = paste(x108q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 8)", subtitle = "n = 23")+
  annotate("text", x = 4, y = 1:23, size=2, label = text108q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q4 = paste(x108q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 8)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text108q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q5 = paste(x108q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 8)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text108q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q6 = paste(x108q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 8)", subtitle = "n = 22")+
  annotate("text", x = 4, y = 1:22, size=2, label = text108q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q7 = paste(x108q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 8)", subtitle = "n = 22")+
  annotate("text", x = 4, y = 1:22, size=2, label = text108q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q8 = paste(x108q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 8)", subtitle = "n = 22")+
  annotate("text", x = 4, y = 1:22, size=2, label = text108q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q9 = paste(x108q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 8)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text108q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q10 = paste(x108q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 8)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text108q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q11 = paste(x108q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 8)", subtitle = "n = 19")+
  annotate("text", x = 4, y = 1:19, size=2, label = text108q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

#Age 8 grid graph
text108q1graph = paste(x108q1$Answer)
x108q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=1.5, label = text108q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q2graph = paste(x108q2$Answer)
x108q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:18, size=1.5, label = text108q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q3graph = paste(x108q3$Answer)
x108q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:23, size=1.5, label = text108q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q4graph = paste(x108q4$Answer)
x108q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=1.5, label = text108q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q5graph = paste(x108q5$Answer)
x108q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=1.5, label = text108q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q6graph = paste(x108q6$Answer)
x108q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:22, size=1.5, label = text108q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q7graph = paste(x108q7$Answer)
x108q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:22, size=1.5, label = text108q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q8graph = paste(x108q8$Answer)
x108q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:22, size=1.5, label = text108q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q9graph = paste(x108q9$Answer)
x108q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=1.5, label = text108q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q10graph = paste(x108q10$Answer)
x108q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=1.5, label = text108q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108q11graph = paste(x108q11$Answer)
x108q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:19, size=1.5, label = text108q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text108qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x108qKey <- ggplot() + 
  ggtitle("Question Key (Age 8)", subtitle = "n = 21") + #(21+21+19+18+23+21+21+22+22+22+21)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text108qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

 ggarrange(x108q1graph, x108q2graph, x108q3graph, x108q4graph, x108q5graph, x108q6graph, x108q7graph, x108q8graph, x108q9graph, x108q10graph, x108q11graph, x108qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

#Age 9
text109q1 = paste(x109q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 9)", subtitle = "n = 21") +
  annotate("text", x = 4, y = 1:21, size=2, label = text109q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q2 = paste(x109q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 9)", subtitle = "n = 17")+
  annotate("text", x = 4, y = 1:17, size=2, label = text109q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q3 = paste(x109q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 9)", subtitle = "n = 20")+
  annotate("text", x = 4, y = 1:20, size=2, label = text109q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q4 = paste(x109q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 9)", subtitle = "n = 20")+
  annotate("text", x = 4, y = 1:20, size=2, label = text109q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q5 = paste(x109q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 9)", subtitle = "n = 20")+
  annotate("text", x = 4, y = 1:20, size=2, label = text109q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q6 = paste(x109q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 9)", subtitle = "n = 20")+
  annotate("text", x = 4, y = 1:20, size=2, label = text109q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q7 = paste(x109q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 9)", subtitle = "n = 20")+
  annotate("text", x = 4, y = 1:20, size=2, label = text109q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q8 = paste(x109q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 9)", subtitle = "n = 17")+
  annotate("text", x = 4, y = 1:17, size=2, label = text109q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q9 = paste(x109q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 9)", subtitle = "n = 21")+
  annotate("text", x = 4, y = 1:21, size=2, label = text109q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q10 = paste(x109q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 9)", subtitle = "n = 20")+
  annotate("text", x = 4, y = 1:20, size=2, label = text109q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q11 = paste(x109q11$Answer)
ggplot() + 
  ggtitle("Question 11. Are you enjoying lockdown? (Age 9)", subtitle = "n = 19")+
  annotate("text", x = 4, y = 1:19, size=2, label = text109q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


#Age 9 grid graph
text109q1graph = paste(x109q1$Answer)
x109q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text109q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q2graph = paste(x109q2$Answer)
x109q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:17, size=2, label = text109q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q3graph = paste(x109q3$Answer)
x109q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:20, size=2, label = text109q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q4graph = paste(x109q4$Answer)
x109q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:20, size=2, label = text109q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q5graph = paste(x109q5$Answer)
x109q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:20, size=2, label = text109q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q6graph = paste(x109q6$Answer)
x109q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:20, size=2, label = text109q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q7graph = paste(x109q7$Answer)
x109q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:20, size=2, label = text109q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q8graph = paste(x109q8$Answer)
x109q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:17, size=2, label = text109q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q9graph = paste(x109q9$Answer)
x109q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:21, size=2, label = text109q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q10graph = paste(x109q10$Answer)
x109q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:20, size=2, label = text109q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109q11graph = paste(x109q11$Answer)
x109q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:19, size=2, label = text109q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text109qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x109qKey <- ggplot() + 
  ggtitle("Question Key (Age 9)", subtitle = "n = 19.54545") + #(21+20+19+17+20+20+20+20+20+17+21)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text109qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color="dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x109q1graph, x109q2graph, x109q3graph, x109q4graph, x109q5graph, x109q6graph, x109q7graph, x109q8graph, x109q9graph, x109q10graph, x109q11graph, x109qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)



#Age 10
text110q1 = paste(x110q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 10)", subtitle = "n = 17")+
  annotate("text", x = 4, y = 1:17, size=2, label = text110q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q2 = paste(x110q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 10)", subtitle = "n = 12")+
  annotate("text", x = 4, y = 1:12, size=2, label = text110q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q3 = paste(x110q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 10)", subtitle = "n = 18")+
  annotate("text", x = 4, y = 1:18, size=2, label = text110q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q4 = paste(x110q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 10)", subtitle = "n = 17")+
  annotate("text", x = 4, y = 1:17, size=2, label = text110q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q5 = paste(x110q5$Answer)
 ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 10)", subtitle = "n = 18")+
  annotate("text", x = 4, y = 1:18, size=2, label = text110q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q6 = paste(x110q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 10)", subtitle = "n = 18")+
  annotate("text", x = 4, y = 1:18, size=2, label = text110q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q7 = paste(x110q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 10)", subtitle = "n = 17")+
  annotate("text", x = 4, y = 1:17, size=2, label = text110q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q8 = paste(x110q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 10)", subtitle = "n = 17")+
  annotate("text", x = 4, y = 1:17, size=2, label = text110q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q9 = paste(x110q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 10)", subtitle = "n = 15")+
  annotate("text", x = 4, y = 1:15, size=2, label = text110q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q10 = paste(x110q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 10)", subtitle = "n = 17")+
  annotate("text", x = 4, y = 1:17, size=2, label = text110q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q11 = paste(x110q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 10)", subtitle = "n = 16")+
  annotate("text", x = 4, y = 1:16, size=2, label = text110q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


#Age 10 grid graph
text110q1graph = paste(x110q1$Answer)
x110q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:17, size=2, label = text110q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q2graph = paste(x110q2$Answer)
x110q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:12, size=2, label = text110q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q3graph = paste(x110q3$Answer)
x110q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:18, size=1.1, label = text110q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q4graph = paste(x110q4$Answer)
x110q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:17, size=2, label = text110q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q5graph = paste(x110q5$Answer)
x110q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:18, size=2, label = text110q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q6graph = paste(x110q6$Answer)
x110q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:18, size=2, label = text110q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q7graph = paste(x110q7$Answer)
x110q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:17, size=1.5, label = text110q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q8graph = paste(x110q8$Answer)
x110q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:17, size=2, label = text110q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q9graph = paste(x110q9$Answer)
x110q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:15, size=1.5, label = text110q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q10graph = paste(x110q10$Answer)
x110q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:17, size=2, label = text110q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110q11graph = paste(x110q11$Answer)
x110q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:16, size=1.5, label = text110q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text110qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x110qKey <- ggplot() + 
  ggtitle("Question Key (Age 10)", subtitle = "n = 16.54545") + #(17+17+16+12+18+17+18+18+17+17+15)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text110qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x110q1graph, x110q2graph, x110q3graph, x110q4graph, x110q5graph, x110q6graph, x110q7graph, x110q8graph, x110q9graph, x110q10graph, x110q11graph, x110qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)


#Age 11
text111q1 = paste(x111q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 11)", subtitle = "n = 11")+
  annotate("text", x = 4, y = 1:11, size=2, label = text111q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q2 = paste(x111q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 11)", subtitle = "n = 12")+
  annotate("text", x = 4, y = 1:12, size=2, label = text111q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q3 = paste(x111q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 11)", subtitle = "n = 12")+
  annotate("text", x = 4, y = 1:12, size=2, label = text111q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q4 = paste(x111q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 11)", subtitle = "n = 12")+
  annotate("text", x = 4, y = 1:12, size=2, label = text111q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q5 = paste(x111q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 11)", subtitle = "n = 11")+
  annotate("text", x = 4, y = 1:11, size=2, label = text111q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q6 = paste(x111q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 11)", subtitle = "n = 11")+
  annotate("text", x = 4, y = 1:11, size=2, label = text111q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q7 = paste(x111q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 11)", subtitle = "n = 12")+
  annotate("text", x = 4, y = 1:12, size=2, label = text111q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q8 = paste(x111q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 11)", subtitle = "n = 12")+
  annotate("text", x = 4, y = 1:12, size=2, label = text111q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q9 = paste(x111q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 11)", subtitle = "n = 12")+
  annotate("text", x = 4, y = 1:12, size=2, label = text111q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q10 = paste(x111q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 11)", subtitle = "n = 13")+
  annotate("text", x = 4, y = 1:13, size=2, label = text111q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q11 = paste(x111q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 11)", subtitle = "n = 10")+
  annotate("text", x = 4, y = 1:10, size=2, label = text111q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


#Age 11 grid graph
text111q1graph = paste(x111q1$Answer)
x111q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:11, size=1, label = text111q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q2graph = paste(x111q2$Answer)
x111q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:12, size=.9, label = text111q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q3graph = paste(x111q3$Answer)
x111q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:12, size=2, label = text111q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q4graph = paste(x111q4$Answer)
x111q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:12, size=1.75, label = text111q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q5graph = paste(x111q5$Answer)
x111q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:11, size=1.25, label = text111q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q6graph = paste(x111q6$Answer)
x111q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:11, size=2, label = text111q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q7graph = paste(x111q7$Answer)
x111q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:12, size=1.1, label = text111q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q8graph = paste(x111q8$Answer)
x111q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:12, size=1.25, label = text111q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q9graph = paste(x111q9$Answer)
x111q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:12, size=.9, label = text111q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q10graph = paste(x111q10$Answer)
x111q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:13, size=1.25, label = text111q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111q11graph = paste(x111q11$Answer)
x111q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:10, size=.75, label = text111q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text111qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x111qKey <- ggplot() + 
  ggtitle("Question Key (Age 11)", subtitle = "n = 11.63636") + #(11+13+10+12+12+12+11+11+12+12+12)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text111qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x111q1graph, x111q2graph, x111q3graph, x111q4graph, x111q5graph, x111q6graph, x111q7graph, x111q8graph, x111q9graph, x111q10graph, x111q11graph, x111qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

#Age 12
text112q1 = paste(x112q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q2 = paste(x112q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q3 = paste(x112q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q4 = paste(x112q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q5 = paste(x112q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q6 = paste(x112q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q7 = paste(x112q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q8 = paste(x112q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q9 = paste(x112q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 12)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text112q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q10 = paste(x112q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 12)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text112q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q11 = paste(x112q11$Answer)
ggplot() + 
  ggtitle("Question 11. Are you enjoying lockdown? (Age 12)", subtitle = "n = 4")+
  annotate("text", x = 4, y = 1:4, size=2, label = text112q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

#Age 12 grid graph
text112q1graph = paste(x112q1$Answer)
x112q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q2graph = paste(x112q2$Answer)
x112q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q3graph = paste(x112q3$Answer)
x112q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q4graph = paste(x112q4$Answer)
x112q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q5graph = paste(x112q5$Answer)
x112q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q6graph = paste(x112q6$Answer)
x112q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q7graph = paste(x112q7$Answer)
x112q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q8graph = paste(x112q8$Answer)
x112q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q9graph = paste(x112q9$Answer)
x112q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text112q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q10graph = paste(x112q10$Answer)
x112q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text112q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112q11graph = paste(x112q11$Answer)
x112q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:4, size=2, label = text112q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text112qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x112qKey <- ggplot() + 
  ggtitle("Question Key (Age 12)", subtitle = "n = 3.818182") + #(4+4+4+4+4+4+4+4+4+3+3)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text112qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x112q1graph, x112q2graph, x112q3graph, x112q4graph, x112q5graph, x112q6graph, x112q7graph, x112q8graph, x112q9graph, x112q10graph, x112q11graph, x112qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)

#Age 13
text113q1 = paste(x113q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q2 = paste(x113q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q3 = paste(x113q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q4 = paste(x113q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q5 = paste(x113q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q6 = paste(x113q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 13)", subtitle = "n = 2")+
  annotate("text", x = 4, y = 1:2, size=2, label = text113q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q7 = paste(x113q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q8 = paste(x113q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q9 = paste(x113q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q10 = paste(x113q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q11 = paste(x113q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 13)", subtitle = "n = 3")+
  annotate("text", x = 4, y = 1:3, size=2, label = text113q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


#Age 13 grid graph
text113q1graph = paste(x113q1$Answer)
x113q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q2graph = paste(x113q2$Answer)
x113q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q3graph = paste(x113q3$Answer)
x113q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q4graph = paste(x113q4$Answer)
x113q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q5graph = paste(x113q5$Answer)
x113q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q6graph = paste(x113q6$Answer)
x113q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:2, size=2, label = text113q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q7graph = paste(x113q7$Answer)
x113q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q8graph = paste(x113q8$Answer)
x113q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q9graph = paste(x113q9$Answer)
x113q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q10graph = paste(x113q10$Answer)
x113q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113q11graph = paste(x113q11$Answer)
x113q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:3, size=2, label = text113q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text113qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x113qKey <- ggplot() + 
  ggtitle("Question Key (Age 13)", subtitle = "n = 2.909091") + #(3+3+3+3+3+3+3+2+3+3+3)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text113qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x113q1graph, x113q2graph, x113q3graph, x113q4graph, x113q5graph, x113q6graph, x113q7graph, x113q8graph, x113q9graph, x113q10graph, x113q11graph, x113qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)


#Age 14
text114q1 = paste(x114q1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q2 = paste(x114q2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q3 = paste(x114q3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q4 = paste(x114q4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q5 = paste(x114q5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q6 = paste(x114q6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q7 = paste(x114q7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q8 = paste(x114q8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q9 = paste(x114q9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q10 = paste(x114q10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q11 = paste(x114q11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age 14)", subtitle = "n = 1")+
  annotate("text", x = 4, y = 1:1, size=2, label = text114q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

#Age 14 grid graph
text114q1graph = paste(x114q1$Answer)
x114q1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q2graph = paste(x114q2$Answer)
x114q2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q3graph = paste(x114q3$Answer)
x114q3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q4graph = paste(x114q4$Answer)
x114q4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q5graph = paste(x114q5$Answer)
x114q5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q6graph = paste(x114q6$Answer)
x114q6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q7graph = paste(x114q7$Answer)
x114q7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q8graph = paste(x114q8$Answer)
x114q8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=1.5, label = text114q8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q9graph = paste(x114q9$Answer)
x114q9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q10graph = paste(x114q10$Answer)
x114q10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114q11graph = paste(x114q11$Answer)
x114q11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:1, size=2, label = text114q11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

text114qKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
x114qKey <- ggplot() + 
  ggtitle("Question Key (Age 14)", subtitle = "n = 1") + #(1+1+1+1+1+1+1+1+1+1+1)/11
  annotate("text", x = 1, y = 11:1, size=2, label = text114qKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(x114q1graph, x114q2graph, x114q3graph, x114q4graph, x114q5graph, x114q6graph, x114q7graph, x114q8graph, x114q9graph, x114q10graph, x114q11graph, x114qKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)


#AgeUnknown
textUnknownq1 = paste(xUnknownq1$Answer)
ggplot() + 
  ggtitle("Question 1: What is the coronavirus? (Age Unknown)", subtitle = "n = 65")+
  annotate("text", x = 4, y = 1:65, size=2, label = textUnknownq1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq2 = paste(xUnknownq2$Answer)
ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister? (Age Unknown)", subtitle = "n = 56")+
  annotate("text", x = 4, y = 1:56, size=2, label = textUnknownq2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq3 = paste(xUnknownq3$Answer)
ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown? (Age Unknown)", subtitle = "n = 68")+
  annotate("text", x = 4, y = 1:68, size=2, label = textUnknownq3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq4 = paste(xUnknownq4$Answer)
ggplot() + 
  ggtitle("Question 4: Do you want to go back to school? (Age Unknown)", subtitle = "n = 59")+
  annotate("text", x = 4, y = 1:59, size=2, label = textUnknownq4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq5 = paste(xUnknownq5$Answer)
ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over? (Age Unknown)", subtitle = "n = 68")+
  annotate("text", x = 4, y = 1:68, size=2, label = textUnknownq5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq6 = paste(xUnknownq6$Answer)
ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go? (Age Unknown)", subtitle = "n = 67")+
  annotate("text", x = 4, y = 1:67, size=2, label = textUnknownq6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq7 = paste(xUnknownq7$Answer)
ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus? (Age Unknown)", subtitle = "n = 69")+
  annotate("text", x = 4, y = 1:69, size=2, label = textUnknownq7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq8 = paste(xUnknownq8$Answer)
ggplot() + 
  ggtitle("Question 8: Are your parents good teachers? (Age Unknown)", subtitle = "n = 65")+
  annotate("text", x = 4, y = 1:65, size=2, label = textUnknownq8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq9 = paste(xUnknownq9$Answer)
ggplot() + 
  ggtitle("Question 9: How did the coronavirus start? (Age Unknown)", subtitle = "n = 67")+
  annotate("text", x = 4, y = 1:67, size=2, label = textUnknownq9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq10 = paste(xUnknownq10$Answer)
ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be? (Age Unknown)", subtitle = "n = 62")+
  annotate("text", x = 4, y = 1:62, size=2, label = textUnknownq10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq11 = paste(xUnknownq11$Answer)
ggplot() + 
  ggtitle("Question 11: Are you enjoying lockdown? (Age Unknown)", subtitle = "n = 61")+
  annotate("text", x = 4, y = 1:61, size=2, label = textUnknownq11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=10, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=8, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

#Age Unknown grid graph
textUnknownq1graph = paste(xUnknownq1$Answer)
xUnknownq1graph <- ggplot() + 
  annotate("text", x = 4, y = 1:65, size=.65, label = textUnknownq1,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq2graph = paste(xUnknownq2$Answer)
xUnknownq2graph <- ggplot() + 
  annotate("text", x = 4, y = 1:56, size=.75, label = textUnknownq2,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq3graph = paste(xUnknownq3$Answer)
xUnknownq3graph <- ggplot() + 
  annotate("text", x = 4, y = 1:68, size=.75, label = textUnknownq3,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq4graph = paste(xUnknownq4$Answer)
xUnknownq4graph <- ggplot() + 
  annotate("text", x = 4, y = 1:59, size=.75, label = textUnknownq4,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq5graph = paste(xUnknownq5$Answer)
xUnknownq5graph <- ggplot() + 
  annotate("text", x = 4, y = 1:68, size=.75, label = textUnknownq5,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq6graph = paste(xUnknownq6$Answer)
xUnknownq6graph <- ggplot() + 
  annotate("text", x = 4, y = 1:67, size=.75, label = textUnknownq6,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq7graph = paste(xUnknownq7$Answer)
xUnknownq7graph <- ggplot() + 
  annotate("text", x = 4, y = 1:69, size=.75, label = textUnknownq7,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq8graph = paste(xUnknownq8$Answer)
xUnknownq8graph <- ggplot() + 
  annotate("text", x = 4, y = 1:65, size=.75, label = textUnknownq8,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq9graph = paste(xUnknownq9$Answer)
xUnknownq9graph <- ggplot() + 
  annotate("text", x = 4, y = 1:67, size=.75, label = textUnknownq9,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq10graph = paste(xUnknownq10$Answer)
xUnknownq10graph <- ggplot() + 
  annotate("text", x = 4, y = 1:62, size=.75, label = textUnknownq10,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownq11graph = paste(xUnknownq11$Answer)
xUnknownq11graph <- ggplot() + 
  annotate("text", x = 4, y = 1:61, size=.75, label = textUnknownq11,) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

textUnknownqKey = paste(c("Q1=What is the coronavirus?", "Q2=Who is the president/prime minister?", "Q3=How many days have we been in lockdown?", "Q4=Do you want to go back to school?", "Q5=Who is the first person you are going to hug when lockdown is over?", "Q6=Where is the first place you want to go?", "Q7=What can we do to get rid of the coronavirus?", "Q8=Are your parents good teachers?", "Q9=How did the coronavirus start?", "Q10=If you had to wear protective clothing to help you what would it be?", "Q11=Are you enjoying lockdown?"))
xUnknownqKey <- ggplot() + 
  ggtitle("Question Key (Age Unknown)", subtitle = "n = 64.27273") + #(65+62+61+56+68+59+68+67+69+65+67)/11
  annotate("text", x = 1, y = 11:1, size=2, label = textUnknownqKey, hjust = 0.5) + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5),
        plot.subtitle = element_text(size=10, color = "dark blue", hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

ggarrange(xUnknownq1graph, xUnknownq2graph, xUnknownq3graph, xUnknownq4graph, xUnknownq5graph, xUnknownq6graph, xUnknownq7graph, xUnknownq8graph, xUnknownq9graph, xUnknownq10graph, xUnknownq11graph, xUnknownqKey + rremove("x.text"),
          labels = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11"),
          ncol = 3, nrow = 4)


#######################################################################################
                                           ### Ggarrange by question ###
#######################################################################################

# Question 1 variable (in order to get question key/n= square in grid)
q1nameKeygraph <- ggplot() + 
  ggtitle("Question 1: What is the coronavirus?", subtitle = "n = 371") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


# Ggarrange Question 1 by age to see all squares in grid
ggarrange(x102q1graph, x103q1graph, x104q1graph, x105q1graph, x106q1graph, x107q1graph, x108q1graph, x109q1graph, x110q1graph, x111q1graph, x112q1graph, x113q1graph, x114q1graph, xUnknownq1graph, q1nameKeygraph +  rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)


# Question 2 variable (in order to get question key/n= square in grid)
q2nameKeygraph <- ggplot() + 
  ggtitle("Question 2: Who is the president/prime minister?", subtitle = "n = 331") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 2
ggarrange(x102q2graph, x103q2graph, x104q2graph, x105q2graph, x106q2graph, x107q2graph, x108q2graph, x109q2graph, x110q2graph, x111q2graph, x112q2graph, x113q2graph, x114q2graph, xUnknownq2graph, q2nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)

# Question 3 variable (in order to get question key/n= square in grid)
q3nameKeygraph <- ggplot() + 
  ggtitle("Question 3: How many days have we been in lockdown?", subtitle = "n = 383") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 3
ggarrange(x102q3graph, x103q3graph, x104q3graph, x105q3graph, x106q3graph, x107q3graph, x108q3graph, x109q3graph, x110q3graph, x111q3graph, x112q3graph, x113q3graph, x114q3graph, xUnknownq3graph, q3nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)


# Question 4 variable (in order to get question key/n= square in grid)
q4nameKeygraph <- ggplot() + 
  ggtitle("Question 4: Do you want to go back to school?", subtitle = "n = 353") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 4
ggarrange(x102q4graph, x103q4graph, x104q4graph, x105q4graph, x106q4graph, x107q4graph, x108q4graph, x109q4graph, x110q4graph, x111q4graph, x112q4graph, x113q4graph, x114q4graph, xUnknownq4graph, q4nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)

# Question 5 variable (in order to get question key/n= square in grid)
q5nameKeygraph <- ggplot() + 
  ggtitle("Question 5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 377") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 5
ggarrange(x102q5graph, x103q5graph, x104q5graph, x105q5graph, x106q5graph, x107q5graph, x108q5graph, x109q5graph, x110q5graph, x111q5graph, x112q5graph, x113q5graph, x114q5graph, xUnknownq5graph, q5nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)

# Question 6 variable (in order to get question key/n= square in grid)
q6nameKeygraph <- ggplot() + 
  ggtitle("Question 6: Where is the first place you want to go?", subtitle = "n = 381") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 6
ggarrange(x102q6graph, x103q6graph, x104q6graph, x105q6graph, x106q6graph, x107q6graph, x108q6graph, x109q6graph, x110q6graph, x111q6graph, x112q6graph, x113q6graph, x114q6graph, xUnknownq6graph, q6nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)


# Question 7 variable (in order to get question key/n= square in grid)
q7nameKeygraph <- ggplot() + 
  ggtitle("Question 7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 385") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=11, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=9, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 7
ggarrange(x102q7graph, x103q7graph, x104q7graph, x105q7graph, x106q7graph, x107q7graph, x108q7graph, x109q7graph, x110q7graph, x111q7graph, x112q7graph, x113q7graph, x114q7graph, xUnknownq7graph, q7nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)


# Question 8 variable (in order to get question key/n= square in grid)
q8nameKeygraph <- ggplot() + 
  ggtitle("Question 8: Are your parents good teachers?", subtitle = "n = 360") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 8
ggarrange(x102q8graph, x103q8graph, x104q8graph, x105q8graph, x106q8graph, x107q8graph, x108q8graph, x109q8graph, x110q8graph, x111q8graph, x112q8graph, x113q8graph, x114q8graph, xUnknownq8graph, q8nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)

# Question 9 variable (in order to get question key/n= square in grid)
q9nameKeygraph <- ggplot() + 
  ggtitle("Question 9: How did the coronavirus start?", subtitle = "n = 384") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 9
ggarrange(x102q9graph, x103q9graph, x104q9graph, x105q9graph, x106q9graph, x107q9graph, x108q9graph, x109q9graph, x110q9graph, x111q9graph, x112q9graph, x113q9graph, x114q9graph, xUnknownq9graph, q9nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)

# Question 10 variable (in order to get question key/n= square in grid)
q10nameKeygraph <- ggplot() + 
  ggtitle("Question 10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 356") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=11, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=9, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 10
ggarrange(x102q10graph, x103q10graph, x104q10graph, x105q10graph, x106q10graph, x107q10graph, x108q10graph, x109q10graph, x110q10graph, x111q10graph, x112q10graph, x113q10graph, x114q10graph, xUnknownq10graph, q10nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)

# Question 11 variable (in order to get question key/n= square in grid)
q11nameKeygraph <- ggplot() + 
  ggtitle("Question 11. Are you enjoying lockdown?", subtitle = "n = 339") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Ggarrange Question 11
ggarrange(x102q11graph, x103q11graph, x104q11graph, x105q11graph, x106q11graph, x107q11graph, x108q11graph, x109q11graph, x110q11graph, x111q11graph, x112q11graph, x113q11graph, x114q11graph, xUnknownq11graph, q11nameKeygraph + rremove("x.text"),
          labels = c("Age 2", "Age 3", "Age 4", "Age 5", "Age 6", "Age 7", "Age 8", "Age 9", "Age 10", "Age 11", "Age 12", "Age 13", "Age 14", "Age Unknown"),
          ncol = 3, nrow = 5)

#################################################################################################
                                        ### Word clouds & bar charts by question, no age ###
#################################################################################################

# Corpusq1
textq1Answer <- paste(q1$Answer)
corpusq1 <- Corpus(VectorSource(textq1Answer))

# Load the data as a corpus
inspect(corpusq1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq1 <- content_transformer(function (corpusq1, pattern) gsub(pattern, "", corpusq1))
docsq1 <- tm_map(corpusq1, toSpaceCorpusq1, "/")
docsq1 <- tm_map(corpusq1, toSpaceCorpusq1, "@")
docsq1 <- tm_map(corpusq1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docsq1 <- tm_map(docsq1, content_transformer(tolower))
# Remove numbers
docsq1 <- tm_map(docsq1, removeNumbers)
# Remove english common stopwords
docsq1 <- tm_map(docsq1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq1 <- tm_map(docsq1, removeWords)
# Remove punctuations
docsq1 <- tm_map(docsq1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq1 <- tm_map(docsq1, stripWhitespace)
# Text stemming
# docsq1 <- tm_map(docsq1, stemDocument)

dtmq1 <- TermDocumentMatrix(docsq1)
mq1 <- as.matrix(dtmq1)
vq1 <- sort(rowSums(mq1),decreasing=TRUE)
dq1 <- data.frame(word = names(vq1),freq=vq1)

head(dq1, 500)

# View answers :)
wordcloudq1 <- wordcloud(words = dq1$word, freq = dq1$freq, min.freq = 1, # change min.freq to see different results
                         max.words=393, random.order=FALSE, rot.per=0.0, 
                         colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q1
dq1tibble <- list(word = dq1$word[1:10], freq = dq1$freq[1:10])
dq1tibble <- as_tibble(dq1tibble)

ggplot(dq1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q1: What is the coronavirus?", subtitle = "n = 371") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq2
textq2Answer <- paste(q2$Answer)
corpusq2 <- Corpus(VectorSource(textq2Answer))

# Load the data as a corpus
inspect(corpusq2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpusq2, pattern) gsub(pattern, "", corpusq2))
docsq2 <- tm_map(corpusq2, toSpaceCorpusq2, "/")
docsq2 <- tm_map(corpusq2, toSpaceCorpusq2, "@")
docsq2 <- tm_map(corpusq2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docsq2 <- tm_map(docsq2, content_transformer(tolower))
# Remove numbers
docsq2 <- tm_map(docsq2, removeNumbers)
# Remove english common stopwords
docsq2 <- tm_map(docsq2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq2 <- tm_map(docsq2, removeWords)
# Remove punctuations
docsq2 <- tm_map(docsq2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq2 <- tm_map(docsq2, stripWhitespace)
# Text stemming
# docsq2 <- tm_map(docsq2, stemDocument)

dtmq2 <- TermDocumentMatrix(docsq2)
mq2 <- as.matrix(dtmq2)
vq2 <- sort(rowSums(mq2),decreasing=TRUE)
dq2 <- data.frame(word = names(vq2),freq=vq2)

head(dq2, 500)

# View answers :)
wordcloud(words = dq2$word, freq = dq2$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
dq2tibble <- list(word = dq2$word[1:10], freq = dq2$freq[1:10])
dq2tibble <- as_tibble(dq2tibble)

ggplot(dq2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q2: Who is the president/prime minister?", subtitle = "n = 331") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq3
textq3Answer <- paste(q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpusq3 <- Corpus(VectorSource(textq3Answer))

# Load the data as a corpus
inspect(corpusq3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq3 <- content_transformer(function (corpusq3, pattern) gsub(pattern, "", corpusq3))
docsq3 <- tm_map(corpusq3, toSpaceCorpusq3, "/")
docsq3 <- tm_map(corpusq3, toSpaceCorpusq3, "@")
docsq3 <- tm_map(corpusq3, toSpaceCorpusq3, "\\|")

# Convert the text to lower case
docsq3 <- tm_map(docsq3, content_transformer(tolower))
# Remove numbers
# docsq3 <- tm_map(docsq3, removeNumbers)
# Remove english common stopwords
docsq3 <- tm_map(docsq3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq3 <- tm_map(docsq3, removeWords)
# Remove punctuations
docsq3 <- tm_map(docsq3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq3 <- tm_map(docsq3, stripWhitespace)
# Text stemming
# docsq3 <- tm_map(docsq3, stemDocument)

dtmq3 <- TermDocumentMatrix(docsq3)
mq3 <- as.matrix(dtmq3)
vq3 <- sort(rowSums(mq3),decreasing=TRUE)
dq3 <- data.frame(word = names(vq3),freq=vq3)

head(dq3, 500)

# View answers :)
wordcloud(words = dq3$word, freq = dq3$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
dq3tibble <- list(word = dq3$word[1:10], freq = dq3$freq[1:10])
dq3tibble <- as_tibble(dq3tibble)

ggplot(dq3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q3: How many days have we been in lockdown?", subtitle = "n = 383") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq4
textq4Answer <- paste(q4$Answer)
corpusq4 <- Corpus(VectorSource(textq4Answer))

# Load the data as a corpus
inspect(corpusq4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq4 <- content_transformer(function (corpusq4, pattern) gsub(pattern, "", corpusq4))
docsq4 <- tm_map(corpusq4, toSpaceCorpusq4, "/")
docsq4 <- tm_map(corpusq4, toSpaceCorpusq4, "@")
docsq4 <- tm_map(corpusq4, toSpaceCorpusq4, "\\|")

# Convert the text to lower case
docsq4 <- tm_map(docsq4, content_transformer(tolower))
# Remove numbers
docsq4 <- tm_map(docsq4, removeNumbers)
# Remove english common stopwords
docsq4 <- tm_map(docsq4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq4 <- tm_map(docsq4, removeWords)
# Remove punctuations
docsq4 <- tm_map(docsq4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq4 <- tm_map(docsq4, stripWhitespace)
# Text stemming
# docsq4 <- tm_map(docsq4, stemDocument)

dtmq4 <- TermDocumentMatrix(docsq4)
mq4 <- as.matrix(dtmq4)
vq4 <- sort(rowSums(mq4),decreasing=TRUE)
dq4 <- data.frame(word = names(vq4),freq=vq4)

head(dq4, 500)

# View answers :)
wordcloud(words = dq4$word, freq = dq4$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
dq4tibble <- list(word = dq4$word[1:10], freq = dq4$freq[1:10])
dq4tibble <- as_tibble(dq4tibble)

ggplot(dq4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q4: Do you want to go back to school?", subtitle = "n = 353") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq5
textq5Answer <- paste(q5$Answer)
corpusq5 <- Corpus(VectorSource(textq5Answer))

# Load the data as a corpus
inspect(corpusq5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq5 <- content_transformer(function (corpusq5, pattern) gsub(pattern, "", corpusq5))
docsq5 <- tm_map(corpusq5, toSpaceCorpusq5, "/")
docsq5 <- tm_map(corpusq5, toSpaceCorpusq5, "@")
docsq5 <- tm_map(corpusq5, toSpaceCorpusq5, "\\|")

# Convert the text to lower case
docsq5 <- tm_map(docsq5, content_transformer(tolower))
# Remove numbers
docsq5 <- tm_map(docsq5, removeNumbers)
# Remove english common stopwords
docsq5 <- tm_map(docsq5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq5 <- tm_map(docsq5, removeWords)
# Remove punctuations
docsq5 <- tm_map(docsq5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq5 <- tm_map(docsq5, stripWhitespace)
# Text stemming
# docsq5 <- tm_map(docsq5, stemDocument)

dtmq5 <- TermDocumentMatrix(docsq5)
mq5 <- as.matrix(dtmq5)
vq5 <- sort(rowSums(mq5),decreasing=TRUE)
dq5 <- data.frame(word = names(vq5),freq=vq5)

head(dq5, 500)

# View answers :)
wordcloud(words = dq5$word, freq = dq5$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
dq5tibble <- list(word = dq5$word[1:10], freq = dq5$freq[1:10])
dq5tibble <- as_tibble(dq5tibble)

ggplot(dq5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 377") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq6
textq6Answer <- paste(q6$Answer)
corpusq6 <- Corpus(VectorSource(textq6Answer))

# Load the data as a corpus
inspect(corpusq6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq6 <- content_transformer(function (corpusq6, pattern) gsub(pattern, "", corpusq6))
docsq6 <- tm_map(corpusq6, toSpaceCorpusq6, "/")
docsq6 <- tm_map(corpusq6, toSpaceCorpusq6, "@")
docsq6 <- tm_map(corpusq6, toSpaceCorpusq6, "\\|")

# Convert the text to lower case
docsq6 <- tm_map(docsq6, content_transformer(tolower))
# Remove numbers
docsq6 <- tm_map(docsq6, removeNumbers)
# Remove english common stopwords
docsq6 <- tm_map(docsq6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq6 <- tm_map(docsq6, removeWords)
# Remove punctuations
docsq6 <- tm_map(docsq6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq6 <- tm_map(docsq6, stripWhitespace)
# Text stemming
# docsq6 <- tm_map(docsq6, stemDocument)

dtmq6 <- TermDocumentMatrix(docsq6)
mq6 <- as.matrix(dtmq6)
vq6 <- sort(rowSums(mq6),decreasing=TRUE)
dq6 <- data.frame(word = names(vq6),freq=vq6)

head(dq6, 500)

# View answers :)
wordcloud(words = dq6$word, freq = dq6$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
dq6tibble <- list(word = dq6$word[1:10], freq = dq6$freq[1:10])
dq6tibble <- as_tibble(dq6tibble)

ggplot(dq6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q6: Where is the first place you want to go?", subtitle = "n = 381") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq7
textq7Answer <- paste(q7$Answer)
corpusq7 <- Corpus(VectorSource(textq7Answer))

# Load the data as a corpus
inspect(corpusq7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq7 <- content_transformer(function (corpusq7, pattern) gsub(pattern, "", corpusq7))
docsq7 <- tm_map(corpusq7, toSpaceCorpusq7, "/")
docsq7 <- tm_map(corpusq7, toSpaceCorpusq7, "@")
docsq7 <- tm_map(corpusq7, toSpaceCorpusq7, "\\|")

# Convert the text to lower case
docsq7 <- tm_map(docsq7, content_transformer(tolower))
# Remove numbers
docsq7 <- tm_map(docsq7, removeNumbers)
# Remove english common stopwords
docsq7 <- tm_map(docsq7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq7 <- tm_map(docsq7, removeWords)
# Remove punctuations
docsq7 <- tm_map(docsq7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq7 <- tm_map(docsq7, stripWhitespace)
# Text stemming
# docsq7 <- tm_map(docsq7, stemDocument)

dtmq7 <- TermDocumentMatrix(docsq7)
mq7 <- as.matrix(dtmq7)
vq7 <- sort(rowSums(mq7),decreasing=TRUE)
dq7 <- data.frame(word = names(vq7),freq=vq7)

head(dq7, 500)

# View answers :)
wordcloud(words = dq7$word, freq = dq7$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
dq7tibble <- list(word = dq7$word[1:10], freq = dq7$freq[1:10])
dq7tibble <- as_tibble(dq7tibble)

ggplot(dq7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 385") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq8
textq8Answer <- paste(q8$Answer)
corpusq8 <- Corpus(VectorSource(textq8Answer))

# Load the data as a corpus
inspect(corpusq8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq8 <- content_transformer(function (corpusq8, pattern) gsub(pattern, "", corpusq8))
docsq8 <- tm_map(corpusq8, toSpaceCorpusq8, "/")
docsq8 <- tm_map(corpusq8, toSpaceCorpusq8, "@")
docsq8 <- tm_map(corpusq8, toSpaceCorpusq8, "\\|")

# Convert the text to lower case
docsq8 <- tm_map(docsq8, content_transformer(tolower))
# Remove numbers
docsq8 <- tm_map(docsq8, removeNumbers)
# Remove english common stopwords
docsq8 <- tm_map(docsq8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docsq8 <- tm_map(docsq8, removeWords)
# Remove punctuations
docsq8 <- tm_map(docsq8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq8 <- tm_map(docsq8, stripWhitespace)
# Text stemming
# docsq8 <- tm_map(docsq8, stemDocument)

dtmq8 <- TermDocumentMatrix(docsq8)
mq8 <- as.matrix(dtmq8)
vq8 <- sort(rowSums(mq8),decreasing=TRUE)
dq8 <- data.frame(word = names(vq8),freq=vq8)

head(dq8, 500)

# View answers :)
wordcloud(words = dq8$word, freq = dq8$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
dq8tibble <- list(word = dq8$word[1:10], freq = dq8$freq[1:10])
dq8tibble <- as_tibble(dq8tibble)

ggplot(dq8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q8: Are your parents good teachers?", subtitle = "n = 360") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq9
textq9Answer <- paste(q9$Answer)
corpusq9 <- Corpus(VectorSource(textq9Answer))

# Load the data as a corpus
inspect(corpusq9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq9 <- content_transformer(function (corpusq9, pattern) gsub(pattern, "", corpusq9))
docsq9 <- tm_map(corpusq9, toSpaceCorpusq9, "/")
docsq9 <- tm_map(corpusq9, toSpaceCorpusq9, "@")
docsq9 <- tm_map(corpusq9, toSpaceCorpusq9, "\\|")

# Convert the text to lower case
docsq9 <- tm_map(docsq9, content_transformer(tolower))
# Remove numbers
docsq9 <- tm_map(docsq9, removeNumbers)
# Remove english common stopwords
docsq9 <- tm_map(docsq9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq9 <- tm_map(docsq9, removeWords)
# Remove punctuations
docsq9 <- tm_map(docsq9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq9 <- tm_map(docsq9, stripWhitespace)
# Text stemming
# docsq9 <- tm_map(docsq9, stemDocument)

dtmq9 <- TermDocumentMatrix(docsq9)
mq9 <- as.matrix(dtmq9)
vq9 <- sort(rowSums(mq9),decreasing=TRUE)
dq9 <- data.frame(word = names(vq9),freq=vq9)

head(dq9, 500)

# View answers :)
wordcloud(words = dq9$word, freq = dq9$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
dq9tibble <- list(word = dq9$word[1:10], freq = dq9$freq[1:10])
dq9tibble <- as_tibble(dq9tibble)

ggplot(dq9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q9: How did the coronavirus start?", subtitle = "n = 384") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq10
textq10Answer <- paste(q10$Answer)
corpusq10 <- Corpus(VectorSource(textq10Answer))

# Load the data as a corpus
inspect(corpusq10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq10 <- content_transformer(function (corpusq10, pattern) gsub(pattern, "", corpusq10))
docsq10 <- tm_map(corpusq10, toSpaceCorpusq10, "/")
docsq10 <- tm_map(corpusq10, toSpaceCorpusq10, "@")
docsq10 <- tm_map(corpusq10, toSpaceCorpusq10, "\\|")

# Convert the text to lower case
docsq10 <- tm_map(docsq10, content_transformer(tolower))
# Remove numbers
docsq10 <- tm_map(docsq10, removeNumbers)
# Remove english common stopwords
docsq10 <- tm_map(docsq10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq10 <- tm_map(docsq10, removeWords)
# Remove punctuations
docsq10 <- tm_map(docsq10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq10 <- tm_map(docsq10, stripWhitespace)
# Text stemming
# docsq10 <- tm_map(docsq10, stemDocument)

dtmq10 <- TermDocumentMatrix(docsq10)
mq10 <- as.matrix(dtmq10)
vq10 <- sort(rowSums(mq10),decreasing=TRUE)
dq10 <- data.frame(word = names(vq10),freq=vq10)

head(dq10, 500)

# View answers :)
wordcloud(words = dq10$word, freq = dq10$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
dq10tibble <- list(word = dq10$word[1:10], freq = dq10$freq[1:10])
dq10tibble <- as_tibble(dq10tibble)

ggplot(dq10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 356") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpusq11
textq11Answer <- paste(q11$Answer)
corpusq11 <- Corpus(VectorSource(textq11Answer))

# Load the data as a corpus
inspect(corpusq11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq11 <- content_transformer(function (corpusq11, pattern) gsub(pattern, "", corpusq11))
docsq11 <- tm_map(corpusq11, toSpaceCorpusq11, "/")
docsq11 <- tm_map(corpusq11, toSpaceCorpusq11, "@")
docsq11 <- tm_map(corpusq11, toSpaceCorpusq11, "\\|")

# Convert the text to lower case
docsq11 <- tm_map(docsq11, content_transformer(tolower))
# Remove numbers
docsq11 <- tm_map(docsq11, removeNumbers)
# Remove english common stopwords
docsq11 <- tm_map(docsq11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsq11 <- tm_map(docsq11, removeWords)
# Remove punctuations
docsq11 <- tm_map(docsq11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsq11 <- tm_map(docsq11, stripWhitespace)
# Text stemming
# docsq11 <- tm_map(docsq11, stemDocument)

dtmq11 <- TermDocumentMatrix(docsq11)
mq11 <- as.matrix(dtmq11)
vq11 <- sort(rowSums(mq11),decreasing=TRUE)
dq11 <- data.frame(word = names(vq11),freq=vq11)

head(dq11, 500)

# View answers :)
wordcloud(words = dq11$word, freq = dq11$freq, min.freq = 1,
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
dq11tibble <- list(word = dq11$word[1:10], freq = dq11$freq[1:10])
dq11tibble <- as_tibble(dq11tibble)

ggplot(dq11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words for Q11: Are you enjoying lockdown?", subtitle = "n = 339") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Now by age
x101qAll <- paste(x101$Answer)
x102qAll <- paste(x102$Answer)
x103qAll <- paste(x103$Answer)
x104qAll <- paste(x104$Answer)
x105qAll <- paste(x105$Answer)
x106qAll <- paste(x106$Answer)
x107qAll <- paste(x107$Answer)
x108qAll <- paste(x108$Answer)
x109qAll <- paste(x109$Answer)
x110qAll <- paste(x110$Answer)
x111qAll <- paste(x111$Answer)
x112qAll <- paste(x112$Answer)
x113qAll <- paste(x113$Answer)
x114qAll <- paste(x114$Answer)
xUnknownqAll <- paste(xUnknown$Answer)


##################################################################################################
### Age 2 ###
##################################################################################################

# Corpus102q1
text102q1Answer <- paste(x102q1$Answer)
corpus102q1 <- Corpus(VectorSource(text102q1Answer))

# Load the data as a corpus
inspect(corpus102q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q1 <- content_transformer(function (corpus102q1, pattern) gsub(pattern, "", corpus102q1))
docs102q1 <- tm_map(corpus102q1, toSpaceCorpusq1, "/")
docs102q1 <- tm_map(corpus102q1, toSpaceCorpusq1, "@")
docs102q1 <- tm_map(corpus102q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs102q1 <- tm_map(docs102q1, content_transformer(tolower))
# Remove numbers
docs102q1 <- tm_map(docs102q1, removeNumbers)
# Remove english common stopwords
docs102q1 <- tm_map(docs102q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q1 <- tm_map(docs102q1, removeWords)
# Remove punctuations
docs102q1 <- tm_map(docs102q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q1 <- tm_map(docs102q1, stripWhitespace)
# Text stemming
# docs102q1 <- tm_map(docs102q1, stemDocument)

dtm102q1 <- TermDocumentMatrix(docs102q1)
m102q1 <- as.matrix(dtm102q1)
v102q1 <- sort(rowSums(m102q1),decreasing=TRUE)
d102q1 <- data.frame(word = names(v102q1),freq=v102q1)

head(d102q1, 500)


# View answers :)
wordcloud102q1 <- wordcloud(words = d102q1$word, freq = d102q1$freq, min.freq = 1,scale=c(3,.5), # change min.freq to see different results
                            max.words=999, random.order=FALSE, rot.per=0.0, 
                            colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 102q1
d102q1tibble <- list(word = d102q1$word[1:10], freq = d102q1$freq[1:10])
d102q1tibble <- as_tibble(d102q1tibble)

ggplot(d102q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q1: What is the coronavirus?", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q2
text102q2Answer <- paste(x102q2$Answer)
corpus102q2 <- Corpus(VectorSource(text102q2Answer))

# Load the data as a corpus
inspect(corpus102q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus102q2, pattern) gsub(pattern, "", corpus102q2))
docs102q2 <- tm_map(corpus102q2, toSpaceCorpusq2, "/")
docs102q2 <- tm_map(corpus102q2, toSpaceCorpusq2, "@")
docs102q2 <- tm_map(corpus102q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs102q2 <- tm_map(docs102q2, content_transformer(tolower))
# Remove numbers
docs102q2 <- tm_map(docs102q2, removeNumbers)
# Remove english common stopwords
docs102q2 <- tm_map(docs102q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q2 <- tm_map(docs102q2, removeWords)
# Remove punctuations
docs102q2 <- tm_map(docs102q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q2 <- tm_map(docs102q2, stripWhitespace)
# Text stemming
# docs102q2 <- tm_map(docs102q2, stemDocument)

dtm102q2 <- TermDocumentMatrix(docs102q2)
m102q2 <- as.matrix(dtm102q2)
v102q2 <- sort(rowSums(m102q2),decreasing=TRUE)
d102q2 <- data.frame(word = names(v102q2),freq=v102q2)

head(d102q2, 500)

# View answers :)
wordcloud102q2 <- wordcloud(words = d102q2$word, freq = d102q2$freq, min.freq = 1,scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d102q2tibble <- list(word = d102q2$word[1:10], freq = d102q2$freq[1:10])
d102q2tibble <- as_tibble(d102q2tibble)

ggplot(d102q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q2: Who is the president/prime minister?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q3
text102q3Answer <- paste(x102q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus102q3 <- Corpus(VectorSource(text102q3Answer))

# Load the data as a corpus
inspect(corpus102q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q3 <- content_transformer(function (corpus102q3, pattern) gsub(pattern, "", corpus102q3))
docs102q3 <- tm_map(corpus102q3, toSpaceCorpus102q3, "/")
docs102q3 <- tm_map(corpus102q3, toSpaceCorpus102q3, "@")
docs102q3 <- tm_map(corpus102q3, toSpaceCorpus102q3, "\\|")

# Convert the text to lower case
docs102q3 <- tm_map(docs102q3, content_transformer(tolower))
# Remove numbers
# docs102q3 <- tm_map(docs102q3, removeNumbers)
# Remove english common stopwords
docs102q3 <- tm_map(docs102q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q3 <- tm_map(docs102q3, removeWords)
# Remove punctuations
docs102q3 <- tm_map(docs102q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q3 <- tm_map(docs102q3, stripWhitespace)
# Text stemming
# docs102q3 <- tm_map(docs102q3, stemDocument)

dtm102q3 <- TermDocumentMatrix(docs102q3)
m102q3 <- as.matrix(dtm102q3)
v102q3 <- sort(rowSums(m102q3),decreasing=TRUE)
d102q3 <- data.frame(word = names(v102q3),freq=v102q3)

head(d102q3, 500)

# View answers :)
wordcloud102q3 <- wordcloud(words = d102q3$word, freq = d102q3$freq, min.freq = 1, scale=c(3,.5), 
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d102q3tibble <- list(word = d102q3$word[1:10], freq = d102q3$freq[1:10])
d102q3tibble <- as_tibble(d102q3tibble)

ggplot(d102q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q3: How many days have we been in lockdown?", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q4
text102q4Answer <- paste(x102q4$Answer)
corpus102q4 <- Corpus(VectorSource(text102q4Answer))

# Load the data as a corpus
inspect(corpus102q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q4 <- content_transformer(function (corpus102q4, pattern) gsub(pattern, "", corpus102q4))
docs102q4 <- tm_map(corpus102q4, toSpaceCorpus102q4, "/")
docs102q4 <- tm_map(corpus102q4, toSpaceCorpus102q4, "@")
docs102q4 <- tm_map(corpus102q4, toSpaceCorpus102q4, "\\|")

# Convert the text to lower case
docs102q4 <- tm_map(docs102q4, content_transformer(tolower))
# Remove numbers
docs102q4 <- tm_map(docs102q4, removeNumbers)
# Remove english common stopwords
docs102q4 <- tm_map(docs102q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q4 <- tm_map(docs102q4, removeWords)
# Remove punctuations
docs102q4 <- tm_map(docs102q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q4 <- tm_map(docs102q4, stripWhitespace)
# Text stemming
# docs102q4 <- tm_map(docs102q4, stemDocument)

dtm102q4 <- TermDocumentMatrix(docs102q4)
m102q4 <- as.matrix(dtm102q4)
v102q4 <- sort(rowSums(m102q4),decreasing=TRUE)
d102q4 <- data.frame(word = names(v102q4),freq=v102q4)

head(d102q4, 500)

# View answers :)
wordcloud(words = d102q4$word, freq = d102q4$freq, min.freq = 1, scale=c(3,.5), 
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d102q4tibble <- list(word = d102q4$word[1:10], freq = d102q4$freq[1:10])
d102q4tibble <- as_tibble(d102q4tibble)

ggplot(d102q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q4: Do you want to go back to school?", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q5
text102q5Answer <- paste(x102q5$Answer)
corpus102q5 <- Corpus(VectorSource(text102q5Answer))

# Load the data as a corpus
inspect(corpus102q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q5 <- content_transformer(function (corpus102q5, pattern) gsub(pattern, "", corpus102q5))
docs102q5 <- tm_map(corpus102q5, toSpaceCorpus102q5, "/")
docs102q5 <- tm_map(corpus102q5, toSpaceCorpus102q5, "@")
docs102q5 <- tm_map(corpus102q5, toSpaceCorpus102q5, "\\|")

# Convert the text to lower case
docs102q5 <- tm_map(docs102q5, content_transformer(tolower))
# Remove numbers
docs102q5 <- tm_map(docs102q5, removeNumbers)
# Remove english common stopwords
docs102q5 <- tm_map(docs102q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q5 <- tm_map(docs102q5, removeWords)
# Remove punctuations
docs102q5 <- tm_map(docs102q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q5 <- tm_map(docs102q5, stripWhitespace)
# Text stemming
# docs102q5 <- tm_map(docs102q5, stemDocument)

dtm102q5 <- TermDocumentMatrix(docs102q5)
m102q5 <- as.matrix(dtm102q5)
v102q5 <- sort(rowSums(m102q5),decreasing=TRUE)
d102q5 <- data.frame(word = names(v102q5),freq=v102q5)

head(d102q5, 500)

# View answers :)
wordcloud102q5 <- wordcloud(words = d102q5$word, freq = d102q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d102q5tibble <- list(word = d102q5$word[1:10], freq = d102q5$freq[1:10])
d102q5tibble <- as_tibble(d102q5tibble)

ggplot(d102q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q6
text102q6Answer <- paste(x102q6$Answer)
corpus102q6 <- Corpus(VectorSource(text102q6Answer))

# Load the data as a corpus
inspect(corpus102q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q6 <- content_transformer(function (corpus102q6, pattern) gsub(pattern, "", corpus102q6))
docs102q6 <- tm_map(corpus102q6, toSpaceCorpus102q6, "/")
docs102q6 <- tm_map(corpus102q6, toSpaceCorpus102q6, "@")
docs102q6 <- tm_map(corpus102q6, toSpaceCorpus102q6, "\\|")

# Convert the text to lower case
docs102q6 <- tm_map(docs102q6, content_transformer(tolower))
# Remove numbers
docs102q6 <- tm_map(docs102q6, removeNumbers)
# Remove english common stopwords
docs102q6 <- tm_map(docs102q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q6 <- tm_map(docs102q6, removeWords)
# Remove punctuations
docs102q6 <- tm_map(docs102q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q6 <- tm_map(docs102q6, stripWhitespace)
# Text stemming
# docs102q6 <- tm_map(docs102q6, stemDocument)

dtm102q6 <- TermDocumentMatrix(docs102q6)
m102q6 <- as.matrix(dtm102q6)
v102q6 <- sort(rowSums(m102q6),decreasing=TRUE)
d102q6 <- data.frame(word = names(v102q6),freq=v102q6)

head(d102q6, 500)

# View answers :)
wordcloud102q6 <- wordcloud(words = d102q6$word, freq = d102q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d102q6tibble <- list(word = d102q6$word[1:10], freq = d102q6$freq[1:10])
d102q6tibble <- as_tibble(d102q6tibble)

ggplot(d102q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q6: Where is the first place you want to go?", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q7
text102q7Answer <- paste(x102q7$Answer)
corpus102q7 <- Corpus(VectorSource(text102q7Answer))

# Load the data as a corpus
inspect(corpus102q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q7 <- content_transformer(function (corpus102q7, pattern) gsub(pattern, "", corpus102q7))
docs102q7 <- tm_map(corpus102q7, toSpaceCorpus102q7, "/")
docs102q7 <- tm_map(corpus102q7, toSpaceCorpus102q7, "@")
docs102q7 <- tm_map(corpus102q7, toSpaceCorpus102q7, "\\|")

# Convert the text to lower case
docs102q7 <- tm_map(docs102q7, content_transformer(tolower))
# Remove numbers
docs102q7 <- tm_map(docs102q7, removeNumbers)
# Remove english common stopwords
docs102q7 <- tm_map(docs102q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q7 <- tm_map(docs102q7, removeWords)
# Remove punctuations
docs102q7 <- tm_map(docs102q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q7 <- tm_map(docs102q7, stripWhitespace)
# Text stemming
# docs102q7 <- tm_map(docs102q7, stemDocument)

dtm102q7 <- TermDocumentMatrix(docs102q7)
m102q7 <- as.matrix(dtm102q7)
v102q7 <- sort(rowSums(m102q7),decreasing=TRUE)
d102q7 <- data.frame(word = names(v102q7),freq=v102q7)

head(d102q7, 500)

# View answers :)
wordcloud102q7 <- wordcloud(words = d102q7$word, freq = d102q7$freq, min.freq = 1, scale=c(2.5,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d102q7tibble <- list(word = d102q7$word[1:10], freq = d102q7$freq[1:10])
d102q7tibble <- as_tibble(d102q7tibble)

ggplot(d102q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q8
text102q8Answer <- paste(x102q8$Answer)
corpus102q8 <- Corpus(VectorSource(text102q8Answer))

# Load the data as a corpus
inspect(corpus102q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q8 <- content_transformer(function (corpus102q8, pattern) gsub(pattern, "", corpus102q8))
docs102q8 <- tm_map(corpus102q8, toSpaceCorpus102q8, "/")
docs102q8 <- tm_map(corpus102q8, toSpaceCorpus102q8, "@")
docs102q8 <- tm_map(corpus102q8, toSpaceCorpus102q8, "\\|")

# Convert the text to lower case
docs102q8 <- tm_map(docs102q8, content_transformer(tolower))
# Remove numbers
docs102q8 <- tm_map(docs102q8, removeNumbers)
# Remove english common stopwords
docs102q8 <- tm_map(docs102q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs102q8 <- tm_map(docs102q8, removeWords)
# Remove punctuations
docs102q8 <- tm_map(docs102q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q8 <- tm_map(docs102q8, stripWhitespace)
# Text stemming
# docs102q8 <- tm_map(docs102q8, stemDocument)

dtm102q8 <- TermDocumentMatrix(docs102q8)
m102q8 <- as.matrix(dtm102q8)
v102q8 <- sort(rowSums(m102q8),decreasing=TRUE)
d102q8 <- data.frame(word = names(v102q8),freq=v102q8)

head(d102q8, 500)

# View answers :)
wordcloud102q8 <- wordcloud(words = d102q8$word, freq = d102q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d102q8tibble <- list(word = d102q8$word[1:10], freq = d102q8$freq[1:10])
d102q8tibble <- as_tibble(d102q8tibble)

ggplot(d102q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q8: Are your parents good teachers?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q9
text102q9Answer <- paste(x102q9$Answer)
corpus102q9 <- Corpus(VectorSource(text102q9Answer))

# Load the data as a corpus
inspect(corpus102q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q9 <- content_transformer(function (corpus102q9, pattern) gsub(pattern, "", corpus102q9))
docs102q9 <- tm_map(corpus102q9, toSpaceCorpus102q9, "/")
docs102q9 <- tm_map(corpus102q9, toSpaceCorpus102q9, "@")
docs102q9 <- tm_map(corpus102q9, toSpaceCorpus102q9, "\\|")

# Convert the text to lower case
docs102q9 <- tm_map(docs102q9, content_transformer(tolower))
# Remove numbers
docs102q9 <- tm_map(docs102q9, removeNumbers)
# Remove english common stopwords
docs102q9 <- tm_map(docs102q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q9 <- tm_map(docs102q9, removeWords)
# Remove punctuations
docs102q9 <- tm_map(docs102q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q9 <- tm_map(docs102q9, stripWhitespace)
# Text stemming
# docs102q9 <- tm_map(docs102q9, stemDocument)

dtm102q9 <- TermDocumentMatrix(docs102q9)
m102q9 <- as.matrix(dtm102q9)
v102q9 <- sort(rowSums(m102q9),decreasing=TRUE)
d102q9 <- data.frame(word = names(v102q9),freq=v102q9)

head(d102q9, 500)

# View answers :)
wordcloud102q9 <- wordcloud(words = d102q9$word, freq = d102q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d102q9tibble <- list(word = d102q9$word[1:10], freq = d102q9$freq[1:10])
d102q9tibble <- as_tibble(d102q9tibble)

ggplot(d102q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q9: How did the coronavirus start?", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q10
text102q10Answer <- paste(x102q10$Answer)
corpus102q10 <- Corpus(VectorSource(text102q10Answer))

# Load the data as a corpus
inspect(corpus102q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q10 <- content_transformer(function (corpus102q10, pattern) gsub(pattern, "", corpus102q10))
docs102q10 <- tm_map(corpus102q10, toSpaceCorpus102q10, "/")
docs102q10 <- tm_map(corpus102q10, toSpaceCorpus102q10, "@")
docs102q10 <- tm_map(corpus102q10, toSpaceCorpus102q10, "\\|")

# Convert the text to lower case
docs102q10 <- tm_map(docs102q10, content_transformer(tolower))
# Remove numbers
docs102q10 <- tm_map(docs102q10, removeNumbers)
# Remove english common stopwords
docs102q10 <- tm_map(docs102q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q10 <- tm_map(docs102q10, removeWords)
# Remove punctuations
docs102q10 <- tm_map(docs102q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q10 <- tm_map(docs102q10, stripWhitespace)
# Text stemming
# docs102q10 <- tm_map(docs102q10, stemDocument)

dtm102q10 <- TermDocumentMatrix(docs102q10)
m102q10 <- as.matrix(dtm102q10)
v102q10 <- sort(rowSums(m102q10),decreasing=TRUE)
d102q10 <- data.frame(word = names(v102q10),freq=v102q10)

head(d102q10, 500)

# View answers :)
wordcloud102q10 <- wordcloud(words = d102q10$word, freq = d102q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d102q10tibble <- list(word = d102q10$word[1:10], freq = d102q10$freq[1:10])
d102q10tibble <- as_tibble(d102q10tibble)

ggplot(d102q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus102q11
text102q11Answer <- paste(x102q11$Answer)
corpus102q11 <- Corpus(VectorSource(text102q11Answer))

# Load the data as a corpus
inspect(corpus102q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus102q11 <- content_transformer(function (corpus102q11, pattern) gsub(pattern, "", corpus102q11))
docs102q11 <- tm_map(corpus102q11, toSpaceCorpus102q11, "/")
docs102q11 <- tm_map(corpus102q11, toSpaceCorpus102q11, "@")
docs102q11 <- tm_map(corpus102q11, toSpaceCorpus102q11, "\\|")

# Convert the text to lower case
docs102q11 <- tm_map(docs102q11, content_transformer(tolower))
# Remove numbers
docs102q11 <- tm_map(docs102q11, removeNumbers)
# Remove english common stopwords
docs102q11 <- tm_map(docs102q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs102q11 <- tm_map(docs102q11, removeWords)
# Remove punctuations
docs102q11 <- tm_map(docs102q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs102q11 <- tm_map(docs102q11, stripWhitespace)
# Text stemming
# docs102q11 <- tm_map(docs102q11, stemDocument)

dtm102q11 <- TermDocumentMatrix(docs102q11)
m102q11 <- as.matrix(dtm102q11)
v102q11 <- sort(rowSums(m102q11),decreasing=TRUE)
d102q11 <- data.frame(word = names(v102q11),freq=v102q11)

head(d102q11, 500)

# View answers :)
wordcloud102q11 <- wordcloud(words = d102q11$word, freq = d102q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d102q11tibble <- list(word = d102q11$word[1:10], freq = d102q11$freq[1:10])
d102q11tibble <- as_tibble(d102q11tibble)

ggplot(d102q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 2 - Q11: Are you enjoying lockdown?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())




##################################################################################################
### Age 3 ###
##################################################################################################
# Corpus103q1
text103q1Answer <- paste(x103q1$Answer)
corpus103q1 <- Corpus(VectorSource(text103q1Answer))

# Load the data as a corpus
inspect(corpus103q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q1 <- content_transformer(function (corpus103q1, pattern) gsub(pattern, "", corpus103q1))
docs103q1 <- tm_map(corpus103q1, toSpaceCorpusq1, "/")
docs103q1 <- tm_map(corpus103q1, toSpaceCorpusq1, "@")
docs103q1 <- tm_map(corpus103q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs103q1 <- tm_map(docs103q1, content_transformer(tolower))
# Remove numbers
docs103q1 <- tm_map(docs103q1, removeNumbers)
# Remove english common stopwords
docs103q1 <- tm_map(docs103q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q1 <- tm_map(docs103q1, removeWords)
# Remove punctuations
docs103q1 <- tm_map(docs103q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q1 <- tm_map(docs103q1, stripWhitespace)
# Text stemming
# docs103q1 <- tm_map(docs103q1, stemDocument)

dtm103q1 <- TermDocumentMatrix(docs103q1)
m103q1 <- as.matrix(dtm103q1)
v103q1 <- sort(rowSums(m103q1),decreasing=TRUE)
d103q1 <- data.frame(word = names(v103q1),freq=v103q1)

head(d103q1, 500)

# View answers :)
wordcloud103q1 <- wordcloud(words = d103q1$word, freq = d103q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
                            max.words=999, random.order=FALSE, rot.per=0.0, 
                            colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 103q1
d103q1tibble <- list(word = d103q1$word[1:10], freq = d103q1$freq[1:10])
d103q1tibble <- as_tibble(d103q1tibble)

ggplot(d103q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q1: What is the coronavirus?", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q2
text103q2Answer <- paste(x103q2$Answer)
corpus103q2 <- Corpus(VectorSource(text103q2Answer))

# Load the data as a corpus
inspect(corpus103q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus103q2, pattern) gsub(pattern, "", corpus103q2))
docs103q2 <- tm_map(corpus103q2, toSpaceCorpusq2, "/")
docs103q2 <- tm_map(corpus103q2, toSpaceCorpusq2, "@")
docs103q2 <- tm_map(corpus103q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs103q2 <- tm_map(docs103q2, content_transformer(tolower))
# Remove numbers
docs103q2 <- tm_map(docs103q2, removeNumbers)
# Remove english common stopwords
docs103q2 <- tm_map(docs103q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q2 <- tm_map(docs103q2, removeWords)
# Remove punctuations
docs103q2 <- tm_map(docs103q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q2 <- tm_map(docs103q2, stripWhitespace)
# Text stemming
# docs103q2 <- tm_map(docs103q2, stemDocument)

dtm103q2 <- TermDocumentMatrix(docs103q2)
m103q2 <- as.matrix(dtm103q2)
v103q2 <- sort(rowSums(m103q2),decreasing=TRUE)
d103q2 <- data.frame(word = names(v103q2),freq=v103q2)

head(d103q2, 500)

# View answers :)
wordcloud103q2 <- wordcloud(words = d103q2$word, freq = d103q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d103q2tibble <- list(word = d103q2$word[1:10], freq = d103q2$freq[1:10])
d103q2tibble <- as_tibble(d103q2tibble)

ggplot(d103q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q2: Who is the president/prime minister?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q3
text103q3Answer <- paste(x103q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus103q3 <- Corpus(VectorSource(text103q3Answer))

# Load the data as a corpus
inspect(corpus103q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q3 <- content_transformer(function (corpus103q3, pattern) gsub(pattern, "", corpus103q3))
docs103q3 <- tm_map(corpus103q3, toSpaceCorpus103q3, "/")
docs103q3 <- tm_map(corpus103q3, toSpaceCorpus103q3, "@")
docs103q3 <- tm_map(corpus103q3, toSpaceCorpus103q3, "\\|")

# Convert the text to lower case
docs103q3 <- tm_map(docs103q3, content_transformer(tolower))
# Remove numbers
# docs103q3 <- tm_map(docs103q3, removeNumbers)
# Remove english common stopwords
docs103q3 <- tm_map(docs103q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q3 <- tm_map(docs103q3, removeWords)
# Remove punctuations
docs103q3 <- tm_map(docs103q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q3 <- tm_map(docs103q3, stripWhitespace)
# Text stemming
# docs103q3 <- tm_map(docs103q3, stemDocument)

dtm103q3 <- TermDocumentMatrix(docs103q3)
m103q3 <- as.matrix(dtm103q3)
v103q3 <- sort(rowSums(m103q3),decreasing=TRUE)
d103q3 <- data.frame(word = names(v103q3),freq=v103q3)

head(d103q3, 500)

# View answers :)
wordcloud103q3 <- wordcloud(words = d103q3$word, freq = d103q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d103q3tibble <- list(word = d103q3$word[1:10], freq = d103q3$freq[1:10])
d103q3tibble <- as_tibble(d103q3tibble)

ggplot(d103q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q3: How many days have we been in lockdown?", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q4
text103q4Answer <- paste(x103q4$Answer)
corpus103q4 <- Corpus(VectorSource(text103q4Answer))

# Load the data as a corpus
inspect(corpus103q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q4 <- content_transformer(function (corpus103q4, pattern) gsub(pattern, "", corpus103q4))
docs103q4 <- tm_map(corpus103q4, toSpaceCorpus103q4, "/")
docs103q4 <- tm_map(corpus103q4, toSpaceCorpus103q4, "@")
docs103q4 <- tm_map(corpus103q4, toSpaceCorpus103q4, "\\|")

# Convert the text to lower case
docs103q4 <- tm_map(docs103q4, content_transformer(tolower))
# Remove numbers
docs103q4 <- tm_map(docs103q4, removeNumbers)
# Remove english common stopwords
docs103q4 <- tm_map(docs103q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q4 <- tm_map(docs103q4, removeWords)
# Remove punctuations
docs103q4 <- tm_map(docs103q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q4 <- tm_map(docs103q4, stripWhitespace)
# Text stemming
# docs103q4 <- tm_map(docs103q4, stemDocument)

dtm103q4 <- TermDocumentMatrix(docs103q4)
m103q4 <- as.matrix(dtm103q4)
v103q4 <- sort(rowSums(m103q4),decreasing=TRUE)
d103q4 <- data.frame(word = names(v103q4),freq=v103q4)

head(d103q4, 500)

# View answers :)
wordcloud103q4 <- wordcloud(words = d103q4$word, freq = d103q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d103q4tibble <- list(word = d103q4$word[1:10], freq = d103q4$freq[1:10])
d103q4tibble <- as_tibble(d103q4tibble)

ggplot(d103q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q4: Do you want to go back to school?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q5
text103q5Answer <- paste(x103q5$Answer)
corpus103q5 <- Corpus(VectorSource(text103q5Answer))

# Load the data as a corpus
inspect(corpus103q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q5 <- content_transformer(function (corpus103q5, pattern) gsub(pattern, "", corpus103q5))
docs103q5 <- tm_map(corpus103q5, toSpaceCorpus103q5, "/")
docs103q5 <- tm_map(corpus103q5, toSpaceCorpus103q5, "@")
docs103q5 <- tm_map(corpus103q5, toSpaceCorpus103q5, "\\|")

# Convert the text to lower case
docs103q5 <- tm_map(docs103q5, content_transformer(tolower))
# Remove numbers
docs103q5 <- tm_map(docs103q5, removeNumbers)
# Remove english common stopwords
docs103q5 <- tm_map(docs103q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q5 <- tm_map(docs103q5, removeWords)
# Remove punctuations
docs103q5 <- tm_map(docs103q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q5 <- tm_map(docs103q5, stripWhitespace)
# Text stemming
# docs103q5 <- tm_map(docs103q5, stemDocument)

dtm103q5 <- TermDocumentMatrix(docs103q5)
m103q5 <- as.matrix(dtm103q5)
v103q5 <- sort(rowSums(m103q5),decreasing=TRUE)
d103q5 <- data.frame(word = names(v103q5),freq=v103q5)

head(d103q5, 500)

# View answers :)
wordcloud103q5 <- wordcloud(words = d103q5$word, freq = d103q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d103q5tibble <- list(word = d103q5$word[1:10], freq = d103q5$freq[1:10])
d103q5tibble <- as_tibble(d103q5tibble)

ggplot(d103q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q6
text103q6Answer <- paste(x103q6$Answer)
corpus103q6 <- Corpus(VectorSource(text103q6Answer))

# Load the data as a corpus
inspect(corpus103q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q6 <- content_transformer(function (corpus103q6, pattern) gsub(pattern, "", corpus103q6))
docs103q6 <- tm_map(corpus103q6, toSpaceCorpus103q6, "/")
docs103q6 <- tm_map(corpus103q6, toSpaceCorpus103q6, "@")
docs103q6 <- tm_map(corpus103q6, toSpaceCorpus103q6, "\\|")

# Convert the text to lower case
docs103q6 <- tm_map(docs103q6, content_transformer(tolower))
# Remove numbers
docs103q6 <- tm_map(docs103q6, removeNumbers)
# Remove english common stopwords
docs103q6 <- tm_map(docs103q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q6 <- tm_map(docs103q6, removeWords)
# Remove punctuations
docs103q6 <- tm_map(docs103q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q6 <- tm_map(docs103q6, stripWhitespace)
# Text stemming
# docs103q6 <- tm_map(docs103q6, stemDocument)

dtm103q6 <- TermDocumentMatrix(docs103q6)
m103q6 <- as.matrix(dtm103q6)
v103q6 <- sort(rowSums(m103q6),decreasing=TRUE)
d103q6 <- data.frame(word = names(v103q6),freq=v103q6)

head(d103q6, 500)

# View answers :)
wordcloud103q6 <- wordcloud(words = d103q6$word, freq = d103q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d103q6tibble <- list(word = d103q6$word[1:10], freq = d103q6$freq[1:10])
d103q6tibble <- as_tibble(d103q6tibble)

ggplot(d103q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q6: Where is the first place you want to go?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q7
text103q7Answer <- paste(x103q7$Answer)
corpus103q7 <- Corpus(VectorSource(text103q7Answer))

# Load the data as a corpus
inspect(corpus103q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q7 <- content_transformer(function (corpus103q7, pattern) gsub(pattern, "", corpus103q7))
docs103q7 <- tm_map(corpus103q7, toSpaceCorpus103q7, "/")
docs103q7 <- tm_map(corpus103q7, toSpaceCorpus103q7, "@")
docs103q7 <- tm_map(corpus103q7, toSpaceCorpus103q7, "\\|")

# Convert the text to lower case
docs103q7 <- tm_map(docs103q7, content_transformer(tolower))
# Remove numbers
docs103q7 <- tm_map(docs103q7, removeNumbers)
# Remove english common stopwords
docs103q7 <- tm_map(docs103q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q7 <- tm_map(docs103q7, removeWords)
# Remove punctuations
docs103q7 <- tm_map(docs103q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q7 <- tm_map(docs103q7, stripWhitespace)
# Text stemming
# docs103q7 <- tm_map(docs103q7, stemDocument)

dtm103q7 <- TermDocumentMatrix(docs103q7)
m103q7 <- as.matrix(dtm103q7)
v103q7 <- sort(rowSums(m103q7),decreasing=TRUE)
d103q7 <- data.frame(word = names(v103q7),freq=v103q7)

head(d103q7, 500)

# View answers :)
wordcloud103q7 <- wordcloud(words = d103q7$word, freq = d103q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d103q7tibble <- list(word = d103q7$word[1:10], freq = d103q7$freq[1:10])
d103q7tibble <- as_tibble(d103q7tibble)

ggplot(d103q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q8
text103q8Answer <- paste(x103q8$Answer)
corpus103q8 <- Corpus(VectorSource(text103q8Answer))

# Load the data as a corpus
inspect(corpus103q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q8 <- content_transformer(function (corpus103q8, pattern) gsub(pattern, "", corpus103q8))
docs103q8 <- tm_map(corpus103q8, toSpaceCorpus103q8, "/")
docs103q8 <- tm_map(corpus103q8, toSpaceCorpus103q8, "@")
docs103q8 <- tm_map(corpus103q8, toSpaceCorpus103q8, "\\|")

# Convert the text to lower case
docs103q8 <- tm_map(docs103q8, content_transformer(tolower))
# Remove numbers
docs103q8 <- tm_map(docs103q8, removeNumbers)
# Remove english common stopwords
docs103q8 <- tm_map(docs103q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs103q8 <- tm_map(docs103q8, removeWords)
# Remove punctuations
docs103q8 <- tm_map(docs103q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q8 <- tm_map(docs103q8, stripWhitespace)
# Text stemming
# docs103q8 <- tm_map(docs103q8, stemDocument)

dtm103q8 <- TermDocumentMatrix(docs103q8)
m103q8 <- as.matrix(dtm103q8)
v103q8 <- sort(rowSums(m103q8),decreasing=TRUE)
d103q8 <- data.frame(word = names(v103q8),freq=v103q8)

head(d103q8, 500)

# View answers :)
wordcloud103q8 <- wordcloud(words = d103q8$word, freq = d103q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d103q8tibble <- list(word = d103q8$word[1:10], freq = d103q8$freq[1:10])
d103q8tibble <- as_tibble(d103q8tibble)

ggplot(d103q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q8: Are your parents good teachers?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q9
text103q9Answer <- paste(x103q9$Answer)
corpus103q9 <- Corpus(VectorSource(text103q9Answer))

# Load the data as a corpus
inspect(corpus103q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q9 <- content_transformer(function (corpus103q9, pattern) gsub(pattern, "", corpus103q9))
docs103q9 <- tm_map(corpus103q9, toSpaceCorpus103q9, "/")
docs103q9 <- tm_map(corpus103q9, toSpaceCorpus103q9, "@")
docs103q9 <- tm_map(corpus103q9, toSpaceCorpus103q9, "\\|")

# Convert the text to lower case
docs103q9 <- tm_map(docs103q9, content_transformer(tolower))
# Remove numbers
docs103q9 <- tm_map(docs103q9, removeNumbers)
# Remove english common stopwords
docs103q9 <- tm_map(docs103q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q9 <- tm_map(docs103q9, removeWords)
# Remove punctuations
docs103q9 <- tm_map(docs103q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q9 <- tm_map(docs103q9, stripWhitespace)
# Text stemming
# docs103q9 <- tm_map(docs103q9, stemDocument)

dtm103q9 <- TermDocumentMatrix(docs103q9)
m103q9 <- as.matrix(dtm103q9)
v103q9 <- sort(rowSums(m103q9),decreasing=TRUE)
d103q9 <- data.frame(word = names(v103q9),freq=v103q9)

head(d103q9, 500)

# View answers :)
wordcloud103q9 <- wordcloud(words = d103q9$word, freq = d103q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d103q9tibble <- list(word = d103q9$word[1:10], freq = d103q9$freq[1:10])
d103q9tibble <- as_tibble(d103q9tibble)

ggplot(d103q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q9: How did the coronavirus start?", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q10
text103q10Answer <- paste(x103q10$Answer)
corpus103q10 <- Corpus(VectorSource(text103q10Answer))

# Load the data as a corpus
inspect(corpus103q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q10 <- content_transformer(function (corpus103q10, pattern) gsub(pattern, "", corpus103q10))
docs103q10 <- tm_map(corpus103q10, toSpaceCorpus103q10, "/")
docs103q10 <- tm_map(corpus103q10, toSpaceCorpus103q10, "@")
docs103q10 <- tm_map(corpus103q10, toSpaceCorpus103q10, "\\|")

# Convert the text to lower case
docs103q10 <- tm_map(docs103q10, content_transformer(tolower))
# Remove numbers
docs103q10 <- tm_map(docs103q10, removeNumbers)
# Remove english common stopwords
docs103q10 <- tm_map(docs103q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q10 <- tm_map(docs103q10, removeWords)
# Remove punctuations
docs103q10 <- tm_map(docs103q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q10 <- tm_map(docs103q10, stripWhitespace)
# Text stemming
# docs103q10 <- tm_map(docs103q10, stemDocument)

dtm103q10 <- TermDocumentMatrix(docs103q10)
m103q10 <- as.matrix(dtm103q10)
v103q10 <- sort(rowSums(m103q10),decreasing=TRUE)
d103q10 <- data.frame(word = names(v103q10),freq=v103q10)

head(d103q10, 500)

# View answers :)
wordcloud103q10 <- wordcloud(words = d103q10$word, freq = d103q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d103q10tibble <- list(word = d103q10$word[1:10], freq = d103q10$freq[1:10])
d103q10tibble <- as_tibble(d103q10tibble)

ggplot(d103q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus103q11
text103q11Answer <- paste(x103q11$Answer)
corpus103q11 <- Corpus(VectorSource(text103q11Answer))

# Load the data as a corpus
inspect(corpus103q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus103q11 <- content_transformer(function (corpus103q11, pattern) gsub(pattern, "", corpus103q11))
docs103q11 <- tm_map(corpus103q11, toSpaceCorpus103q11, "/")
docs103q11 <- tm_map(corpus103q11, toSpaceCorpus103q11, "@")
docs103q11 <- tm_map(corpus103q11, toSpaceCorpus103q11, "\\|")

# Convert the text to lower case
docs103q11 <- tm_map(docs103q11, content_transformer(tolower))
# Remove numbers
docs103q11 <- tm_map(docs103q11, removeNumbers)
# Remove english common stopwords
docs103q11 <- tm_map(docs103q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs103q11 <- tm_map(docs103q11, removeWords)
# Remove punctuations
docs103q11 <- tm_map(docs103q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs103q11 <- tm_map(docs103q11, stripWhitespace)
# Text stemming
# docs103q11 <- tm_map(docs103q11, stemDocument)

dtm103q11 <- TermDocumentMatrix(docs103q11)
m103q11 <- as.matrix(dtm103q11)
v103q11 <- sort(rowSums(m103q11),decreasing=TRUE)
d103q11 <- data.frame(word = names(v103q11),freq=v103q11)

head(d103q11, 500)

# View answers :)
wordcloud103q11 <- wordcloud(words = d103q11$word, freq = d103q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d103q11tibble <- list(word = d103q11$word[1:10], freq = d103q11$freq[1:10])
d103q11tibble <- as_tibble(d103q11tibble)

ggplot(d103q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 3 - Q11: Are you enjoying lockdown?", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 4 ###
##################################################################################################
# Corpus104q1
text104q1Answer <- paste(x104q1$Answer)
corpus104q1 <- Corpus(VectorSource(text104q1Answer))

# Load the data as a corpus
inspect(corpus104q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q1 <- content_transformer(function (corpus104q1, pattern) gsub(pattern, "", corpus104q1))
docs104q1 <- tm_map(corpus104q1, toSpaceCorpusq1, "/")
docs104q1 <- tm_map(corpus104q1, toSpaceCorpusq1, "@")
docs104q1 <- tm_map(corpus104q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs104q1 <- tm_map(docs104q1, content_transformer(tolower))
# Remove numbers
docs104q1 <- tm_map(docs104q1, removeNumbers)
# Remove english common stopwords
docs104q1 <- tm_map(docs104q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q1 <- tm_map(docs104q1, removeWords)
# Remove punctuations
docs104q1 <- tm_map(docs104q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q1 <- tm_map(docs104q1, stripWhitespace)
# Text stemming
# docs104q1 <- tm_map(docs104q1, stemDocument)

dtm104q1 <- TermDocumentMatrix(docs104q1)
m104q1 <- as.matrix(dtm104q1)
v104q1 <- sort(rowSums(m104q1),decreasing=TRUE)
d104q1 <- data.frame(word = names(v104q1),freq=v104q1)

head(d104q1, 500)

# View answers :)
wordcloud104q1 <- wordcloud(words = d104q1$word, freq = d104q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 104q1
d104q1tibble <- list(word = d104q1$word[1:10], freq = d104q1$freq[1:10])
d104q1tibble <- as_tibble(d104q1tibble)

ggplot(d104q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q1: What is the coronavirus?", subtitle = "n = 53") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q2
text104q2Answer <- paste(x104q2$Answer)
corpus104q2 <- Corpus(VectorSource(text104q2Answer))

# Load the data as a corpus
inspect(corpus104q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus104q2, pattern) gsub(pattern, "", corpus104q2))
docs104q2 <- tm_map(corpus104q2, toSpaceCorpusq2, "/")
docs104q2 <- tm_map(corpus104q2, toSpaceCorpusq2, "@")
docs104q2 <- tm_map(corpus104q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs104q2 <- tm_map(docs104q2, content_transformer(tolower))
# Remove numbers
docs104q2 <- tm_map(docs104q2, removeNumbers)
# Remove english common stopwords
docs104q2 <- tm_map(docs104q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q2 <- tm_map(docs104q2, removeWords)
# Remove punctuations
docs104q2 <- tm_map(docs104q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q2 <- tm_map(docs104q2, stripWhitespace)
# Text stemming
# docs104q2 <- tm_map(docs104q2, stemDocument)

dtm104q2 <- TermDocumentMatrix(docs104q2)
m104q2 <- as.matrix(dtm104q2)
v104q2 <- sort(rowSums(m104q2),decreasing=TRUE)
d104q2 <- data.frame(word = names(v104q2),freq=v104q2)

head(d104q2, 500)

# View answers :)
wordcloud104q2 <- wordcloud(words = d104q2$word, freq = d104q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d104q2tibble <- list(word = d104q2$word[1:10], freq = d104q2$freq[1:10])
d104q2tibble <- as_tibble(d104q2tibble)

ggplot(d104q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q2: Who is the president/prime minister?", subtitle = "n = 53") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q3
text104q3Answer <- paste(x104q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus104q3 <- Corpus(VectorSource(text104q3Answer))

# Load the data as a corpus
inspect(corpus104q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q3 <- content_transformer(function (corpus104q3, pattern) gsub(pattern, "", corpus104q3))
docs104q3 <- tm_map(corpus104q3, toSpaceCorpus104q3, "/")
docs104q3 <- tm_map(corpus104q3, toSpaceCorpus104q3, "@")
docs104q3 <- tm_map(corpus104q3, toSpaceCorpus104q3, "\\|")

# Convert the text to lower case
docs104q3 <- tm_map(docs104q3, content_transformer(tolower))
# Remove numbers
# docs104q3 <- tm_map(docs104q3, removeNumbers)
# Remove english common stopwords
docs104q3 <- tm_map(docs104q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q3 <- tm_map(docs104q3, removeWords)
# Remove punctuations
docs104q3 <- tm_map(docs104q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q3 <- tm_map(docs104q3, stripWhitespace)
# Text stemming
# docs104q3 <- tm_map(docs104q3, stemDocument)

dtm104q3 <- TermDocumentMatrix(docs104q3)
m104q3 <- as.matrix(dtm104q3)
v104q3 <- sort(rowSums(m104q3),decreasing=TRUE)
d104q3 <- data.frame(word = names(v104q3),freq=v104q3)

head(d104q3, 500)

# View answers :)
wordcloud104q3 <- wordcloud(words = d104q3$word, freq = d104q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d104q3tibble <- list(word = d104q3$word[1:10], freq = d104q3$freq[1:10])
d104q3tibble <- as_tibble(d104q3tibble)

ggplot(d104q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q3: How many days have we been in lockdown?", subtitle = "n = 55") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q4
text104q4Answer <- paste(x104q4$Answer)
corpus104q4 <- Corpus(VectorSource(text104q4Answer))

# Load the data as a corpus
inspect(corpus104q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q4 <- content_transformer(function (corpus104q4, pattern) gsub(pattern, "", corpus104q4))
docs104q4 <- tm_map(corpus104q4, toSpaceCorpus104q4, "/")
docs104q4 <- tm_map(corpus104q4, toSpaceCorpus104q4, "@")
docs104q4 <- tm_map(corpus104q4, toSpaceCorpus104q4, "\\|")

# Convert the text to lower case
docs104q4 <- tm_map(docs104q4, content_transformer(tolower))
# Remove numbers
docs104q4 <- tm_map(docs104q4, removeNumbers)
# Remove english common stopwords
docs104q4 <- tm_map(docs104q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q4 <- tm_map(docs104q4, removeWords)
# Remove punctuations
docs104q4 <- tm_map(docs104q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q4 <- tm_map(docs104q4, stripWhitespace)
# Text stemming
# docs104q4 <- tm_map(docs104q4, stemDocument)

dtm104q4 <- TermDocumentMatrix(docs104q4)
m104q4 <- as.matrix(dtm104q4)
v104q4 <- sort(rowSums(m104q4),decreasing=TRUE)
d104q4 <- data.frame(word = names(v104q4),freq=v104q4)

head(d104q4, 500)

# View answers :)
wordcloud104q4 <- wordcloud(words = d104q4$word, freq = d104q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d104q4tibble <- list(word = d104q4$word[1:10], freq = d104q4$freq[1:10])
d104q4tibble <- as_tibble(d104q4tibble)

ggplot(d104q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q4: Do you want to go back to school?", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q5
text104q5Answer <- paste(x104q5$Answer)
corpus104q5 <- Corpus(VectorSource(text104q5Answer))

# Load the data as a corpus
inspect(corpus104q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q5 <- content_transformer(function (corpus104q5, pattern) gsub(pattern, "", corpus104q5))
docs104q5 <- tm_map(corpus104q5, toSpaceCorpus104q5, "/")
docs104q5 <- tm_map(corpus104q5, toSpaceCorpus104q5, "@")
docs104q5 <- tm_map(corpus104q5, toSpaceCorpus104q5, "\\|")

# Convert the text to lower case
docs104q5 <- tm_map(docs104q5, content_transformer(tolower))
# Remove numbers
docs104q5 <- tm_map(docs104q5, removeNumbers)
# Remove english common stopwords
docs104q5 <- tm_map(docs104q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q5 <- tm_map(docs104q5, removeWords)
# Remove punctuations
docs104q5 <- tm_map(docs104q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q5 <- tm_map(docs104q5, stripWhitespace)
# Text stemming
# docs104q5 <- tm_map(docs104q5, stemDocument)

dtm104q5 <- TermDocumentMatrix(docs104q5)
m104q5 <- as.matrix(dtm104q5)
v104q5 <- sort(rowSums(m104q5),decreasing=TRUE)
d104q5 <- data.frame(word = names(v104q5),freq=v104q5)

head(d104q5, 500)

# View answers :)
wordcloud104q5 <- wordcloud(words = d104q5$word, freq = d104q5$freq, min.freq = 1, scale=c(2, .5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d104q5tibble <- list(word = d104q5$word[1:10], freq = d104q5$freq[1:10])
d104q5tibble <- as_tibble(d104q5tibble)

ggplot(d104q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 51") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q6
text104q6Answer <- paste(x104q6$Answer)
corpus104q6 <- Corpus(VectorSource(text104q6Answer))

# Load the data as a corpus
inspect(corpus104q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q6 <- content_transformer(function (corpus104q6, pattern) gsub(pattern, "", corpus104q6))
docs104q6 <- tm_map(corpus104q6, toSpaceCorpus104q6, "/")
docs104q6 <- tm_map(corpus104q6, toSpaceCorpus104q6, "@")
docs104q6 <- tm_map(corpus104q6, toSpaceCorpus104q6, "\\|")

# Convert the text to lower case
docs104q6 <- tm_map(docs104q6, content_transformer(tolower))
# Remove numbers
docs104q6 <- tm_map(docs104q6, removeNumbers)
# Remove english common stopwords
docs104q6 <- tm_map(docs104q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q6 <- tm_map(docs104q6, removeWords)
# Remove punctuations
docs104q6 <- tm_map(docs104q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q6 <- tm_map(docs104q6, stripWhitespace)
# Text stemming
# docs104q6 <- tm_map(docs104q6, stemDocument)

dtm104q6 <- TermDocumentMatrix(docs104q6)
m104q6 <- as.matrix(dtm104q6)
v104q6 <- sort(rowSums(m104q6),decreasing=TRUE)
d104q6 <- data.frame(word = names(v104q6),freq=v104q6)

head(d104q6, 500)

# View answers :)
wordcloud104q6 <- wordcloud(words = d104q6$word, freq = d104q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d104q6tibble <- list(word = d104q6$word[1:10], freq = d104q6$freq[1:10])
d104q6tibble <- as_tibble(d104q6tibble)

ggplot(d104q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q6: Where is the first place you want to go?", subtitle = "n = 53") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q7
text104q7Answer <- paste(x104q7$Answer)
corpus104q7 <- Corpus(VectorSource(text104q7Answer))

# Load the data as a corpus
inspect(corpus104q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q7 <- content_transformer(function (corpus104q7, pattern) gsub(pattern, "", corpus104q7))
docs104q7 <- tm_map(corpus104q7, toSpaceCorpus104q7, "/")
docs104q7 <- tm_map(corpus104q7, toSpaceCorpus104q7, "@")
docs104q7 <- tm_map(corpus104q7, toSpaceCorpus104q7, "\\|")

# Convert the text to lower case
docs104q7 <- tm_map(docs104q7, content_transformer(tolower))
# Remove numbers
docs104q7 <- tm_map(docs104q7, removeNumbers)
# Remove english common stopwords
docs104q7 <- tm_map(docs104q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q7 <- tm_map(docs104q7, removeWords)
# Remove punctuations
docs104q7 <- tm_map(docs104q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q7 <- tm_map(docs104q7, stripWhitespace)
# Text stemming
# docs104q7 <- tm_map(docs104q7, stemDocument)

dtm104q7 <- TermDocumentMatrix(docs104q7)
m104q7 <- as.matrix(dtm104q7)
v104q7 <- sort(rowSums(m104q7),decreasing=TRUE)
d104q7 <- data.frame(word = names(v104q7),freq=v104q7)

head(d104q7, 500)

# View answers :)
wordcloud104q7 <- wordcloud(words = d104q7$word, freq = d104q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d104q7tibble <- list(word = d104q7$word[1:10], freq = d104q7$freq[1:10])
d104q7tibble <- as_tibble(d104q7tibble)

ggplot(d104q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 54") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q8
text104q8Answer <- paste(x104q8$Answer)
corpus104q8 <- Corpus(VectorSource(text104q8Answer))

# Load the data as a corpus
inspect(corpus104q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q8 <- content_transformer(function (corpus104q8, pattern) gsub(pattern, "", corpus104q8))
docs104q8 <- tm_map(corpus104q8, toSpaceCorpus104q8, "/")
docs104q8 <- tm_map(corpus104q8, toSpaceCorpus104q8, "@")
docs104q8 <- tm_map(corpus104q8, toSpaceCorpus104q8, "\\|")

# Convert the text to lower case
docs104q8 <- tm_map(docs104q8, content_transformer(tolower))
# Remove numbers
docs104q8 <- tm_map(docs104q8, removeNumbers)
# Remove english common stopwords
docs104q8 <- tm_map(docs104q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs104q8 <- tm_map(docs104q8, removeWords)
# Remove punctuations
docs104q8 <- tm_map(docs104q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q8 <- tm_map(docs104q8, stripWhitespace)
# Text stemming
# docs104q8 <- tm_map(docs104q8, stemDocument)

dtm104q8 <- TermDocumentMatrix(docs104q8)
m104q8 <- as.matrix(dtm104q8)
v104q8 <- sort(rowSums(m104q8),decreasing=TRUE)
d104q8 <- data.frame(word = names(v104q8),freq=v104q8)

head(d104q8, 500)

# View answers :)
wordcloud104q8 <- wordcloud(words = d104q8$word, freq = d104q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d104q8tibble <- list(word = d104q8$word[1:10], freq = d104q8$freq[1:10])
d104q8tibble <- as_tibble(d104q8tibble)

ggplot(d104q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q8: Are your parents good teachers?", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q9
text104q9Answer <- paste(x104q9$Answer)
corpus104q9 <- Corpus(VectorSource(text104q9Answer))

# Load the data as a corpus
inspect(corpus104q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q9 <- content_transformer(function (corpus104q9, pattern) gsub(pattern, "", corpus104q9))
docs104q9 <- tm_map(corpus104q9, toSpaceCorpus104q9, "/")
docs104q9 <- tm_map(corpus104q9, toSpaceCorpus104q9, "@")
docs104q9 <- tm_map(corpus104q9, toSpaceCorpus104q9, "\\|")

# Convert the text to lower case
docs104q9 <- tm_map(docs104q9, content_transformer(tolower))
# Remove numbers
docs104q9 <- tm_map(docs104q9, removeNumbers)
# Remove english common stopwords
docs104q9 <- tm_map(docs104q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q9 <- tm_map(docs104q9, removeWords)
# Remove punctuations
docs104q9 <- tm_map(docs104q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q9 <- tm_map(docs104q9, stripWhitespace)
# Text stemming
# docs104q9 <- tm_map(docs104q9, stemDocument)

dtm104q9 <- TermDocumentMatrix(docs104q9)
m104q9 <- as.matrix(dtm104q9)
v104q9 <- sort(rowSums(m104q9),decreasing=TRUE)
d104q9 <- data.frame(word = names(v104q9),freq=v104q9)

head(d104q9, 500)

# View answers :)
wordcloud104q9 <- wordcloud(words = d104q9$word, freq = d104q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d104q9tibble <- list(word = d104q9$word[1:10], freq = d104q9$freq[1:10])
d104q9tibble <- as_tibble(d104q9tibble)

ggplot(d104q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q9: How did the coronavirus start?", subtitle = "n = 56") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q10
text104q10Answer <- paste(x104q10$Answer)
corpus104q10 <- Corpus(VectorSource(text104q10Answer))

# Load the data as a corpus
inspect(corpus104q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q10 <- content_transformer(function (corpus104q10, pattern) gsub(pattern, "", corpus104q10))
docs104q10 <- tm_map(corpus104q10, toSpaceCorpus104q10, "/")
docs104q10 <- tm_map(corpus104q10, toSpaceCorpus104q10, "@")
docs104q10 <- tm_map(corpus104q10, toSpaceCorpus104q10, "\\|")

# Convert the text to lower case
docs104q10 <- tm_map(docs104q10, content_transformer(tolower))
# Remove numbers
docs104q10 <- tm_map(docs104q10, removeNumbers)
# Remove english common stopwords
docs104q10 <- tm_map(docs104q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q10 <- tm_map(docs104q10, removeWords)
# Remove punctuations
docs104q10 <- tm_map(docs104q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q10 <- tm_map(docs104q10, stripWhitespace)
# Text stemming
# docs104q10 <- tm_map(docs104q10, stemDocument)

dtm104q10 <- TermDocumentMatrix(docs104q10)
m104q10 <- as.matrix(dtm104q10)
v104q10 <- sort(rowSums(m104q10),decreasing=TRUE)
d104q10 <- data.frame(word = names(v104q10),freq=v104q10)

head(d104q10, 500)

# View answers :)
wordcloud104q10 <- wordcloud(words = d104q10$word, freq = d104q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d104q10tibble <- list(word = d104q10$word[1:10], freq = d104q10$freq[1:10])
d104q10tibble <- as_tibble(d104q10tibble)

ggplot(d104q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 49") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus104q11
text104q11Answer <- paste(x104q11$Answer)
corpus104q11 <- Corpus(VectorSource(text104q11Answer))

# Load the data as a corpus
inspect(corpus104q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus104q11 <- content_transformer(function (corpus104q11, pattern) gsub(pattern, "", corpus104q11))
docs104q11 <- tm_map(corpus104q11, toSpaceCorpus104q11, "/")
docs104q11 <- tm_map(corpus104q11, toSpaceCorpus104q11, "@")
docs104q11 <- tm_map(corpus104q11, toSpaceCorpus104q11, "\\|")

# Convert the text to lower case
docs104q11 <- tm_map(docs104q11, content_transformer(tolower))
# Remove numbers
docs104q11 <- tm_map(docs104q11, removeNumbers)
# Remove english common stopwords
docs104q11 <- tm_map(docs104q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs104q11 <- tm_map(docs104q11, removeWords)
# Remove punctuations
docs104q11 <- tm_map(docs104q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs104q11 <- tm_map(docs104q11, stripWhitespace)
# Text stemming
# docs104q11 <- tm_map(docs104q11, stemDocument)

dtm104q11 <- TermDocumentMatrix(docs104q11)
m104q11 <- as.matrix(dtm104q11)
v104q11 <- sort(rowSums(m104q11),decreasing=TRUE)
d104q11 <- data.frame(word = names(v104q11),freq=v104q11)

head(d104q11, 500)

# View answers :)
wordcloud104q11 <- wordcloud(words = d104q11$word, freq = d104q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d104q11tibble <- list(word = d104q11$word[1:10], freq = d104q11$freq[1:10])
d104q11tibble <- as_tibble(d104q11tibble)

ggplot(d104q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 4 - Q11: Are you enjoying lockdown?", subtitle = "n = 44") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 5 ###
##################################################################################################
# Corpus105q1
text105q1Answer <- paste(x105q1$Answer)
corpus105q1 <- Corpus(VectorSource(text105q1Answer))

# Load the data as a corpus
inspect(corpus105q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q1 <- content_transformer(function (corpus105q1, pattern) gsub(pattern, "", corpus105q1))
docs105q1 <- tm_map(corpus105q1, toSpaceCorpusq1, "/")
docs105q1 <- tm_map(corpus105q1, toSpaceCorpusq1, "@")
docs105q1 <- tm_map(corpus105q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs105q1 <- tm_map(docs105q1, content_transformer(tolower))
# Remove numbers
docs105q1 <- tm_map(docs105q1, removeNumbers)
# Remove english common stopwords
docs105q1 <- tm_map(docs105q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q1 <- tm_map(docs105q1, removeWords)
# Remove punctuations
docs105q1 <- tm_map(docs105q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q1 <- tm_map(docs105q1, stripWhitespace)
# Text stemming
# docs105q1 <- tm_map(docs105q1, stemDocument)

dtm105q1 <- TermDocumentMatrix(docs105q1)
m105q1 <- as.matrix(dtm105q1)
v105q1 <- sort(rowSums(m105q1),decreasing=TRUE)
d105q1 <- data.frame(word = names(v105q1),freq=v105q1)

head(d105q1, 500)

# View answers :)
wordcloud105q1 <- wordcloud(words = d105q1$word, freq = d105q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 105q1
d105q1tibble <- list(word = d105q1$word[1:10], freq = d105q1$freq[1:10])
d105q1tibble <- as_tibble(d105q1tibble)

ggplot(d105q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q1: What is the coronavirus?", subtitle = "n = 64") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q2
text105q2Answer <- paste(x105q2$Answer)
corpus105q2 <- Corpus(VectorSource(text105q2Answer))

# Load the data as a corpus
inspect(corpus105q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus105q2, pattern) gsub(pattern, "", corpus105q2))
docs105q2 <- tm_map(corpus105q2, toSpaceCorpusq2, "/")
docs105q2 <- tm_map(corpus105q2, toSpaceCorpusq2, "@")
docs105q2 <- tm_map(corpus105q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs105q2 <- tm_map(docs105q2, content_transformer(tolower))
# Remove numbers
docs105q2 <- tm_map(docs105q2, removeNumbers)
# Remove english common stopwords
docs105q2 <- tm_map(docs105q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q2 <- tm_map(docs105q2, removeWords)
# Remove punctuations
docs105q2 <- tm_map(docs105q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q2 <- tm_map(docs105q2, stripWhitespace)
# Text stemming
# docs105q2 <- tm_map(docs105q2, stemDocument)

dtm105q2 <- TermDocumentMatrix(docs105q2)
m105q2 <- as.matrix(dtm105q2)
v105q2 <- sort(rowSums(m105q2),decreasing=TRUE)
d105q2 <- data.frame(word = names(v105q2),freq=v105q2)

head(d105q2, 500)

# View answers :)
wordcloud105q2 <- wordcloud(words = d105q2$word, freq = d105q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d105q2tibble <- list(word = d105q2$word[1:10], freq = d105q2$freq[1:10])
d105q2tibble <- as_tibble(d105q2tibble)

ggplot(d105q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q2: Who is the president/prime minister?", subtitle = "n = 60") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q3
text105q3Answer <- paste(x105q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus105q3 <- Corpus(VectorSource(text105q3Answer))

# Load the data as a corpus
inspect(corpus105q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q3 <- content_transformer(function (corpus105q3, pattern) gsub(pattern, "", corpus105q3))
docs105q3 <- tm_map(corpus105q3, toSpaceCorpus105q3, "/")
docs105q3 <- tm_map(corpus105q3, toSpaceCorpus105q3, "@")
docs105q3 <- tm_map(corpus105q3, toSpaceCorpus105q3, "\\|")

# Convert the text to lower case
docs105q3 <- tm_map(docs105q3, content_transformer(tolower))
# Remove numbers
# docs105q3 <- tm_map(docs105q3, removeNumbers)
# Remove english common stopwords
docs105q3 <- tm_map(docs105q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q3 <- tm_map(docs105q3, removeWords)
# Remove punctuations
docs105q3 <- tm_map(docs105q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q3 <- tm_map(docs105q3, stripWhitespace)
# Text stemming
# docs105q3 <- tm_map(docs105q3, stemDocument)

dtm105q3 <- TermDocumentMatrix(docs105q3)
m105q3 <- as.matrix(dtm105q3)
v105q3 <- sort(rowSums(m105q3),decreasing=TRUE)
d105q3 <- data.frame(word = names(v105q3),freq=v105q3)

head(d105q3, 500)

# View answers :)
wordcloud105q3 <- wordcloud(words = d105q3$word, freq = d105q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d105q3tibble <- list(word = d105q3$word[1:10], freq = d105q3$freq[1:10])
d105q3tibble <- as_tibble(d105q3tibble)

ggplot(d105q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q3: How many days have we been in lockdown?", subtitle = "n = 64") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q4
text105q4Answer <- paste(x105q4$Answer)
corpus105q4 <- Corpus(VectorSource(text105q4Answer))

# Load the data as a corpus
inspect(corpus105q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q4 <- content_transformer(function (corpus105q4, pattern) gsub(pattern, "", corpus105q4))
docs105q4 <- tm_map(corpus105q4, toSpaceCorpus105q4, "/")
docs105q4 <- tm_map(corpus105q4, toSpaceCorpus105q4, "@")
docs105q4 <- tm_map(corpus105q4, toSpaceCorpus105q4, "\\|")

# Convert the text to lower case
docs105q4 <- tm_map(docs105q4, content_transformer(tolower))
# Remove numbers
docs105q4 <- tm_map(docs105q4, removeNumbers)
# Remove english common stopwords
docs105q4 <- tm_map(docs105q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q4 <- tm_map(docs105q4, removeWords)
# Remove punctuations
docs105q4 <- tm_map(docs105q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q4 <- tm_map(docs105q4, stripWhitespace)
# Text stemming
# docs105q4 <- tm_map(docs105q4, stemDocument)

dtm105q4 <- TermDocumentMatrix(docs105q4)
m105q4 <- as.matrix(dtm105q4)
v105q4 <- sort(rowSums(m105q4),decreasing=TRUE)
d105q4 <- data.frame(word = names(v105q4),freq=v105q4)

head(d105q4, 500)

# View answers :)
wordcloud105q4 <- wordcloud(words = d105q4$word, freq = d105q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d105q4tibble <- list(word = d105q4$word[1:10], freq = d105q4$freq[1:10])
d105q4tibble <- as_tibble(d105q4tibble)

ggplot(d105q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q4: Do you want to go back to school?", subtitle = "n = 58") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q5
text105q5Answer <- paste(x105q5$Answer)
corpus105q5 <- Corpus(VectorSource(text105q5Answer))

# Load the data as a corpus
inspect(corpus105q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q5 <- content_transformer(function (corpus105q5, pattern) gsub(pattern, "", corpus105q5))
docs105q5 <- tm_map(corpus105q5, toSpaceCorpus105q5, "/")
docs105q5 <- tm_map(corpus105q5, toSpaceCorpus105q5, "@")
docs105q5 <- tm_map(corpus105q5, toSpaceCorpus105q5, "\\|")

# Convert the text to lower case
docs105q5 <- tm_map(docs105q5, content_transformer(tolower))
# Remove numbers
docs105q5 <- tm_map(docs105q5, removeNumbers)
# Remove english common stopwords
docs105q5 <- tm_map(docs105q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q5 <- tm_map(docs105q5, removeWords)
# Remove punctuations
docs105q5 <- tm_map(docs105q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q5 <- tm_map(docs105q5, stripWhitespace)
# Text stemming
# docs105q5 <- tm_map(docs105q5, stemDocument)

dtm105q5 <- TermDocumentMatrix(docs105q5)
m105q5 <- as.matrix(dtm105q5)
v105q5 <- sort(rowSums(m105q5),decreasing=TRUE)
d105q5 <- data.frame(word = names(v105q5),freq=v105q5)

head(d105q5, 500)

# View answers :)
wordcloud105q5 <- wordcloud(words = d105q5$word, freq = d105q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d105q5tibble <- list(word = d105q5$word[1:10], freq = d105q5$freq[1:10])
d105q5tibble <- as_tibble(d105q5tibble)

ggplot(d105q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q6
text105q6Answer <- paste(x105q6$Answer)
corpus105q6 <- Corpus(VectorSource(text105q6Answer))

# Load the data as a corpus
inspect(corpus105q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q6 <- content_transformer(function (corpus105q6, pattern) gsub(pattern, "", corpus105q6))
docs105q6 <- tm_map(corpus105q6, toSpaceCorpus105q6, "/")
docs105q6 <- tm_map(corpus105q6, toSpaceCorpus105q6, "@")
docs105q6 <- tm_map(corpus105q6, toSpaceCorpus105q6, "\\|")

# Convert the text to lower case
docs105q6 <- tm_map(docs105q6, content_transformer(tolower))
# Remove numbers
docs105q6 <- tm_map(docs105q6, removeNumbers)
# Remove english common stopwords
docs105q6 <- tm_map(docs105q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q6 <- tm_map(docs105q6, removeWords)
# Remove punctuations
docs105q6 <- tm_map(docs105q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q6 <- tm_map(docs105q6, stripWhitespace)
# Text stemming
# docs105q6 <- tm_map(docs105q6, stemDocument)

dtm105q6 <- TermDocumentMatrix(docs105q6)
m105q6 <- as.matrix(dtm105q6)
v105q6 <- sort(rowSums(m105q6),decreasing=TRUE)
d105q6 <- data.frame(word = names(v105q6),freq=v105q6)

head(d105q6, 500)

# View answers :)
wordcloud105q6 <- wordcloud(words = d105q6$word, freq = d105q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d105q6tibble <- list(word = d105q6$word[1:10], freq = d105q6$freq[1:10])
d105q6tibble <- as_tibble(d105q6tibble)

ggplot(d105q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q6: Where is the first place you want to go?", subtitle = "n = 63") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q7
text105q7Answer <- paste(x105q7$Answer)
corpus105q7 <- Corpus(VectorSource(text105q7Answer))

# Load the data as a corpus
inspect(corpus105q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q7 <- content_transformer(function (corpus105q7, pattern) gsub(pattern, "", corpus105q7))
docs105q7 <- tm_map(corpus105q7, toSpaceCorpus105q7, "/")
docs105q7 <- tm_map(corpus105q7, toSpaceCorpus105q7, "@")
docs105q7 <- tm_map(corpus105q7, toSpaceCorpus105q7, "\\|")

# Convert the text to lower case
docs105q7 <- tm_map(docs105q7, content_transformer(tolower))
# Remove numbers
docs105q7 <- tm_map(docs105q7, removeNumbers)
# Remove english common stopwords
docs105q7 <- tm_map(docs105q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q7 <- tm_map(docs105q7, removeWords)
# Remove punctuations
docs105q7 <- tm_map(docs105q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q7 <- tm_map(docs105q7, stripWhitespace)
# Text stemming
# docs105q7 <- tm_map(docs105q7, stemDocument)

dtm105q7 <- TermDocumentMatrix(docs105q7)
m105q7 <- as.matrix(dtm105q7)
v105q7 <- sort(rowSums(m105q7),decreasing=TRUE)
d105q7 <- data.frame(word = names(v105q7),freq=v105q7)

head(d105q7, 500)

# View answers :)
wordcloud105q7 <- wordcloud(words = d105q7$word, freq = d105q7$freq, min.freq = 1,scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d105q7tibble <- list(word = d105q7$word[1:10], freq = d105q7$freq[1:10])
d105q7tibble <- as_tibble(d105q7tibble)

ggplot(d105q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 64") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q8
text105q8Answer <- paste(x105q8$Answer)
corpus105q8 <- Corpus(VectorSource(text105q8Answer))

# Load the data as a corpus
inspect(corpus105q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q8 <- content_transformer(function (corpus105q8, pattern) gsub(pattern, "", corpus105q8))
docs105q8 <- tm_map(corpus105q8, toSpaceCorpus105q8, "/")
docs105q8 <- tm_map(corpus105q8, toSpaceCorpus105q8, "@")
docs105q8 <- tm_map(corpus105q8, toSpaceCorpus105q8, "\\|")

# Convert the text to lower case
docs105q8 <- tm_map(docs105q8, content_transformer(tolower))
# Remove numbers
docs105q8 <- tm_map(docs105q8, removeNumbers)
# Remove english common stopwords
docs105q8 <- tm_map(docs105q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs105q8 <- tm_map(docs105q8, removeWords)
# Remove punctuations
docs105q8 <- tm_map(docs105q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q8 <- tm_map(docs105q8, stripWhitespace)
# Text stemming
# docs105q8 <- tm_map(docs105q8, stemDocument)

dtm105q8 <- TermDocumentMatrix(docs105q8)
m105q8 <- as.matrix(dtm105q8)
v105q8 <- sort(rowSums(m105q8),decreasing=TRUE)
d105q8 <- data.frame(word = names(v105q8),freq=v105q8)

head(d105q8, 500)

# View answers :)
wordcloud105q8 <- wordcloud(words = d105q8$word, freq = d105q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d105q8tibble <- list(word = d105q8$word[1:10], freq = d105q8$freq[1:10])
d105q8tibble <- as_tibble(d105q8tibble)

ggplot(d105q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q8: Are your parents good teachers?", subtitle = "n = 60") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q9
text105q9Answer <- paste(x105q9$Answer)
corpus105q9 <- Corpus(VectorSource(text105q9Answer))

# Load the data as a corpus
inspect(corpus105q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q9 <- content_transformer(function (corpus105q9, pattern) gsub(pattern, "", corpus105q9))
docs105q9 <- tm_map(corpus105q9, toSpaceCorpus105q9, "/")
docs105q9 <- tm_map(corpus105q9, toSpaceCorpus105q9, "@")
docs105q9 <- tm_map(corpus105q9, toSpaceCorpus105q9, "\\|")

# Convert the text to lower case
docs105q9 <- tm_map(docs105q9, content_transformer(tolower))
# Remove numbers
docs105q9 <- tm_map(docs105q9, removeNumbers)
# Remove english common stopwords
docs105q9 <- tm_map(docs105q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q9 <- tm_map(docs105q9, removeWords)
# Remove punctuations
docs105q9 <- tm_map(docs105q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q9 <- tm_map(docs105q9, stripWhitespace)
# Text stemming
# docs105q9 <- tm_map(docs105q9, stemDocument)

dtm105q9 <- TermDocumentMatrix(docs105q9)
m105q9 <- as.matrix(dtm105q9)
v105q9 <- sort(rowSums(m105q9),decreasing=TRUE)
d105q9 <- data.frame(word = names(v105q9),freq=v105q9)

head(d105q9, 500)

# View answers :)
wordcloud105q9 <- wordcloud(words = d105q9$word, freq = d105q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d105q9tibble <- list(word = d105q9$word[1:10], freq = d105q9$freq[1:10])
d105q9tibble <- as_tibble(d105q9tibble)

ggplot(d105q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q9: How did the coronavirus start?", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q10
text105q10Answer <- paste(x105q10$Answer)
corpus105q10 <- Corpus(VectorSource(text105q10Answer))

# Load the data as a corpus
inspect(corpus105q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q10 <- content_transformer(function (corpus105q10, pattern) gsub(pattern, "", corpus105q10))
docs105q10 <- tm_map(corpus105q10, toSpaceCorpus105q10, "/")
docs105q10 <- tm_map(corpus105q10, toSpaceCorpus105q10, "@")
docs105q10 <- tm_map(corpus105q10, toSpaceCorpus105q10, "\\|")

# Convert the text to lower case
docs105q10 <- tm_map(docs105q10, content_transformer(tolower))
# Remove numbers
docs105q10 <- tm_map(docs105q10, removeNumbers)
# Remove english common stopwords
docs105q10 <- tm_map(docs105q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q10 <- tm_map(docs105q10, removeWords)
# Remove punctuations
docs105q10 <- tm_map(docs105q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q10 <- tm_map(docs105q10, stripWhitespace)
# Text stemming
# docs105q10 <- tm_map(docs105q10, stemDocument)

dtm105q10 <- TermDocumentMatrix(docs105q10)
m105q10 <- as.matrix(dtm105q10)
v105q10 <- sort(rowSums(m105q10),decreasing=TRUE)
d105q10 <- data.frame(word = names(v105q10),freq=v105q10)

head(d105q10, 500)

# View answers :)
wordcloud105q10 <- wordcloud(words = d105q10$word, freq = d105q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d105q10tibble <- list(word = d105q10$word[1:10], freq = d105q10$freq[1:10])
d105q10tibble <- as_tibble(d105q10tibble)

ggplot(d105q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 58") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus105q11
text105q11Answer <- paste(x105q11$Answer)
corpus105q11 <- Corpus(VectorSource(text105q11Answer))

# Load the data as a corpus
inspect(corpus105q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus105q11 <- content_transformer(function (corpus105q11, pattern) gsub(pattern, "", corpus105q11))
docs105q11 <- tm_map(corpus105q11, toSpaceCorpus105q11, "/")
docs105q11 <- tm_map(corpus105q11, toSpaceCorpus105q11, "@")
docs105q11 <- tm_map(corpus105q11, toSpaceCorpus105q11, "\\|")

# Convert the text to lower case
docs105q11 <- tm_map(docs105q11, content_transformer(tolower))
# Remove numbers
docs105q11 <- tm_map(docs105q11, removeNumbers)
# Remove english common stopwords
docs105q11 <- tm_map(docs105q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs105q11 <- tm_map(docs105q11, removeWords)
# Remove punctuations
docs105q11 <- tm_map(docs105q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs105q11 <- tm_map(docs105q11, stripWhitespace)
# Text stemming
# docs105q11 <- tm_map(docs105q11, stemDocument)

dtm105q11 <- TermDocumentMatrix(docs105q11)
m105q11 <- as.matrix(dtm105q11)
v105q11 <- sort(rowSums(m105q11),decreasing=TRUE)
d105q11 <- data.frame(word = names(v105q11),freq=v105q11)

head(d105q11, 500)

# View answers :)
wordcloud105q11 <- wordcloud(words = d105q11$word, freq = d105q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d105q11tibble <- list(word = d105q11$word[1:10], freq = d105q11$freq[1:10])
d105q11tibble <- as_tibble(d105q11tibble)

ggplot(d105q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 5 - Q11: Are you enjoying lockdown?", subtitle = "n = 59") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 6 ###
##################################################################################################
# Corpus106q1
text106q1Answer <- paste(x106q1$Answer)
corpus106q1 <- Corpus(VectorSource(text106q1Answer))

# Load the data as a corpus
inspect(corpus106q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q1 <- content_transformer(function (corpus106q1, pattern) gsub(pattern, "", corpus106q1))
docs106q1 <- tm_map(corpus106q1, toSpaceCorpusq1, "/")
docs106q1 <- tm_map(corpus106q1, toSpaceCorpusq1, "@")
docs106q1 <- tm_map(corpus106q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs106q1 <- tm_map(docs106q1, content_transformer(tolower))
# Remove numbers
docs106q1 <- tm_map(docs106q1, removeNumbers)
# Remove english common stopwords
docs106q1 <- tm_map(docs106q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q1 <- tm_map(docs106q1, removeWords)
# Remove punctuations
docs106q1 <- tm_map(docs106q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q1 <- tm_map(docs106q1, stripWhitespace)
# Text stemming
# docs106q1 <- tm_map(docs106q1, stemDocument)

dtm106q1 <- TermDocumentMatrix(docs106q1)
m106q1 <- as.matrix(dtm106q1)
v106q1 <- sort(rowSums(m106q1),decreasing=TRUE)
d106q1 <- data.frame(word = names(v106q1),freq=v106q1)

head(d106q1, 500)

# View answers :)
wordcloud106q1 <- wordcloud(words = d106q1$word, freq = d106q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 106q1
d106q1tibble <- list(word = d106q1$word[1:10], freq = d106q1$freq[1:10])
d106q1tibble <- as_tibble(d106q1tibble)

ggplot(d106q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q1: What is the coronavirus?", subtitle = "n = 45") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q2
text106q2Answer <- paste(x106q2$Answer)
corpus106q2 <- Corpus(VectorSource(text106q2Answer))

# Load the data as a corpus
inspect(corpus106q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus106q2, pattern) gsub(pattern, "", corpus106q2))
docs106q2 <- tm_map(corpus106q2, toSpaceCorpusq2, "/")
docs106q2 <- tm_map(corpus106q2, toSpaceCorpusq2, "@")
docs106q2 <- tm_map(corpus106q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs106q2 <- tm_map(docs106q2, content_transformer(tolower))
# Remove numbers
docs106q2 <- tm_map(docs106q2, removeNumbers)
# Remove english common stopwords
docs106q2 <- tm_map(docs106q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q2 <- tm_map(docs106q2, removeWords)
# Remove punctuations
docs106q2 <- tm_map(docs106q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q2 <- tm_map(docs106q2, stripWhitespace)
# Text stemming
# docs106q2 <- tm_map(docs106q2, stemDocument)

dtm106q2 <- TermDocumentMatrix(docs106q2)
m106q2 <- as.matrix(dtm106q2)
v106q2 <- sort(rowSums(m106q2),decreasing=TRUE)
d106q2 <- data.frame(word = names(v106q2),freq=v106q2)

head(d106q2, 500)

# View answers :)
wordcloud106q2 <- wordcloud(words = d106q2$word, freq = d106q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d106q2tibble <- list(word = d106q2$word[1:10], freq = d106q2$freq[1:10])
d106q2tibble <- as_tibble(d106q2tibble)

ggplot(d106q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q2: Who is the president/prime minister?", subtitle = "n = 37") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q3
text106q3Answer <- paste(x106q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus106q3 <- Corpus(VectorSource(text106q3Answer))

# Load the data as a corpus
inspect(corpus106q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q3 <- content_transformer(function (corpus106q3, pattern) gsub(pattern, "", corpus106q3))
docs106q3 <- tm_map(corpus106q3, toSpaceCorpus106q3, "/")
docs106q3 <- tm_map(corpus106q3, toSpaceCorpus106q3, "@")
docs106q3 <- tm_map(corpus106q3, toSpaceCorpus106q3, "\\|")

# Convert the text to lower case
docs106q3 <- tm_map(docs106q3, content_transformer(tolower))
# Remove numbers
# docs106q3 <- tm_map(docs106q3, removeNumbers)
# Remove english common stopwords
docs106q3 <- tm_map(docs106q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q3 <- tm_map(docs106q3, removeWords)
# Remove punctuations
docs106q3 <- tm_map(docs106q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q3 <- tm_map(docs106q3, stripWhitespace)
# Text stemming
# docs106q3 <- tm_map(docs106q3, stemDocument)

dtm106q3 <- TermDocumentMatrix(docs106q3)
m106q3 <- as.matrix(dtm106q3)
v106q3 <- sort(rowSums(m106q3),decreasing=TRUE)
d106q3 <- data.frame(word = names(v106q3),freq=v106q3)

head(d106q3, 500)

# View answers :)
wordcloud106q3 <- wordcloud(words = d106q3$word, freq = d106q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d106q3tibble <- list(word = d106q3$word[1:10], freq = d106q3$freq[1:10])
d106q3tibble <- as_tibble(d106q3tibble)

ggplot(d106q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q3: How many days have we been in lockdown?", subtitle = "n = 49") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q4
text106q4Answer <- paste(x106q4$Answer)
corpus106q4 <- Corpus(VectorSource(text106q4Answer))

# Load the data as a corpus
inspect(corpus106q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q4 <- content_transformer(function (corpus106q4, pattern) gsub(pattern, "", corpus106q4))
docs106q4 <- tm_map(corpus106q4, toSpaceCorpus106q4, "/")
docs106q4 <- tm_map(corpus106q4, toSpaceCorpus106q4, "@")
docs106q4 <- tm_map(corpus106q4, toSpaceCorpus106q4, "\\|")

# Convert the text to lower case
docs106q4 <- tm_map(docs106q4, content_transformer(tolower))
# Remove numbers
docs106q4 <- tm_map(docs106q4, removeNumbers)
# Remove english common stopwords
docs106q4 <- tm_map(docs106q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q4 <- tm_map(docs106q4, removeWords)
# Remove punctuations
docs106q4 <- tm_map(docs106q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q4 <- tm_map(docs106q4, stripWhitespace)
# Text stemming
# docs106q4 <- tm_map(docs106q4, stemDocument)

dtm106q4 <- TermDocumentMatrix(docs106q4)
m106q4 <- as.matrix(dtm106q4)
v106q4 <- sort(rowSums(m106q4),decreasing=TRUE)
d106q4 <- data.frame(word = names(v106q4),freq=v106q4)

head(d106q4, 500)

# View answers :)
wordcloud106q4 <- wordcloud(words = d106q4$word, freq = d106q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d106q4tibble <- list(word = d106q4$word[1:10], freq = d106q4$freq[1:10])
d106q4tibble <- as_tibble(d106q4tibble)

ggplot(d106q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q4: Do you want to go back to school?", subtitle = "n = 41") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q5
text106q5Answer <- paste(x106q5$Answer)
corpus106q5 <- Corpus(VectorSource(text106q5Answer))

# Load the data as a corpus
inspect(corpus106q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q5 <- content_transformer(function (corpus106q5, pattern) gsub(pattern, "", corpus106q5))
docs106q5 <- tm_map(corpus106q5, toSpaceCorpus106q5, "/")
docs106q5 <- tm_map(corpus106q5, toSpaceCorpus106q5, "@")
docs106q5 <- tm_map(corpus106q5, toSpaceCorpus106q5, "\\|")

# Convert the text to lower case
docs106q5 <- tm_map(docs106q5, content_transformer(tolower))
# Remove numbers
docs106q5 <- tm_map(docs106q5, removeNumbers)
# Remove english common stopwords
docs106q5 <- tm_map(docs106q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q5 <- tm_map(docs106q5, removeWords)
# Remove punctuations
docs106q5 <- tm_map(docs106q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q5 <- tm_map(docs106q5, stripWhitespace)
# Text stemming
# docs106q5 <- tm_map(docs106q5, stemDocument)

dtm106q5 <- TermDocumentMatrix(docs106q5)
m106q5 <- as.matrix(dtm106q5)
v106q5 <- sort(rowSums(m106q5),decreasing=TRUE)
d106q5 <- data.frame(word = names(v106q5),freq=v106q5)

head(d106q5, 500)

# View answers :)
wordcloud106q5 <- wordcloud(words = d106q5$word, freq = d106q5$freq, min.freq = 1, scale=c(3,.25),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d106q5tibble <- list(word = d106q5$word[1:10], freq = d106q5$freq[1:10])
d106q5tibble <- as_tibble(d106q5tibble)

ggplot(d106q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 49") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q6
text106q6Answer <- paste(x106q6$Answer)
corpus106q6 <- Corpus(VectorSource(text106q6Answer))

# Load the data as a corpus
inspect(corpus106q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q6 <- content_transformer(function (corpus106q6, pattern) gsub(pattern, "", corpus106q6))
docs106q6 <- tm_map(corpus106q6, toSpaceCorpus106q6, "/")
docs106q6 <- tm_map(corpus106q6, toSpaceCorpus106q6, "@")
docs106q6 <- tm_map(corpus106q6, toSpaceCorpus106q6, "\\|")

# Convert the text to lower case
docs106q6 <- tm_map(docs106q6, content_transformer(tolower))
# Remove numbers
docs106q6 <- tm_map(docs106q6, removeNumbers)
# Remove english common stopwords
docs106q6 <- tm_map(docs106q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q6 <- tm_map(docs106q6, removeWords)
# Remove punctuations
docs106q6 <- tm_map(docs106q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q6 <- tm_map(docs106q6, stripWhitespace)
# Text stemming
# docs106q6 <- tm_map(docs106q6, stemDocument)

dtm106q6 <- TermDocumentMatrix(docs106q6)
m106q6 <- as.matrix(dtm106q6)
v106q6 <- sort(rowSums(m106q6),decreasing=TRUE)
d106q6 <- data.frame(word = names(v106q6),freq=v106q6)

head(d106q6, 500)

# View answers :)
wordcloud106q6 <- wordcloud(words = d106q6$word, freq = d106q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d106q6tibble <- list(word = d106q6$word[1:10], freq = d106q6$freq[1:10])
d106q6tibble <- as_tibble(d106q6tibble)

ggplot(d106q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q6: Where is the first place you want to go?", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q7
text106q7Answer <- paste(x106q7$Answer)
corpus106q7 <- Corpus(VectorSource(text106q7Answer))

# Load the data as a corpus
inspect(corpus106q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q7 <- content_transformer(function (corpus106q7, pattern) gsub(pattern, "", corpus106q7))
docs106q7 <- tm_map(corpus106q7, toSpaceCorpus106q7, "/")
docs106q7 <- tm_map(corpus106q7, toSpaceCorpus106q7, "@")
docs106q7 <- tm_map(corpus106q7, toSpaceCorpus106q7, "\\|")

# Convert the text to lower case
docs106q7 <- tm_map(docs106q7, content_transformer(tolower))
# Remove numbers
docs106q7 <- tm_map(docs106q7, removeNumbers)
# Remove english common stopwords
docs106q7 <- tm_map(docs106q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q7 <- tm_map(docs106q7, removeWords)
# Remove punctuations
docs106q7 <- tm_map(docs106q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q7 <- tm_map(docs106q7, stripWhitespace)
# Text stemming
# docs106q7 <- tm_map(docs106q7, stemDocument)

dtm106q7 <- TermDocumentMatrix(docs106q7)
m106q7 <- as.matrix(dtm106q7)
v106q7 <- sort(rowSums(m106q7),decreasing=TRUE)
d106q7 <- data.frame(word = names(v106q7),freq=v106q7)

head(d106q7, 500)

# View answers :)
wordcloud106q7 <- wordcloud(words = d106q7$word, freq = d106q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d106q7tibble <- list(word = d106q7$word[1:10], freq = d106q7$freq[1:10])
d106q7tibble <- as_tibble(d106q7tibble)

ggplot(d106q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q8
text106q8Answer <- paste(x106q8$Answer)
corpus106q8 <- Corpus(VectorSource(text106q8Answer))

# Load the data as a corpus
inspect(corpus106q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q8 <- content_transformer(function (corpus106q8, pattern) gsub(pattern, "", corpus106q8))
docs106q8 <- tm_map(corpus106q8, toSpaceCorpus106q8, "/")
docs106q8 <- tm_map(corpus106q8, toSpaceCorpus106q8, "@")
docs106q8 <- tm_map(corpus106q8, toSpaceCorpus106q8, "\\|")

# Convert the text to lower case
docs106q8 <- tm_map(docs106q8, content_transformer(tolower))
# Remove numbers
docs106q8 <- tm_map(docs106q8, removeNumbers)
# Remove english common stopwords
docs106q8 <- tm_map(docs106q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs106q8 <- tm_map(docs106q8, removeWords)
# Remove punctuations
docs106q8 <- tm_map(docs106q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q8 <- tm_map(docs106q8, stripWhitespace)
# Text stemming
# docs106q8 <- tm_map(docs106q8, stemDocument)

dtm106q8 <- TermDocumentMatrix(docs106q8)
m106q8 <- as.matrix(dtm106q8)
v106q8 <- sort(rowSums(m106q8),decreasing=TRUE)
d106q8 <- data.frame(word = names(v106q8),freq=v106q8)

head(d106q8, 500)

# View answers :)
wordcloud106q8 <- wordcloud(words = d106q8$word, freq = d106q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d106q8tibble <- list(word = d106q8$word[1:10], freq = d106q8$freq[1:10])
d106q8tibble <- as_tibble(d106q8tibble)

ggplot(d106q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q8: Are your parents good teachers?", subtitle = "n = 45") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q9
text106q9Answer <- paste(x106q9$Answer)
corpus106q9 <- Corpus(VectorSource(text106q9Answer))

# Load the data as a corpus
inspect(corpus106q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q9 <- content_transformer(function (corpus106q9, pattern) gsub(pattern, "", corpus106q9))
docs106q9 <- tm_map(corpus106q9, toSpaceCorpus106q9, "/")
docs106q9 <- tm_map(corpus106q9, toSpaceCorpus106q9, "@")
docs106q9 <- tm_map(corpus106q9, toSpaceCorpus106q9, "\\|")

# Convert the text to lower case
docs106q9 <- tm_map(docs106q9, content_transformer(tolower))
# Remove numbers
docs106q9 <- tm_map(docs106q9, removeNumbers)
# Remove english common stopwords
docs106q9 <- tm_map(docs106q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q9 <- tm_map(docs106q9, removeWords)
# Remove punctuations
docs106q9 <- tm_map(docs106q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q9 <- tm_map(docs106q9, stripWhitespace)
# Text stemming
# docs106q9 <- tm_map(docs106q9, stemDocument)

dtm106q9 <- TermDocumentMatrix(docs106q9)
m106q9 <- as.matrix(dtm106q9)
v106q9 <- sort(rowSums(m106q9),decreasing=TRUE)
d106q9 <- data.frame(word = names(v106q9),freq=v106q9)

head(d106q9, 500)

# View answers :)
wordcloud106q9 <- wordcloud(words = d106q9$word, freq = d106q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d106q9tibble <- list(word = d106q9$word[1:10], freq = d106q9$freq[1:10])
d106q9tibble <- as_tibble(d106q9tibble)

ggplot(d106q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q9: How did the coronavirus start?", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q10
text106q10Answer <- paste(x106q10$Answer)
corpus106q10 <- Corpus(VectorSource(text106q10Answer))

# Load the data as a corpus
inspect(corpus106q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q10 <- content_transformer(function (corpus106q10, pattern) gsub(pattern, "", corpus106q10))
docs106q10 <- tm_map(corpus106q10, toSpaceCorpus106q10, "/")
docs106q10 <- tm_map(corpus106q10, toSpaceCorpus106q10, "@")
docs106q10 <- tm_map(corpus106q10, toSpaceCorpus106q10, "\\|")

# Convert the text to lower case
docs106q10 <- tm_map(docs106q10, content_transformer(tolower))
# Remove numbers
docs106q10 <- tm_map(docs106q10, removeNumbers)
# Remove english common stopwords
docs106q10 <- tm_map(docs106q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q10 <- tm_map(docs106q10, removeWords)
# Remove punctuations
docs106q10 <- tm_map(docs106q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q10 <- tm_map(docs106q10, stripWhitespace)
# Text stemming
# docs106q10 <- tm_map(docs106q10, stemDocument)

dtm106q10 <- TermDocumentMatrix(docs106q10)
m106q10 <- as.matrix(dtm106q10)
v106q10 <- sort(rowSums(m106q10),decreasing=TRUE)
d106q10 <- data.frame(word = names(v106q10),freq=v106q10)

head(d106q10, 500)

# View answers :)
wordcloud106q10 <- wordcloud(words = d106q10$word, freq = d106q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d106q10tibble <- list(word = d106q10$word[1:10], freq = d106q10$freq[1:10])
d106q10tibble <- as_tibble(d106q10tibble)

ggplot(d106q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 45") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus106q11
text106q11Answer <- paste(x106q11$Answer)
corpus106q11 <- Corpus(VectorSource(text106q11Answer))

# Load the data as a corpus
inspect(corpus106q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus106q11 <- content_transformer(function (corpus106q11, pattern) gsub(pattern, "", corpus106q11))
docs106q11 <- tm_map(corpus106q11, toSpaceCorpus106q11, "/")
docs106q11 <- tm_map(corpus106q11, toSpaceCorpus106q11, "@")
docs106q11 <- tm_map(corpus106q11, toSpaceCorpus106q11, "\\|")

# Convert the text to lower case
docs106q11 <- tm_map(docs106q11, content_transformer(tolower))
# Remove numbers
docs106q11 <- tm_map(docs106q11, removeNumbers)
# Remove english common stopwords
docs106q11 <- tm_map(docs106q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs106q11 <- tm_map(docs106q11, removeWords)
# Remove punctuations
docs106q11 <- tm_map(docs106q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs106q11 <- tm_map(docs106q11, stripWhitespace)
# Text stemming
# docs106q11 <- tm_map(docs106q11, stemDocument)

dtm106q11 <- TermDocumentMatrix(docs106q11)
m106q11 <- as.matrix(dtm106q11)
v106q11 <- sort(rowSums(m106q11),decreasing=TRUE)
d106q11 <- data.frame(word = names(v106q11),freq=v106q11)

head(d106q11, 500)

# View answers :)
wordcloud106q11 <- wordcloud(words = d106q11$word, freq = d106q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d106q11tibble <- list(word = d106q11$word[1:10], freq = d106q11$freq[1:10])
d106q11tibble <- as_tibble(d106q11tibble)

ggplot(d106q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 6 - Q11: Are you enjoying lockdown?", subtitle = "n = 41") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 7 ###
##################################################################################################
# Corpus107q1
text107q1Answer <- paste(x107q1$Answer)
corpus107q1 <- Corpus(VectorSource(text107q1Answer))

# Load the data as a corpus
inspect(corpus107q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q1 <- content_transformer(function (corpus107q1, pattern) gsub(pattern, "", corpus107q1))
docs107q1 <- tm_map(corpus107q1, toSpaceCorpusq1, "/")
docs107q1 <- tm_map(corpus107q1, toSpaceCorpusq1, "@")
docs107q1 <- tm_map(corpus107q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs107q1 <- tm_map(docs107q1, content_transformer(tolower))
# Remove numbers
docs107q1 <- tm_map(docs107q1, removeNumbers)
# Remove english common stopwords
docs107q1 <- tm_map(docs107q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q1 <- tm_map(docs107q1, removeWords)
# Remove punctuations
docs107q1 <- tm_map(docs107q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q1 <- tm_map(docs107q1, stripWhitespace)
# Text stemming
# docs107q1 <- tm_map(docs107q1, stemDocument)

dtm107q1 <- TermDocumentMatrix(docs107q1)
m107q1 <- as.matrix(dtm107q1)
v107q1 <- sort(rowSums(m107q1),decreasing=TRUE)
d107q1 <- data.frame(word = names(v107q1),freq=v107q1)

head(d107q1, 500)

# View answers :)
wordcloud107q1 <- wordcloud(words = d107q1$word, freq = d107q1$freq, min.freq = 1, scale=c(3,.5),# change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 107q1
d107q1tibble <- list(word = d107q1$word[1:10], freq = d107q1$freq[1:10])
d107q1tibble <- as_tibble(d107q1tibble)

ggplot(d107q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q1: What is the coronavirus?", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q2
text107q2Answer <- paste(x107q2$Answer)
corpus107q2 <- Corpus(VectorSource(text107q2Answer))

# Load the data as a corpus
inspect(corpus107q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus107q2, pattern) gsub(pattern, "", corpus107q2))
docs107q2 <- tm_map(corpus107q2, toSpaceCorpusq2, "/")
docs107q2 <- tm_map(corpus107q2, toSpaceCorpusq2, "@")
docs107q2 <- tm_map(corpus107q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs107q2 <- tm_map(docs107q2, content_transformer(tolower))
# Remove numbers
docs107q2 <- tm_map(docs107q2, removeNumbers)
# Remove english common stopwords
docs107q2 <- tm_map(docs107q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q2 <- tm_map(docs107q2, removeWords)
# Remove punctuations
docs107q2 <- tm_map(docs107q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q2 <- tm_map(docs107q2, stripWhitespace)
# Text stemming
# docs107q2 <- tm_map(docs107q2, stemDocument)

dtm107q2 <- TermDocumentMatrix(docs107q2)
m107q2 <- as.matrix(dtm107q2)
v107q2 <- sort(rowSums(m107q2),decreasing=TRUE)
d107q2 <- data.frame(word = names(v107q2),freq=v107q2)

head(d107q2, 500)

# View answers :)
wordcloud107q2 <- wordcloud(words = d107q2$word, freq = d107q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d107q2tibble <- list(word = d107q2$word[1:10], freq = d107q2$freq[1:10])
d107q2tibble <- as_tibble(d107q2tibble)

ggplot(d107q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q2: Who is the president/prime minister?", subtitle = "n = 33") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q3
text107q3Answer <- paste(x107q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus107q3 <- Corpus(VectorSource(text107q3Answer))

# Load the data as a corpus
inspect(corpus107q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q3 <- content_transformer(function (corpus107q3, pattern) gsub(pattern, "", corpus107q3))
docs107q3 <- tm_map(corpus107q3, toSpaceCorpus107q3, "/")
docs107q3 <- tm_map(corpus107q3, toSpaceCorpus107q3, "@")
docs107q3 <- tm_map(corpus107q3, toSpaceCorpus107q3, "\\|")

# Convert the text to lower case
docs107q3 <- tm_map(docs107q3, content_transformer(tolower))
# Remove numbers
# docs107q3 <- tm_map(docs107q3, removeNumbers)
# Remove english common stopwords
docs107q3 <- tm_map(docs107q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q3 <- tm_map(docs107q3, removeWords)
# Remove punctuations
docs107q3 <- tm_map(docs107q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q3 <- tm_map(docs107q3, stripWhitespace)
# Text stemming
# docs107q3 <- tm_map(docs107q3, stemDocument)

dtm107q3 <- TermDocumentMatrix(docs107q3)
m107q3 <- as.matrix(dtm107q3)
v107q3 <- sort(rowSums(m107q3),decreasing=TRUE)
d107q3 <- data.frame(word = names(v107q3),freq=v107q3)

head(d107q3, 500)

# View answers :)
wordcloud107q3 <- wordcloud(words = d107q3$word, freq = d107q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d107q3tibble <- list(word = d107q3$word[1:10], freq = d107q3$freq[1:10])
d107q3tibble <- as_tibble(d107q3tibble)

ggplot(d107q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q3: How many days have we been in lockdown?", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q4
text107q4Answer <- paste(x107q4$Answer)
corpus107q4 <- Corpus(VectorSource(text107q4Answer))

# Load the data as a corpus
inspect(corpus107q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q4 <- content_transformer(function (corpus107q4, pattern) gsub(pattern, "", corpus107q4))
docs107q4 <- tm_map(corpus107q4, toSpaceCorpus107q4, "/")
docs107q4 <- tm_map(corpus107q4, toSpaceCorpus107q4, "@")
docs107q4 <- tm_map(corpus107q4, toSpaceCorpus107q4, "\\|")

# Convert the text to lower case
docs107q4 <- tm_map(docs107q4, content_transformer(tolower))
# Remove numbers
docs107q4 <- tm_map(docs107q4, removeNumbers)
# Remove english common stopwords
docs107q4 <- tm_map(docs107q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q4 <- tm_map(docs107q4, removeWords)
# Remove punctuations
docs107q4 <- tm_map(docs107q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q4 <- tm_map(docs107q4, stripWhitespace)
# Text stemming
# docs107q4 <- tm_map(docs107q4, stemDocument)

dtm107q4 <- TermDocumentMatrix(docs107q4)
m107q4 <- as.matrix(dtm107q4)
v107q4 <- sort(rowSums(m107q4),decreasing=TRUE)
d107q4 <- data.frame(word = names(v107q4),freq=v107q4)

head(d107q4, 500)

# View answers :)
wordcloud107q4 <- wordcloud(words = d107q4$word, freq = d107q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d107q4tibble <- list(word = d107q4$word[1:10], freq = d107q4$freq[1:10])
d107q4tibble <- as_tibble(d107q4tibble)

ggplot(d107q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q4: Do you want to go back to school?", subtitle = "n = 41") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q5
text107q5Answer <- paste(x107q5$Answer)
corpus107q5 <- Corpus(VectorSource(text107q5Answer))

# Load the data as a corpus
inspect(corpus107q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q5 <- content_transformer(function (corpus107q5, pattern) gsub(pattern, "", corpus107q5))
docs107q5 <- tm_map(corpus107q5, toSpaceCorpus107q5, "/")
docs107q5 <- tm_map(corpus107q5, toSpaceCorpus107q5, "@")
docs107q5 <- tm_map(corpus107q5, toSpaceCorpus107q5, "\\|")

# Convert the text to lower case
docs107q5 <- tm_map(docs107q5, content_transformer(tolower))
# Remove numbers
docs107q5 <- tm_map(docs107q5, removeNumbers)
# Remove english common stopwords
docs107q5 <- tm_map(docs107q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q5 <- tm_map(docs107q5, removeWords)
# Remove punctuations
docs107q5 <- tm_map(docs107q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q5 <- tm_map(docs107q5, stripWhitespace)
# Text stemming
# docs107q5 <- tm_map(docs107q5, stemDocument)

dtm107q5 <- TermDocumentMatrix(docs107q5)
m107q5 <- as.matrix(dtm107q5)
v107q5 <- sort(rowSums(m107q5),decreasing=TRUE)
d107q5 <- data.frame(word = names(v107q5),freq=v107q5)

head(d107q5, 500)

# View answers :)
wordcloud107q5 <- wordcloud(words = d107q5$word, freq = d107q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d107q5tibble <- list(word = d107q5$word[1:10], freq = d107q5$freq[1:10])
d107q5tibble <- as_tibble(d107q5tibble)

ggplot(d107q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q6
text107q6Answer <- paste(x107q6$Answer)
corpus107q6 <- Corpus(VectorSource(text107q6Answer))

# Load the data as a corpus
inspect(corpus107q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q6 <- content_transformer(function (corpus107q6, pattern) gsub(pattern, "", corpus107q6))
docs107q6 <- tm_map(corpus107q6, toSpaceCorpus107q6, "/")
docs107q6 <- tm_map(corpus107q6, toSpaceCorpus107q6, "@")
docs107q6 <- tm_map(corpus107q6, toSpaceCorpus107q6, "\\|")

# Convert the text to lower case
docs107q6 <- tm_map(docs107q6, content_transformer(tolower))
# Remove numbers
docs107q6 <- tm_map(docs107q6, removeNumbers)
# Remove english common stopwords
docs107q6 <- tm_map(docs107q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q6 <- tm_map(docs107q6, removeWords)
# Remove punctuations
docs107q6 <- tm_map(docs107q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q6 <- tm_map(docs107q6, stripWhitespace)
# Text stemming
# docs107q6 <- tm_map(docs107q6, stemDocument)

dtm107q6 <- TermDocumentMatrix(docs107q6)
m107q6 <- as.matrix(dtm107q6)
v107q6 <- sort(rowSums(m107q6),decreasing=TRUE)
d107q6 <- data.frame(word = names(v107q6),freq=v107q6)

head(d107q6, 500)

# View answers :)
wordcloud107q6 <- wordcloud(words = d107q6$word, freq = d107q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d107q6tibble <- list(word = d107q6$word[1:10], freq = d107q6$freq[1:10])
d107q6tibble <- as_tibble(d107q6tibble)

ggplot(d107q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q6: Where is the first place you want to go?", subtitle = "n = 44") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q7
text107q7Answer <- paste(x107q7$Answer)
corpus107q7 <- Corpus(VectorSource(text107q7Answer))

# Load the data as a corpus
inspect(corpus107q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q7 <- content_transformer(function (corpus107q7, pattern) gsub(pattern, "", corpus107q7))
docs107q7 <- tm_map(corpus107q7, toSpaceCorpus107q7, "/")
docs107q7 <- tm_map(corpus107q7, toSpaceCorpus107q7, "@")
docs107q7 <- tm_map(corpus107q7, toSpaceCorpus107q7, "\\|")

# Convert the text to lower case
docs107q7 <- tm_map(docs107q7, content_transformer(tolower))
# Remove numbers
docs107q7 <- tm_map(docs107q7, removeNumbers)
# Remove english common stopwords
docs107q7 <- tm_map(docs107q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q7 <- tm_map(docs107q7, removeWords)
# Remove punctuations
docs107q7 <- tm_map(docs107q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q7 <- tm_map(docs107q7, stripWhitespace)
# Text stemming
# docs107q7 <- tm_map(docs107q7, stemDocument)

dtm107q7 <- TermDocumentMatrix(docs107q7)
m107q7 <- as.matrix(dtm107q7)
v107q7 <- sort(rowSums(m107q7),decreasing=TRUE)
d107q7 <- data.frame(word = names(v107q7),freq=v107q7)

head(d107q7, 500)

# View answers :)
wordcloud107q7 <- wordcloud(words = d107q7$word, freq = d107q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d107q7tibble <- list(word = d107q7$word[1:10], freq = d107q7$freq[1:10])
d107q7tibble <- as_tibble(d107q7tibble)

ggplot(d107q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q8
text107q8Answer <- paste(x107q8$Answer)
corpus107q8 <- Corpus(VectorSource(text107q8Answer))

# Load the data as a corpus
inspect(corpus107q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q8 <- content_transformer(function (corpus107q8, pattern) gsub(pattern, "", corpus107q8))
docs107q8 <- tm_map(corpus107q8, toSpaceCorpus107q8, "/")
docs107q8 <- tm_map(corpus107q8, toSpaceCorpus107q8, "@")
docs107q8 <- tm_map(corpus107q8, toSpaceCorpus107q8, "\\|")

# Convert the text to lower case
docs107q8 <- tm_map(docs107q8, content_transformer(tolower))
# Remove numbers
docs107q8 <- tm_map(docs107q8, removeNumbers)
# Remove english common stopwords
docs107q8 <- tm_map(docs107q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs107q8 <- tm_map(docs107q8, removeWords)
# Remove punctuations
docs107q8 <- tm_map(docs107q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q8 <- tm_map(docs107q8, stripWhitespace)
# Text stemming
# docs107q8 <- tm_map(docs107q8, stemDocument)

dtm107q8 <- TermDocumentMatrix(docs107q8)
m107q8 <- as.matrix(dtm107q8)
v107q8 <- sort(rowSums(m107q8),decreasing=TRUE)
d107q8 <- data.frame(word = names(v107q8),freq=v107q8)

head(d107q8, 500)

# View answers :)
wordcloud107q8 <- wordcloud(words = d107q8$word, freq = d107q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d107q8tibble <- list(word = d107q8$word[1:10], freq = d107q8$freq[1:10])
d107q8tibble <- as_tibble(d107q8tibble)

ggplot(d107q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q8: Are your parents good teachers?", subtitle = "n = 39") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q9
text107q9Answer <- paste(x107q9$Answer)
corpus107q9 <- Corpus(VectorSource(text107q9Answer))

# Load the data as a corpus
inspect(corpus107q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q9 <- content_transformer(function (corpus107q9, pattern) gsub(pattern, "", corpus107q9))
docs107q9 <- tm_map(corpus107q9, toSpaceCorpus107q9, "/")
docs107q9 <- tm_map(corpus107q9, toSpaceCorpus107q9, "@")
docs107q9 <- tm_map(corpus107q9, toSpaceCorpus107q9, "\\|")

# Convert the text to lower case
docs107q9 <- tm_map(docs107q9, content_transformer(tolower))
# Remove numbers
docs107q9 <- tm_map(docs107q9, removeNumbers)
# Remove english common stopwords
docs107q9 <- tm_map(docs107q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q9 <- tm_map(docs107q9, removeWords)
# Remove punctuations
docs107q9 <- tm_map(docs107q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q9 <- tm_map(docs107q9, stripWhitespace)
# Text stemming
# docs107q9 <- tm_map(docs107q9, stemDocument)

dtm107q9 <- TermDocumentMatrix(docs107q9)
m107q9 <- as.matrix(dtm107q9)
v107q9 <- sort(rowSums(m107q9),decreasing=TRUE)
d107q9 <- data.frame(word = names(v107q9),freq=v107q9)

head(d107q9, 500)

# View answers :)
wordcloud107q9 <- wordcloud(words = d107q9$word, freq = d107q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d107q9tibble <- list(word = d107q9$word[1:10], freq = d107q9$freq[1:10])
d107q9tibble <- as_tibble(d107q9tibble)

ggplot(d107q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q9: How did the coronavirus start?", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q10
text107q10Answer <- paste(x107q10$Answer)
corpus107q10 <- Corpus(VectorSource(text107q10Answer))

# Load the data as a corpus
inspect(corpus107q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q10 <- content_transformer(function (corpus107q10, pattern) gsub(pattern, "", corpus107q10))
docs107q10 <- tm_map(corpus107q10, toSpaceCorpus107q10, "/")
docs107q10 <- tm_map(corpus107q10, toSpaceCorpus107q10, "@")
docs107q10 <- tm_map(corpus107q10, toSpaceCorpus107q10, "\\|")

# Convert the text to lower case
docs107q10 <- tm_map(docs107q10, content_transformer(tolower))
# Remove numbers
docs107q10 <- tm_map(docs107q10, removeNumbers)
# Remove english common stopwords
docs107q10 <- tm_map(docs107q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q10 <- tm_map(docs107q10, removeWords)
# Remove punctuations
docs107q10 <- tm_map(docs107q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q10 <- tm_map(docs107q10, stripWhitespace)
# Text stemming
# docs107q10 <- tm_map(docs107q10, stemDocument)

dtm107q10 <- TermDocumentMatrix(docs107q10)
m107q10 <- as.matrix(dtm107q10)
v107q10 <- sort(rowSums(m107q10),decreasing=TRUE)
d107q10 <- data.frame(word = names(v107q10),freq=v107q10)

head(d107q10, 500)

# View answers :)
wordcloud107q10 <- wordcloud(words = d107q10$word, freq = d107q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d107q10tibble <- list(word = d107q10$word[1:10], freq = d107q10$freq[1:10])
d107q10tibble <- as_tibble(d107q10tibble)

ggplot(d107q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 39") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus107q11
text107q11Answer <- paste(x107q11$Answer)
corpus107q11 <- Corpus(VectorSource(text107q11Answer))

# Load the data as a corpus
inspect(corpus107q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus107q11 <- content_transformer(function (corpus107q11, pattern) gsub(pattern, "", corpus107q11))
docs107q11 <- tm_map(corpus107q11, toSpaceCorpus107q11, "/")
docs107q11 <- tm_map(corpus107q11, toSpaceCorpus107q11, "@")
docs107q11 <- tm_map(corpus107q11, toSpaceCorpus107q11, "\\|")

# Convert the text to lower case
docs107q11 <- tm_map(docs107q11, content_transformer(tolower))
# Remove numbers
docs107q11 <- tm_map(docs107q11, removeNumbers)
# Remove english common stopwords
docs107q11 <- tm_map(docs107q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs107q11 <- tm_map(docs107q11, removeWords)
# Remove punctuations
docs107q11 <- tm_map(docs107q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs107q11 <- tm_map(docs107q11, stripWhitespace)
# Text stemming
# docs107q11 <- tm_map(docs107q11, stemDocument)

dtm107q11 <- TermDocumentMatrix(docs107q11)
m107q11 <- as.matrix(dtm107q11)
v107q11 <- sort(rowSums(m107q11),decreasing=TRUE)
d107q11 <- data.frame(word = names(v107q11),freq=v107q11)

head(d107q11, 500)

# View answers :)
wordcloud107q11 <- wordcloud(words = d107q11$word, freq = d107q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d107q11tibble <- list(word = d107q11$word[1:10], freq = d107q11$freq[1:10])
d107q11tibble <- as_tibble(d107q11tibble)

ggplot(d107q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 7 - Q11: Are you enjoying lockdown?", subtitle = "n = 40") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 8 ###
##################################################################################################
# Corpus108q1
text108q1Answer <- paste(x108q1$Answer)
corpus108q1 <- Corpus(VectorSource(text108q1Answer))

# Load the data as a corpus
inspect(corpus108q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q1 <- content_transformer(function (corpus108q1, pattern) gsub(pattern, "", corpus108q1))
docs108q1 <- tm_map(corpus108q1, toSpaceCorpusq1, "/")
docs108q1 <- tm_map(corpus108q1, toSpaceCorpusq1, "@")
docs108q1 <- tm_map(corpus108q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs108q1 <- tm_map(docs108q1, content_transformer(tolower))
# Remove numbers
docs108q1 <- tm_map(docs108q1, removeNumbers)
# Remove english common stopwords
docs108q1 <- tm_map(docs108q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q1 <- tm_map(docs108q1, removeWords)
# Remove punctuations
docs108q1 <- tm_map(docs108q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q1 <- tm_map(docs108q1, stripWhitespace)
# Text stemming
# docs108q1 <- tm_map(docs108q1, stemDocument)

dtm108q1 <- TermDocumentMatrix(docs108q1)
m108q1 <- as.matrix(dtm108q1)
v108q1 <- sort(rowSums(m108q1),decreasing=TRUE)
d108q1 <- data.frame(word = names(v108q1),freq=v108q1)

head(d108q1, 500)

# View answers :)
wordcloud108q1 <- wordcloud(words = d108q1$word, freq = d108q1$freq, min.freq = 1,scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 108q1
d108q1tibble <- list(word = d108q1$word[1:10], freq = d108q1$freq[1:10])
d108q1tibble <- as_tibble(d108q1tibble)

ggplot(d108q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q1: What is the coronavirus?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q2
text108q2Answer <- paste(x108q2$Answer)
corpus108q2 <- Corpus(VectorSource(text108q2Answer))

# Load the data as a corpus
inspect(corpus108q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus108q2, pattern) gsub(pattern, "", corpus108q2))
docs108q2 <- tm_map(corpus108q2, toSpaceCorpusq2, "/")
docs108q2 <- tm_map(corpus108q2, toSpaceCorpusq2, "@")
docs108q2 <- tm_map(corpus108q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs108q2 <- tm_map(docs108q2, content_transformer(tolower))
# Remove numbers
docs108q2 <- tm_map(docs108q2, removeNumbers)
# Remove english common stopwords
docs108q2 <- tm_map(docs108q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q2 <- tm_map(docs108q2, removeWords)
# Remove punctuations
docs108q2 <- tm_map(docs108q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q2 <- tm_map(docs108q2, stripWhitespace)
# Text stemming
# docs108q2 <- tm_map(docs108q2, stemDocument)

dtm108q2 <- TermDocumentMatrix(docs108q2)
m108q2 <- as.matrix(dtm108q2)
v108q2 <- sort(rowSums(m108q2),decreasing=TRUE)
d108q2 <- data.frame(word = names(v108q2),freq=v108q2)

head(d108q2, 500)

# View answers :)
wordcloud108q2 <- wordcloud(words = d108q2$word, freq = d108q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d108q2tibble <- list(word = d108q2$word[1:10], freq = d108q2$freq[1:10])
d108q2tibble <- as_tibble(d108q2tibble)

ggplot(d108q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q2: Who is the president/prime minister?", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q3
text108q3Answer <- paste(x108q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus108q3 <- Corpus(VectorSource(text108q3Answer))

# Load the data as a corpus
inspect(corpus108q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q3 <- content_transformer(function (corpus108q3, pattern) gsub(pattern, "", corpus108q3))
docs108q3 <- tm_map(corpus108q3, toSpaceCorpus108q3, "/")
docs108q3 <- tm_map(corpus108q3, toSpaceCorpus108q3, "@")
docs108q3 <- tm_map(corpus108q3, toSpaceCorpus108q3, "\\|")

# Convert the text to lower case
docs108q3 <- tm_map(docs108q3, content_transformer(tolower))
# Remove numbers
# docs108q3 <- tm_map(docs108q3, removeNumbers)
# Remove english common stopwords
docs108q3 <- tm_map(docs108q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q3 <- tm_map(docs108q3, removeWords)
# Remove punctuations
docs108q3 <- tm_map(docs108q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q3 <- tm_map(docs108q3, stripWhitespace)
# Text stemming
# docs108q3 <- tm_map(docs108q3, stemDocument)

dtm108q3 <- TermDocumentMatrix(docs108q3)
m108q3 <- as.matrix(dtm108q3)
v108q3 <- sort(rowSums(m108q3),decreasing=TRUE)
d108q3 <- data.frame(word = names(v108q3),freq=v108q3)

head(d108q3, 500)

# View answers :)
wordcloud108q3 <- wordcloud(words = d108q3$word, freq = d108q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d108q3tibble <- list(word = d108q3$word[1:10], freq = d108q3$freq[1:10])
d108q3tibble <- as_tibble(d108q3tibble)

ggplot(d108q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q3: How many days have we been in lockdown?", subtitle = "n = 23") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q4
text108q4Answer <- paste(x108q4$Answer)
corpus108q4 <- Corpus(VectorSource(text108q4Answer))

# Load the data as a corpus
inspect(corpus108q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q4 <- content_transformer(function (corpus108q4, pattern) gsub(pattern, "", corpus108q4))
docs108q4 <- tm_map(corpus108q4, toSpaceCorpus108q4, "/")
docs108q4 <- tm_map(corpus108q4, toSpaceCorpus108q4, "@")
docs108q4 <- tm_map(corpus108q4, toSpaceCorpus108q4, "\\|")

# Convert the text to lower case
docs108q4 <- tm_map(docs108q4, content_transformer(tolower))
# Remove numbers
docs108q4 <- tm_map(docs108q4, removeNumbers)
# Remove english common stopwords
docs108q4 <- tm_map(docs108q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q4 <- tm_map(docs108q4, removeWords)
# Remove punctuations
docs108q4 <- tm_map(docs108q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q4 <- tm_map(docs108q4, stripWhitespace)
# Text stemming
# docs108q4 <- tm_map(docs108q4, stemDocument)

dtm108q4 <- TermDocumentMatrix(docs108q4)
m108q4 <- as.matrix(dtm108q4)
v108q4 <- sort(rowSums(m108q4),decreasing=TRUE)
d108q4 <- data.frame(word = names(v108q4),freq=v108q4)

head(d108q4, 500)

# View answers :)
wordcloud108q4 <- wordcloud(words = d108q4$word, freq = d108q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d108q4tibble <- list(word = d108q4$word[1:10], freq = d108q4$freq[1:10])
d108q4tibble <- as_tibble(d108q4tibble)

ggplot(d108q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q4: Do you want to go back to school?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q5
text108q5Answer <- paste(x108q5$Answer)
corpus108q5 <- Corpus(VectorSource(text108q5Answer))

# Load the data as a corpus
inspect(corpus108q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q5 <- content_transformer(function (corpus108q5, pattern) gsub(pattern, "", corpus108q5))
docs108q5 <- tm_map(corpus108q5, toSpaceCorpus108q5, "/")
docs108q5 <- tm_map(corpus108q5, toSpaceCorpus108q5, "@")
docs108q5 <- tm_map(corpus108q5, toSpaceCorpus108q5, "\\|")

# Convert the text to lower case
docs108q5 <- tm_map(docs108q5, content_transformer(tolower))
# Remove numbers
docs108q5 <- tm_map(docs108q5, removeNumbers)
# Remove english common stopwords
docs108q5 <- tm_map(docs108q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q5 <- tm_map(docs108q5, removeWords)
# Remove punctuations
docs108q5 <- tm_map(docs108q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q5 <- tm_map(docs108q5, stripWhitespace)
# Text stemming
# docs108q5 <- tm_map(docs108q5, stemDocument)

dtm108q5 <- TermDocumentMatrix(docs108q5)
m108q5 <- as.matrix(dtm108q5)
v108q5 <- sort(rowSums(m108q5),decreasing=TRUE)
d108q5 <- data.frame(word = names(v108q5),freq=v108q5)

head(d108q5, 500)

# View answers :)
wordcloud108q5 <- wordcloud(words = d108q5$word, freq = d108q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d108q5tibble <- list(word = d108q5$word[1:10], freq = d108q5$freq[1:10])
d108q5tibble <- as_tibble(d108q5tibble)

ggplot(d108q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q6
text108q6Answer <- paste(x108q6$Answer)
corpus108q6 <- Corpus(VectorSource(text108q6Answer))

# Load the data as a corpus
inspect(corpus108q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q6 <- content_transformer(function (corpus108q6, pattern) gsub(pattern, "", corpus108q6))
docs108q6 <- tm_map(corpus108q6, toSpaceCorpus108q6, "/")
docs108q6 <- tm_map(corpus108q6, toSpaceCorpus108q6, "@")
docs108q6 <- tm_map(corpus108q6, toSpaceCorpus108q6, "\\|")

# Convert the text to lower case
docs108q6 <- tm_map(docs108q6, content_transformer(tolower))
# Remove numbers
docs108q6 <- tm_map(docs108q6, removeNumbers)
# Remove english common stopwords
docs108q6 <- tm_map(docs108q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q6 <- tm_map(docs108q6, removeWords)
# Remove punctuations
docs108q6 <- tm_map(docs108q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q6 <- tm_map(docs108q6, stripWhitespace)
# Text stemming
# docs108q6 <- tm_map(docs108q6, stemDocument)

dtm108q6 <- TermDocumentMatrix(docs108q6)
m108q6 <- as.matrix(dtm108q6)
v108q6 <- sort(rowSums(m108q6),decreasing=TRUE)
d108q6 <- data.frame(word = names(v108q6),freq=v108q6)

head(d108q6, 500)

# View answers :)
wordcloud108q6 <- wordcloud(words = d108q6$word, freq = d108q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d108q6tibble <- list(word = d108q6$word[1:10], freq = d108q6$freq[1:10])
d108q6tibble <- as_tibble(d108q6tibble)

ggplot(d108q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q6: Where is the first place you want to go?", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q7
text108q7Answer <- paste(x108q7$Answer)
corpus108q7 <- Corpus(VectorSource(text108q7Answer))

# Load the data as a corpus
inspect(corpus108q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q7 <- content_transformer(function (corpus108q7, pattern) gsub(pattern, "", corpus108q7))
docs108q7 <- tm_map(corpus108q7, toSpaceCorpus108q7, "/")
docs108q7 <- tm_map(corpus108q7, toSpaceCorpus108q7, "@")
docs108q7 <- tm_map(corpus108q7, toSpaceCorpus108q7, "\\|")

# Convert the text to lower case
docs108q7 <- tm_map(docs108q7, content_transformer(tolower))
# Remove numbers
docs108q7 <- tm_map(docs108q7, removeNumbers)
# Remove english common stopwords
docs108q7 <- tm_map(docs108q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q7 <- tm_map(docs108q7, removeWords)
# Remove punctuations
docs108q7 <- tm_map(docs108q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q7 <- tm_map(docs108q7, stripWhitespace)
# Text stemming
# docs108q7 <- tm_map(docs108q7, stemDocument)

dtm108q7 <- TermDocumentMatrix(docs108q7)
m108q7 <- as.matrix(dtm108q7)
v108q7 <- sort(rowSums(m108q7),decreasing=TRUE)
d108q7 <- data.frame(word = names(v108q7),freq=v108q7)

head(d108q7, 500)

# View answers :)
wordcloud108q7 <- wordcloud(words = d108q7$word, freq = d108q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d108q7tibble <- list(word = d108q7$word[1:10], freq = d108q7$freq[1:10])
d108q7tibble <- as_tibble(d108q7tibble)

ggplot(d108q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q8
text108q8Answer <- paste(x108q8$Answer)
corpus108q8 <- Corpus(VectorSource(text108q8Answer))

# Load the data as a corpus
inspect(corpus108q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q8 <- content_transformer(function (corpus108q8, pattern) gsub(pattern, "", corpus108q8))
docs108q8 <- tm_map(corpus108q8, toSpaceCorpus108q8, "/")
docs108q8 <- tm_map(corpus108q8, toSpaceCorpus108q8, "@")
docs108q8 <- tm_map(corpus108q8, toSpaceCorpus108q8, "\\|")

# Convert the text to lower case
docs108q8 <- tm_map(docs108q8, content_transformer(tolower))
# Remove numbers
docs108q8 <- tm_map(docs108q8, removeNumbers)
# Remove english common stopwords
docs108q8 <- tm_map(docs108q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs108q8 <- tm_map(docs108q8, removeWords)
# Remove punctuations
docs108q8 <- tm_map(docs108q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q8 <- tm_map(docs108q8, stripWhitespace)
# Text stemming
# docs108q8 <- tm_map(docs108q8, stemDocument)

dtm108q8 <- TermDocumentMatrix(docs108q8)
m108q8 <- as.matrix(dtm108q8)
v108q8 <- sort(rowSums(m108q8),decreasing=TRUE)
d108q8 <- data.frame(word = names(v108q8),freq=v108q8)

head(d108q8, 500)

# View answers :)
wordcloud108q8 <- wordcloud(words = d108q8$word, freq = d108q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d108q8tibble <- list(word = d108q8$word[1:10], freq = d108q8$freq[1:10])
d108q8tibble <- as_tibble(d108q8tibble)

ggplot(d108q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q8: Are your parents good teachers?", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q9
text108q9Answer <- paste(x108q9$Answer)
corpus108q9 <- Corpus(VectorSource(text108q9Answer))

# Load the data as a corpus
inspect(corpus108q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q9 <- content_transformer(function (corpus108q9, pattern) gsub(pattern, "", corpus108q9))
docs108q9 <- tm_map(corpus108q9, toSpaceCorpus108q9, "/")
docs108q9 <- tm_map(corpus108q9, toSpaceCorpus108q9, "@")
docs108q9 <- tm_map(corpus108q9, toSpaceCorpus108q9, "\\|")

# Convert the text to lower case
docs108q9 <- tm_map(docs108q9, content_transformer(tolower))
# Remove numbers
docs108q9 <- tm_map(docs108q9, removeNumbers)
# Remove english common stopwords
docs108q9 <- tm_map(docs108q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q9 <- tm_map(docs108q9, removeWords)
# Remove punctuations
docs108q9 <- tm_map(docs108q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q9 <- tm_map(docs108q9, stripWhitespace)
# Text stemming
# docs108q9 <- tm_map(docs108q9, stemDocument)

dtm108q9 <- TermDocumentMatrix(docs108q9)
m108q9 <- as.matrix(dtm108q9)
v108q9 <- sort(rowSums(m108q9),decreasing=TRUE)
d108q9 <- data.frame(word = names(v108q9),freq=v108q9)

head(d108q9, 500)

# View answers :)
wordcloud108q9 <- wordcloud(words = d108q9$word, freq = d108q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d108q9tibble <- list(word = d108q9$word[1:10], freq = d108q9$freq[1:10])
d108q9tibble <- as_tibble(d108q9tibble)

ggplot(d108q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q9: How did the coronavirus start?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q10
text108q10Answer <- paste(x108q10$Answer)
corpus108q10 <- Corpus(VectorSource(text108q10Answer))

# Load the data as a corpus
inspect(corpus108q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q10 <- content_transformer(function (corpus108q10, pattern) gsub(pattern, "", corpus108q10))
docs108q10 <- tm_map(corpus108q10, toSpaceCorpus108q10, "/")
docs108q10 <- tm_map(corpus108q10, toSpaceCorpus108q10, "@")
docs108q10 <- tm_map(corpus108q10, toSpaceCorpus108q10, "\\|")

# Convert the text to lower case
docs108q10 <- tm_map(docs108q10, content_transformer(tolower))
# Remove numbers
docs108q10 <- tm_map(docs108q10, removeNumbers)
# Remove english common stopwords
docs108q10 <- tm_map(docs108q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q10 <- tm_map(docs108q10, removeWords)
# Remove punctuations
docs108q10 <- tm_map(docs108q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q10 <- tm_map(docs108q10, stripWhitespace)
# Text stemming
# docs108q10 <- tm_map(docs108q10, stemDocument)

dtm108q10 <- TermDocumentMatrix(docs108q10)
m108q10 <- as.matrix(dtm108q10)
v108q10 <- sort(rowSums(m108q10),decreasing=TRUE)
d108q10 <- data.frame(word = names(v108q10),freq=v108q10)

head(d108q10, 500)

# View answers :)
wordcloud108q10 <- wordcloud(words = d108q10$word, freq = d108q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d108q10tibble <- list(word = d108q10$word[1:10], freq = d108q10$freq[1:10])
d108q10tibble <- as_tibble(d108q10tibble)

ggplot(d108q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus108q11
text108q11Answer <- paste(x108q11$Answer)
corpus108q11 <- Corpus(VectorSource(text108q11Answer))

# Load the data as a corpus
inspect(corpus108q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus108q11 <- content_transformer(function (corpus108q11, pattern) gsub(pattern, "", corpus108q11))
docs108q11 <- tm_map(corpus108q11, toSpaceCorpus108q11, "/")
docs108q11 <- tm_map(corpus108q11, toSpaceCorpus108q11, "@")
docs108q11 <- tm_map(corpus108q11, toSpaceCorpus108q11, "\\|")

# Convert the text to lower case
docs108q11 <- tm_map(docs108q11, content_transformer(tolower))
# Remove numbers
docs108q11 <- tm_map(docs108q11, removeNumbers)
# Remove english common stopwords
docs108q11 <- tm_map(docs108q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs108q11 <- tm_map(docs108q11, removeWords)
# Remove punctuations
docs108q11 <- tm_map(docs108q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs108q11 <- tm_map(docs108q11, stripWhitespace)
# Text stemming
# docs108q11 <- tm_map(docs108q11, stemDocument)

dtm108q11 <- TermDocumentMatrix(docs108q11)
m108q11 <- as.matrix(dtm108q11)
v108q11 <- sort(rowSums(m108q11),decreasing=TRUE)
d108q11 <- data.frame(word = names(v108q11),freq=v108q11)

head(d108q11, 500)

# View answers :)
wordcloud108q11 <- wordcloud(words = d108q11$word, freq = d108q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d108q11tibble <- list(word = d108q11$word[1:10], freq = d108q11$freq[1:10])
d108q11tibble <- as_tibble(d108q11tibble)

ggplot(d108q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 8 - Q11: Are you enjoying lockdown?", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 9 ###
##################################################################################################
# Corpus109q1
text109q1Answer <- paste(x109q1$Answer)
corpus109q1 <- Corpus(VectorSource(text109q1Answer))

# Load the data as a corpus
inspect(corpus109q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q1 <- content_transformer(function (corpus109q1, pattern) gsub(pattern, "", corpus109q1))
docs109q1 <- tm_map(corpus109q1, toSpaceCorpusq1, "/")
docs109q1 <- tm_map(corpus109q1, toSpaceCorpusq1, "@")
docs109q1 <- tm_map(corpus109q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs109q1 <- tm_map(docs109q1, content_transformer(tolower))
# Remove numbers
docs109q1 <- tm_map(docs109q1, removeNumbers)
# Remove english common stopwords
docs109q1 <- tm_map(docs109q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q1 <- tm_map(docs109q1, removeWords)
# Remove punctuations
docs109q1 <- tm_map(docs109q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q1 <- tm_map(docs109q1, stripWhitespace)
# Text stemming
# docs109q1 <- tm_map(docs109q1, stemDocument)

dtm109q1 <- TermDocumentMatrix(docs109q1)
m109q1 <- as.matrix(dtm109q1)
v109q1 <- sort(rowSums(m109q1),decreasing=TRUE)
d109q1 <- data.frame(word = names(v109q1),freq=v109q1)

head(d109q1, 500)

# View answers :)
wordcloud109q1 <- wordcloud(words = d109q1$word, freq = d109q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 109q1
d109q1tibble <- list(word = d109q1$word[1:10], freq = d109q1$freq[1:10])
d109q1tibble <- as_tibble(d109q1tibble)

ggplot(d109q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q1: What is the coronavirus?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q2
text109q2Answer <- paste(x109q2$Answer)
corpus109q2 <- Corpus(VectorSource(text109q2Answer))

# Load the data as a corpus
inspect(corpus109q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus109q2, pattern) gsub(pattern, "", corpus109q2))
docs109q2 <- tm_map(corpus109q2, toSpaceCorpusq2, "/")
docs109q2 <- tm_map(corpus109q2, toSpaceCorpusq2, "@")
docs109q2 <- tm_map(corpus109q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs109q2 <- tm_map(docs109q2, content_transformer(tolower))
# Remove numbers
docs109q2 <- tm_map(docs109q2, removeNumbers)
# Remove english common stopwords
docs109q2 <- tm_map(docs109q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q2 <- tm_map(docs109q2, removeWords)
# Remove punctuations
docs109q2 <- tm_map(docs109q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q2 <- tm_map(docs109q2, stripWhitespace)
# Text stemming
# docs109q2 <- tm_map(docs109q2, stemDocument)

dtm109q2 <- TermDocumentMatrix(docs109q2)
m109q2 <- as.matrix(dtm109q2)
v109q2 <- sort(rowSums(m109q2),decreasing=TRUE)
d109q2 <- data.frame(word = names(v109q2),freq=v109q2)

head(d109q2, 500)

# View answers :)
wordcloud109q2 <- wordcloud(words = d109q2$word, freq = d109q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d109q2tibble <- list(word = d109q2$word[1:10], freq = d109q2$freq[1:10])
d109q2tibble <- as_tibble(d109q2tibble)

ggplot(d109q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q2: Who is the president/prime minister?", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q3
text109q3Answer <- paste(x109q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus109q3 <- Corpus(VectorSource(text109q3Answer))

# Load the data as a corpus
inspect(corpus109q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q3 <- content_transformer(function (corpus109q3, pattern) gsub(pattern, "", corpus109q3))
docs109q3 <- tm_map(corpus109q3, toSpaceCorpus109q3, "/")
docs109q3 <- tm_map(corpus109q3, toSpaceCorpus109q3, "@")
docs109q3 <- tm_map(corpus109q3, toSpaceCorpus109q3, "\\|")

# Convert the text to lower case
docs109q3 <- tm_map(docs109q3, content_transformer(tolower))
# Remove numbers
# docs109q3 <- tm_map(docs109q3, removeNumbers)
# Remove english common stopwords
docs109q3 <- tm_map(docs109q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q3 <- tm_map(docs109q3, removeWords)
# Remove punctuations
docs109q3 <- tm_map(docs109q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q3 <- tm_map(docs109q3, stripWhitespace)
# Text stemming
# docs109q3 <- tm_map(docs109q3, stemDocument)

dtm109q3 <- TermDocumentMatrix(docs109q3)
m109q3 <- as.matrix(dtm109q3)
v109q3 <- sort(rowSums(m109q3),decreasing=TRUE)
d109q3 <- data.frame(word = names(v109q3),freq=v109q3)

head(d109q3, 500)

# View answers :)
wordcloud109q3 <- wordcloud(words = d109q3$word, freq = d109q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d109q3tibble <- list(word = d109q3$word[1:10], freq = d109q3$freq[1:10])
d109q3tibble <- as_tibble(d109q3tibble)

ggplot(d109q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q3: How many days have we been in lockdown?", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q4
text109q4Answer <- paste(x109q4$Answer)
corpus109q4 <- Corpus(VectorSource(text109q4Answer))

# Load the data as a corpus
inspect(corpus109q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q4 <- content_transformer(function (corpus109q4, pattern) gsub(pattern, "", corpus109q4))
docs109q4 <- tm_map(corpus109q4, toSpaceCorpus109q4, "/")
docs109q4 <- tm_map(corpus109q4, toSpaceCorpus109q4, "@")
docs109q4 <- tm_map(corpus109q4, toSpaceCorpus109q4, "\\|")

# Convert the text to lower case
docs109q4 <- tm_map(docs109q4, content_transformer(tolower))
# Remove numbers
docs109q4 <- tm_map(docs109q4, removeNumbers)
# Remove english common stopwords
docs109q4 <- tm_map(docs109q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q4 <- tm_map(docs109q4, removeWords)
# Remove punctuations
docs109q4 <- tm_map(docs109q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q4 <- tm_map(docs109q4, stripWhitespace)
# Text stemming
# docs109q4 <- tm_map(docs109q4, stemDocument)

dtm109q4 <- TermDocumentMatrix(docs109q4)
m109q4 <- as.matrix(dtm109q4)
v109q4 <- sort(rowSums(m109q4),decreasing=TRUE)
d109q4 <- data.frame(word = names(v109q4),freq=v109q4)

head(d109q4, 500)

# View answers :)
wordcloud109q4 <- wordcloud(words = d109q4$word, freq = d109q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d109q4tibble <- list(word = d109q4$word[1:10], freq = d109q4$freq[1:10])
d109q4tibble <- as_tibble(d109q4tibble)

ggplot(d109q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q4: Do you want to go back to school?", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q5
text109q5Answer <- paste(x109q5$Answer)
corpus109q5 <- Corpus(VectorSource(text109q5Answer))

# Load the data as a corpus
inspect(corpus109q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q5 <- content_transformer(function (corpus109q5, pattern) gsub(pattern, "", corpus109q5))
docs109q5 <- tm_map(corpus109q5, toSpaceCorpus109q5, "/")
docs109q5 <- tm_map(corpus109q5, toSpaceCorpus109q5, "@")
docs109q5 <- tm_map(corpus109q5, toSpaceCorpus109q5, "\\|")

# Convert the text to lower case
docs109q5 <- tm_map(docs109q5, content_transformer(tolower))
# Remove numbers
docs109q5 <- tm_map(docs109q5, removeNumbers)
# Remove english common stopwords
docs109q5 <- tm_map(docs109q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q5 <- tm_map(docs109q5, removeWords)
# Remove punctuations
docs109q5 <- tm_map(docs109q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q5 <- tm_map(docs109q5, stripWhitespace)
# Text stemming
# docs109q5 <- tm_map(docs109q5, stemDocument)

dtm109q5 <- TermDocumentMatrix(docs109q5)
m109q5 <- as.matrix(dtm109q5)
v109q5 <- sort(rowSums(m109q5),decreasing=TRUE)
d109q5 <- data.frame(word = names(v109q5),freq=v109q5)

head(d109q5, 500)

# View answers :)
wordcloud109q5 <- wordcloud(words = d109q5$word, freq = d109q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d109q5tibble <- list(word = d109q5$word[1:10], freq = d109q5$freq[1:10])
d109q5tibble <- as_tibble(d109q5tibble)

ggplot(d109q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q6
text109q6Answer <- paste(x109q6$Answer)
corpus109q6 <- Corpus(VectorSource(text109q6Answer))

# Load the data as a corpus
inspect(corpus109q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q6 <- content_transformer(function (corpus109q6, pattern) gsub(pattern, "", corpus109q6))
docs109q6 <- tm_map(corpus109q6, toSpaceCorpus109q6, "/")
docs109q6 <- tm_map(corpus109q6, toSpaceCorpus109q6, "@")
docs109q6 <- tm_map(corpus109q6, toSpaceCorpus109q6, "\\|")

# Convert the text to lower case
docs109q6 <- tm_map(docs109q6, content_transformer(tolower))
# Remove numbers
docs109q6 <- tm_map(docs109q6, removeNumbers)
# Remove english common stopwords
docs109q6 <- tm_map(docs109q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q6 <- tm_map(docs109q6, removeWords)
# Remove punctuations
docs109q6 <- tm_map(docs109q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q6 <- tm_map(docs109q6, stripWhitespace)
# Text stemming
# docs109q6 <- tm_map(docs109q6, stemDocument)

dtm109q6 <- TermDocumentMatrix(docs109q6)
m109q6 <- as.matrix(dtm109q6)
v109q6 <- sort(rowSums(m109q6),decreasing=TRUE)
d109q6 <- data.frame(word = names(v109q6),freq=v109q6)

head(d109q6, 500)

# View answers :)
wordcloud109q6 <- wordcloud(words = d109q6$word, freq = d109q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d109q6tibble <- list(word = d109q6$word[1:10], freq = d109q6$freq[1:10])
d109q6tibble <- as_tibble(d109q6tibble)

ggplot(d109q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q6: Where is the first place you want to go?", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q7
text109q7Answer <- paste(x109q7$Answer)
corpus109q7 <- Corpus(VectorSource(text109q7Answer))

# Load the data as a corpus
inspect(corpus109q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q7 <- content_transformer(function (corpus109q7, pattern) gsub(pattern, "", corpus109q7))
docs109q7 <- tm_map(corpus109q7, toSpaceCorpus109q7, "/")
docs109q7 <- tm_map(corpus109q7, toSpaceCorpus109q7, "@")
docs109q7 <- tm_map(corpus109q7, toSpaceCorpus109q7, "\\|")

# Convert the text to lower case
docs109q7 <- tm_map(docs109q7, content_transformer(tolower))
# Remove numbers
docs109q7 <- tm_map(docs109q7, removeNumbers)
# Remove english common stopwords
docs109q7 <- tm_map(docs109q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q7 <- tm_map(docs109q7, removeWords)
# Remove punctuations
docs109q7 <- tm_map(docs109q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q7 <- tm_map(docs109q7, stripWhitespace)
# Text stemming
# docs109q7 <- tm_map(docs109q7, stemDocument)

dtm109q7 <- TermDocumentMatrix(docs109q7)
m109q7 <- as.matrix(dtm109q7)
v109q7 <- sort(rowSums(m109q7),decreasing=TRUE)
d109q7 <- data.frame(word = names(v109q7),freq=v109q7)

head(d109q7, 500)

# View answers :)
wordcloud109q7 <- wordcloud(words = d109q7$word, freq = d109q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d109q7tibble <- list(word = d109q7$word[1:10], freq = d109q7$freq[1:10])
d109q7tibble <- as_tibble(d109q7tibble)

ggplot(d109q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q8
text109q8Answer <- paste(x109q8$Answer)
corpus109q8 <- Corpus(VectorSource(text109q8Answer))

# Load the data as a corpus
inspect(corpus109q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q8 <- content_transformer(function (corpus109q8, pattern) gsub(pattern, "", corpus109q8))
docs109q8 <- tm_map(corpus109q8, toSpaceCorpus109q8, "/")
docs109q8 <- tm_map(corpus109q8, toSpaceCorpus109q8, "@")
docs109q8 <- tm_map(corpus109q8, toSpaceCorpus109q8, "\\|")

# Convert the text to lower case
docs109q8 <- tm_map(docs109q8, content_transformer(tolower))
# Remove numbers
docs109q8 <- tm_map(docs109q8, removeNumbers)
# Remove english common stopwords
docs109q8 <- tm_map(docs109q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs109q8 <- tm_map(docs109q8, removeWords)
# Remove punctuations
docs109q8 <- tm_map(docs109q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q8 <- tm_map(docs109q8, stripWhitespace)
# Text stemming
# docs109q8 <- tm_map(docs109q8, stemDocument)

dtm109q8 <- TermDocumentMatrix(docs109q8)
m109q8 <- as.matrix(dtm109q8)
v109q8 <- sort(rowSums(m109q8),decreasing=TRUE)
d109q8 <- data.frame(word = names(v109q8),freq=v109q8)

head(d109q8, 500)

# View answers :)
wordcloud109q8 <- wordcloud(words = d109q8$word, freq = d109q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d109q8tibble <- list(word = d109q8$word[1:10], freq = d109q8$freq[1:10])
d109q8tibble <- as_tibble(d109q8tibble)

ggplot(d109q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q8: Are your parents good teachers?", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q9
text109q9Answer <- paste(x109q9$Answer)
corpus109q9 <- Corpus(VectorSource(text109q9Answer))

# Load the data as a corpus
inspect(corpus109q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q9 <- content_transformer(function (corpus109q9, pattern) gsub(pattern, "", corpus109q9))
docs109q9 <- tm_map(corpus109q9, toSpaceCorpus109q9, "/")
docs109q9 <- tm_map(corpus109q9, toSpaceCorpus109q9, "@")
docs109q9 <- tm_map(corpus109q9, toSpaceCorpus109q9, "\\|")

# Convert the text to lower case
docs109q9 <- tm_map(docs109q9, content_transformer(tolower))
# Remove numbers
docs109q9 <- tm_map(docs109q9, removeNumbers)
# Remove english common stopwords
docs109q9 <- tm_map(docs109q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q9 <- tm_map(docs109q9, removeWords)
# Remove punctuations
docs109q9 <- tm_map(docs109q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q9 <- tm_map(docs109q9, stripWhitespace)
# Text stemming
# docs109q9 <- tm_map(docs109q9, stemDocument)

dtm109q9 <- TermDocumentMatrix(docs109q9)
m109q9 <- as.matrix(dtm109q9)
v109q9 <- sort(rowSums(m109q9),decreasing=TRUE)
d109q9 <- data.frame(word = names(v109q9),freq=v109q9)

head(d109q9, 500)

# View answers :)
wordcloud109q9 <- wordcloud(words = d109q9$word, freq = d109q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d109q9tibble <- list(word = d109q9$word[1:10], freq = d109q9$freq[1:10])
d109q9tibble <- as_tibble(d109q9tibble)

ggplot(d109q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q9: How did the coronavirus start?", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q10
text109q10Answer <- paste(x109q10$Answer)
corpus109q10 <- Corpus(VectorSource(text109q10Answer))

# Load the data as a corpus
inspect(corpus109q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q10 <- content_transformer(function (corpus109q10, pattern) gsub(pattern, "", corpus109q10))
docs109q10 <- tm_map(corpus109q10, toSpaceCorpus109q10, "/")
docs109q10 <- tm_map(corpus109q10, toSpaceCorpus109q10, "@")
docs109q10 <- tm_map(corpus109q10, toSpaceCorpus109q10, "\\|")

# Convert the text to lower case
docs109q10 <- tm_map(docs109q10, content_transformer(tolower))
# Remove numbers
docs109q10 <- tm_map(docs109q10, removeNumbers)
# Remove english common stopwords
docs109q10 <- tm_map(docs109q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q10 <- tm_map(docs109q10, removeWords)
# Remove punctuations
docs109q10 <- tm_map(docs109q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q10 <- tm_map(docs109q10, stripWhitespace)
# Text stemming
# docs109q10 <- tm_map(docs109q10, stemDocument)

dtm109q10 <- TermDocumentMatrix(docs109q10)
m109q10 <- as.matrix(dtm109q10)
v109q10 <- sort(rowSums(m109q10),decreasing=TRUE)
d109q10 <- data.frame(word = names(v109q10),freq=v109q10)

head(d109q10, 500)

# View answers :)
wordcloud109q10 <- wordcloud(words = d109q10$word, freq = d109q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d109q10tibble <- list(word = d109q10$word[1:10], freq = d109q10$freq[1:10])
d109q10tibble <- as_tibble(d109q10tibble)

ggplot(d109q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus109q11
text109q11Answer <- paste(x109q11$Answer)
corpus109q11 <- Corpus(VectorSource(text109q11Answer))

# Load the data as a corpus
inspect(corpus109q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus109q11 <- content_transformer(function (corpus109q11, pattern) gsub(pattern, "", corpus109q11))
docs109q11 <- tm_map(corpus109q11, toSpaceCorpus109q11, "/")
docs109q11 <- tm_map(corpus109q11, toSpaceCorpus109q11, "@")
docs109q11 <- tm_map(corpus109q11, toSpaceCorpus109q11, "\\|")

# Convert the text to lower case
docs109q11 <- tm_map(docs109q11, content_transformer(tolower))
# Remove numbers
docs109q11 <- tm_map(docs109q11, removeNumbers)
# Remove english common stopwords
docs109q11 <- tm_map(docs109q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs109q11 <- tm_map(docs109q11, removeWords)
# Remove punctuations
docs109q11 <- tm_map(docs109q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs109q11 <- tm_map(docs109q11, stripWhitespace)
# Text stemming
# docs109q11 <- tm_map(docs109q11, stemDocument)

dtm109q11 <- TermDocumentMatrix(docs109q11)
m109q11 <- as.matrix(dtm109q11)
v109q11 <- sort(rowSums(m109q11),decreasing=TRUE)
d109q11 <- data.frame(word = names(v109q11),freq=v109q11)

head(d109q11, 500)

# View answers :)
wordcloud109q11 <- wordcloud(words = d109q11$word, freq = d109q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d109q11tibble <- list(word = d109q11$word[1:10], freq = d109q11$freq[1:10])
d109q11tibble <- as_tibble(d109q11tibble)

ggplot(d109q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 9 - Q11: Are you enjoying lockdown?", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 10 ###
##################################################################################################
# Corpus110q1
text110q1Answer <- paste(x110q1$Answer)
corpus110q1 <- Corpus(VectorSource(text110q1Answer))

# Load the data as a corpus
inspect(corpus110q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q1 <- content_transformer(function (corpus110q1, pattern) gsub(pattern, "", corpus110q1))
docs110q1 <- tm_map(corpus110q1, toSpaceCorpusq1, "/")
docs110q1 <- tm_map(corpus110q1, toSpaceCorpusq1, "@")
docs110q1 <- tm_map(corpus110q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs110q1 <- tm_map(docs110q1, content_transformer(tolower))
# Remove numbers
docs110q1 <- tm_map(docs110q1, removeNumbers)
# Remove english common stopwords
docs110q1 <- tm_map(docs110q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q1 <- tm_map(docs110q1, removeWords)
# Remove punctuations
docs110q1 <- tm_map(docs110q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q1 <- tm_map(docs110q1, stripWhitespace)
# Text stemming
# docs110q1 <- tm_map(docs110q1, stemDocument)

dtm110q1 <- TermDocumentMatrix(docs110q1)
m110q1 <- as.matrix(dtm110q1)
v110q1 <- sort(rowSums(m110q1),decreasing=TRUE)
d110q1 <- data.frame(word = names(v110q1),freq=v110q1)

head(d110q1, 500)

# View answers :)
wordcloud110q1 <- wordcloud(words = d110q1$word, freq = d110q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 110q1
d110q1tibble <- list(word = d110q1$word[1:10], freq = d110q1$freq[1:10])
d110q1tibble <- as_tibble(d110q1tibble)

ggplot(d110q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q1: What is the coronavirus?", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q2
text110q2Answer <- paste(x110q2$Answer)
corpus110q2 <- Corpus(VectorSource(text110q2Answer))

# Load the data as a corpus
inspect(corpus110q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus110q2, pattern) gsub(pattern, "", corpus110q2))
docs110q2 <- tm_map(corpus110q2, toSpaceCorpusq2, "/")
docs110q2 <- tm_map(corpus110q2, toSpaceCorpusq2, "@")
docs110q2 <- tm_map(corpus110q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs110q2 <- tm_map(docs110q2, content_transformer(tolower))
# Remove numbers
docs110q2 <- tm_map(docs110q2, removeNumbers)
# Remove english common stopwords
docs110q2 <- tm_map(docs110q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q2 <- tm_map(docs110q2, removeWords)
# Remove punctuations
docs110q2 <- tm_map(docs110q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q2 <- tm_map(docs110q2, stripWhitespace)
# Text stemming
# docs110q2 <- tm_map(docs110q2, stemDocument)

dtm110q2 <- TermDocumentMatrix(docs110q2)
m110q2 <- as.matrix(dtm110q2)
v110q2 <- sort(rowSums(m110q2),decreasing=TRUE)
d110q2 <- data.frame(word = names(v110q2),freq=v110q2)

head(d110q2, 500)

# View answers :)
wordcloud110q2 <- wordcloud(words = d110q2$word, freq = d110q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d110q2tibble <- list(word = d110q2$word[1:10], freq = d110q2$freq[1:10])
d110q2tibble <- as_tibble(d110q2tibble)

ggplot(d110q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q2: Who is the president/prime minister?", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q3
text110q3Answer <- paste(x110q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus110q3 <- Corpus(VectorSource(text110q3Answer))

# Load the data as a corpus
inspect(corpus110q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q3 <- content_transformer(function (corpus110q3, pattern) gsub(pattern, "", corpus110q3))
docs110q3 <- tm_map(corpus110q3, toSpaceCorpus110q3, "/")
docs110q3 <- tm_map(corpus110q3, toSpaceCorpus110q3, "@")
docs110q3 <- tm_map(corpus110q3, toSpaceCorpus110q3, "\\|")

# Convert the text to lower case
docs110q3 <- tm_map(docs110q3, content_transformer(tolower))
# Remove numbers
# docs110q3 <- tm_map(docs110q3, removeNumbers)
# Remove english common stopwords
docs110q3 <- tm_map(docs110q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q3 <- tm_map(docs110q3, removeWords)
# Remove punctuations
docs110q3 <- tm_map(docs110q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q3 <- tm_map(docs110q3, stripWhitespace)
# Text stemming
# docs110q3 <- tm_map(docs110q3, stemDocument)

dtm110q3 <- TermDocumentMatrix(docs110q3)
m110q3 <- as.matrix(dtm110q3)
v110q3 <- sort(rowSums(m110q3),decreasing=TRUE)
d110q3 <- data.frame(word = names(v110q3),freq=v110q3)

head(d110q3, 500)

# View answers :)
wordcloud110q3 <- wordcloud(words = d110q3$word, freq = d110q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d110q3tibble <- list(word = d110q3$word[1:10], freq = d110q3$freq[1:10])
d110q3tibble <- as_tibble(d110q3tibble)

ggplot(d110q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q3: How many days have we been in lockdown?", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q4
text110q4Answer <- paste(x110q4$Answer)
corpus110q4 <- Corpus(VectorSource(text110q4Answer))

# Load the data as a corpus
inspect(corpus110q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q4 <- content_transformer(function (corpus110q4, pattern) gsub(pattern, "", corpus110q4))
docs110q4 <- tm_map(corpus110q4, toSpaceCorpus110q4, "/")
docs110q4 <- tm_map(corpus110q4, toSpaceCorpus110q4, "@")
docs110q4 <- tm_map(corpus110q4, toSpaceCorpus110q4, "\\|")

# Convert the text to lower case
docs110q4 <- tm_map(docs110q4, content_transformer(tolower))
# Remove numbers
docs110q4 <- tm_map(docs110q4, removeNumbers)
# Remove english common stopwords
docs110q4 <- tm_map(docs110q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q4 <- tm_map(docs110q4, removeWords)
# Remove punctuations
docs110q4 <- tm_map(docs110q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q4 <- tm_map(docs110q4, stripWhitespace)
# Text stemming
# docs110q4 <- tm_map(docs110q4, stemDocument)

dtm110q4 <- TermDocumentMatrix(docs110q4)
m110q4 <- as.matrix(dtm110q4)
v110q4 <- sort(rowSums(m110q4),decreasing=TRUE)
d110q4 <- data.frame(word = names(v110q4),freq=v110q4)

head(d110q4, 500)

# View answers :)
wordcloud110q4 <- wordcloud(words = d110q4$word, freq = d110q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d110q4tibble <- list(word = d110q4$word[1:10], freq = d110q4$freq[1:10])
d110q4tibble <- as_tibble(d110q4tibble)

ggplot(d110q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q4: Do you want to go back to school?", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q5
text110q5Answer <- paste(x110q5$Answer)
corpus110q5 <- Corpus(VectorSource(text110q5Answer))

# Load the data as a corpus
inspect(corpus110q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q5 <- content_transformer(function (corpus110q5, pattern) gsub(pattern, "", corpus110q5))
docs110q5 <- tm_map(corpus110q5, toSpaceCorpus110q5, "/")
docs110q5 <- tm_map(corpus110q5, toSpaceCorpus110q5, "@")
docs110q5 <- tm_map(corpus110q5, toSpaceCorpus110q5, "\\|")

# Convert the text to lower case
docs110q5 <- tm_map(docs110q5, content_transformer(tolower))
# Remove numbers
docs110q5 <- tm_map(docs110q5, removeNumbers)
# Remove english common stopwords
docs110q5 <- tm_map(docs110q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q5 <- tm_map(docs110q5, removeWords)
# Remove punctuations
docs110q5 <- tm_map(docs110q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q5 <- tm_map(docs110q5, stripWhitespace)
# Text stemming
# docs110q5 <- tm_map(docs110q5, stemDocument)

dtm110q5 <- TermDocumentMatrix(docs110q5)
m110q5 <- as.matrix(dtm110q5)
v110q5 <- sort(rowSums(m110q5),decreasing=TRUE)
d110q5 <- data.frame(word = names(v110q5),freq=v110q5)

head(d110q5, 500)

# View answers :)
wordcloud110q5 <- wordcloud(words = d110q5$word, freq = d110q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d110q5tibble <- list(word = d110q5$word[1:10], freq = d110q5$freq[1:10])
d110q5tibble <- as_tibble(d110q5tibble)

ggplot(d110q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q6
text110q6Answer <- paste(x110q6$Answer)
corpus110q6 <- Corpus(VectorSource(text110q6Answer))

# Load the data as a corpus
inspect(corpus110q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q6 <- content_transformer(function (corpus110q6, pattern) gsub(pattern, "", corpus110q6))
docs110q6 <- tm_map(corpus110q6, toSpaceCorpus110q6, "/")
docs110q6 <- tm_map(corpus110q6, toSpaceCorpus110q6, "@")
docs110q6 <- tm_map(corpus110q6, toSpaceCorpus110q6, "\\|")

# Convert the text to lower case
docs110q6 <- tm_map(docs110q6, content_transformer(tolower))
# Remove numbers
docs110q6 <- tm_map(docs110q6, removeNumbers)
# Remove english common stopwords
docs110q6 <- tm_map(docs110q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q6 <- tm_map(docs110q6, removeWords)
# Remove punctuations
docs110q6 <- tm_map(docs110q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q6 <- tm_map(docs110q6, stripWhitespace)
# Text stemming
# docs110q6 <- tm_map(docs110q6, stemDocument)

dtm110q6 <- TermDocumentMatrix(docs110q6)
m110q6 <- as.matrix(dtm110q6)
v110q6 <- sort(rowSums(m110q6),decreasing=TRUE)
d110q6 <- data.frame(word = names(v110q6),freq=v110q6)

head(d110q6, 500)

# View answers :)
wordcloud110q6 <- wordcloud(words = d110q6$word, freq = d110q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d110q6tibble <- list(word = d110q6$word[1:10], freq = d110q6$freq[1:10])
d110q6tibble <- as_tibble(d110q6tibble)

ggplot(d110q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q6: Where is the first place you want to go?", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q7
text110q7Answer <- paste(x110q7$Answer)
corpus110q7 <- Corpus(VectorSource(text110q7Answer))

# Load the data as a corpus
inspect(corpus110q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q7 <- content_transformer(function (corpus110q7, pattern) gsub(pattern, "", corpus110q7))
docs110q7 <- tm_map(corpus110q7, toSpaceCorpus110q7, "/")
docs110q7 <- tm_map(corpus110q7, toSpaceCorpus110q7, "@")
docs110q7 <- tm_map(corpus110q7, toSpaceCorpus110q7, "\\|")

# Convert the text to lower case
docs110q7 <- tm_map(docs110q7, content_transformer(tolower))
# Remove numbers
docs110q7 <- tm_map(docs110q7, removeNumbers)
# Remove english common stopwords
docs110q7 <- tm_map(docs110q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q7 <- tm_map(docs110q7, removeWords)
# Remove punctuations
docs110q7 <- tm_map(docs110q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q7 <- tm_map(docs110q7, stripWhitespace)
# Text stemming
# docs110q7 <- tm_map(docs110q7, stemDocument)

dtm110q7 <- TermDocumentMatrix(docs110q7)
m110q7 <- as.matrix(dtm110q7)
v110q7 <- sort(rowSums(m110q7),decreasing=TRUE)
d110q7 <- data.frame(word = names(v110q7),freq=v110q7)

head(d110q7, 500)

# View answers :)
wordcloud110q7 <- wordcloud(words = d110q7$word, freq = d110q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d110q7tibble <- list(word = d110q7$word[1:10], freq = d110q7$freq[1:10])
d110q7tibble <- as_tibble(d110q7tibble)

ggplot(d110q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q8
text110q8Answer <- paste(x110q8$Answer)
corpus110q8 <- Corpus(VectorSource(text110q8Answer))

# Load the data as a corpus
inspect(corpus110q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q8 <- content_transformer(function (corpus110q8, pattern) gsub(pattern, "", corpus110q8))
docs110q8 <- tm_map(corpus110q8, toSpaceCorpus110q8, "/")
docs110q8 <- tm_map(corpus110q8, toSpaceCorpus110q8, "@")
docs110q8 <- tm_map(corpus110q8, toSpaceCorpus110q8, "\\|")

# Convert the text to lower case
docs110q8 <- tm_map(docs110q8, content_transformer(tolower))
# Remove numbers
docs110q8 <- tm_map(docs110q8, removeNumbers)
# Remove english common stopwords
docs110q8 <- tm_map(docs110q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs110q8 <- tm_map(docs110q8, removeWords)
# Remove punctuations
docs110q8 <- tm_map(docs110q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q8 <- tm_map(docs110q8, stripWhitespace)
# Text stemming
# docs110q8 <- tm_map(docs110q8, stemDocument)

dtm110q8 <- TermDocumentMatrix(docs110q8)
m110q8 <- as.matrix(dtm110q8)
v110q8 <- sort(rowSums(m110q8),decreasing=TRUE)
d110q8 <- data.frame(word = names(v110q8),freq=v110q8)

head(d110q8, 500)

# View answers :)
wordcloud110q8 <- wordcloud(words = d110q8$word, freq = d110q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d110q8tibble <- list(word = d110q8$word[1:10], freq = d110q8$freq[1:10])
d110q8tibble <- as_tibble(d110q8tibble)

ggplot(d110q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q8: Are your parents good teachers?", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q9
text110q9Answer <- paste(x110q9$Answer)
corpus110q9 <- Corpus(VectorSource(text110q9Answer))

# Load the data as a corpus
inspect(corpus110q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q9 <- content_transformer(function (corpus110q9, pattern) gsub(pattern, "", corpus110q9))
docs110q9 <- tm_map(corpus110q9, toSpaceCorpus110q9, "/")
docs110q9 <- tm_map(corpus110q9, toSpaceCorpus110q9, "@")
docs110q9 <- tm_map(corpus110q9, toSpaceCorpus110q9, "\\|")

# Convert the text to lower case
docs110q9 <- tm_map(docs110q9, content_transformer(tolower))
# Remove numbers
docs110q9 <- tm_map(docs110q9, removeNumbers)
# Remove english common stopwords
docs110q9 <- tm_map(docs110q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q9 <- tm_map(docs110q9, removeWords)
# Remove punctuations
docs110q9 <- tm_map(docs110q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q9 <- tm_map(docs110q9, stripWhitespace)
# Text stemming
# docs110q9 <- tm_map(docs110q9, stemDocument)

dtm110q9 <- TermDocumentMatrix(docs110q9)
m110q9 <- as.matrix(dtm110q9)
v110q9 <- sort(rowSums(m110q9),decreasing=TRUE)
d110q9 <- data.frame(word = names(v110q9),freq=v110q9)

head(d110q9, 500)

# View answers :)
wordcloud110q9 <- wordcloud(words = d110q9$word, freq = d110q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d110q9tibble <- list(word = d110q9$word[1:10], freq = d110q9$freq[1:10])
d110q9tibble <- as_tibble(d110q9tibble)

ggplot(d110q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q9: How did the coronavirus start?", subtitle = "n = 15") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q10
text110q10Answer <- paste(x110q10$Answer)
corpus110q10 <- Corpus(VectorSource(text110q10Answer))

# Load the data as a corpus
inspect(corpus110q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q10 <- content_transformer(function (corpus110q10, pattern) gsub(pattern, "", corpus110q10))
docs110q10 <- tm_map(corpus110q10, toSpaceCorpus110q10, "/")
docs110q10 <- tm_map(corpus110q10, toSpaceCorpus110q10, "@")
docs110q10 <- tm_map(corpus110q10, toSpaceCorpus110q10, "\\|")

# Convert the text to lower case
docs110q10 <- tm_map(docs110q10, content_transformer(tolower))
# Remove numbers
docs110q10 <- tm_map(docs110q10, removeNumbers)
# Remove english common stopwords
docs110q10 <- tm_map(docs110q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q10 <- tm_map(docs110q10, removeWords)
# Remove punctuations
docs110q10 <- tm_map(docs110q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q10 <- tm_map(docs110q10, stripWhitespace)
# Text stemming
# docs110q10 <- tm_map(docs110q10, stemDocument)

dtm110q10 <- TermDocumentMatrix(docs110q10)
m110q10 <- as.matrix(dtm110q10)
v110q10 <- sort(rowSums(m110q10),decreasing=TRUE)
d110q10 <- data.frame(word = names(v110q10),freq=v110q10)

head(d110q10, 500)

# View answers :)
wordcloud110q10 <- wordcloud(words = d110q10$word, freq = d110q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d110q10tibble <- list(word = d110q10$word[1:10], freq = d110q10$freq[1:10])
d110q10tibble <- as_tibble(d110q10tibble)

ggplot(d110q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus110q11
text110q11Answer <- paste(x110q11$Answer)
corpus110q11 <- Corpus(VectorSource(text110q11Answer))

# Load the data as a corpus
inspect(corpus110q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus110q11 <- content_transformer(function (corpus110q11, pattern) gsub(pattern, "", corpus110q11))
docs110q11 <- tm_map(corpus110q11, toSpaceCorpus110q11, "/")
docs110q11 <- tm_map(corpus110q11, toSpaceCorpus110q11, "@")
docs110q11 <- tm_map(corpus110q11, toSpaceCorpus110q11, "\\|")

# Convert the text to lower case
docs110q11 <- tm_map(docs110q11, content_transformer(tolower))
# Remove numbers
docs110q11 <- tm_map(docs110q11, removeNumbers)
# Remove english common stopwords
docs110q11 <- tm_map(docs110q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs110q11 <- tm_map(docs110q11, removeWords)
# Remove punctuations
docs110q11 <- tm_map(docs110q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs110q11 <- tm_map(docs110q11, stripWhitespace)
# Text stemming
# docs110q11 <- tm_map(docs110q11, stemDocument)

dtm110q11 <- TermDocumentMatrix(docs110q11)
m110q11 <- as.matrix(dtm110q11)
v110q11 <- sort(rowSums(m110q11),decreasing=TRUE)
d110q11 <- data.frame(word = names(v110q11),freq=v110q11)

head(d110q11, 500)

# View answers :)
wordcloud110q11 <- wordcloud(words = d110q11$word, freq = d110q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d110q11tibble <- list(word = d110q11$word[1:10], freq = d110q11$freq[1:10])
d110q11tibble <- as_tibble(d110q11tibble)

ggplot(d110q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 10 - Q11: Are you enjoying lockdown?", subtitle = "n = 16") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 11 ###
##################################################################################################
# Corpus111q1
text111q1Answer <- paste(x111q1$Answer)
corpus111q1 <- Corpus(VectorSource(text111q1Answer))

# Load the data as a corpus
inspect(corpus111q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q1 <- content_transformer(function (corpus111q1, pattern) gsub(pattern, "", corpus111q1))
docs111q1 <- tm_map(corpus111q1, toSpaceCorpusq1, "/")
docs111q1 <- tm_map(corpus111q1, toSpaceCorpusq1, "@")
docs111q1 <- tm_map(corpus111q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs111q1 <- tm_map(docs111q1, content_transformer(tolower))
# Remove numbers
docs111q1 <- tm_map(docs111q1, removeNumbers)
# Remove english common stopwords
docs111q1 <- tm_map(docs111q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q1 <- tm_map(docs111q1, removeWords)
# Remove punctuations
docs111q1 <- tm_map(docs111q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q1 <- tm_map(docs111q1, stripWhitespace)
# Text stemming
# docs111q1 <- tm_map(docs111q1, stemDocument)

dtm111q1 <- TermDocumentMatrix(docs111q1)
m111q1 <- as.matrix(dtm111q1)
v111q1 <- sort(rowSums(m111q1),decreasing=TRUE)
d111q1 <- data.frame(word = names(v111q1),freq=v111q1)

head(d111q1, 500)

# View answers :)
wordcloud111q1 <- wordcloud(words = d111q1$word, freq = d111q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 111q1
d111q1tibble <- list(word = d111q1$word[1:10], freq = d111q1$freq[1:10])
d111q1tibble <- as_tibble(d111q1tibble)

ggplot(d111q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q1: What is the coronavirus?", subtitle = "n = 11") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q2
text111q2Answer <- paste(x111q2$Answer)
corpus111q2 <- Corpus(VectorSource(text111q2Answer))

# Load the data as a corpus
inspect(corpus111q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus111q2, pattern) gsub(pattern, "", corpus111q2))
docs111q2 <- tm_map(corpus111q2, toSpaceCorpusq2, "/")
docs111q2 <- tm_map(corpus111q2, toSpaceCorpusq2, "@")
docs111q2 <- tm_map(corpus111q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs111q2 <- tm_map(docs111q2, content_transformer(tolower))
# Remove numbers
docs111q2 <- tm_map(docs111q2, removeNumbers)
# Remove english common stopwords
docs111q2 <- tm_map(docs111q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q2 <- tm_map(docs111q2, removeWords)
# Remove punctuations
docs111q2 <- tm_map(docs111q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q2 <- tm_map(docs111q2, stripWhitespace)
# Text stemming
# docs111q2 <- tm_map(docs111q2, stemDocument)

dtm111q2 <- TermDocumentMatrix(docs111q2)
m111q2 <- as.matrix(dtm111q2)
v111q2 <- sort(rowSums(m111q2),decreasing=TRUE)
d111q2 <- data.frame(word = names(v111q2),freq=v111q2)

head(d111q2, 500)

# View answers :)
wordcloud111q2 <- wordcloud(words = d111q2$word, freq = d111q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d111q2tibble <- list(word = d111q2$word[1:10], freq = d111q2$freq[1:10])
d111q2tibble <- as_tibble(d111q2tibble)

ggplot(d111q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q2: Who is the president/prime minister?", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q3
text111q3Answer <- paste(x111q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus111q3 <- Corpus(VectorSource(text111q3Answer))

# Load the data as a corpus
inspect(corpus111q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q3 <- content_transformer(function (corpus111q3, pattern) gsub(pattern, "", corpus111q3))
docs111q3 <- tm_map(corpus111q3, toSpaceCorpus111q3, "/")
docs111q3 <- tm_map(corpus111q3, toSpaceCorpus111q3, "@")
docs111q3 <- tm_map(corpus111q3, toSpaceCorpus111q3, "\\|")

# Convert the text to lower case
docs111q3 <- tm_map(docs111q3, content_transformer(tolower))
# Remove numbers
# docs111q3 <- tm_map(docs111q3, removeNumbers)
# Remove english common stopwords
docs111q3 <- tm_map(docs111q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q3 <- tm_map(docs111q3, removeWords)
# Remove punctuations
docs111q3 <- tm_map(docs111q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q3 <- tm_map(docs111q3, stripWhitespace)
# Text stemming
# docs111q3 <- tm_map(docs111q3, stemDocument)

dtm111q3 <- TermDocumentMatrix(docs111q3)
m111q3 <- as.matrix(dtm111q3)
v111q3 <- sort(rowSums(m111q3),decreasing=TRUE)
d111q3 <- data.frame(word = names(v111q3),freq=v111q3)

head(d111q3, 500)

# View answers :)
wordcloud111q3 <- wordcloud(words = d111q3$word, freq = d111q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d111q3tibble <- list(word = d111q3$word[1:10], freq = d111q3$freq[1:10])
d111q3tibble <- as_tibble(d111q3tibble)

ggplot(d111q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q3: How many days have we been in lockdown?", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q4
text111q4Answer <- paste(x111q4$Answer)
corpus111q4 <- Corpus(VectorSource(text111q4Answer))

# Load the data as a corpus
inspect(corpus111q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q4 <- content_transformer(function (corpus111q4, pattern) gsub(pattern, "", corpus111q4))
docs111q4 <- tm_map(corpus111q4, toSpaceCorpus111q4, "/")
docs111q4 <- tm_map(corpus111q4, toSpaceCorpus111q4, "@")
docs111q4 <- tm_map(corpus111q4, toSpaceCorpus111q4, "\\|")

# Convert the text to lower case
docs111q4 <- tm_map(docs111q4, content_transformer(tolower))
# Remove numbers
docs111q4 <- tm_map(docs111q4, removeNumbers)
# Remove english common stopwords
docs111q4 <- tm_map(docs111q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q4 <- tm_map(docs111q4, removeWords)
# Remove punctuations
docs111q4 <- tm_map(docs111q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q4 <- tm_map(docs111q4, stripWhitespace)
# Text stemming
# docs111q4 <- tm_map(docs111q4, stemDocument)

dtm111q4 <- TermDocumentMatrix(docs111q4)
m111q4 <- as.matrix(dtm111q4)
v111q4 <- sort(rowSums(m111q4),decreasing=TRUE)
d111q4 <- data.frame(word = names(v111q4),freq=v111q4)

head(d111q4, 500)

# View answers :)
wordcloud111q4 <- wordcloud(words = d111q4$word, freq = d111q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d111q4tibble <- list(word = d111q4$word[1:10], freq = d111q4$freq[1:10])
d111q4tibble <- as_tibble(d111q4tibble)

ggplot(d111q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q4: Do you want to go back to school?", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q5
text111q5Answer <- paste(x111q5$Answer)
corpus111q5 <- Corpus(VectorSource(text111q5Answer))

# Load the data as a corpus
inspect(corpus111q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q5 <- content_transformer(function (corpus111q5, pattern) gsub(pattern, "", corpus111q5))
docs111q5 <- tm_map(corpus111q5, toSpaceCorpus111q5, "/")
docs111q5 <- tm_map(corpus111q5, toSpaceCorpus111q5, "@")
docs111q5 <- tm_map(corpus111q5, toSpaceCorpus111q5, "\\|")

# Convert the text to lower case
docs111q5 <- tm_map(docs111q5, content_transformer(tolower))
# Remove numbers
docs111q5 <- tm_map(docs111q5, removeNumbers)
# Remove english common stopwords
docs111q5 <- tm_map(docs111q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q5 <- tm_map(docs111q5, removeWords)
# Remove punctuations
docs111q5 <- tm_map(docs111q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q5 <- tm_map(docs111q5, stripWhitespace)
# Text stemming
# docs111q5 <- tm_map(docs111q5, stemDocument)

dtm111q5 <- TermDocumentMatrix(docs111q5)
m111q5 <- as.matrix(dtm111q5)
v111q5 <- sort(rowSums(m111q5),decreasing=TRUE)
d111q5 <- data.frame(word = names(v111q5),freq=v111q5)

head(d111q5, 500)

# View answers :)
wordcloud111q5 <- wordcloud(words = d111q5$word, freq = d111q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d111q5tibble <- list(word = d111q5$word[1:10], freq = d111q5$freq[1:10])
d111q5tibble <- as_tibble(d111q5tibble)

ggplot(d111q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 11") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q6
text111q6Answer <- paste(x111q6$Answer)
corpus111q6 <- Corpus(VectorSource(text111q6Answer))

# Load the data as a corpus
inspect(corpus111q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q6 <- content_transformer(function (corpus111q6, pattern) gsub(pattern, "", corpus111q6))
docs111q6 <- tm_map(corpus111q6, toSpaceCorpus111q6, "/")
docs111q6 <- tm_map(corpus111q6, toSpaceCorpus111q6, "@")
docs111q6 <- tm_map(corpus111q6, toSpaceCorpus111q6, "\\|")

# Convert the text to lower case
docs111q6 <- tm_map(docs111q6, content_transformer(tolower))
# Remove numbers
docs111q6 <- tm_map(docs111q6, removeNumbers)
# Remove english common stopwords
docs111q6 <- tm_map(docs111q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q6 <- tm_map(docs111q6, removeWords)
# Remove punctuations
docs111q6 <- tm_map(docs111q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q6 <- tm_map(docs111q6, stripWhitespace)
# Text stemming
# docs111q6 <- tm_map(docs111q6, stemDocument)

dtm111q6 <- TermDocumentMatrix(docs111q6)
m111q6 <- as.matrix(dtm111q6)
v111q6 <- sort(rowSums(m111q6),decreasing=TRUE)
d111q6 <- data.frame(word = names(v111q6),freq=v111q6)

head(d111q6, 500)

# View answers :)
wordcloud111q6 <- wordcloud(words = d111q6$word, freq = d111q6$freq, min.freq = 1, scale=c(2,.25),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d111q6tibble <- list(word = d111q6$word[1:10], freq = d111q6$freq[1:10])
d111q6tibble <- as_tibble(d111q6tibble)

ggplot(d111q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q6: Where is the first place you want to go?", subtitle = "n = 11") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q7
text111q7Answer <- paste(x111q7$Answer)
corpus111q7 <- Corpus(VectorSource(text111q7Answer))

# Load the data as a corpus
inspect(corpus111q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q7 <- content_transformer(function (corpus111q7, pattern) gsub(pattern, "", corpus111q7))
docs111q7 <- tm_map(corpus111q7, toSpaceCorpus111q7, "/")
docs111q7 <- tm_map(corpus111q7, toSpaceCorpus111q7, "@")
docs111q7 <- tm_map(corpus111q7, toSpaceCorpus111q7, "\\|")

# Convert the text to lower case
docs111q7 <- tm_map(docs111q7, content_transformer(tolower))
# Remove numbers
docs111q7 <- tm_map(docs111q7, removeNumbers)
# Remove english common stopwords
docs111q7 <- tm_map(docs111q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q7 <- tm_map(docs111q7, removeWords)
# Remove punctuations
docs111q7 <- tm_map(docs111q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q7 <- tm_map(docs111q7, stripWhitespace)
# Text stemming
# docs111q7 <- tm_map(docs111q7, stemDocument)

dtm111q7 <- TermDocumentMatrix(docs111q7)
m111q7 <- as.matrix(dtm111q7)
v111q7 <- sort(rowSums(m111q7),decreasing=TRUE)
d111q7 <- data.frame(word = names(v111q7),freq=v111q7)

head(d111q7, 500)

# View answers :)
wordcloud111q7 <- wordcloud(words = d111q7$word, freq = d111q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d111q7tibble <- list(word = d111q7$word[1:10], freq = d111q7$freq[1:10])
d111q7tibble <- as_tibble(d111q7tibble)

ggplot(d111q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q8
text111q8Answer <- paste(x111q8$Answer)
corpus111q8 <- Corpus(VectorSource(text111q8Answer))

# Load the data as a corpus
inspect(corpus111q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q8 <- content_transformer(function (corpus111q8, pattern) gsub(pattern, "", corpus111q8))
docs111q8 <- tm_map(corpus111q8, toSpaceCorpus111q8, "/")
docs111q8 <- tm_map(corpus111q8, toSpaceCorpus111q8, "@")
docs111q8 <- tm_map(corpus111q8, toSpaceCorpus111q8, "\\|")

# Convert the text to lower case
docs111q8 <- tm_map(docs111q8, content_transformer(tolower))
# Remove numbers
docs111q8 <- tm_map(docs111q8, removeNumbers)
# Remove english common stopwords
docs111q8 <- tm_map(docs111q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs111q8 <- tm_map(docs111q8, removeWords)
# Remove punctuations
docs111q8 <- tm_map(docs111q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q8 <- tm_map(docs111q8, stripWhitespace)
# Text stemming
# docs111q8 <- tm_map(docs111q8, stemDocument)

dtm111q8 <- TermDocumentMatrix(docs111q8)
m111q8 <- as.matrix(dtm111q8)
v111q8 <- sort(rowSums(m111q8),decreasing=TRUE)
d111q8 <- data.frame(word = names(v111q8),freq=v111q8)

head(d111q8, 500)

# View answers :)
wordcloud111q8 <- wordcloud(words = d111q8$word, freq = d111q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d111q8tibble <- list(word = d111q8$word[1:10], freq = d111q8$freq[1:10])
d111q8tibble <- as_tibble(d111q8tibble)

ggplot(d111q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q8: Are your parents good teachers?", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q9
text111q9Answer <- paste(x111q9$Answer)
corpus111q9 <- Corpus(VectorSource(text111q9Answer))

# Load the data as a corpus
inspect(corpus111q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q9 <- content_transformer(function (corpus111q9, pattern) gsub(pattern, "", corpus111q9))
docs111q9 <- tm_map(corpus111q9, toSpaceCorpus111q9, "/")
docs111q9 <- tm_map(corpus111q9, toSpaceCorpus111q9, "@")
docs111q9 <- tm_map(corpus111q9, toSpaceCorpus111q9, "\\|")

# Convert the text to lower case
docs111q9 <- tm_map(docs111q9, content_transformer(tolower))
# Remove numbers
docs111q9 <- tm_map(docs111q9, removeNumbers)
# Remove english common stopwords
docs111q9 <- tm_map(docs111q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q9 <- tm_map(docs111q9, removeWords)
# Remove punctuations
docs111q9 <- tm_map(docs111q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q9 <- tm_map(docs111q9, stripWhitespace)
# Text stemming
# docs111q9 <- tm_map(docs111q9, stemDocument)

dtm111q9 <- TermDocumentMatrix(docs111q9)
m111q9 <- as.matrix(dtm111q9)
v111q9 <- sort(rowSums(m111q9),decreasing=TRUE)
d111q9 <- data.frame(word = names(v111q9),freq=v111q9)

head(d111q9, 500)

# View answers :)
wordcloud111q9 <- wordcloud(words = d111q9$word, freq = d111q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d111q9tibble <- list(word = d111q9$word[1:10], freq = d111q9$freq[1:10])
d111q9tibble <- as_tibble(d111q9tibble)

ggplot(d111q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q9: How did the coronavirus start?", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q10
text111q10Answer <- paste(x111q10$Answer)
corpus111q10 <- Corpus(VectorSource(text111q10Answer))

# Load the data as a corpus
inspect(corpus111q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q10 <- content_transformer(function (corpus111q10, pattern) gsub(pattern, "", corpus111q10))
docs111q10 <- tm_map(corpus111q10, toSpaceCorpus111q10, "/")
docs111q10 <- tm_map(corpus111q10, toSpaceCorpus111q10, "@")
docs111q10 <- tm_map(corpus111q10, toSpaceCorpus111q10, "\\|")

# Convert the text to lower case
docs111q10 <- tm_map(docs111q10, content_transformer(tolower))
# Remove numbers
docs111q10 <- tm_map(docs111q10, removeNumbers)
# Remove english common stopwords
docs111q10 <- tm_map(docs111q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q10 <- tm_map(docs111q10, removeWords)
# Remove punctuations
docs111q10 <- tm_map(docs111q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q10 <- tm_map(docs111q10, stripWhitespace)
# Text stemming
# docs111q10 <- tm_map(docs111q10, stemDocument)

dtm111q10 <- TermDocumentMatrix(docs111q10)
m111q10 <- as.matrix(dtm111q10)
v111q10 <- sort(rowSums(m111q10),decreasing=TRUE)
d111q10 <- data.frame(word = names(v111q10),freq=v111q10)

head(d111q10, 500)

# View answers :)
wordcloud111q10 <- wordcloud(words = d111q10$word, freq = d111q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d111q10tibble <- list(word = d111q10$word[1:10], freq = d111q10$freq[1:10])
d111q10tibble <- as_tibble(d111q10tibble)

ggplot(d111q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 13") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus111q11
text111q11Answer <- paste(x111q11$Answer)
corpus111q11 <- Corpus(VectorSource(text111q11Answer))

# Load the data as a corpus
inspect(corpus111q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus111q11 <- content_transformer(function (corpus111q11, pattern) gsub(pattern, "", corpus111q11))
docs111q11 <- tm_map(corpus111q11, toSpaceCorpus111q11, "/")
docs111q11 <- tm_map(corpus111q11, toSpaceCorpus111q11, "@")
docs111q11 <- tm_map(corpus111q11, toSpaceCorpus111q11, "\\|")

# Convert the text to lower case
docs111q11 <- tm_map(docs111q11, content_transformer(tolower))
# Remove numbers
docs111q11 <- tm_map(docs111q11, removeNumbers)
# Remove english common stopwords
docs111q11 <- tm_map(docs111q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs111q11 <- tm_map(docs111q11, removeWords)
# Remove punctuations
docs111q11 <- tm_map(docs111q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs111q11 <- tm_map(docs111q11, stripWhitespace)
# Text stemming
# docs111q11 <- tm_map(docs111q11, stemDocument)

dtm111q11 <- TermDocumentMatrix(docs111q11)
m111q11 <- as.matrix(dtm111q11)
v111q11 <- sort(rowSums(m111q11),decreasing=TRUE)
d111q11 <- data.frame(word = names(v111q11),freq=v111q11)

head(d111q11, 500)

# View answers :)
wordcloud111q11 <- wordcloud(words = d111q11$word, freq = d111q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d111q11tibble <- list(word = d111q11$word[1:10], freq = d111q11$freq[1:10])
d111q11tibble <- as_tibble(d111q11tibble)

ggplot(d111q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 11 - Q11: Are you enjoying lockdown?", subtitle = "n = 10") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 12 ###
##################################################################################################
# Corpus112q1
text112q1Answer <- paste(x112q1$Answer)
corpus112q1 <- Corpus(VectorSource(text112q1Answer))

# Load the data as a corpus
inspect(corpus112q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q1 <- content_transformer(function (corpus112q1, pattern) gsub(pattern, "", corpus112q1))
docs112q1 <- tm_map(corpus112q1, toSpaceCorpusq1, "/")
docs112q1 <- tm_map(corpus112q1, toSpaceCorpusq1, "@")
docs112q1 <- tm_map(corpus112q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs112q1 <- tm_map(docs112q1, content_transformer(tolower))
# Remove numbers
docs112q1 <- tm_map(docs112q1, removeNumbers)
# Remove english common stopwords
docs112q1 <- tm_map(docs112q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q1 <- tm_map(docs112q1, removeWords)
# Remove punctuations
docs112q1 <- tm_map(docs112q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q1 <- tm_map(docs112q1, stripWhitespace)
# Text stemming
# docs112q1 <- tm_map(docs112q1, stemDocument)

dtm112q1 <- TermDocumentMatrix(docs112q1)
m112q1 <- as.matrix(dtm112q1)
v112q1 <- sort(rowSums(m112q1),decreasing=TRUE)
d112q1 <- data.frame(word = names(v112q1),freq=v112q1)

head(d112q1, 500)

# View answers :)
wordcloud112q1 <- wordcloud(words = d112q1$word, freq = d112q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 112q1
d112q1tibble <- list(word = d112q1$word[1:10], freq = d112q1$freq[1:10])
d112q1tibble <- as_tibble(d112q1tibble)

ggplot(d112q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q1: What is the coronavirus?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q2
text112q2Answer <- paste(x112q2$Answer)
corpus112q2 <- Corpus(VectorSource(text112q2Answer))

# Load the data as a corpus
inspect(corpus112q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus112q2, pattern) gsub(pattern, "", corpus112q2))
docs112q2 <- tm_map(corpus112q2, toSpaceCorpusq2, "/")
docs112q2 <- tm_map(corpus112q2, toSpaceCorpusq2, "@")
docs112q2 <- tm_map(corpus112q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs112q2 <- tm_map(docs112q2, content_transformer(tolower))
# Remove numbers
docs112q2 <- tm_map(docs112q2, removeNumbers)
# Remove english common stopwords
docs112q2 <- tm_map(docs112q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q2 <- tm_map(docs112q2, removeWords)
# Remove punctuations
docs112q2 <- tm_map(docs112q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q2 <- tm_map(docs112q2, stripWhitespace)
# Text stemming
# docs112q2 <- tm_map(docs112q2, stemDocument)

dtm112q2 <- TermDocumentMatrix(docs112q2)
m112q2 <- as.matrix(dtm112q2)
v112q2 <- sort(rowSums(m112q2),decreasing=TRUE)
d112q2 <- data.frame(word = names(v112q2),freq=v112q2)

head(d112q2, 500)

# View answers :)
wordcloud112q2 <- wordcloud(words = d112q2$word, freq = d112q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d112q2tibble <- list(word = d112q2$word[1:10], freq = d112q2$freq[1:10])
d112q2tibble <- as_tibble(d112q2tibble)

ggplot(d112q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q2: Who is the president/prime minister?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q3
text112q3Answer <- paste(x112q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus112q3 <- Corpus(VectorSource(text112q3Answer))

# Load the data as a corpus
inspect(corpus112q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q3 <- content_transformer(function (corpus112q3, pattern) gsub(pattern, "", corpus112q3))
docs112q3 <- tm_map(corpus112q3, toSpaceCorpus112q3, "/")
docs112q3 <- tm_map(corpus112q3, toSpaceCorpus112q3, "@")
docs112q3 <- tm_map(corpus112q3, toSpaceCorpus112q3, "\\|")

# Convert the text to lower case
docs112q3 <- tm_map(docs112q3, content_transformer(tolower))
# Remove numbers
# docs112q3 <- tm_map(docs112q3, removeNumbers)
# Remove english common stopwords
docs112q3 <- tm_map(docs112q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q3 <- tm_map(docs112q3, removeWords)
# Remove punctuations
docs112q3 <- tm_map(docs112q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q3 <- tm_map(docs112q3, stripWhitespace)
# Text stemming
# docs112q3 <- tm_map(docs112q3, stemDocument)

dtm112q3 <- TermDocumentMatrix(docs112q3)
m112q3 <- as.matrix(dtm112q3)
v112q3 <- sort(rowSums(m112q3),decreasing=TRUE)
d112q3 <- data.frame(word = names(v112q3),freq=v112q3)

head(d112q3, 500)

# View answers :)
wordcloud112q3 <- wordcloud(words = d112q3$word, freq = d112q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d112q3tibble <- list(word = d112q3$word[1:10], freq = d112q3$freq[1:10])
d112q3tibble <- as_tibble(d112q3tibble)

ggplot(d112q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q3: How many days have we been in lockdown?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q4
text112q4Answer <- paste(x112q4$Answer)
corpus112q4 <- Corpus(VectorSource(text112q4Answer))

# Load the data as a corpus
inspect(corpus112q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q4 <- content_transformer(function (corpus112q4, pattern) gsub(pattern, "", corpus112q4))
docs112q4 <- tm_map(corpus112q4, toSpaceCorpus112q4, "/")
docs112q4 <- tm_map(corpus112q4, toSpaceCorpus112q4, "@")
docs112q4 <- tm_map(corpus112q4, toSpaceCorpus112q4, "\\|")

# Convert the text to lower case
docs112q4 <- tm_map(docs112q4, content_transformer(tolower))
# Remove numbers
docs112q4 <- tm_map(docs112q4, removeNumbers)
# Remove english common stopwords
docs112q4 <- tm_map(docs112q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q4 <- tm_map(docs112q4, removeWords)
# Remove punctuations
docs112q4 <- tm_map(docs112q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q4 <- tm_map(docs112q4, stripWhitespace)
# Text stemming
# docs112q4 <- tm_map(docs112q4, stemDocument)

dtm112q4 <- TermDocumentMatrix(docs112q4)
m112q4 <- as.matrix(dtm112q4)
v112q4 <- sort(rowSums(m112q4),decreasing=TRUE)
d112q4 <- data.frame(word = names(v112q4),freq=v112q4)

head(d112q4, 500)

# View answers :)
wordcloud112q4 <- wordcloud(words = d112q4$word, freq = d112q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d112q4tibble <- list(word = d112q4$word[1:10], freq = d112q4$freq[1:10])
d112q4tibble <- as_tibble(d112q4tibble)

ggplot(d112q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q4: Do you want to go back to school?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q5
text112q5Answer <- paste(x112q5$Answer)
corpus112q5 <- Corpus(VectorSource(text112q5Answer))

# Load the data as a corpus
inspect(corpus112q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q5 <- content_transformer(function (corpus112q5, pattern) gsub(pattern, "", corpus112q5))
docs112q5 <- tm_map(corpus112q5, toSpaceCorpus112q5, "/")
docs112q5 <- tm_map(corpus112q5, toSpaceCorpus112q5, "@")
docs112q5 <- tm_map(corpus112q5, toSpaceCorpus112q5, "\\|")

# Convert the text to lower case
docs112q5 <- tm_map(docs112q5, content_transformer(tolower))
# Remove numbers
docs112q5 <- tm_map(docs112q5, removeNumbers)
# Remove english common stopwords
docs112q5 <- tm_map(docs112q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q5 <- tm_map(docs112q5, removeWords)
# Remove punctuations
docs112q5 <- tm_map(docs112q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q5 <- tm_map(docs112q5, stripWhitespace)
# Text stemming
# docs112q5 <- tm_map(docs112q5, stemDocument)

dtm112q5 <- TermDocumentMatrix(docs112q5)
m112q5 <- as.matrix(dtm112q5)
v112q5 <- sort(rowSums(m112q5),decreasing=TRUE)
d112q5 <- data.frame(word = names(v112q5),freq=v112q5)

head(d112q5, 500)

# View answers :)
wordcloud112q5 <- wordcloud(words = d112q5$word, freq = d112q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d112q5tibble <- list(word = d112q5$word[1:10], freq = d112q5$freq[1:10])
d112q5tibble <- as_tibble(d112q5tibble)

ggplot(d112q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q6
text112q6Answer <- paste(x112q6$Answer)
corpus112q6 <- Corpus(VectorSource(text112q6Answer))

# Load the data as a corpus
inspect(corpus112q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q6 <- content_transformer(function (corpus112q6, pattern) gsub(pattern, "", corpus112q6))
docs112q6 <- tm_map(corpus112q6, toSpaceCorpus112q6, "/")
docs112q6 <- tm_map(corpus112q6, toSpaceCorpus112q6, "@")
docs112q6 <- tm_map(corpus112q6, toSpaceCorpus112q6, "\\|")

# Convert the text to lower case
docs112q6 <- tm_map(docs112q6, content_transformer(tolower))
# Remove numbers
docs112q6 <- tm_map(docs112q6, removeNumbers)
# Remove english common stopwords
docs112q6 <- tm_map(docs112q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q6 <- tm_map(docs112q6, removeWords)
# Remove punctuations
docs112q6 <- tm_map(docs112q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q6 <- tm_map(docs112q6, stripWhitespace)
# Text stemming
# docs112q6 <- tm_map(docs112q6, stemDocument)

dtm112q6 <- TermDocumentMatrix(docs112q6)
m112q6 <- as.matrix(dtm112q6)
v112q6 <- sort(rowSums(m112q6),decreasing=TRUE)
d112q6 <- data.frame(word = names(v112q6),freq=v112q6)

head(d112q6, 500)

# View answers :)
wordcloud112q6 <- wordcloud(words = d112q6$word, freq = d112q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d112q6tibble <- list(word = d112q6$word[1:10], freq = d112q6$freq[1:10])
d112q6tibble <- as_tibble(d112q6tibble)

ggplot(d112q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q6: Where is the first place you want to go?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q7
text112q7Answer <- paste(x112q7$Answer)
corpus112q7 <- Corpus(VectorSource(text112q7Answer))

# Load the data as a corpus
inspect(corpus112q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q7 <- content_transformer(function (corpus112q7, pattern) gsub(pattern, "", corpus112q7))
docs112q7 <- tm_map(corpus112q7, toSpaceCorpus112q7, "/")
docs112q7 <- tm_map(corpus112q7, toSpaceCorpus112q7, "@")
docs112q7 <- tm_map(corpus112q7, toSpaceCorpus112q7, "\\|")

# Convert the text to lower case
docs112q7 <- tm_map(docs112q7, content_transformer(tolower))
# Remove numbers
docs112q7 <- tm_map(docs112q7, removeNumbers)
# Remove english common stopwords
docs112q7 <- tm_map(docs112q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q7 <- tm_map(docs112q7, removeWords)
# Remove punctuations
docs112q7 <- tm_map(docs112q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q7 <- tm_map(docs112q7, stripWhitespace)
# Text stemming
# docs112q7 <- tm_map(docs112q7, stemDocument)

dtm112q7 <- TermDocumentMatrix(docs112q7)
m112q7 <- as.matrix(dtm112q7)
v112q7 <- sort(rowSums(m112q7),decreasing=TRUE)
d112q7 <- data.frame(word = names(v112q7),freq=v112q7)

head(d112q7, 500)

# View answers :)
wordcloud112q7 <- wordcloud(words = d112q7$word, freq = d112q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d112q7tibble <- list(word = d112q7$word[1:10], freq = d112q7$freq[1:10])
d112q7tibble <- as_tibble(d112q7tibble)

ggplot(d112q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q8
text112q8Answer <- paste(x112q8$Answer)
corpus112q8 <- Corpus(VectorSource(text112q8Answer))

# Load the data as a corpus
inspect(corpus112q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q8 <- content_transformer(function (corpus112q8, pattern) gsub(pattern, "", corpus112q8))
docs112q8 <- tm_map(corpus112q8, toSpaceCorpus112q8, "/")
docs112q8 <- tm_map(corpus112q8, toSpaceCorpus112q8, "@")
docs112q8 <- tm_map(corpus112q8, toSpaceCorpus112q8, "\\|")

# Convert the text to lower case
docs112q8 <- tm_map(docs112q8, content_transformer(tolower))
# Remove numbers
docs112q8 <- tm_map(docs112q8, removeNumbers)
# Remove english common stopwords
docs112q8 <- tm_map(docs112q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs112q8 <- tm_map(docs112q8, removeWords)
# Remove punctuations
docs112q8 <- tm_map(docs112q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q8 <- tm_map(docs112q8, stripWhitespace)
# Text stemming
# docs112q8 <- tm_map(docs112q8, stemDocument)

dtm112q8 <- TermDocumentMatrix(docs112q8)
m112q8 <- as.matrix(dtm112q8)
v112q8 <- sort(rowSums(m112q8),decreasing=TRUE)
d112q8 <- data.frame(word = names(v112q8),freq=v112q8)

head(d112q8, 500)

# View answers :)
wordcloud112q8 <- wordcloud(words = d112q8$word, freq = d112q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d112q8tibble <- list(word = d112q8$word[1:10], freq = d112q8$freq[1:10])
d112q8tibble <- as_tibble(d112q8tibble)

ggplot(d112q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q8: Are your parents good teachers?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q9
text112q9Answer <- paste(x112q9$Answer)
corpus112q9 <- Corpus(VectorSource(text112q9Answer))

# Load the data as a corpus
inspect(corpus112q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q9 <- content_transformer(function (corpus112q9, pattern) gsub(pattern, "", corpus112q9))
docs112q9 <- tm_map(corpus112q9, toSpaceCorpus112q9, "/")
docs112q9 <- tm_map(corpus112q9, toSpaceCorpus112q9, "@")
docs112q9 <- tm_map(corpus112q9, toSpaceCorpus112q9, "\\|")

# Convert the text to lower case
docs112q9 <- tm_map(docs112q9, content_transformer(tolower))
# Remove numbers
docs112q9 <- tm_map(docs112q9, removeNumbers)
# Remove english common stopwords
docs112q9 <- tm_map(docs112q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q9 <- tm_map(docs112q9, removeWords)
# Remove punctuations
docs112q9 <- tm_map(docs112q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q9 <- tm_map(docs112q9, stripWhitespace)
# Text stemming
# docs112q9 <- tm_map(docs112q9, stemDocument)

dtm112q9 <- TermDocumentMatrix(docs112q9)
m112q9 <- as.matrix(dtm112q9)
v112q9 <- sort(rowSums(m112q9),decreasing=TRUE)
d112q9 <- data.frame(word = names(v112q9),freq=v112q9)

head(d112q9, 500)

# View answers :)
wordcloud112q9 <- wordcloud(words = d112q9$word, freq = d112q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d112q9tibble <- list(word = d112q9$word[1:10], freq = d112q9$freq[1:10])
d112q9tibble <- as_tibble(d112q9tibble)

ggplot(d112q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q9: How did the coronavirus start?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q10
text112q10Answer <- paste(x112q10$Answer)
corpus112q10 <- Corpus(VectorSource(text112q10Answer))

# Load the data as a corpus
inspect(corpus112q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q10 <- content_transformer(function (corpus112q10, pattern) gsub(pattern, "", corpus112q10))
docs112q10 <- tm_map(corpus112q10, toSpaceCorpus112q10, "/")
docs112q10 <- tm_map(corpus112q10, toSpaceCorpus112q10, "@")
docs112q10 <- tm_map(corpus112q10, toSpaceCorpus112q10, "\\|")

# Convert the text to lower case
docs112q10 <- tm_map(docs112q10, content_transformer(tolower))
# Remove numbers
docs112q10 <- tm_map(docs112q10, removeNumbers)
# Remove english common stopwords
docs112q10 <- tm_map(docs112q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q10 <- tm_map(docs112q10, removeWords)
# Remove punctuations
docs112q10 <- tm_map(docs112q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q10 <- tm_map(docs112q10, stripWhitespace)
# Text stemming
# docs112q10 <- tm_map(docs112q10, stemDocument)

dtm112q10 <- TermDocumentMatrix(docs112q10)
m112q10 <- as.matrix(dtm112q10)
v112q10 <- sort(rowSums(m112q10),decreasing=TRUE)
d112q10 <- data.frame(word = names(v112q10),freq=v112q10)

head(d112q10, 500)

# View answers :)
wordcloud112q10 <- wordcloud(words = d112q10$word, freq = d112q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d112q10tibble <- list(word = d112q10$word[1:10], freq = d112q10$freq[1:10])
d112q10tibble <- as_tibble(d112q10tibble)

ggplot(d112q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus112q11
text112q11Answer <- paste(x112q11$Answer)
corpus112q11 <- Corpus(VectorSource(text112q11Answer))

# Load the data as a corpus
inspect(corpus112q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus112q11 <- content_transformer(function (corpus112q11, pattern) gsub(pattern, "", corpus112q11))
docs112q11 <- tm_map(corpus112q11, toSpaceCorpus112q11, "/")
docs112q11 <- tm_map(corpus112q11, toSpaceCorpus112q11, "@")
docs112q11 <- tm_map(corpus112q11, toSpaceCorpus112q11, "\\|")

# Convert the text to lower case
docs112q11 <- tm_map(docs112q11, content_transformer(tolower))
# Remove numbers
docs112q11 <- tm_map(docs112q11, removeNumbers)
# Remove english common stopwords
docs112q11 <- tm_map(docs112q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs112q11 <- tm_map(docs112q11, removeWords)
# Remove punctuations
docs112q11 <- tm_map(docs112q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs112q11 <- tm_map(docs112q11, stripWhitespace)
# Text stemming
# docs112q11 <- tm_map(docs112q11, stemDocument)

dtm112q11 <- TermDocumentMatrix(docs112q11)
m112q11 <- as.matrix(dtm112q11)
v112q11 <- sort(rowSums(m112q11),decreasing=TRUE)
d112q11 <- data.frame(word = names(v112q11),freq=v112q11)

head(d112q11, 500)

# View answers :)
wordcloud112q11 <- wordcloud(words = d112q11$word, freq = d112q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d112q11tibble <- list(word = d112q11$word[1:10], freq = d112q11$freq[1:10])
d112q11tibble <- as_tibble(d112q11tibble)

ggplot(d112q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 12 - Q11: Are you enjoying lockdown?", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 13 ###
##################################################################################################
# Corpus113q1
text113q1Answer <- paste(x113q1$Answer)
corpus113q1 <- Corpus(VectorSource(text113q1Answer))

# Load the data as a corpus
inspect(corpus113q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q1 <- content_transformer(function (corpus113q1, pattern) gsub(pattern, "", corpus113q1))
docs113q1 <- tm_map(corpus113q1, toSpaceCorpusq1, "/")
docs113q1 <- tm_map(corpus113q1, toSpaceCorpusq1, "@")
docs113q1 <- tm_map(corpus113q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs113q1 <- tm_map(docs113q1, content_transformer(tolower))
# Remove numbers
docs113q1 <- tm_map(docs113q1, removeNumbers)
# Remove english common stopwords
docs113q1 <- tm_map(docs113q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q1 <- tm_map(docs113q1, removeWords)
# Remove punctuations
docs113q1 <- tm_map(docs113q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q1 <- tm_map(docs113q1, stripWhitespace)
# Text stemming
# docs113q1 <- tm_map(docs113q1, stemDocument)

dtm113q1 <- TermDocumentMatrix(docs113q1)
m113q1 <- as.matrix(dtm113q1)
v113q1 <- sort(rowSums(m113q1),decreasing=TRUE)
d113q1 <- data.frame(word = names(v113q1),freq=v113q1)

head(d113q1, 500)

# View answers :)
wordcloud113q1 <- wordcloud(words = d113q1$word, freq = d113q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 113q1
d113q1tibble <- list(word = d113q1$word[1:10], freq = d113q1$freq[1:10])
d113q1tibble <- as_tibble(d113q1tibble)

ggplot(d113q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("9 most frequent words - age 13 - Q1: What is the coronavirus?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q2
text113q2Answer <- paste(x113q2$Answer)
corpus113q2 <- Corpus(VectorSource(text113q2Answer))

# Load the data as a corpus
inspect(corpus113q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus113q2, pattern) gsub(pattern, "", corpus113q2))
docs113q2 <- tm_map(corpus113q2, toSpaceCorpusq2, "/")
docs113q2 <- tm_map(corpus113q2, toSpaceCorpusq2, "@")
docs113q2 <- tm_map(corpus113q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs113q2 <- tm_map(docs113q2, content_transformer(tolower))
# Remove numbers
docs113q2 <- tm_map(docs113q2, removeNumbers)
# Remove english common stopwords
docs113q2 <- tm_map(docs113q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q2 <- tm_map(docs113q2, removeWords)
# Remove punctuations
docs113q2 <- tm_map(docs113q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q2 <- tm_map(docs113q2, stripWhitespace)
# Text stemming
# docs113q2 <- tm_map(docs113q2, stemDocument)

dtm113q2 <- TermDocumentMatrix(docs113q2)
m113q2 <- as.matrix(dtm113q2)
v113q2 <- sort(rowSums(m113q2),decreasing=TRUE)
d113q2 <- data.frame(word = names(v113q2),freq=v113q2)

head(d113q2, 500)

# View answers :)
wordcloud113q2 <- wordcloud(words = d113q2$word, freq = d113q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d113q2tibble <- list(word = d113q2$word[1:10], freq = d113q2$freq[1:10])
d113q2tibble <- as_tibble(d113q2tibble)

ggplot(d113q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q2: Who is the president/prime minister?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q3
text113q3Answer <- paste(x113q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus113q3 <- Corpus(VectorSource(text113q3Answer))

# Load the data as a corpus
inspect(corpus113q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q3 <- content_transformer(function (corpus113q3, pattern) gsub(pattern, "", corpus113q3))
docs113q3 <- tm_map(corpus113q3, toSpaceCorpus113q3, "/")
docs113q3 <- tm_map(corpus113q3, toSpaceCorpus113q3, "@")
docs113q3 <- tm_map(corpus113q3, toSpaceCorpus113q3, "\\|")

# Convert the text to lower case
docs113q3 <- tm_map(docs113q3, content_transformer(tolower))
# Remove numbers
# docs113q3 <- tm_map(docs113q3, removeNumbers)
# Remove english common stopwords
docs113q3 <- tm_map(docs113q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q3 <- tm_map(docs113q3, removeWords)
# Remove punctuations
docs113q3 <- tm_map(docs113q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q3 <- tm_map(docs113q3, stripWhitespace)
# Text stemming
# docs113q3 <- tm_map(docs113q3, stemDocument)

dtm113q3 <- TermDocumentMatrix(docs113q3)
m113q3 <- as.matrix(dtm113q3)
v113q3 <- sort(rowSums(m113q3),decreasing=TRUE)
d113q3 <- data.frame(word = names(v113q3),freq=v113q3)

head(d113q3, 500)

# View answers :)
wordcloud113q3 <- wordcloud(words = d113q3$word, freq = d113q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d113q3tibble <- list(word = d113q3$word[1:10], freq = d113q3$freq[1:10])
d113q3tibble <- as_tibble(d113q3tibble)

ggplot(d113q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q3: How many days have we been in lockdown?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q4
text113q4Answer <- paste(x113q4$Answer)
corpus113q4 <- Corpus(VectorSource(text113q4Answer))

# Load the data as a corpus
inspect(corpus113q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q4 <- content_transformer(function (corpus113q4, pattern) gsub(pattern, "", corpus113q4))
docs113q4 <- tm_map(corpus113q4, toSpaceCorpus113q4, "/")
docs113q4 <- tm_map(corpus113q4, toSpaceCorpus113q4, "@")
docs113q4 <- tm_map(corpus113q4, toSpaceCorpus113q4, "\\|")

# Convert the text to lower case
docs113q4 <- tm_map(docs113q4, content_transformer(tolower))
# Remove numbers
docs113q4 <- tm_map(docs113q4, removeNumbers)
# Remove english common stopwords
docs113q4 <- tm_map(docs113q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q4 <- tm_map(docs113q4, removeWords)
# Remove punctuations
docs113q4 <- tm_map(docs113q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q4 <- tm_map(docs113q4, stripWhitespace)
# Text stemming
# docs113q4 <- tm_map(docs113q4, stemDocument)

dtm113q4 <- TermDocumentMatrix(docs113q4)
m113q4 <- as.matrix(dtm113q4)
v113q4 <- sort(rowSums(m113q4),decreasing=TRUE)
d113q4 <- data.frame(word = names(v113q4),freq=v113q4)

head(d113q4, 500)

# View answers :)
wordcloud113q4 <- wordcloud(words = d113q4$word, freq = d113q4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d113q4tibble <- list(word = d113q4$word[1:10], freq = d113q4$freq[1:10])
d113q4tibble <- as_tibble(d113q4tibble)

ggplot(d113q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q4: Do you want to go back to school?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q5
text113q5Answer <- paste(x113q5$Answer)
corpus113q5 <- Corpus(VectorSource(text113q5Answer))

# Load the data as a corpus
inspect(corpus113q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q5 <- content_transformer(function (corpus113q5, pattern) gsub(pattern, "", corpus113q5))
docs113q5 <- tm_map(corpus113q5, toSpaceCorpus113q5, "/")
docs113q5 <- tm_map(corpus113q5, toSpaceCorpus113q5, "@")
docs113q5 <- tm_map(corpus113q5, toSpaceCorpus113q5, "\\|")

# Convert the text to lower case
docs113q5 <- tm_map(docs113q5, content_transformer(tolower))
# Remove numbers
docs113q5 <- tm_map(docs113q5, removeNumbers)
# Remove english common stopwords
docs113q5 <- tm_map(docs113q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q5 <- tm_map(docs113q5, removeWords)
# Remove punctuations
docs113q5 <- tm_map(docs113q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q5 <- tm_map(docs113q5, stripWhitespace)
# Text stemming
# docs113q5 <- tm_map(docs113q5, stemDocument)

dtm113q5 <- TermDocumentMatrix(docs113q5)
m113q5 <- as.matrix(dtm113q5)
v113q5 <- sort(rowSums(m113q5),decreasing=TRUE)
d113q5 <- data.frame(word = names(v113q5),freq=v113q5)

head(d113q5, 500)

# View answers :)
wordcloud113q5 <- wordcloud(words = d113q5$word, freq = d113q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d113q5tibble <- list(word = d113q5$word[1:10], freq = d113q5$freq[1:10])
d113q5tibble <- as_tibble(d113q5tibble)

ggplot(d113q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q6
text113q6Answer <- paste(x113q6$Answer)
corpus113q6 <- Corpus(VectorSource(text113q6Answer))

# Load the data as a corpus
inspect(corpus113q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q6 <- content_transformer(function (corpus113q6, pattern) gsub(pattern, "", corpus113q6))
docs113q6 <- tm_map(corpus113q6, toSpaceCorpus113q6, "/")
docs113q6 <- tm_map(corpus113q6, toSpaceCorpus113q6, "@")
docs113q6 <- tm_map(corpus113q6, toSpaceCorpus113q6, "\\|")

# Convert the text to lower case
docs113q6 <- tm_map(docs113q6, content_transformer(tolower))
# Remove numbers
docs113q6 <- tm_map(docs113q6, removeNumbers)
# Remove english common stopwords
docs113q6 <- tm_map(docs113q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q6 <- tm_map(docs113q6, removeWords)
# Remove punctuations
docs113q6 <- tm_map(docs113q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q6 <- tm_map(docs113q6, stripWhitespace)
# Text stemming
# docs113q6 <- tm_map(docs113q6, stemDocument)

dtm113q6 <- TermDocumentMatrix(docs113q6)
m113q6 <- as.matrix(dtm113q6)
v113q6 <- sort(rowSums(m113q6),decreasing=TRUE)
d113q6 <- data.frame(word = names(v113q6),freq=v113q6)

head(d113q6, 500)

# View answers :)
wordcloud113q6 <- wordcloud(words = d113q6$word, freq = d113q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d113q6tibble <- list(word = d113q6$word[1:10], freq = d113q6$freq[1:10])
d113q6tibble <- as_tibble(d113q6tibble)

ggplot(d113q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q6: Where is the first place you want to go?", subtitle = "n = 2") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q7
text113q7Answer <- paste(x113q7$Answer)
corpus113q7 <- Corpus(VectorSource(text113q7Answer))

# Load the data as a corpus
inspect(corpus113q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q7 <- content_transformer(function (corpus113q7, pattern) gsub(pattern, "", corpus113q7))
docs113q7 <- tm_map(corpus113q7, toSpaceCorpus113q7, "/")
docs113q7 <- tm_map(corpus113q7, toSpaceCorpus113q7, "@")
docs113q7 <- tm_map(corpus113q7, toSpaceCorpus113q7, "\\|")

# Convert the text to lower case
docs113q7 <- tm_map(docs113q7, content_transformer(tolower))
# Remove numbers
docs113q7 <- tm_map(docs113q7, removeNumbers)
# Remove english common stopwords
docs113q7 <- tm_map(docs113q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q7 <- tm_map(docs113q7, removeWords)
# Remove punctuations
docs113q7 <- tm_map(docs113q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q7 <- tm_map(docs113q7, stripWhitespace)
# Text stemming
# docs113q7 <- tm_map(docs113q7, stemDocument)

dtm113q7 <- TermDocumentMatrix(docs113q7)
m113q7 <- as.matrix(dtm113q7)
v113q7 <- sort(rowSums(m113q7),decreasing=TRUE)
d113q7 <- data.frame(word = names(v113q7),freq=v113q7)

head(d113q7, 500)

# View answers :)
wordcloud113q7 <- wordcloud(words = d113q7$word, freq = d113q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d113q7tibble <- list(word = d113q7$word[1:10], freq = d113q7$freq[1:10])
d113q7tibble <- as_tibble(d113q7tibble)

ggplot(d113q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q8
text113q8Answer <- paste(x113q8$Answer)
corpus113q8 <- Corpus(VectorSource(text113q8Answer))

# Load the data as a corpus
inspect(corpus113q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q8 <- content_transformer(function (corpus113q8, pattern) gsub(pattern, "", corpus113q8))
docs113q8 <- tm_map(corpus113q8, toSpaceCorpus113q8, "/")
docs113q8 <- tm_map(corpus113q8, toSpaceCorpus113q8, "@")
docs113q8 <- tm_map(corpus113q8, toSpaceCorpus113q8, "\\|")

# Convert the text to lower case
docs113q8 <- tm_map(docs113q8, content_transformer(tolower))
# Remove numbers
docs113q8 <- tm_map(docs113q8, removeNumbers)
# Remove english common stopwords
docs113q8 <- tm_map(docs113q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs113q8 <- tm_map(docs113q8, removeWords)
# Remove punctuations
docs113q8 <- tm_map(docs113q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q8 <- tm_map(docs113q8, stripWhitespace)
# Text stemming
# docs113q8 <- tm_map(docs113q8, stemDocument)

dtm113q8 <- TermDocumentMatrix(docs113q8)
m113q8 <- as.matrix(dtm113q8)
v113q8 <- sort(rowSums(m113q8),decreasing=TRUE)
d113q8 <- data.frame(word = names(v113q8),freq=v113q8)

head(d113q8, 500)

# View answers :)
wordcloud113q8 <- wordcloud(words = d113q8$word, freq = d113q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d113q8tibble <- list(word = d113q8$word[1:10], freq = d113q8$freq[1:10])
d113q8tibble <- as_tibble(d113q8tibble)

ggplot(d113q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q8: Are your parents good teachers?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q9
text113q9Answer <- paste(x113q9$Answer)
corpus113q9 <- Corpus(VectorSource(text113q9Answer))

# Load the data as a corpus
inspect(corpus113q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q9 <- content_transformer(function (corpus113q9, pattern) gsub(pattern, "", corpus113q9))
docs113q9 <- tm_map(corpus113q9, toSpaceCorpus113q9, "/")
docs113q9 <- tm_map(corpus113q9, toSpaceCorpus113q9, "@")
docs113q9 <- tm_map(corpus113q9, toSpaceCorpus113q9, "\\|")

# Convert the text to lower case
docs113q9 <- tm_map(docs113q9, content_transformer(tolower))
# Remove numbers
docs113q9 <- tm_map(docs113q9, removeNumbers)
# Remove english common stopwords
docs113q9 <- tm_map(docs113q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q9 <- tm_map(docs113q9, removeWords)
# Remove punctuations
docs113q9 <- tm_map(docs113q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q9 <- tm_map(docs113q9, stripWhitespace)
# Text stemming
# docs113q9 <- tm_map(docs113q9, stemDocument)

dtm113q9 <- TermDocumentMatrix(docs113q9)
m113q9 <- as.matrix(dtm113q9)
v113q9 <- sort(rowSums(m113q9),decreasing=TRUE)
d113q9 <- data.frame(word = names(v113q9),freq=v113q9)

head(d113q9, 500)

# View answers :)
wordcloud113q9 <- wordcloud(words = d113q9$word, freq = d113q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d113q9tibble <- list(word = d113q9$word[1:10], freq = d113q9$freq[1:10])
d113q9tibble <- as_tibble(d113q9tibble)

ggplot(d113q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q9: How did the coronavirus start?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q10
text113q10Answer <- paste(x113q10$Answer)
corpus113q10 <- Corpus(VectorSource(text113q10Answer))

# Load the data as a corpus
inspect(corpus113q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q10 <- content_transformer(function (corpus113q10, pattern) gsub(pattern, "", corpus113q10))
docs113q10 <- tm_map(corpus113q10, toSpaceCorpus113q10, "/")
docs113q10 <- tm_map(corpus113q10, toSpaceCorpus113q10, "@")
docs113q10 <- tm_map(corpus113q10, toSpaceCorpus113q10, "\\|")

# Convert the text to lower case
docs113q10 <- tm_map(docs113q10, content_transformer(tolower))
# Remove numbers
docs113q10 <- tm_map(docs113q10, removeNumbers)
# Remove english common stopwords
docs113q10 <- tm_map(docs113q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q10 <- tm_map(docs113q10, removeWords)
# Remove punctuations
docs113q10 <- tm_map(docs113q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q10 <- tm_map(docs113q10, stripWhitespace)
# Text stemming
# docs113q10 <- tm_map(docs113q10, stemDocument)

dtm113q10 <- TermDocumentMatrix(docs113q10)
m113q10 <- as.matrix(dtm113q10)
v113q10 <- sort(rowSums(m113q10),decreasing=TRUE)
d113q10 <- data.frame(word = names(v113q10),freq=v113q10)

head(d113q10, 500)

# View answers :)
wordcloud113q10 <- wordcloud(words = d113q10$word, freq = d113q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d113q10tibble <- list(word = d113q10$word[1:10], freq = d113q10$freq[1:10])
d113q10tibble <- as_tibble(d113q10tibble)

ggplot(d113q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus113q11
text113q11Answer <- paste(x113q11$Answer)
corpus113q11 <- Corpus(VectorSource(text113q11Answer))

# Load the data as a corpus
inspect(corpus113q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus113q11 <- content_transformer(function (corpus113q11, pattern) gsub(pattern, "", corpus113q11))
docs113q11 <- tm_map(corpus113q11, toSpaceCorpus113q11, "/")
docs113q11 <- tm_map(corpus113q11, toSpaceCorpus113q11, "@")
docs113q11 <- tm_map(corpus113q11, toSpaceCorpus113q11, "\\|")

# Convert the text to lower case
docs113q11 <- tm_map(docs113q11, content_transformer(tolower))
# Remove numbers
docs113q11 <- tm_map(docs113q11, removeNumbers)
# Remove english common stopwords
docs113q11 <- tm_map(docs113q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs113q11 <- tm_map(docs113q11, removeWords)
# Remove punctuations
docs113q11 <- tm_map(docs113q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs113q11 <- tm_map(docs113q11, stripWhitespace)
# Text stemming
# docs113q11 <- tm_map(docs113q11, stemDocument)

dtm113q11 <- TermDocumentMatrix(docs113q11)
m113q11 <- as.matrix(dtm113q11)
v113q11 <- sort(rowSums(m113q11),decreasing=TRUE)
d113q11 <- data.frame(word = names(v113q11),freq=v113q11)

head(d113q11, 500)

# View answers :)
wordcloud113q11 <- wordcloud(words = d113q11$word, freq = d113q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d113q11tibble <- list(word = d113q11$word[1:10], freq = d113q11$freq[1:10])
d113q11tibble <- as_tibble(d113q11tibble)

ggplot(d113q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 13 - Q11: Are you enjoying lockdown?", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age 14 ###
##################################################################################################
# Corpus114q1
text114q1Answer <- paste(x114q1$Answer)
corpus114q1 <- Corpus(VectorSource(text114q1Answer))

# Load the data as a corpus
inspect(corpus114q1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q1 <- content_transformer(function (corpus114q1, pattern) gsub(pattern, "", corpus114q1))
docs114q1 <- tm_map(corpus114q1, toSpaceCorpusq1, "/")
docs114q1 <- tm_map(corpus114q1, toSpaceCorpusq1, "@")
docs114q1 <- tm_map(corpus114q1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docs114q1 <- tm_map(docs114q1, content_transformer(tolower))
# Remove numbers
docs114q1 <- tm_map(docs114q1, removeNumbers)
# Remove english common stopwords
docs114q1 <- tm_map(docs114q1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q1 <- tm_map(docs114q1, removeWords)
# Remove punctuations
docs114q1 <- tm_map(docs114q1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q1 <- tm_map(docs114q1, stripWhitespace)
# Text stemming
# docs114q1 <- tm_map(docs114q1, stemDocument)

dtm114q1 <- TermDocumentMatrix(docs114q1)
m114q1 <- as.matrix(dtm114q1)
v114q1 <- sort(rowSums(m114q1),decreasing=TRUE)
d114q1 <- data.frame(word = names(v114q1),freq=v114q1)

head(d114q1, 500)

# View answers :)
wordcloud114q1 <- wordcloud(words = d114q1$word, freq = d114q1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - 114q1
d114q1tibble <- list(word = d114q1$word[1:10], freq = d114q1$freq[1:10])
d114q1tibble <- as_tibble(d114q1tibble)

ggplot(d114q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q1: What is the coronavirus?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q2
text114q2Answer <- paste(x114q2$Answer)
corpus114q2 <- Corpus(VectorSource(text114q2Answer))

# Load the data as a corpus
inspect(corpus114q2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpus114q2, pattern) gsub(pattern, "", corpus114q2))
docs114q2 <- tm_map(corpus114q2, toSpaceCorpusq2, "/")
docs114q2 <- tm_map(corpus114q2, toSpaceCorpusq2, "@")
docs114q2 <- tm_map(corpus114q2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docs114q2 <- tm_map(docs114q2, content_transformer(tolower))
# Remove numbers
docs114q2 <- tm_map(docs114q2, removeNumbers)
# Remove english common stopwords
docs114q2 <- tm_map(docs114q2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q2 <- tm_map(docs114q2, removeWords)
# Remove punctuations
docs114q2 <- tm_map(docs114q2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q2 <- tm_map(docs114q2, stripWhitespace)
# Text stemming
# docs114q2 <- tm_map(docs114q2, stemDocument)

dtm114q2 <- TermDocumentMatrix(docs114q2)
m114q2 <- as.matrix(dtm114q2)
v114q2 <- sort(rowSums(m114q2),decreasing=TRUE)
d114q2 <- data.frame(word = names(v114q2),freq=v114q2)

head(d114q2, 500)

# View answers :)
wordcloud114q2 <- wordcloud(words = d114q2$word, freq = d114q2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
d114q2tibble <- list(word = d114q2$word[1:10], freq = d114q2$freq[1:10])
d114q2tibble <- as_tibble(d114q2tibble)

ggplot(d114q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q2: Who is the president/prime minister?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q3
text114q3Answer <- paste(x114q3$Answer) # so to remove "999" specifically, for xUnknownAge
corpus114q3 <- Corpus(VectorSource(text114q3Answer))

# Load the data as a corpus
inspect(corpus114q3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q3 <- content_transformer(function (corpus114q3, pattern) gsub(pattern, "", corpus114q3))
docs114q3 <- tm_map(corpus114q3, toSpaceCorpus114q3, "/")
docs114q3 <- tm_map(corpus114q3, toSpaceCorpus114q3, "@")
docs114q3 <- tm_map(corpus114q3, toSpaceCorpus114q3, "\\|")

# Convert the text to lower case
docs114q3 <- tm_map(docs114q3, content_transformer(tolower))
# Remove numbers
# docs114q3 <- tm_map(docs114q3, removeNumbers)
# Remove english common stopwords
docs114q3 <- tm_map(docs114q3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q3 <- tm_map(docs114q3, removeWords)
# Remove punctuations
docs114q3 <- tm_map(docs114q3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q3 <- tm_map(docs114q3, stripWhitespace)
# Text stemming
# docs114q3 <- tm_map(docs114q3, stemDocument)

dtm114q3 <- TermDocumentMatrix(docs114q3)
m114q3 <- as.matrix(dtm114q3)
v114q3 <- sort(rowSums(m114q3),decreasing=TRUE)
d114q3 <- data.frame(word = names(v114q3),freq=v114q3)

head(d114q3, 500)

# View answers :)
wordcloud114q3 <- wordcloud(words = d114q3$word, freq = d114q3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
d114q3tibble <- list(word = d114q3$word[1:10], freq = d114q3$freq[1:10])
d114q3tibble <- as_tibble(d114q3tibble)

ggplot(d114q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q3: How many days have we been in lockdown?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q4
text114q4Answer <- paste(x114q4$Answer)
corpus114q4 <- Corpus(VectorSource(text114q4Answer))

# Load the data as a corpus
inspect(corpus114q4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q4 <- content_transformer(function (corpus114q4, pattern) gsub(pattern, "", corpus114q4))
docs114q4 <- tm_map(corpus114q4, toSpaceCorpus114q4, "/")
docs114q4 <- tm_map(corpus114q4, toSpaceCorpus114q4, "@")
docs114q4 <- tm_map(corpus114q4, toSpaceCorpus114q4, "\\|")

# Convert the text to lower case
docs114q4 <- tm_map(docs114q4, content_transformer(tolower))
# Remove numbers
docs114q4 <- tm_map(docs114q4, removeNumbers)
# Remove english common stopwords
docs114q4 <- tm_map(docs114q4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q4 <- tm_map(docs114q4, removeWords)
# Remove punctuations
docs114q4 <- tm_map(docs114q4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q4 <- tm_map(docs114q4, stripWhitespace)
# Text stemming
# docs114q4 <- tm_map(docs114q4, stemDocument)

dtm114q4 <- TermDocumentMatrix(docs114q4)
m114q4 <- as.matrix(dtm114q4)
v114q4 <- sort(rowSums(m114q4),decreasing=TRUE)
d114q4 <- data.frame(word = names(v114q4),freq=v114q4)

head(d114q4, 500)

# View answers :)
wordcloud114q4 <- wordcloud(words = d114q4$word, freq = d114q4$freq, min.freq = 1,scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
d114q4tibble <- list(word = d114q4$word[1:10], freq = d114q4$freq[1:10])
d114q4tibble <- as_tibble(d114q4tibble)

ggplot(d114q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q4: Do you want to go back to school?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q5
text114q5Answer <- paste(x114q5$Answer)
corpus114q5 <- Corpus(VectorSource(text114q5Answer))

# Load the data as a corpus
inspect(corpus114q5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q5 <- content_transformer(function (corpus114q5, pattern) gsub(pattern, "", corpus114q5))
docs114q5 <- tm_map(corpus114q5, toSpaceCorpus114q5, "/")
docs114q5 <- tm_map(corpus114q5, toSpaceCorpus114q5, "@")
docs114q5 <- tm_map(corpus114q5, toSpaceCorpus114q5, "\\|")

# Convert the text to lower case
docs114q5 <- tm_map(docs114q5, content_transformer(tolower))
# Remove numbers
docs114q5 <- tm_map(docs114q5, removeNumbers)
# Remove english common stopwords
docs114q5 <- tm_map(docs114q5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q5 <- tm_map(docs114q5, removeWords)
# Remove punctuations
docs114q5 <- tm_map(docs114q5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q5 <- tm_map(docs114q5, stripWhitespace)
# Text stemming
# docs114q5 <- tm_map(docs114q5, stemDocument)

dtm114q5 <- TermDocumentMatrix(docs114q5)
m114q5 <- as.matrix(dtm114q5)
v114q5 <- sort(rowSums(m114q5),decreasing=TRUE)
d114q5 <- data.frame(word = names(v114q5),freq=v114q5)

head(d114q5, 500)

# View answers :)
wordcloud114q5 <- wordcloud(words = d114q5$word, freq = d114q5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
d114q5tibble <- list(word = d114q5$word[1:10], freq = d114q5$freq[1:10])
d114q5tibble <- as_tibble(d114q5tibble)

ggplot(d114q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q6
text114q6Answer <- paste(x114q6$Answer)
corpus114q6 <- Corpus(VectorSource(text114q6Answer))

# Load the data as a corpus
inspect(corpus114q6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q6 <- content_transformer(function (corpus114q6, pattern) gsub(pattern, "", corpus114q6))
docs114q6 <- tm_map(corpus114q6, toSpaceCorpus114q6, "/")
docs114q6 <- tm_map(corpus114q6, toSpaceCorpus114q6, "@")
docs114q6 <- tm_map(corpus114q6, toSpaceCorpus114q6, "\\|")

# Convert the text to lower case
docs114q6 <- tm_map(docs114q6, content_transformer(tolower))
# Remove numbers
docs114q6 <- tm_map(docs114q6, removeNumbers)
# Remove english common stopwords
docs114q6 <- tm_map(docs114q6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q6 <- tm_map(docs114q6, removeWords)
# Remove punctuations
docs114q6 <- tm_map(docs114q6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q6 <- tm_map(docs114q6, stripWhitespace)
# Text stemming
# docs114q6 <- tm_map(docs114q6, stemDocument)

dtm114q6 <- TermDocumentMatrix(docs114q6)
m114q6 <- as.matrix(dtm114q6)
v114q6 <- sort(rowSums(m114q6),decreasing=TRUE)
d114q6 <- data.frame(word = names(v114q6),freq=v114q6)

head(d114q6, 500)

# View answers :)
wordcloud114q6 <- wordcloud(words = d114q6$word, freq = d114q6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
d114q6tibble <- list(word = d114q6$word[1:10], freq = d114q6$freq[1:10])
d114q6tibble <- as_tibble(d114q6tibble)

ggplot(d114q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q6: Where is the first place you want to go?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q7
text114q7Answer <- paste(x114q7$Answer)
corpus114q7 <- Corpus(VectorSource(text114q7Answer))

# Load the data as a corpus
inspect(corpus114q7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q7 <- content_transformer(function (corpus114q7, pattern) gsub(pattern, "", corpus114q7))
docs114q7 <- tm_map(corpus114q7, toSpaceCorpus114q7, "/")
docs114q7 <- tm_map(corpus114q7, toSpaceCorpus114q7, "@")
docs114q7 <- tm_map(corpus114q7, toSpaceCorpus114q7, "\\|")

# Convert the text to lower case
docs114q7 <- tm_map(docs114q7, content_transformer(tolower))
# Remove numbers
docs114q7 <- tm_map(docs114q7, removeNumbers)
# Remove english common stopwords
docs114q7 <- tm_map(docs114q7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q7 <- tm_map(docs114q7, removeWords)
# Remove punctuations
docs114q7 <- tm_map(docs114q7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q7 <- tm_map(docs114q7, stripWhitespace)
# Text stemming
# docs114q7 <- tm_map(docs114q7, stemDocument)

dtm114q7 <- TermDocumentMatrix(docs114q7)
m114q7 <- as.matrix(dtm114q7)
v114q7 <- sort(rowSums(m114q7),decreasing=TRUE)
d114q7 <- data.frame(word = names(v114q7),freq=v114q7)

head(d114q7, 500)

# View answers :)
wordcloud114q7 <- wordcloud(words = d114q7$word, freq = d114q7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
d114q7tibble <- list(word = d114q7$word[1:10], freq = d114q7$freq[1:10])
d114q7tibble <- as_tibble(d114q7tibble)

ggplot(d114q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q8
text114q8Answer <- paste(x114q8$Answer)
corpus114q8 <- Corpus(VectorSource(text114q8Answer))

# Load the data as a corpus
inspect(corpus114q8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q8 <- content_transformer(function (corpus114q8, pattern) gsub(pattern, "", corpus114q8))
docs114q8 <- tm_map(corpus114q8, toSpaceCorpus114q8, "/")
docs114q8 <- tm_map(corpus114q8, toSpaceCorpus114q8, "@")
docs114q8 <- tm_map(corpus114q8, toSpaceCorpus114q8, "\\|")

# Convert the text to lower case
docs114q8 <- tm_map(docs114q8, content_transformer(tolower))
# Remove numbers
docs114q8 <- tm_map(docs114q8, removeNumbers)
# Remove english common stopwords
docs114q8 <- tm_map(docs114q8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docs114q8 <- tm_map(docs114q8, removeWords)
# Remove punctuations
docs114q8 <- tm_map(docs114q8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q8 <- tm_map(docs114q8, stripWhitespace)
# Text stemming
# docs114q8 <- tm_map(docs114q8, stemDocument)

dtm114q8 <- TermDocumentMatrix(docs114q8)
m114q8 <- as.matrix(dtm114q8)
v114q8 <- sort(rowSums(m114q8),decreasing=TRUE)
d114q8 <- data.frame(word = names(v114q8),freq=v114q8)

head(d114q8, 500)

# View answers :)
wordcloud114q8 <- wordcloud(words = d114q8$word, freq = d114q8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
d114q8tibble <- list(word = d114q8$word[1:10], freq = d114q8$freq[1:10])
d114q8tibble <- as_tibble(d114q8tibble)

ggplot(d114q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q8: Are your parents good teachers?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q9
text114q9Answer <- paste(x114q9$Answer)
corpus114q9 <- Corpus(VectorSource(text114q9Answer))

# Load the data as a corpus
inspect(corpus114q9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q9 <- content_transformer(function (corpus114q9, pattern) gsub(pattern, "", corpus114q9))
docs114q9 <- tm_map(corpus114q9, toSpaceCorpus114q9, "/")
docs114q9 <- tm_map(corpus114q9, toSpaceCorpus114q9, "@")
docs114q9 <- tm_map(corpus114q9, toSpaceCorpus114q9, "\\|")

# Convert the text to lower case
docs114q9 <- tm_map(docs114q9, content_transformer(tolower))
# Remove numbers
docs114q9 <- tm_map(docs114q9, removeNumbers)
# Remove english common stopwords
docs114q9 <- tm_map(docs114q9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q9 <- tm_map(docs114q9, removeWords)
# Remove punctuations
docs114q9 <- tm_map(docs114q9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q9 <- tm_map(docs114q9, stripWhitespace)
# Text stemming
# docs114q9 <- tm_map(docs114q9, stemDocument)

dtm114q9 <- TermDocumentMatrix(docs114q9)
m114q9 <- as.matrix(dtm114q9)
v114q9 <- sort(rowSums(m114q9),decreasing=TRUE)
d114q9 <- data.frame(word = names(v114q9),freq=v114q9)

head(d114q9, 500)

# View answers :)
wordcloud114q9 <- wordcloud(words = d114q9$word, freq = d114q9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
d114q9tibble <- list(word = d114q9$word[1:10], freq = d114q9$freq[1:10])
d114q9tibble <- as_tibble(d114q9tibble)

ggplot(d114q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q9: How did the coronavirus start?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q10
text114q10Answer <- paste(x114q10$Answer)
corpus114q10 <- Corpus(VectorSource(text114q10Answer))

# Load the data as a corpus
inspect(corpus114q10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q10 <- content_transformer(function (corpus114q10, pattern) gsub(pattern, "", corpus114q10))
docs114q10 <- tm_map(corpus114q10, toSpaceCorpus114q10, "/")
docs114q10 <- tm_map(corpus114q10, toSpaceCorpus114q10, "@")
docs114q10 <- tm_map(corpus114q10, toSpaceCorpus114q10, "\\|")

# Convert the text to lower case
docs114q10 <- tm_map(docs114q10, content_transformer(tolower))
# Remove numbers
docs114q10 <- tm_map(docs114q10, removeNumbers)
# Remove english common stopwords
docs114q10 <- tm_map(docs114q10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q10 <- tm_map(docs114q10, removeWords)
# Remove punctuations
docs114q10 <- tm_map(docs114q10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q10 <- tm_map(docs114q10, stripWhitespace)
# Text stemming
# docs114q10 <- tm_map(docs114q10, stemDocument)

dtm114q10 <- TermDocumentMatrix(docs114q10)
m114q10 <- as.matrix(dtm114q10)
v114q10 <- sort(rowSums(m114q10),decreasing=TRUE)
d114q10 <- data.frame(word = names(v114q10),freq=v114q10)

head(d114q10, 500)

# View answers :)
wordcloud114q10 <- wordcloud(words = d114q10$word, freq = d114q10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
d114q10tibble <- list(word = d114q10$word[1:10], freq = d114q10$freq[1:10])
d114q10tibble <- as_tibble(d114q10tibble)

ggplot(d114q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Corpus114q11
text114q11Answer <- paste(x114q11$Answer)
corpus114q11 <- Corpus(VectorSource(text114q11Answer))

# Load the data as a corpus
inspect(corpus114q11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpus114q11 <- content_transformer(function (corpus114q11, pattern) gsub(pattern, "", corpus114q11))
docs114q11 <- tm_map(corpus114q11, toSpaceCorpus114q11, "/")
docs114q11 <- tm_map(corpus114q11, toSpaceCorpus114q11, "@")
docs114q11 <- tm_map(corpus114q11, toSpaceCorpus114q11, "\\|")

# Convert the text to lower case
docs114q11 <- tm_map(docs114q11, content_transformer(tolower))
# Remove numbers
docs114q11 <- tm_map(docs114q11, removeNumbers)
# Remove english common stopwords
docs114q11 <- tm_map(docs114q11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs114q11 <- tm_map(docs114q11, removeWords)
# Remove punctuations
docs114q11 <- tm_map(docs114q11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docs114q11 <- tm_map(docs114q11, stripWhitespace)
# Text stemming
# docs114q11 <- tm_map(docs114q11, stemDocument)

dtm114q11 <- TermDocumentMatrix(docs114q11)
m114q11 <- as.matrix(dtm114q11)
v114q11 <- sort(rowSums(m114q11),decreasing=TRUE)
d114q11 <- data.frame(word = names(v114q11),freq=v114q11)

head(d114q11, 500)

# View answers :)
wordcloud114q11 <- wordcloud(words = d114q11$word, freq = d114q11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
d114q11tibble <- list(word = d114q11$word[1:10], freq = d114q11$freq[1:10])
d114q11tibble <- as_tibble(d114q11tibble)

ggplot(d114q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age 14 - Q11: Are you enjoying lockdown?", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##################################################################################################
### Age Unknown ###
##################################################################################################
# CorpusUnknownq1
textUnknownq1Answer <- paste(xUnknownq1$Answer)
corpusUnknownq1 <- Corpus(VectorSource(textUnknownq1Answer))

# Load the data as a corpus
inspect(corpusUnknownq1)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq1 <- content_transformer(function (corpusUnknownq1, pattern) gsub(pattern, "", corpusUnknownq1))
docsUnknownq1 <- tm_map(corpusUnknownq1, toSpaceCorpusq1, "/")
docsUnknownq1 <- tm_map(corpusUnknownq1, toSpaceCorpusq1, "@")
docsUnknownq1 <- tm_map(corpusUnknownq1, toSpaceCorpusq1, "\\|")

# Convert the text to lower case
docsUnknownq1 <- tm_map(docsUnknownq1, content_transformer(tolower))
# Remove numbers
docsUnknownq1 <- tm_map(docsUnknownq1, removeNumbers)
# Remove english common stopwords
docsUnknownq1 <- tm_map(docsUnknownq1, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq1 <- tm_map(docsUnknownq1, removeWords)
# Remove punctuations
docsUnknownq1 <- tm_map(docsUnknownq1, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq1 <- tm_map(docsUnknownq1, stripWhitespace)
# Text stemming
# docsUnknownq1 <- tm_map(docsUnknownq1, stemDocument)

dtmUnknownq1 <- TermDocumentMatrix(docsUnknownq1)
mUnknownq1 <- as.matrix(dtmUnknownq1)
vUnknownq1 <- sort(rowSums(mUnknownq1),decreasing=TRUE)
dUnknownq1 <- data.frame(word = names(vUnknownq1),freq=vUnknownq1)

head(dUnknownq1, 500)

# View answers :)
wordcloudUnknownq1 <- wordcloud(words = dUnknownq1$word, freq = dUnknownq1$freq, min.freq = 1, scale=c(3,.5), # change min.freq to see different results
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - Unknownq1
dUnknownq1tibble <- list(word = dUnknownq1$word[1:10], freq = dUnknownq1$freq[1:10])
dUnknownq1tibble <- as_tibble(dUnknownq1tibble)

ggplot(dUnknownq1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q1: What is the coronavirus?", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq2
textUnknownq2Answer <- paste(xUnknownq2$Answer)
corpusUnknownq2 <- Corpus(VectorSource(textUnknownq2Answer))

# Load the data as a corpus
inspect(corpusUnknownq2)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusq2 <- content_transformer(function (corpusUnknownq2, pattern) gsub(pattern, "", corpusUnknownq2))
docsUnknownq2 <- tm_map(corpusUnknownq2, toSpaceCorpusq2, "/")
docsUnknownq2 <- tm_map(corpusUnknownq2, toSpaceCorpusq2, "@")
docsUnknownq2 <- tm_map(corpusUnknownq2, toSpaceCorpusq2, "\\|")

# Convert the text to lower case
docsUnknownq2 <- tm_map(docsUnknownq2, content_transformer(tolower))
# Remove numbers
docsUnknownq2 <- tm_map(docsUnknownq2, removeNumbers)
# Remove english common stopwords
docsUnknownq2 <- tm_map(docsUnknownq2, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq2 <- tm_map(docsUnknownq2, removeWords)
# Remove punctuations
docsUnknownq2 <- tm_map(docsUnknownq2, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq2 <- tm_map(docsUnknownq2, stripWhitespace)
# Text stemming
# docsUnknownq2 <- tm_map(docsUnknownq2, stemDocument)

dtmUnknownq2 <- TermDocumentMatrix(docsUnknownq2)
mUnknownq2 <- as.matrix(dtmUnknownq2)
vUnknownq2 <- sort(rowSums(mUnknownq2),decreasing=TRUE)
dUnknownq2 <- data.frame(word = names(vUnknownq2),freq=vUnknownq2)

head(dUnknownq2, 500)

# View answers :)
wordcloudUnknownq2 <- wordcloud(words = dUnknownq2$word, freq = dUnknownq2$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))


# Bar plot of the frequency for the top 10 - q2
dUnknownq2tibble <- list(word = dUnknownq2$word[1:10], freq = dUnknownq2$freq[1:10])
dUnknownq2tibble <- as_tibble(dUnknownq2tibble)

ggplot(dUnknownq2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q2: Who is the president/prime minister?", subtitle = "n = 56") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq3
textUnknownq3Answer <- paste(xUnknownq3$Answer) # so to remove "999" specifically, for xUnknownAge
corpusUnknownq3 <- Corpus(VectorSource(textUnknownq3Answer))

# Load the data as a corpus
inspect(corpusUnknownq3)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq3 <- content_transformer(function (corpusUnknownq3, pattern) gsub(pattern, "", corpusUnknownq3))
docsUnknownq3 <- tm_map(corpusUnknownq3, toSpaceCorpusUnknownq3, "/")
docsUnknownq3 <- tm_map(corpusUnknownq3, toSpaceCorpusUnknownq3, "@")
docsUnknownq3 <- tm_map(corpusUnknownq3, toSpaceCorpusUnknownq3, "\\|")

# Convert the text to lower case
docsUnknownq3 <- tm_map(docsUnknownq3, content_transformer(tolower))
# Remove numbers
# docsUnknownq3 <- tm_map(docsUnknownq3, removeNumbers)
# Remove english common stopwords
docsUnknownq3 <- tm_map(docsUnknownq3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq3 <- tm_map(docsUnknownq3, removeWords)
# Remove punctuations
docsUnknownq3 <- tm_map(docsUnknownq3, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq3 <- tm_map(docsUnknownq3, stripWhitespace)
# Text stemming
# docsUnknownq3 <- tm_map(docsUnknownq3, stemDocument)

dtmUnknownq3 <- TermDocumentMatrix(docsUnknownq3)
mUnknownq3 <- as.matrix(dtmUnknownq3)
vUnknownq3 <- sort(rowSums(mUnknownq3),decreasing=TRUE)
dUnknownq3 <- data.frame(word = names(vUnknownq3),freq=vUnknownq3)

head(dUnknownq3, 500)

# View answers :)
wordcloudUnknownq3 <- wordcloud(words = dUnknownq3$word, freq = dUnknownq3$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q3
dUnknownq3tibble <- list(word = dUnknownq3$word[1:10], freq = dUnknownq3$freq[1:10])
dUnknownq3tibble <- as_tibble(dUnknownq3tibble)

ggplot(dUnknownq3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q3: How many days have we been in lockdown?", subtitle = "n = 68") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq4
textUnknownq4Answer <- paste(xUnknownq4$Answer)
corpusUnknownq4 <- Corpus(VectorSource(textUnknownq4Answer))

# Load the data as a corpus
inspect(corpusUnknownq4)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq4 <- content_transformer(function (corpusUnknownq4, pattern) gsub(pattern, "", corpusUnknownq4))
docsUnknownq4 <- tm_map(corpusUnknownq4, toSpaceCorpusUnknownq4, "/")
docsUnknownq4 <- tm_map(corpusUnknownq4, toSpaceCorpusUnknownq4, "@")
docsUnknownq4 <- tm_map(corpusUnknownq4, toSpaceCorpusUnknownq4, "\\|")

# Convert the text to lower case
docsUnknownq4 <- tm_map(docsUnknownq4, content_transformer(tolower))
# Remove numbers
docsUnknownq4 <- tm_map(docsUnknownq4, removeNumbers)
# Remove english common stopwords
docsUnknownq4 <- tm_map(docsUnknownq4, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq4 <- tm_map(docsUnknownq4, removeWords)
# Remove punctuations
docsUnknownq4 <- tm_map(docsUnknownq4, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq4 <- tm_map(docsUnknownq4, stripWhitespace)
# Text stemming
# docsUnknownq4 <- tm_map(docsUnknownq4, stemDocument)

dtmUnknownq4 <- TermDocumentMatrix(docsUnknownq4)
mUnknownq4 <- as.matrix(dtmUnknownq4)
vUnknownq4 <- sort(rowSums(mUnknownq4),decreasing=TRUE)
dUnknownq4 <- data.frame(word = names(vUnknownq4),freq=vUnknownq4)

head(dUnknownq4, 500)

# View answers :)
wordcloudUnknownq4 <- wordcloud(words = dUnknownq4$word, freq = dUnknownq4$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q4
dUnknownq4tibble <- list(word = dUnknownq4$word[1:10], freq = dUnknownq4$freq[1:10])
dUnknownq4tibble <- as_tibble(dUnknownq4tibble)

ggplot(dUnknownq4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q4: Do you want to go back to school?", subtitle = "n = 59") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq5
textUnknownq5Answer <- paste(xUnknownq5$Answer)
corpusUnknownq5 <- Corpus(VectorSource(textUnknownq5Answer))

# Load the data as a corpus
inspect(corpusUnknownq5)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq5 <- content_transformer(function (corpusUnknownq5, pattern) gsub(pattern, "", corpusUnknownq5))
docsUnknownq5 <- tm_map(corpusUnknownq5, toSpaceCorpusUnknownq5, "/")
docsUnknownq5 <- tm_map(corpusUnknownq5, toSpaceCorpusUnknownq5, "@")
docsUnknownq5 <- tm_map(corpusUnknownq5, toSpaceCorpusUnknownq5, "\\|")

# Convert the text to lower case
docsUnknownq5 <- tm_map(docsUnknownq5, content_transformer(tolower))
# Remove numbers
docsUnknownq5 <- tm_map(docsUnknownq5, removeNumbers)
# Remove english common stopwords
docsUnknownq5 <- tm_map(docsUnknownq5, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq5 <- tm_map(docsUnknownq5, removeWords)
# Remove punctuations
docsUnknownq5 <- tm_map(docsUnknownq5, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq5 <- tm_map(docsUnknownq5, stripWhitespace)
# Text stemming
# docsUnknownq5 <- tm_map(docsUnknownq5, stemDocument)

dtmUnknownq5 <- TermDocumentMatrix(docsUnknownq5)
mUnknownq5 <- as.matrix(dtmUnknownq5)
vUnknownq5 <- sort(rowSums(mUnknownq5),decreasing=TRUE)
dUnknownq5 <- data.frame(word = names(vUnknownq5),freq=vUnknownq5)

head(dUnknownq5, 500)

# View answers :)
wordcloudUnknownq5 <- wordcloud(words = dUnknownq5$word, freq = dUnknownq5$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q5
dUnknownq5tibble <- list(word = dUnknownq5$word[1:10], freq = dUnknownq5$freq[1:10])
dUnknownq5tibble <- as_tibble(dUnknownq5tibble)

ggplot(dUnknownq5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 68") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq6
textUnknownq6Answer <- paste(xUnknownq6$Answer)
corpusUnknownq6 <- Corpus(VectorSource(textUnknownq6Answer))

# Load the data as a corpus
inspect(corpusUnknownq6)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq6 <- content_transformer(function (corpusUnknownq6, pattern) gsub(pattern, "", corpusUnknownq6))
docsUnknownq6 <- tm_map(corpusUnknownq6, toSpaceCorpusUnknownq6, "/")
docsUnknownq6 <- tm_map(corpusUnknownq6, toSpaceCorpusUnknownq6, "@")
docsUnknownq6 <- tm_map(corpusUnknownq6, toSpaceCorpusUnknownq6, "\\|")

# Convert the text to lower case
docsUnknownq6 <- tm_map(docsUnknownq6, content_transformer(tolower))
# Remove numbers
docsUnknownq6 <- tm_map(docsUnknownq6, removeNumbers)
# Remove english common stopwords
docsUnknownq6 <- tm_map(docsUnknownq6, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq6 <- tm_map(docsUnknownq6, removeWords)
# Remove punctuations
docsUnknownq6 <- tm_map(docsUnknownq6, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq6 <- tm_map(docsUnknownq6, stripWhitespace)
# Text stemming
# docsUnknownq6 <- tm_map(docsUnknownq6, stemDocument)

dtmUnknownq6 <- TermDocumentMatrix(docsUnknownq6)
mUnknownq6 <- as.matrix(dtmUnknownq6)
vUnknownq6 <- sort(rowSums(mUnknownq6),decreasing=TRUE)
dUnknownq6 <- data.frame(word = names(vUnknownq6),freq=vUnknownq6)

head(dUnknownq6, 500)

# View answers :)
wordcloudUnknownq6 <- wordcloud(words = dUnknownq6$word, freq = dUnknownq6$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q6
dUnknownq6tibble <- list(word = dUnknownq6$word[1:10], freq = dUnknownq6$freq[1:10])
dUnknownq6tibble <- as_tibble(dUnknownq6tibble)

ggplot(dUnknownq6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q6: Where is the first place you want to go?", subtitle = "n = 67") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq7
textUnknownq7Answer <- paste(xUnknownq7$Answer)
corpusUnknownq7 <- Corpus(VectorSource(textUnknownq7Answer))

# Load the data as a corpus
inspect(corpusUnknownq7)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq7 <- content_transformer(function (corpusUnknownq7, pattern) gsub(pattern, "", corpusUnknownq7))
docsUnknownq7 <- tm_map(corpusUnknownq7, toSpaceCorpusUnknownq7, "/")
docsUnknownq7 <- tm_map(corpusUnknownq7, toSpaceCorpusUnknownq7, "@")
docsUnknownq7 <- tm_map(corpusUnknownq7, toSpaceCorpusUnknownq7, "\\|")

# Convert the text to lower case
docsUnknownq7 <- tm_map(docsUnknownq7, content_transformer(tolower))
# Remove numbers
docsUnknownq7 <- tm_map(docsUnknownq7, removeNumbers)
# Remove english common stopwords
docsUnknownq7 <- tm_map(docsUnknownq7, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq7 <- tm_map(docsUnknownq7, removeWords)
# Remove punctuations
docsUnknownq7 <- tm_map(docsUnknownq7, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq7 <- tm_map(docsUnknownq7, stripWhitespace)
# Text stemming
# docsUnknownq7 <- tm_map(docsUnknownq7, stemDocument)

dtmUnknownq7 <- TermDocumentMatrix(docsUnknownq7)
mUnknownq7 <- as.matrix(dtmUnknownq7)
vUnknownq7 <- sort(rowSums(mUnknownq7),decreasing=TRUE)
dUnknownq7 <- data.frame(word = names(vUnknownq7),freq=vUnknownq7)

head(dUnknownq7, 500)

# View answers :)
wordcloudUnknownq7 <- wordcloud(words = dUnknownq7$word, freq = dUnknownq7$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q7
dUnknownq7tibble <- list(word = dUnknownq7$word[1:10], freq = dUnknownq7$freq[1:10])
dUnknownq7tibble <- as_tibble(dUnknownq7tibble)

ggplot(dUnknownq7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 69") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq8
textUnknownq8Answer <- paste(xUnknownq8$Answer)
corpusUnknownq8 <- Corpus(VectorSource(textUnknownq8Answer))

# Load the data as a corpus
inspect(corpusUnknownq8)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq8 <- content_transformer(function (corpusUnknownq8, pattern) gsub(pattern, "", corpusUnknownq8))
docsUnknownq8 <- tm_map(corpusUnknownq8, toSpaceCorpusUnknownq8, "/")
docsUnknownq8 <- tm_map(corpusUnknownq8, toSpaceCorpusUnknownq8, "@")
docsUnknownq8 <- tm_map(corpusUnknownq8, toSpaceCorpusUnknownq8, "\\|")

# Convert the text to lower case
docsUnknownq8 <- tm_map(docsUnknownq8, content_transformer(tolower))
# Remove numbers
docsUnknownq8 <- tm_map(docsUnknownq8, removeNumbers)
# Remove english common stopwords
docsUnknownq8 <- tm_map(docsUnknownq8, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
# docsUnknownq8 <- tm_map(docsUnknownq8, removeWords)
# Remove punctuations
docsUnknownq8 <- tm_map(docsUnknownq8, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq8 <- tm_map(docsUnknownq8, stripWhitespace)
# Text stemming
# docsUnknownq8 <- tm_map(docsUnknownq8, stemDocument)

dtmUnknownq8 <- TermDocumentMatrix(docsUnknownq8)
mUnknownq8 <- as.matrix(dtmUnknownq8)
vUnknownq8 <- sort(rowSums(mUnknownq8),decreasing=TRUE)
dUnknownq8 <- data.frame(word = names(vUnknownq8),freq=vUnknownq8)

head(dUnknownq8, 500)

# View answers :)
wordcloudUnknownq8 <- wordcloud(words = dUnknownq8$word, freq = dUnknownq8$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q8
dUnknownq8tibble <- list(word = dUnknownq8$word[1:10], freq = dUnknownq8$freq[1:10])
dUnknownq8tibble <- as_tibble(dUnknownq8tibble)

ggplot(dUnknownq8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q8: Are your parents good teachers?", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq9
textUnknownq9Answer <- paste(xUnknownq9$Answer)
corpusUnknownq9 <- Corpus(VectorSource(textUnknownq9Answer))

# Load the data as a corpus
inspect(corpusUnknownq9)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq9 <- content_transformer(function (corpusUnknownq9, pattern) gsub(pattern, "", corpusUnknownq9))
docsUnknownq9 <- tm_map(corpusUnknownq9, toSpaceCorpusUnknownq9, "/")
docsUnknownq9 <- tm_map(corpusUnknownq9, toSpaceCorpusUnknownq9, "@")
docsUnknownq9 <- tm_map(corpusUnknownq9, toSpaceCorpusUnknownq9, "\\|")

# Convert the text to lower case
docsUnknownq9 <- tm_map(docsUnknownq9, content_transformer(tolower))
# Remove numbers
docsUnknownq9 <- tm_map(docsUnknownq9, removeNumbers)
# Remove english common stopwords
docsUnknownq9 <- tm_map(docsUnknownq9, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq9 <- tm_map(docsUnknownq9, removeWords)
# Remove punctuations
docsUnknownq9 <- tm_map(docsUnknownq9, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq9 <- tm_map(docsUnknownq9, stripWhitespace)
# Text stemming
# docsUnknownq9 <- tm_map(docsUnknownq9, stemDocument)

dtmUnknownq9 <- TermDocumentMatrix(docsUnknownq9)
mUnknownq9 <- as.matrix(dtmUnknownq9)
vUnknownq9 <- sort(rowSums(mUnknownq9),decreasing=TRUE)
dUnknownq9 <- data.frame(word = names(vUnknownq9),freq=vUnknownq9)

head(dUnknownq9, 500)

# View answers :)
wordcloudUnknownq9 <- wordcloud(words = dUnknownq9$word, freq = dUnknownq9$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q9
dUnknownq9tibble <- list(word = dUnknownq9$word[1:10], freq = dUnknownq9$freq[1:10])
dUnknownq9tibble <- as_tibble(dUnknownq9tibble)

ggplot(dUnknownq9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q9: How did the coronavirus start?", subtitle = "n = 67") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq10
textUnknownq10Answer <- paste(xUnknownq10$Answer)
corpusUnknownq10 <- Corpus(VectorSource(textUnknownq10Answer))

# Load the data as a corpus
inspect(corpusUnknownq10)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq10 <- content_transformer(function (corpusUnknownq10, pattern) gsub(pattern, "", corpusUnknownq10))
docsUnknownq10 <- tm_map(corpusUnknownq10, toSpaceCorpusUnknownq10, "/")
docsUnknownq10 <- tm_map(corpusUnknownq10, toSpaceCorpusUnknownq10, "@")
docsUnknownq10 <- tm_map(corpusUnknownq10, toSpaceCorpusUnknownq10, "\\|")

# Convert the text to lower case
docsUnknownq10 <- tm_map(docsUnknownq10, content_transformer(tolower))
# Remove numbers
docsUnknownq10 <- tm_map(docsUnknownq10, removeNumbers)
# Remove english common stopwords
docsUnknownq10 <- tm_map(docsUnknownq10, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq10 <- tm_map(docsUnknownq10, removeWords)
# Remove punctuations
docsUnknownq10 <- tm_map(docsUnknownq10, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq10 <- tm_map(docsUnknownq10, stripWhitespace)
# Text stemming
# docsUnknownq10 <- tm_map(docsUnknownq10, stemDocument)

dtmUnknownq10 <- TermDocumentMatrix(docsUnknownq10)
mUnknownq10 <- as.matrix(dtmUnknownq10)
vUnknownq10 <- sort(rowSums(mUnknownq10),decreasing=TRUE)
dUnknownq10 <- data.frame(word = names(vUnknownq10),freq=vUnknownq10)

head(dUnknownq10, 500)

# View answers :)
wordcloudUnknownq10 <- wordcloud(words = dUnknownq10$word, freq = dUnknownq10$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q10
dUnknownq10tibble <- list(word = dUnknownq10$word[1:10], freq = dUnknownq10$freq[1:10])
dUnknownq10tibble <- as_tibble(dUnknownq10tibble)

ggplot(dUnknownq10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 62") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# CorpusUnknownq11
textUnknownq11Answer <- paste(xUnknownq11$Answer)
corpusUnknownq11 <- Corpus(VectorSource(textUnknownq11Answer))

# Load the data as a corpus
inspect(corpusUnknownq11)

# Transformation is performed using tm_map() function to replace, for example, special characters from the text.
# Replacing “/”, “@” and “|” with space:
toSpaceCorpusUnknownq11 <- content_transformer(function (corpusUnknownq11, pattern) gsub(pattern, "", corpusUnknownq11))
docsUnknownq11 <- tm_map(corpusUnknownq11, toSpaceCorpusUnknownq11, "/")
docsUnknownq11 <- tm_map(corpusUnknownq11, toSpaceCorpusUnknownq11, "@")
docsUnknownq11 <- tm_map(corpusUnknownq11, toSpaceCorpusUnknownq11, "\\|")

# Convert the text to lower case
docsUnknownq11 <- tm_map(docsUnknownq11, content_transformer(tolower))
# Remove numbers
docsUnknownq11 <- tm_map(docsUnknownq11, removeNumbers)
# Remove english common stopwords
docsUnknownq11 <- tm_map(docsUnknownq11, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docsUnknownq11 <- tm_map(docsUnknownq11, removeWords)
# Remove punctuations
docsUnknownq11 <- tm_map(docsUnknownq11, removePunctuation, ucp = TRUE)
# Eliminate extra white spaces
docsUnknownq11 <- tm_map(docsUnknownq11, stripWhitespace)
# Text stemming
# docsUnknownq11 <- tm_map(docsUnknownq11, stemDocument)

dtmUnknownq11 <- TermDocumentMatrix(docsUnknownq11)
mUnknownq11 <- as.matrix(dtmUnknownq11)
vUnknownq11 <- sort(rowSums(mUnknownq11),decreasing=TRUE)
dUnknownq11 <- data.frame(word = names(vUnknownq11),freq=vUnknownq11)

head(dUnknownq11, 500)

# View answers :)
wordcloudUnknownq11 <- wordcloud(words = dUnknownq11$word, freq = dUnknownq11$freq, min.freq = 1, scale=c(3,.5),
          max.words=999, random.order=FALSE, rot.per=0.0, 
          colors=brewer.pal(8, "Dark2"))

# Bar plot of the frequency for the top 10 - q11
dUnknownq11tibble <- list(word = dUnknownq11$word[1:10], freq = dUnknownq11$freq[1:10])
dUnknownq11tibble <- as_tibble(dUnknownq11tibble)

ggplot(dUnknownq11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("10 most frequent words - age unknown - Q11: Are you enjoying lockdown?", subtitle = "n = 61") +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("y = word") + ylab("x = frequency") + 
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.y = element_text(size=12, face = "italic", color = "dark green", vjust = 2),
        axis.title.x = element_text(size=12, face = "italic", color = "dark green", vjust = -.5),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())













###############################################################################################
### Ggarrange bar charts by question by age ###
###############################################################################################

# Age 2 #
x102q1bar <- ggplot(d102q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x102Q2bar <- ggplot(d102q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
x102Q3bar <- ggplot(d102q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
x102Q4bar <- ggplot(d102q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x102Q5bar <- ggplot(d102q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x102Q6bar <- ggplot(d102q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
x102Q7bar <- ggplot(d102q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x102Q8bar <- ggplot(d102q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x102Q9bar <- ggplot(d102q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 5") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x102q10bar <- ggplot(d102q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x102q11bar <- ggplot(d102q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 2", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Age 3 #
x103q1bar <- ggplot(d103q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x103Q2bar <- ggplot(d103q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x103Q3bar <- ggplot(d103q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
x103Q4bar <- ggplot(d103q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x103Q5bar <- ggplot(d103q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
x103Q6bar <- ggplot(d103q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x103Q7bar <- ggplot(d103q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x103Q8bar <- ggplot(d103q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x103Q9bar <- ggplot(d103q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x103q10bar <- ggplot(d103q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x103q11bar <- ggplot(d103q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 3", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Age 4 #
x104q1bar <- ggplot(d104q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 53") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x104Q2bar <- ggplot(d104q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 53") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x104Q3bar <- ggplot(d104q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 55") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x104Q4bar <- ggplot(d104q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x104Q5bar <- ggplot(d104q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 51") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x104Q6bar <- ggplot(d104q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 53") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x104Q7bar <- ggplot(d104q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 54") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
x104Q8bar <- ggplot(d104q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
x104Q9bar <- ggplot(d104q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 56") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x104q10bar <- ggplot(d104q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 49") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x104q11bar <- ggplot(d104q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 4", subtitle = "n = 44") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# Age 5 #
x105q1bar <- ggplot(d105q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 64") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105Q2bar <- ggplot(d105q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 60") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105Q3bar <- ggplot(d105q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 64") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105Q4bar <- ggplot(d105q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 58") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x105Q5bar <- ggplot(d105q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105Q6bar <- ggplot(d105q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 63") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105Q7bar <- ggplot(d105q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 64") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105Q8bar <- ggplot(d105q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 60") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105Q9bar <- ggplot(d105q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105q10bar <- ggplot(d105q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 58") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x105q11bar <- ggplot(d105q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 5", subtitle = "n = 59") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



# Age 6 #
x106q1bar <- ggplot(d106q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 45") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106Q2bar <- ggplot(d106q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 37") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106Q3bar <- ggplot(d106q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 49") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106Q4bar <- ggplot(d106q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 41") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x106Q5bar <- ggplot(d106q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 49") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106Q6bar <- ggplot(d106q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106Q7bar <- ggplot(d106q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106Q8bar <- ggplot(d106q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 45") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106Q9bar <- ggplot(d106q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 50") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106q10bar <- ggplot(d106q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 45") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x106q11bar <- ggplot(d106q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 6", subtitle = "n = 41") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Age 7 #
x107q1bar <- ggplot(d107q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107Q2bar <- ggplot(d107q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 33") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107Q3bar <- ggplot(d107q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107Q4bar <- ggplot(d107q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 41") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x107Q5bar <- ggplot(d107q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107Q6bar <- ggplot(d107q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 44") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107Q7bar <- ggplot(d107q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107Q8bar <- ggplot(d107q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 39") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107Q9bar <- ggplot(d107q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 43") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107q10bar <- ggplot(d107q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 39") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x107q11bar <- ggplot(d107q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 7", subtitle = "n = 40") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Age 8 #
x108q1bar <- ggplot(d108q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108Q2bar <- ggplot(d108q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108Q3bar <- ggplot(d108q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 23") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108Q4bar <- ggplot(d108q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x108Q5bar <- ggplot(d108q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108Q6bar <- ggplot(d108q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108Q7bar <- ggplot(d108q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108Q8bar <- ggplot(d108q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 22") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108Q9bar <- ggplot(d108q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108q10bar <- ggplot(d108q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x108q11bar <- ggplot(d108q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 8", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# Age 9 #
x109q1bar <- ggplot(d109q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109Q2bar <- ggplot(d109q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109Q3bar <- ggplot(d109q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109Q4bar <- ggplot(d109q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x109Q5bar <- ggplot(d109q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109Q6bar <- ggplot(d109q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109Q7bar <- ggplot(d109q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109Q8bar <- ggplot(d109q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109Q9bar <- ggplot(d109q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 21") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109q10bar <- ggplot(d109q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 20") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x109q11bar <- ggplot(d109q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 9", subtitle = "n = 19") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())




# Age 10 #
x110q1bar <- ggplot(d110q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110Q2bar <- ggplot(d110q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110Q3bar <- ggplot(d110q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110Q4bar <- ggplot(d110q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x110Q5bar <- ggplot(d110q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110Q6bar <- ggplot(d110q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 18") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110Q7bar <- ggplot(d110q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110Q8bar <- ggplot(d110q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110Q9bar <- ggplot(d110q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 15") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110q10bar <- ggplot(d110q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 17") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x110q11bar <- ggplot(d110q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 10", subtitle = "n = 16") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())





# Age 11 #
x111q1bar <- ggplot(d111q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 11") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111Q2bar <- ggplot(d111q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111Q3bar <- ggplot(d111q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111Q4bar <- ggplot(d111q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x111Q5bar <- ggplot(d111q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 11") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111Q6bar <- ggplot(d111q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 11") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111Q7bar <- ggplot(d111q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111Q8bar <- ggplot(d111q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111Q9bar <- ggplot(d111q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 12") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111q10bar <- ggplot(d111q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 13") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x111q11bar <- ggplot(d111q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 11", subtitle = "n = 10") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Age 12 #
x112q1bar <- ggplot(d112q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112Q2bar <- ggplot(d112q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112Q3bar <- ggplot(d112q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112Q4bar <- ggplot(d112q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x112Q5bar <- ggplot(d112q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112Q6bar <- ggplot(d112q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112Q7bar <- ggplot(d112q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112Q8bar <- ggplot(d112q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112Q9bar <- ggplot(d112q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112q10bar <- ggplot(d112q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x112q11bar <- ggplot(d112q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 12", subtitle = "n = 4") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Age 13 #
x113q1bar <- ggplot(d113q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113Q2bar <- ggplot(d113q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113Q3bar <- ggplot(d113q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113Q4bar <- ggplot(d113q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x113Q5bar <- ggplot(d113q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113Q6bar <- ggplot(d113q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 2") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113Q7bar <- ggplot(d113q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113Q8bar <- ggplot(d113q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113Q9bar <- ggplot(d113q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113q10bar <- ggplot(d113q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x113q11bar <- ggplot(d113q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 13", subtitle = "n = 3") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Age 14 #

x114q1bar <- ggplot(d114q1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x114Q2bar <- ggplot(d114q2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114Q3bar <- ggplot(d114q3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114Q4bar <- ggplot(d114q4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

x114Q5bar <- ggplot(d114q5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114Q6bar <- ggplot(d114q6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114Q7bar <- ggplot(d114q7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114Q8bar <- ggplot(d114q8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114Q9bar <- ggplot(d114q9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114q10bar <- ggplot(d114q10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
x114q11bar <- ggplot(d114q11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age 14", subtitle = "n = 1") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Age Unknown #
xUnknownq1bar <- ggplot(dUnknownq1tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownQ2bar <- ggplot(dUnknownq2tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 56") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownQ3bar <- ggplot(dUnknownq3tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 68") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownQ4bar <- ggplot(dUnknownq4tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 59") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

xUnknownQ5bar <- ggplot(dUnknownq5tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 68") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownQ6bar <- ggplot(dUnknownq6tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 67") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownQ7bar <- ggplot(dUnknownq7tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 69") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownQ8bar <- ggplot(dUnknownq8tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 65") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownQ9bar <- ggplot(dUnknownq9tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 67") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownq10bar <- ggplot(dUnknownq10tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 62") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
xUnknownq11bar <- ggplot(dUnknownq11tibble, aes(x = reorder(word, freq), y = freq, fill = freq)) +
  ggtitle("age unknown", subtitle = "n = 61") +
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label=freq), hjust=1.5, color="white", size=2) + 
  theme(axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold", color = "dark green"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#######################################################################################################################################################
# Titles for questions (copied from before) #
#######################################################################################################################################################

# 10 most frequent words - Top 10 most frequent words - Question 1 variable (in order to get 10 most frequent words - Top 10 most frequent words - Question key/n= square in grid)
q1nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 1: What is the coronavirus?", subtitle = "n = 371") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# 10 most frequent words - Question 2 variable (in order to get 10 most frequent words - Question key/n= square in grid)
q2nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 2: Who is the president/prime minister?", subtitle = "n = 331") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 3 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q3nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 3: How many days have we been in lockdown?", subtitle = "n = 383") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 4 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q4nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 4: Do you want to go back to school?", subtitle = "n = 353") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 5 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q5nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 5: Who is the first person you are going to hug when lockdown is over?", subtitle = "n = 377") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 6 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q6nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 6: Where is the first place you want to go?", subtitle = "n = 381") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 7 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q7nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 7: What do you think we can do to get rid of the coronavirus?", subtitle = "n = 385") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=11, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=9, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 8 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q8nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 8: Are your parents good teachers?", subtitle = "n = 360") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 9 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q9nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 9: How did the coronavirus start?", subtitle = "n = 384") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

# Top 10 most frequent words - Question 10 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q10nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 10: If you had to wear protective clothing to help you what would it be?", subtitle = "n = 356") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=11, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=9, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())


# Top 10 most frequent words - Question 11 variable (in order to get Top 10 most frequent words - Question key/n= square in grid)
q11nameKeygraph <- ggplot() + 
  ggtitle("Top 10 most frequent words - Question 11: Are you enjoying the lockdown?", subtitle = "n = 339") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        plot.title = element_text(size=15, color="dark green", hjust = 0.5, vjust = -10),
        plot.subtitle = element_text(size=12, color = "dark blue", hjust = 0.5, vjust = -20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor=element_blank())

#######################################################################################################################################################
# Bar charts grids #
#######################################################################################################################################################

library(ggplot2)
library(dplyr) 

ggarrange(x102q1bar, x103q1bar, x104q1bar, x105q1bar, x106q1bar, x107q1bar, x108q1bar, x109q1bar, x110q1bar, x111q1bar, x112q1bar, x113q1bar, x114q1bar, xUnknownq1bar, q1nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102Q2bar, x103Q2bar, x104Q2bar, x105Q2bar, x106Q2bar, x107Q2bar, x108Q2bar, x109Q2bar, x110Q2bar, x111Q2bar, x112Q2bar, x113Q2bar, x114Q2bar, xUnknownQ2bar, q2nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102Q3bar, x103Q3bar, x104Q3bar, x105Q3bar, x106Q3bar, x107Q3bar, x108Q3bar, x109Q3bar, x110Q3bar, x111Q3bar, x112Q3bar, x113Q3bar, xUnknownQ3bar, q3nameKeygraph + rremove("x.text"), #x114Q3bar,
          ncol = 5, nrow = 3)
ggarrange(x102Q4bar, x103Q4bar, x104Q4bar, x105Q4bar, x106Q4bar, x107Q4bar, x108Q4bar, x109Q4bar, x110Q4bar, x111Q4bar, x112Q4bar, x113Q4bar, xUnknownQ4bar, q4nameKeygraph + rremove("x.text"), #x114Q4bar
          ncol = 5, nrow = 3)
ggarrange(x102Q5bar, x103Q5bar, x104Q5bar, x105Q5bar, x106Q5bar, x107Q5bar, x108Q5bar, x109Q5bar, x110Q5bar, x111Q5bar, x112Q5bar, x113Q5bar, x114Q5bar, xUnknownQ5bar, q5nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102Q6bar, x103Q6bar, x104Q6bar, x105Q6bar, x106Q6bar, x107Q6bar, x108Q6bar, x109Q6bar, x110Q6bar, x111Q6bar, x112Q6bar, x113Q6bar, x114Q6bar, xUnknownQ6bar, q6nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102Q7bar, x103Q7bar, x104Q7bar, x105Q7bar, x106Q7bar, x107Q7bar, x108Q7bar, x109Q7bar, x110Q7bar, x111Q7bar, x112Q7bar, x113Q7bar, x114Q7bar, xUnknownQ7bar, q7nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102Q8bar, x103Q8bar, x104Q8bar, x105Q8bar, x106Q8bar, x107Q8bar, x108Q8bar, x109Q8bar, x110Q8bar, x111Q8bar, x112Q8bar, x113Q8bar, x114Q8bar, xUnknownQ8bar, q8nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102Q9bar, x103Q9bar, x104Q9bar, x105Q9bar, x106Q9bar, x107Q9bar, x108Q9bar, x109Q9bar, x110Q9bar, x111Q9bar, x112Q9bar, x113Q9bar, x114Q9bar, xUnknownQ9bar, q9nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102q10bar, x103q10bar, x104q10bar, x105q10bar, x106q10bar, x107q10bar, x108q10bar, x109q10bar, x110q10bar, x111q10bar, x112q10bar, x113q10bar, x114q10bar, xUnknownq10bar, q10nameKeygraph + rremove("x.text"), 
          ncol = 5, nrow = 3)
ggarrange(x102q11bar, x103q11bar, x104q11bar, x105q11bar, x106q11bar, x107q11bar, x108q11bar, x109q11bar, x110q11bar, x111q11bar, x112q11bar, x114q11bar, xUnknownq11bar, q11nameKeygraph + rremove("x.text"), #x113q11bar
          ncol = 5, nrow = 3)


#######################################################################################################################################################
# Word cloud grids #
#######################################################################################################################################################

# Question 1: What is the coronavirus # 
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q1$word, d102q1$freq, max.word=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q1$word, d103q1$freq, max.words=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q1$word, d104q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q1$word, d105q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q1$word, d106q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q1$word, d107q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q1$word, d108q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q1$word, d109q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q1$word, d110q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q1$word, d111q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q1$word, d112q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q1$word, d113q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q1$word, d114q1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .40)) +
  wordcloud(dUnknownq1$word, dUnknownq1$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 2: Who is the president/prime minister? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q2$word, d102q2$freq, max.word=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q2$word, d103q2$freq, max.words=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q2$word, d104q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q2$word, d105q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q2$word, d106q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q2$word, d107q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q2$word, d108q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q2$word, d109q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q2$word, d110q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q2$word, d111q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q2$word, d112q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q2$word, d113q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q2$word, d114q2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq2$word, dUnknownq2$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 3: How many days have we been in lockdown? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q3$word, d102q3$freq, max.word=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q3$word, d103q3$freq, max.words=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q3$word, d104q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q3$word, d105q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q3$word, d106q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q3$word, d107q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q3$word, d108q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q3$word, d109q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q3$word, d110q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q3$word, d111q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q3$word, d112q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q3$word, d113q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  #wordcloud(d114q3$word, d114q3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .40)) +
  wordcloud(dUnknownq3$word, dUnknownq3$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 4: Do you want to go back to school? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q4$word, d102q4$freq, max.word=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q4$word, d103q4$freq, max.words=10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q4$word, d104q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q4$word, d105q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q4$word, d106q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q4$word, d107q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q4$word, d108q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q4$word, d109q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q4$word, d110q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q4$word, d111q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q4$word, d112q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q4$word, d113q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  #wordcloud(d114q4$word, d114q4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq4$word, dUnknownq4$freq, max.words=10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 5: Who is the first person you are going to hug when lockdown is over? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q5$word, d102q5$freq, max.words = 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q5$word, d103q5$freq, max.words= 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q5$word, d104q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q5$word, d105q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q5$word, d106q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q5$word, d107q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q5$word, d108q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q5$word, d109q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q5$word, d110q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q5$word, d111q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q5$word, d112q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q5$word, d113q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q5$word, d114q5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq5$word, dUnknownq5$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 6: Where is the first place you want to go? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q6$word, d102q6$freq, max.words = 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q6$word, d103q6$freq, max.words= 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q6$word, d104q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q6$word, d105q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q6$word, d106q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q6$word, d107q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q6$word, d108q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q6$word, d109q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q6$word, d110q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q6$word, d111q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q6$word, d112q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q6$word, d113q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q6$word, d114q6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq6$word, dUnknownq6$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 7: What do you think we can do to get rid of the coronavirus? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q7$word, d102q7$freq, max.words = 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q7$word, d103q7$freq, max.words= 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q7$word, d104q7$freq, max.words= 10, min.freq = 1,random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q7$word, d105q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q7$word, d106q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q7$word, d107q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q7$word, d108q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q7$word, d109q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q7$word, d110q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q7$word, d111q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q7$word, d112q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q7$word, d113q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q7$word, d114q7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq7$word, dUnknownq7$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 8: Are your parents good teachers? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q8$word, d102q8$freq, max.words = 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q8$word, d103q8$freq, max.words= 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q8$word, d104q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q8$word, d105q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q8$word, d106q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q8$word, d107q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q8$word, d108q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q8$word, d109q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q8$word, d110q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q8$word, d111q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q8$word, d112q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q8$word, d113q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q8$word, d114q8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq8$word, dUnknownq8$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 9: How did the coronavirus start? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q9$word, d102q9$freq, max.words = 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q9$word, d103q9$freq, max.words= 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q9$word, d104q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q9$word, d105q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q9$word, d106q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q9$word, d107q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q9$word, d108q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q9$word, d109q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q9$word, d110q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q9$word, d111q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q9$word, d112q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q9$word, d113q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q9$word, d114q9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq9$word, dUnknownq9$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 10: If you had to wear protective clothing to help you what would it be? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q10$word, d102q10$freq, max.words = 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q10$word, d103q10$freq, max.words= 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q10$word, d104q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q10$word, d105q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q10$word, d106q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q10$word, d107q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q10$word, d108q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q10$word, d109q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q10$word, d110q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q10$word, d111q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q10$word, d112q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d113q10$word, d113q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q10$word, d114q10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq10$word, dUnknownq10$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))

# Question 11: Are you enjoying lockdown? #
par(mfrow=c(3, 5)) # for 3 rows, 5 cols
wordcloud(d102q11$word, d102q11$freq, max.words = 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d103q11$word, d103q11$freq, max.words= 10, min.freq = 1, random.order=FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) + 
  wordcloud(d104q11$word, d104q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d105q11$word, d105q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d106q11$word, d106q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d107q11$word, d107q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d108q11$word, d108q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d109q11$word, d109q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d110q11$word, d110q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d111q11$word, d111q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d112q11$word, d112q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  #wordcloud(d113q11$word, d113q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(d114q11$word, d114q11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5)) +
  wordcloud(dUnknownq11$word, dUnknownq11$freq, max.words= 10, min.freq = 1, random.order =FALSE, rot.per=0.0, colors=brewer.pal(8, "Dark2"), scale=c(1, .5))







# Still more to do, but for now, this is what I've got #
# Special thanks to COVID, public Facebook, Martin, and Levana #