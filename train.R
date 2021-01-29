# install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gtsummary")
install.packages("gt")
webshot::install_phantomjs()

# load libraries
library(ggplot2)
library(dplyr)
library(gtsummary)
library(gt)
library(webshot)

# check a working directory
getwd()

# read "train.csv" file and avoid converting strings to factors
train <- read.csv("train.csv ", stringsAsFactors = FALSE)

# get the name of factors
names(train)

# take a look at the structure of the dataset
str(train)

# summarize the data set
summary(train)

# summarize missing values in each column
colSums(is.na(train))
colSums(train == "")

# replace empty strings with NA 
train <- train %>%
  mutate_all(na_if,"")

# check missing values again
colSums(is.na(train))

## create the table
sum <- train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(
    Pclass = case_when(Pclass == "1" ~ "1st",
                       Pclass == "2" ~ "2nd",
                       Pclass == "3" ~ "3rd"),
    Embarked = case_when(Embarked == "C" ~ "Cherbourg",
                         Embarked == "Q" ~ "Queenstown",
                         Embarked == "S" ~ "Southampton")
    )

t1 <- sum %>% 
  tbl_summary(
    by = Survived,
    missing = "no"
    ) %>%
  add_n() %>%
  modify_header(update = list(
    label ~ "**Variables**",
    stat_1 ~ "**No**, (N = 549)",
    stat_2 ~ "**Yes** (N = 342)")) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Survived**") %>%
  bold_labels()

# print the table
print(t1)

# save the table as image
t1 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "Table1.png"
  )

## Survived
# bar plot
survival <- train %>%
  count(Survived) 

ggplot(survival, aes(x = as.factor(Survived), y = n, fill = as.factor(Survived))) +
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = -0.5) +
  theme(legend.position = "none") +
  xlab("Survived") +
  ylab("Count (n)") +
  scale_fill_manual(values = c("#FF6666", "#00B492")) +
  scale_x_discrete(label = c("0" = "No","1" = "Yes"))
  
## Sex 
# summarize sex data
train %>%
  count(Sex) %>%
  mutate(pct = round(n/sum(n)*100))

# bar plot
gender <-
  train %>%
  group_by(Sex) %>%
  count(Survived)

ggplot(gender, aes(x = Sex, y = n, fill = as.factor(Survived))) +
  geom_bar(position="dodge", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5,
            position = position_dodge(width = 0.9)) +
  ylab("Count (n)") +
  labs(fill = "Survived") +
  scale_fill_manual(values = c("#FF6666", "#00B492"),
                    label = c("No","Yes" )) +
  scale_x_discrete(label = c("female" = "Female", "male" = "Male"))

## Pclass
# summarize ticket class data
train %>%
  count(Pclass) %>%
  mutate(tpct = round(n/sum(n)*100))

# bar plot
ticket <- train %>%
  group_by(Pclass) %>%
  count(Survived)

ggplot(ticket, aes(x = as.factor(Pclass), y = n, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = n),vjust = -0.5, 
            position = position_dodge(width = 0.9)) +
  xlab("Pclass") +
  ylab("Count (n)") +
  labs(fill = "Survived") +
  scale_fill_manual(values = c("#FF6666", "#00B492"),
                    label = c("No","Yes" )) +
  scale_x_discrete(label = c("1" = "1st", "2" = "2nd", "3" = "3rd"))

## Embarked
port <- train %>%
  group_by(Embarked) %>%
  count(Survived)

# remove missing value rows
port <- na.omit(port)

# bar plot
ggplot(port, aes(x = Embarked, y = n, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = n),vjust = -0.5, 
            position = position_dodge(width = 0.9)) +
  ylab("Count (n)") +
  labs(fill = "Survived") +
  scale_fill_manual(values = c("#FF6666", "#00B492"),
                    label = c("No","Yes" )) +
  scale_x_discrete(label = c("C" = "Cherbourg","Q" = "Queenstown",
                             "S" = "Southampton")) 

## Age
# remove NA in Age column
life <- train %>%
  filter(!is.na(Age))
  
# multi-density chart
ggplot(life, aes(x = Age)) +
  geom_density(aes(fill = as.factor(Survived)), alpha = 0.5) +
  scale_fill_manual(values = c("#FF6666", "#00B492"),
                    label = c("No","Yes" )) +
  theme(legend.position = c(0.85, 0.55),
        legend.background = element_rect(fill = "white", 
                                         color = "black")) +
  xlab("Age (Years)") +
  ylab("Density") +
  labs(fill = "Survived") 


## SibSp
counsin <- train %>%
  group_by(SibSp) %>%
  count(Survived)

# bar plot
ggplot(counsin, aes(x = as.factor(SibSp), y = n, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, 
            position = position_dodge(width = 1)) +
  xlab("SibSp (n)") +
  ylab("Count (n)") +
  labs(fill = "Survived") +
  scale_fill_manual(values = c("#FF6666", "#00B492"),
                    label = c("No","Yes" )) 

## Parch
family <- train %>%
  group_by(Parch) %>%
  count(Survived)

# bar plot  
ggplot(family, aes(x = as.factor(Parch), y = n, fill = as.factor(Survived))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, 
            position = position_dodge(width = 1)) +
  xlab("Parch (n)") +
  ylab("Count (n)") +
  labs(fill = "Survived") +
  scale_fill_manual(values = c("#FF6666", "#00B492"),
                    label = c("No","Yes" ))

## Fare
ggplot(train, aes(x = Fare)) +
  geom_density(aes(fill = as.factor(Survived)),alpha = 0.5) +
  scale_fill_manual(values = c("#FF6666", "#00B492"),
                    label = c("No","Yes" )) +
  theme(legend.position = c(0.85, 0.55),
        legend.background = element_rect(fill = "white", color = "black")) +
  ylab("Density") +
  labs(fill = "Survived") 

  
  


  
  
  


  





  


  

  



