library(tidyverse)
require(stargazer)
library(stats)
library(dplyr)
library(janitor)
library(boot)
library(knitr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(randomForest)
library(gbm)
library(Hmisc)

# Introduction: The goal of this script is to explore the basic trends in the data, especially regarding coup success and violence.
# These outcomes are coded in the dataset as `realized_coup` and `were_others_than_the_incumbent_killed`.

## Build a dataframe: coup_df

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

coup_csv <- read_csv("coupdata.csv")
colnames(coup_csv) %<>% stringr::str_replace_all("\\s","_") %>% tolower

coup_df <- data.frame(coup_csv)
colnames(coup_df)
coup_df <- coup_df %>%
  filter(!is.na(realized_coup))

coup_df <- coup_df %>% clean_names()

sum(is.na(coup_df))

## Manually edit colnames of dataframe

head(coup_df)

colnames(coup_df)[which(names(coup_df) == "ambigious_coup")] <- "ambiguous_coup"
colnames(coup_df)[which(names(coup_df) == "were_others_then_the_incumbent_injured")] <- "were_others_than_the_incumbent_injured"

## Convert character columnns to numeric

# The "ordinary citizens" variable has missing values. To use this variable, we can assume that few ordinary citizens
# participate in these coups. Alternatively, we can leave in the NULLs; in that case, R will create predictors for "0" and "1"

typeof(coup_df$were_ordinary_citizens_involved)
coup_df[["were_ordinary_citizens_involved"]][is.na(coup_df["were_ordinary_citizens_involved"])] <- 0
coup_df$were_ordinary_citizens_involved <- as.numeric(coup_df$were_ordinary_citizens_involved)

coup_df <- coup_df %>%
  mutate(were_ordinary_citizens_involved = (is.na(were_ordinary_citizens_involved) <- 0)) %>%
  mutate(were_ordinary_citizens_involved = as.numeric(were_ordinary_citizens_involved))

sum(is.na(coup_df$were_ordinary_citizens_involved))

# If necessary, code coup type as numeric instead of "conspiracy", "attempt," etc.
coup_df <- coup_df %>%
  mutate(coup_type_numeric = type_of_coup)

coup_df$coup_type_numeric <- gsub("conspiracy", "1", coup_df$coup_type_numeric)
coup_df$coup_type_numeric <- gsub("attempt", "2", coup_df$coup_type_numeric)
coup_df$coup_type_numeric <- gsub("coup", "3", coup_df$coup_type_numeric)
coup_df$coup_type_numeric <- as.numeric(coup_df$coup_type_numeric)

coup_df$type_of_coup

## Create season variables

coup_df$month_of_event
typeof(coup_df$month_of_event)

coup_df <- coup_df %>%
  mutate(winter = case_when(
    month_of_event == (12 | 1:2) ~ 1,
    TRUE ~ 0))

coup_df <- coup_df %>%
  mutate(spring = case_when(
    month_of_event == (3:5) ~ 1,
    TRUE ~ 0))

coup_df <- coup_df %>%
  mutate(summer = case_when(
    month_of_event == (6:8) ~ 1,
    TRUE ~ 0))

coup_df <- coup_df %>%
  mutate(autumn = case_when(
    month_of_event == (9:11) ~ 1,
    TRUE ~ 0))

sum(coup_df$summer) == sum(coup_df$spring)

## Create decade variables

typeof(coup_df$year)
as.numeric(coup_df$year)
coup_df$year

coup_df <- coup_df %>%
  mutate(forties = case_when(
    (year > 1939) & (year < 1950) ~ 1,
    TRUE  ~ 0)) %>%
  mutate(fifties = case_when(
    (year > 1949) & (year < 1960) ~ 1,
    TRUE ~ 0)) %>%
  mutate(sixties = case_when(
    (year > 1959) & (year < 1970) ~ 1,
    TRUE  ~ 0)) %>%
  mutate(seventies = case_when(
    (year > 1969) & (year < 1980) ~ 1,
    TRUE  ~ 0)) %>%
  mutate(eighties = case_when(
    (year > 1979) & (year < 1990) ~ 1,
    TRUE  ~ 0)) %>%
  mutate(nineties = case_when(
    (year > 1989) & (year < 2000) ~ 1,
    TRUE  ~ 0)) %>%
  mutate(aughties = case_when(
    (year > 1999) & (year < 2010) ~ 1,
    TRUE  ~ 0))

sum(coup_df$forties)
sum(coup_df$fifties)
sum(coup_df$sixties)
sum(coup_df$seventies)
sum(coup_df$eighties)
sum(coup_df$nineties)
sum(coup_df$aughties)

# Use select(-x) to drop variables that are irrrelevant to coup success

coup_df <- coup_df %>% select(-x54, -x53, -cow_code, -day_of_event, -coup_id, -month_of_event, -type_of_coup)
coup_df[["country"]][is.na(coup_df["country"])] <- 0

sum(is.na(coup_df))


### Data Exploration

## Means

mean(coup_df$realized_coup)

mean(coup_df$were_others_than_the_incumbent_killed)

coup_df %>%
  summarise(mean())

coup_df %>% filter(year != 0) %>% summarise(min(year))

coup_df %>% filter(year != 0) %>% summarise(max(year))

coup_df %>% filter(coup_conspiracies == 1) %>% summarise(mean(realized_coup))

coup_df %>% filter(coup_conspiracies == 1) %>% summarise(mean(were_others_than_the_incumbent_killed))

# First year = 1946

# Coups by country

mean_country <- coup_df %>% group_by(country) %>% summarise((mean(realized_coup)))
mean_country %>% sort((mean(realized_coup)), decreasing = TRUE)

# Violence in successful coups
coup_df %>% filter(realized_coup == 1) %>% summarise(mean(were_others_than_the_incumbent_killed))
coup_df %>% filter(realized_coup == 1) %>% summarise(mean(were_others_than_the_incumbent_injured))

# Violence in attempted, but failed coups
coup_df %>% filter(coup_type_numeric == 2) %>% summarise(mean(were_others_than_the_incumbent_killed))
coup_df %>% filter(coup_type_numeric == 2) %>% summarise(mean(were_others_than_the_incumbent_injured))

# Egypt
coup_df %>% filter(country == "Egypt") %>% filter(year == 1954) %>% summarise(sum(cow_code))

# Egypt had four coup events in 1954

## Correlations

corr_df <- coup_df %>% select(-country, -year, -ambiguous_coup, -coup_conspiracies, -unrealized)
corr_df <- cor(corr_df)
corr_df <- data.frame(corr_df)

# Realized coup correlations

cor.test(coup_df$attempted_coup, coup_df$were_others_than_the_incumbent_killed, method = "kendall")
cor.test(coup_df$realized_coup, coup_df$were_students_or_academics_involved_in_the_coup, method = "kendall")
cor.test(coup_df$realized_coup, coup_df$were_military_actors_involved_in_the_coup, method = "kendall")
cor.test(coup_df$realized_coup, coup_df$military_coup, method = "kendall")

real_corrs <- corr_df %>% select(realized_coup)
real_corrs <- tibble::rownames_to_column(real_corrs, "variables")

# real_corrs <- real_corrs %>% sort(realized_coup, decreasing = TRUE)

real_corrs <- real_corrs[order(-real_corrs$realized_coup),]
real_corrs <- real_corrs[3:56,]
real_corrs <- real_corrs %>%
  filter(variables != "were_ordinary_citizens_involved") %>%
  filter(variables != "attempted_coup")

real_plot <- ggplot(real_corrs, aes(x = reorder(variables, -realized_coup), y = realized_coup)) + geom_col() +
                      ylab("Coup Success") + xlab("Coup Features") + ggtitle("Correlations: Coup Features => Success")
real_plot <- real_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
ggsave(real_plot, filename = "Success_Corrs.jpeg", height = 8, width = 8)

# real_corrs %>% filter(realized_coup > 0.1)
# real_corrs %>% filter(realized_coup < -0.01)
# write.csv(real_corrs,"real_corrs.csv")

# Violent coup correlations

violence_corrs <- corr_df %>% select(were_others_than_the_incumbent_killed)
violence_corrs <- tibble::rownames_to_column(violence_corrs, "variables")
violence_corrs <- violence_corrs %>%
  filter(variables != "were_others_than_the_incumbent_killed") %>%
  filter(variables != "were_ordinary_citizens_involved") %>%
  filter(variables != "coup_type_numeric")
violence_corrs <- violence_corrs[order(-violence_corrs$were_others_than_the_incumbent_killed),]
violence_corrs

violence_plot <- ggplot(violence_corrs, aes(x = reorder(variables, -were_others_than_the_incumbent_killed), 
                                            y = were_others_than_the_incumbent_killed)) +  geom_col() +  ylab("Coup Violence") + 
                                            xlab("Coup Features") + ggtitle("Correlations: Coup Features => Violence")
violence_plot <- violence_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()
ggsave(violence_plot, filename = "Violence_Corrs.jpeg", height = 8, width = 8)

violence_corrs %>% filter(were_others_than_the_incumbent_killed > 0.1)

violence_corrs %>% filter(were_others_than_the_incumbent_killed < -0.01)

# violence_corrs <- sort(violence_corrs$were_others_than_the_incumbent_killed, decreasing=TRUE, row.names=TRUE)

# write.csv(real_corrs,"violence_corrs", row.names = TRUE)
