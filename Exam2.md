---
title: "Exam"
author: "Clay Cavanaugh"
date: "6/26/2020"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r include=TRUE}
#clear the environment
rm(list=ls(all=TRUE))

#loading dataset
library(rio)
inequality_data = import("inequality.xlsx", which = 1)

head(inequality_data)


#The dataset is cross sectional because the data is all observed within one time frame. In this case, the data set is pulled from 2015. 


```{r include=TRUE}
#clear the environment
rm(list=ls(all=TRUE))

#loading dataset
library(rio)
inequality_data = import("inequality.xlsx", which = 1)

#observing data frame
head(inequality_data)
View(inequality_data)
summary(inequality_data)

library(dplyr)
library(tidyverse)
library(ggplot2)

#subsetting 
subset(inequality_data, inequality_gin)
inequality_data$inequality_gini

#no clue how to do this  so skipping 4 and 5

#6 It would be better to have a low inequality score

#quick peak at data frame
head(inequality_data)

#creating function to remove accent on Belarus
accent.remove <- function(s) {
  old1 <- "ú"
  new1 <- "u"
  s1 <- chartr(old1,new1,s)
}

inequality_data$country <- accent.remove(inequality_data$country)

#confirming function executed properly
head(inequality_data)

#sorting data by lowest inequality_gini
inequality_data <- inequality_data[order(inequality_data$inequality_gini),]

#viewing head command to confirm
head(inequality_data)

#to show the mean of inequality gini
summary(inequality_data)
mean(inequality_data$inequality_gini, na.rm=TRUE)
## 10. The Mean Inequality gini is 36.81- both methods show mean

##ifelse 
low_inequality <
ifelse(test = inequality_data$inequality_gini==1, yes = high_inequality, no = low_inequality)
#did not work

##cross tab
library(doBy)
summaryBy(inequality_gini, data=inequality_data, FUN=c(mean(length))
#cross tab did not work either

#for loop for World Bank
World <- c('The World Bank','African Development Bank','Bill and Melinda Gates Foundation'),
for (i in World){
  print(i)
}

##14
#I picked GDP per capita because I believe that a country's overall economic output
#will correlate well with the country's inequality standards.

#importing variable
library(WDI)
gdp = WDI(indicator='NY.GDP.PCAP.KD', country=all, start=2015, end=2015,
          extra=FALSE, cache= NULL)
##cannot figure  out how to import...
WDIsearch('gdp')

dat = WDI(indicator='NY.GDP.PCAP.KD', country=c('MX','CA','US'), start= 2015, end =2015)
head(dat)

#renaming variable
GDP_per <- dat

#merging data frames
merged_df = left_join(inequality_data, 
                        GDP_per,
                        by = c("country", "year"))
merged_df
#verifying data frames

#removing missing data on inequality_gini
library(tidyverse)
subset(merged_df, inequality_gini=="NA")
subset(merged_df, GDP_per=="NA")

#filtering inequality gini scores
merged_df <- 
  merged_df %>%
  dplyr::filter(!(inequality_gini>=30))

#counting how many countries 
data_greater_30 <- merged_df
data_greater_30 <- 
  dat_greater_30 %>%
  mutate(country = ifelse(country == country"ai"
                                  "yes",
                                  "no"))
##unsure how to do this


#labelling variables
library(labelled)
var_label(data_greater_30) <- list(`country` = "country",
                               `inequality_gini` = "in",
                               `primary_enroll` = "gross primary enrollment by population",
                               `iso2c.x` = "ISO-2 country code",
                               `year` = "year"
                               )
                               ```
                               




