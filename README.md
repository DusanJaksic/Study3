---
title: 'Case Study 1: How Can a Wellness Technology Company Play It Smart?'
author: "Dusan Jaksic"
date: "2023-09-08"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# **Introduction** 

<img src="pexels-cottonbro-studio-3927391.jpg" alt="Image" style="width:500px; hight:500; float:right; margin:0 0 10px 10px;"/>


<br/><br/><br/>


[Bellabeat]( https://bellabeat.com/) is a company that develops fitness products for women. Their products include smart water bottles, fashionable fitness watches, jewelry, and yoga mats. Users can access their health data collected through these devices in the Bellabeat app.

 Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help unlock new growth opportunities for the company. Urška Sršen is confident that an analysis of non-Bellebeat consumer data (ie. FitBit fitness tracker usage data) would reveal more opportunities for growth. The company hopes to use these insights to help guide new marketing strategies for the company. 

<br/><br/><br/>

# **Ask**


## Key stakeholders

1. Urška Sršen: Bellabeat’s cofounder and Chief Creative Officer

2. Sando Mu: Mathematician and Bellabeat’s cofounder

3. The Bellabeat marketing analytics team: a team of data analysts responsible for collecting, analyzing, and reporting data that helps guide Bellabeat’s marketing strategy.



## *Business task*

Analyze non-Bellabeat smart device data and compare with one Bellabeat product to discover insights to help guide marketing strategies for the company.


### Business Objectives:

1. What are some trends in smart device usage?
2. How could these trends apply to Bellabeat customers?
3. How could these trends help influence Bellabeat marketing strategy?

<br/><br/><br/>

# **Prepare** 


FitBit Fitness Tracker Data on [Kaggle]( https://www.kaggle.com/datasets/arashnic/fitbit) in 18 CSV files. The data contains smart health data from personal fitness trackers for thirty fitbit users. The data was collected via a survey of personal tracker data, including minute-level output for physical activity, hear rate, and sleep monitoring, through Amazon Mechanical Turk between March 12, 2016 and May 12, 2016. It was updated two years ago as of August 2022. The data includes information about daily activity, steps, and heart rate. 

### Limitations: 
 

* Data is collected 7 years ago in 2016. Since then, some thing could have changed as users’       daily activity, fitness and sleeping habits, diet and food consumption. Data might be out of     date and irrelevant. 
* The sample size is small as only 30 individuals were considered, so it is not representative of   the entire fitness population.
* Since the data was collected through a survey, the results might not be accurate because         participants could give a misleading answers.

<br/><br/><br/>

# **Process**

In this phase we will process the data by cleaning and ensuring that it is correct,relevant,complete and error free.

* We have to check if data contains any missing or null values
* Transform the data into format we want for the analysis

### Application

I used RStudio for data cleaning, data transformation, data analysis, and visualization.

As `dailyActivity_merged.csv ` provides a good summary of steps and calories burned and the `sleepDay_merged.csv` file provides sleep data these are good overall files to use to analyze patricipant usage. As fitness devices are generally used to track overall health and weight, the file `weightLogInfo_merged` containing weight data will also be used. 

We need to install and read the packages that we need for analysis. All the packages were installed, so I will read all the packages simultaneously.

### Load library and files 

```{r}

library(tidyverse)
```

```{r}
library(lubridate)
```

```{r}
library(ggplot2)            
library(dplyr)               
library(skimr)              
library(sqldf)               

```

```{r}
library(janitor)
```

```{r}
require(forcats)
library(openxlsx)
library(plotrix)
```


```{r}
day_activity <-read_csv("daily_Activity_merged.csv")
sleep <-read_csv("sleepDay_merged.csv")
weight <-read_csv("weightLogInfo_merged.csv")
```



#### Check that the data has loaded correctly. 
#### We need to check if there are any null or missing values in the data.


```{r}
str(day_activity)
str(sleep)
str(weight)
skim(day_activity)
skim(sleep)
skim(weight)
head(day_activity)
head(sleep)
head(weight)
```

#### After executing these commands we discovered:

* Number of records and columns
* Number of null and non null values
* Data type of every columns

There are 940 records in activity data, 413 in sleep and 67 in weight data. There are no null values present in any of the data set, therefore there is no requirement to clean the data. Only the date column is in character format, so it has to be  converted into datetime type.


```{r}
day_activity$Rec_Date <- as.Date(day_activity$ActivityDate,"%m/%d/%y")
day_activity$month <- format(day_activity$Rec_Date,"%B")
day_activity$day_of_week <- format(day_activity$Rec_Date,"%A")
```
#### Now, we are going to count unique IDs to confirm whether data has 30 IDs as claimed by the survey.

```{r}
n_distinct(day_activity$Id)
```


+ There are 33 unique IDs,comparing to 30 unique IDs that was announced. Some users might have created additional IDs during the survey period.

With this being said, the data cleaning and manipulation is done. We can move towards analyzing data.

<br/><br/><br/>

# **Analyze**

## Summary statistics 


```{r}
day_activity %>%  select(TotalSteps,TotalDistance,SedentaryMinutes,VeryActiveMinutes) %>% summary()
```


+ The average count of recorded steps is 7638 which is less than recommended 10000 steps and average of total distance covered is 5.490 km which is also less than recommended 8 km mark.
The average sedentary minutes is 991.2 minutes or 16.52 hours which is very high as it should be at most 7 hours.Even if you are doing enough physical activity, sitting for more than 7 to 10 hours a day is bad for your health. (source: HealthyWA article).
The average of very active minutes is 21.16 which is less than target of 30 minutes per day. (source:verywell fit)


```{r}
weight %>%  select(WeightKg,BMI) %>% summary()
```


+ We can not conclude healthiness of person just by knowing there weight, There are other factors like height,fat that can have an impact on the health.
The average of BMI is 25.19 which is slightly grater than the healthy BMI range which is between 18 and 24.9.

```{r}
Avg_minutes_asleep <- sqldf("SELECT SUM(TotalSleepRecords),SUM(TotalMinutesAsleep)/SUM(TotalSleepRecords) as avg_sleeptime
                            FROM sleep")
Avg_minutes_asleep
```

```{r}
Avg_TimeInBed <- sqldf("SELECT SUM(TotalTimeInBed), SUM(TotalTimeInBed)/SUM(TotalSleepRecords) as avg_timeInBed
                       FROM sleep")

Avg_TimeInBed
```

+ There is difference of 35 minutes between time in bed and sleep time which means that it takes on an average 20 to 30 minutes to fall asleep for people.
We will also calculate number of distinct records in sleep and weight data.


```{r}
n_distinct(sleep$Id)
```


```{r}
n_distinct(weight$Id)
```

<br/><br/><br/>

# **Share**


Now I will create some visualizations based on an analysis and objective of the project.


```{r}
day_activity$day_of_week <- ordered(day_activity$day_of_week,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

ggplot(data=day_activity) + geom_bar(mapping = aes(x=day_of_week),fill="green") +
  labs(x="Day of week",y="Count",title="Usage of the tracker during the week")
```

+ The frequency of usage of FitBit fitness tracker application is high on sunday, monday and tuesday. The reason can be related to different stress levels during the week when everyone is under pressure comparing to end and beginning of the week, when the pressure is less present.

```{r}
mean_steps <- mean(day_activity$TotalSteps)
mean_steps
```
```{r}
mean_calories <- mean(day_activity$Calories)
mean_calories
```

### Total Steps vs Sedentary Minutes

```{r}
ggplot(data=day_activity, aes(x=TotalSteps, y=SedentaryMinutes, color = Calories)) + geom_point() +
geom_smooth(method = "loess",color="green") + 
labs(x="Total Steps",y="Sedentary Minutes",title="Total Steps vs Sedentary Minutes")
```

+ Explanation:
When the total steps are less than 10000 the relation between them is inverse, but as number of steps increases above 10000 there is no big change in relation.
Also, the relation between steps and sedentary minutes after 15000 steps became more positive.

### Active Minutes vs Burned Calories


```{r}
ggplot(data=day_activity,aes(x = VeryActiveMinutes, y = Calories, color = Calories)) + geom_point() + 
geom_smooth(method = "loess",color="purple") +
labs(x="Very Active Minutes",y="Calories",title = "Very Active Minutes vs Burned Calories")
```


+ Very active minutes and burned calories are correlated with each other adding some outliers at bottom left and top left of the plot.


I will calculate now the sum of individual minute column from daily activity data.


```{r}
activity_min <- sqldf("SELECT SUM(VeryActiveMinutes),SUM(FairlyActiveMinutes),
      SUM(LightlyActiveMinutes),SUM(SedentaryMinutes)
      FROM day_activity")
activity_min

```


I will use these values to plot a 3D pie chart to compare the percentage of activity by minutes.


```{r}
x <- c(19895,12751,181244,931738)
x
piepercent <- round(100*x / sum(x), 1)
colors = c("purple","yellow","green","lightblue")
 
pie3D(x,labels = paste0(piepercent,"%"),col=colors,main = "Percentage of Activity in Minutes")
legend("topright",c("VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","SedentaryMinutes"),cex=0.6,fill = colors)
```

+ The percentage of sedentary minutes is very high compared to all the other activities. It covers 81.3 % of pie,which indicates that people are inactive for longer period of time.
+ The percentage of very active and fairly active minutes (1.7% and 1.1%) is very low compared to other activities.

<br/><br/><br/>

# **Act**


The goal of analysis is correct as we got many useful insights from the FitBit data,which will help us to make data driven decision making. Both companies develop similar kind of products.So,the common trends surrounding health and fitness can also be applied to Bellabeat customers.

Based on the analysis I have following recommendations:


+ People prefer to track their activities on sunday, monday and tuesday than other week days.I think this behaviour is because people get busier in week end days due to work pressure and they don’t get enough time to track their activity.That’s why people are more active on sunday and starting 2 days of week.

+ Analysis shows that most of the people use application to track the steps and burned calories. Much lower number of people use it to track sleep and very few use it to track weight records.So, I would recommend to focus on steps,calories and sleep tracking more in application.

+ The relation between very active minutes vs burned calories shows positive correlation.So, this can be a good marketing strategy.

+ Majority of users 81.3% who are using the FitBit app are inactive for longer period of time and not using it for tracking their health habits.So, this can be a great chance to use this information for market strategy as Bellabeat can alert people about their sedentary behaviour time to time either on application or on tracker itself .



Bellabeat marketing team can encourage users by educating them about fitness benefits, calories consumption and burn rate information, and suggest different types of exercises on Bellabeat application.




