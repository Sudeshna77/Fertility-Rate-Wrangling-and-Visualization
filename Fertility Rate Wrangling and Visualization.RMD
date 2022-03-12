---
title: "Assignment1-Sarkar-Fertility Rate Wrangling and Visualization"
author: "Sudeshna Sarkar"
date: "09/03/2022"
output:
  pdf_document: default
  html_document: default
---
## **Fertility Rate of Countries**

*The data used in this project will be used to study the fertility rates of various countries over the world in a given span of time.The total fertility rate in a specific year is defined as the total number of children that would be born to each woman if she were to live to the end of her child-bearing years.Through this project we will try to analyse the variation of fertility rates over the years for different countries and we will also see some of the anomalous behavious shown by some countries*

*Data source: "OECD data" https://data.oecd.org/pop/fertility-rates.htm*
*Years 1970-2020 *

*The original dataset has the following columns:*

*1. LOCATION :  Various countries over the world has been included in the data.*

*2. INDICATOR : Indicates the fertility.*

*3. TIME : FROM 1970 - 2020.*

*4. Value : Fertility index value.*


### Setting up the working directory and loading the packages.

```{r message=FALSE, warning=FALSE}
knitr::opts_chunk$set()

rm(list=ls())
getwd()


library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(ggthemes)
library(kableExtra)
```


### Loading the data in R. Reading the dataset and displying few records..


```{r message=FALSE, warning=FALSE}

df_xl = read_csv("Fertility Rate Wrangling and Visualization.csv")
kable(head(df_xl,10),format="html",table.attr="style='width:30%;'") %>% 
  kableExtra::kable_styling()


```

### Taking a glimpse of the dataset


```{r message=FALSE, warning=FALSE}
glimpse(df_xl)
```
## **Data Cleaning and Wrangling**

Cleaning the data using tidyr package, and making sure:

i. Every value has its own cell 

ii. Every observation has its own row 

iii.Every variable has its own column.

### Hiding unnecessary columns from the dataset
Here we are removing all the unneccessary columns and keeping only the required columns to create a new data frame. The required columns are LOCATION, TIME, Value

```{r}
df_xl=df_xl%>% select(-c(`INDICATOR`,`SUBJECT`, `MEASURE`, `FREQUENCY`, `Flag Codes`))
kable(head(df_xl,10),format="html",table.attr="style='width:30%;'") %>% 
  kableExtra::kable_styling()
```

**Pivot wider** : Then I have put data of TIME and Value more  wider and shorter table.Hence, now every variable has its own column. Then we remove any available Null values.

```{r}
df1=df_xl %>% pivot_wider(names_from = "TIME" ,
                          values_from = "Value")

dfx=df1%>% drop_na()
```

**Here We are changing column names for easier addition in the below steps.**

```{r}
df2= dfx %>% rename_all(funs(str_replace(.,"19","Year_19")))
df2= df2 %>% rename_all(funs(str_replace(.,"20","Year_20")))
```
**mutate** : We are using here the mutate to create 4 new columns that consists the years for each decade.
```{r}
df_p=df2 %>% mutate(Rate_70s = Year_1970+Year_1971+Year_1972+Year_1973+Year_1974+Year_1975+Year_1976+Year_1977+Year_1978+Year_1979,
                    Rate_80s = Year_1980+Year_1981+Year_1982+Year_1983+Year_1984+Year_1985+Year_1986+Year_1987+Year_1988+Year_1989,
                    Rate_90s = Year_1990+Year_1991+Year_1992+Year_1993+Year_1994+Year_1995+Year_1996+Year_1997+Year_1998+Year_1999,
                    Rate_00s = Year_2000+Year_2001+Year_2002+Year_2003+Year_2004+Year_2005+Year_2006+Year_2007+Year_2008+Year_2009,)%>%
 select (LOCATION, Rate_70s, Rate_80s,Rate_90s,Rate_00s)
```
**Pivot longer** : First, I have made a new column for the newly created 4 columns called "Decades" listed and added the values to another column. The structure of the table is now narrower and longer.


```{r}
df_long=df_p %>% pivot_longer(c('Rate_70s','Rate_80s','Rate_90s','Rate_00s'),
                               names_to = "Decades", values_to = "Fertility_Rate")
```


## **Data Visualization**
**Fertility Rate for different countries decadewise**
Here we are using the ggplot package to plot the change in fertility rate for different countries during different decades. We notice that the most countries have had consistent and comparable fertility Rates over over the years except few.
 

```{r}
ggplot(data = df_long, mapping = aes(
  x=LOCATION, y=Fertility_Rate)) +
  geom_point(size=2, color='red')+
  facet_wrap(~Decades,) +
  ggtitle("Fertility Rate decade-wise mapping")+
  xlab("Country")+ylab("Fertility Rate")+
  theme_economist()+
  coord_flip() 
```
**Fertility Rate for different countries**
Here we are using the plot to further illustrate  the change in fertility rate for countrywise also plotting the different trends for a particular country. The highest fertility rate for each country and the different variations reached.


```{r }
ggplot(data = df_long, mapping = aes(
  x=LOCATION, y=Fertility_Rate )) +
  geom_point(stat="identity",aes(col=Fertility_Rate), size=5)+
  scale_color_gradient(low="green",
                       high = "tomato1",
                       guide="colourbar",
                       aesthetics = "colour")+
  ggtitle("Fertility Rate decade-wise mapping")+
  xlab("Country")+ylab("Fertility Rate")+
  coord_flip()
```


**Fertility Rate trends for Czech Republic **

we are using the ggplot package to plot the change in fertility rate for Czech Republic.The fertility rate for Czech Republic was highest during the 1970s and lowest during 2000s. Showing abrupt changes in the fertility rates.

```{r}
dfp2=df_xl %>% pivot_wider(names_from = "LOCATION" ,
                          values_from = "Value")
ggplot(data = dfp2, mapping = aes(
  x=TIME, y=CZE)) +
  geom_bar(stat='identity',color='orange',size=5)+
  ggtitle("Fertility Rate Czech Republic")+
  xlab("Year")+ylab("Fertility Rate")+
  theme_economist()
```



**Fertility Rate trends for Austria **

we are using the ggplot package to plot the change in fertility rate for Austria.The fertility rate for Austria was highest during the 1970s and then it kept decreasing sharply and remained constant throughout from 1990s to 2020s.

```{r}
ggplot(data = dfp2, mapping = aes(
  x=TIME, y=AUT)) +
  geom_bar(stat='identity',color='green',size=5)+
  ggtitle("Fertility Rate Austria")+
  xlab("Year")+ylab("Fertility Rate")+
  theme_economist()
```
**Fertility Rate trends for Sweden **

we are using the ggplot package to plot the change in fertility rate for Sweden.The fertility rate for Sweden mostly remained constant showing minutes changes over the years.


```{r}
ggplot(data = dfp2, mapping = aes(
  x=TIME, y=SWE)) +
  geom_bar(stat='identity',color='red',size=5)+
  ggtitle("Fertility Rate Sweden")+
  xlab("Year")+ylab("Fertility Rate")+
  theme_economist()

```




**Thus,we may conclude the below observations:**

**1. Most countries have had consistent and comparable fertility Rates over over the years.**

**2. Anomalous behaviours were shown by coun tries like Czech Republic, Austria and Sweden**

**3. The fertility rate for Czech Republic was highest during the 1970s and lowest during 2000s. Showing abrupt changes in the fertility rates**

**4. The fertility rate for Austria was highest during the 1970s and then it kept decreasing sharply and remained constant throughout from 1990s to 2020s. **

**5. The fertility rate for Sweden mostly remained constant showing minutes changes over the years**

