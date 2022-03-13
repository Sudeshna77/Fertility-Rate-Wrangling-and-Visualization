Fertility-Rate-Wrangling-and-Visualization
Sudeshna Sarkar
12/03/2022
Fertility Rate of Countries
The data used in this project will be used to study the fertility rates of various countries over the world in a given span of time.The total fertility rate in a specific year is defined as the total number of children that would be born to each woman if she were to live to the end of her child-bearing years.Through this project we will try to analyse the variation of fertility rates over the years for different countries and we will also see some of the anomalous behavious shown by some countries

Data source: “OECD data” https://data.oecd.org/pop/fertility-rates.htm Years 1970-2020

The original dataset has the following columns:

1. LOCATION : Various countries over the world has been included in the data.

2. INDICATOR : Indicates the fertility.

3. TIME : FROM 1970 - 2020.

4. Value : Fertility index value.

Setting up the working directory and loading the packages.
knitr::opts_chunk$set()

rm(list=ls())
getwd()
## [1] "C:/Git projects/Fertility-Rate-Wrangling-and-Visualization"
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(ggthemes)
library(kableExtra)
Loading the data in R. Reading the dataset and displying few records..
df_xl = read_csv("Fertility Rate Wrangling and Visualization.csv")
kable(head(df_xl,10),format="html",table.attr="style='width:30%;'") %>% 
  kableExtra::kable_styling()
LOCATION	INDICATOR	SUBJECT	MEASURE	FREQUENCY	TIME	Value	Flag Codes
AUS	FERTILITY	TOT	CHD_WOMAN	A	1970	2.86	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1971	2.95	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1972	2.74	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1973	2.49	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1974	2.32	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1975	2.15	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1976	2.06	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1977	2.01	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1978	1.95	NA
AUS	FERTILITY	TOT	CHD_WOMAN	A	1979	1.91	NA
Taking a glimpse of the dataset
glimpse(df_xl)
## Rows: 2,683
## Columns: 8
## $ LOCATION     <chr> "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "AUS", "~
## $ INDICATOR    <chr> "FERTILITY", "FERTILITY", "FERTILITY", "FERTILITY", "FERT~
## $ SUBJECT      <chr> "TOT", "TOT", "TOT", "TOT", "TOT", "TOT", "TOT", "TOT", "~
## $ MEASURE      <chr> "CHD_WOMAN", "CHD_WOMAN", "CHD_WOMAN", "CHD_WOMAN", "CHD_~
## $ FREQUENCY    <chr> "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A~
## $ TIME         <dbl> 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 197~
## $ Value        <dbl> 2.86, 2.95, 2.74, 2.49, 2.32, 2.15, 2.06, 2.01, 1.95, 1.9~
## $ `Flag Codes` <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
Data Cleaning and Wrangling
Cleaning the data using tidyr package, and making sure:

Every value has its own cell

Every observation has its own row

iii.Every variable has its own column.

Hiding unnecessary columns from the dataset
Here we are removing all the unneccessary columns and keeping only the required columns to create a new data frame. The required columns are LOCATION, TIME, Value

df_xl=df_xl%>% select(-c(`INDICATOR`,`SUBJECT`, `MEASURE`, `FREQUENCY`, `Flag Codes`))
kable(head(df_xl,10),format="html",table.attr="style='width:30%;'") %>% 
  kableExtra::kable_styling()
LOCATION	TIME	Value
AUS	1970	2.86
AUS	1971	2.95
AUS	1972	2.74
AUS	1973	2.49
AUS	1974	2.32
AUS	1975	2.15
AUS	1976	2.06
AUS	1977	2.01
AUS	1978	1.95
AUS	1979	1.91
Pivot wider : Then I have put data of TIME and Value more wider and shorter table.Hence, now every variable has its own column. Then we remove any available Null values.

df1=df_xl %>% pivot_wider(names_from = "TIME" ,
                          values_from = "Value")

dfx=df1%>% drop_na()
Here We are changing column names for easier addition in the below steps.

df2= dfx %>% rename_all(funs(str_replace(.,"19","Year_19")))
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## Please use a list of either functions or lambdas: 
## 
##   # Simple named list: 
##   list(mean = mean, median = median)
## 
##   # Auto named with `tibble::lst()`: 
##   tibble::lst(mean, median)
## 
##   # Using lambdas
##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
df2= df2 %>% rename_all(funs(str_replace(.,"20","Year_20")))
mutate : We are using here the mutate to create 4 new columns that consists the years for each decade.

df_p=df2 %>% mutate(Rate_70s = Year_1970+Year_1971+Year_1972+Year_1973+Year_1974+Year_1975+Year_1976+Year_1977+Year_1978+Year_1979,
                    Rate_80s = Year_1980+Year_1981+Year_1982+Year_1983+Year_1984+Year_1985+Year_1986+Year_1987+Year_1988+Year_1989,
                    Rate_90s = Year_1990+Year_1991+Year_1992+Year_1993+Year_1994+Year_1995+Year_1996+Year_1997+Year_1998+Year_1999,
                    Rate_00s = Year_2000+Year_2001+Year_2002+Year_2003+Year_2004+Year_2005+Year_2006+Year_2007+Year_2008+Year_2009,)%>%
 select (LOCATION, Rate_70s, Rate_80s,Rate_90s,Rate_00s)
Pivot longer : First, I have made a new column for the newly created 4 columns called “Decades” listed and added the values to another column. The structure of the table is now narrower and longer.

df_long=df_p %>% pivot_longer(c('Rate_70s','Rate_80s','Rate_90s','Rate_00s'),
                               names_to = "Decades", values_to = "Fertility_Rate")
Data Visualization
Fertility Rate for different countries decadewise Here we are using the ggplot package to plot the change in fertility rate for different countries during different decades. We notice that the most countries have had consistent and comparable fertility Rates over over the years except few.

ggplot(data = df_long, mapping = aes(
  x=LOCATION, y=Fertility_Rate)) +
  geom_point(size=2, color='red')+
  facet_wrap(~Decades,) +
  ggtitle("Fertility Rate decade-wise mapping")+
  xlab("Country")+ylab("Fertility Rate")+
  theme_economist()+
  coord_flip() 
 Fertility Rate for different countries Here we are using the plot to further illustrate the change in fertility rate for countrywise also plotting the different trends for a particular country. The highest fertility rate for each country and the different variations reached.

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


Fertility Rate trends for Czech Republic

we are using the ggplot package to plot the change in fertility rate for Czech Republic.The fertility rate for Czech Republic was highest during the 1970s and lowest during 2000s. Showing abrupt changes in the fertility rates.

dfp2=df_xl %>% pivot_wider(names_from = "LOCATION" ,
                          values_from = "Value")
ggplot(data = dfp2, mapping = aes(
  x=TIME, y=CZE)) +
  geom_bar(stat='identity',color='orange',size=5)+
  ggtitle("Fertility Rate Czech Republic")+
  xlab("Year")+ylab("Fertility Rate")+
  theme_economist()


Fertility Rate trends for Austria

we are using the ggplot package to plot the change in fertility rate for Austria.The fertility rate for Austria was highest during the 1970s and then it kept decreasing sharply and remained constant throughout from 1990s to 2020s.

ggplot(data = dfp2, mapping = aes(
  x=TIME, y=AUT)) +
  geom_bar(stat='identity',color='green',size=5)+
  ggtitle("Fertility Rate Austria")+
  xlab("Year")+ylab("Fertility Rate")+
  theme_economist()
 Fertility Rate trends for Sweden

we are using the ggplot package to plot the change in fertility rate for Sweden.The fertility rate for Sweden mostly remained constant showing minutes changes over the years.

ggplot(data = dfp2, mapping = aes(
  x=TIME, y=SWE)) +
  geom_bar(stat='identity',color='red',size=5)+
  ggtitle("Fertility Rate Sweden")+
  xlab("Year")+ylab("Fertility Rate")+
  theme_economist()


Thus,we may conclude the below observations:

1. Most countries have had consistent and comparable fertility Rates over over the years.

2. Anomalous behaviours were shown by coun tries like Czech Republic, Austria and Sweden

3. The fertility rate for Czech Republic was highest during the 1970s and lowest during 2000s. Showing abrupt changes in the fertility rates

4. The fertility rate for Austria was highest during the 1970s and then it kept decreasing sharply and remained constant throughout from 1990s to 2020s.

5. The fertility rate for Sweden mostly remained constant showing minutes changes over the years
