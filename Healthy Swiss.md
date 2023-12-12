---
title: "Swiss Health"
author: "Dan Cusick"
date: "2/4/2023"
---

##### **Deep Pockets and Long Lives in Switzerland**
* A September 20, 2020 broadcast of the *PBS Newhour* profiled Switzerland's high-performing health care system. I decided to take a closer look with some exploratory data analysis. 
  + https://www.pbs.org/newshour/show/how-switzerland-delivered-health-care-for-all-and-kept-its-private-insurance
* According to the *Newshour*:
  + "The average Swiss pays more out-of-pocket for things like co-pays than the average American"
  + "The Swiss live about five years longer, on average, than we do"
* I focused on out-of-pocket (household) spending as a percentage of total health spending, as well as life expectancy.
* All data is from the World Bank World Development Indicators
  + https://databank.worldbank.org/source/world-development-indicators

Set-up includes data.table, fpp3, and kableExtra packages
```{r include= FALSE}
library(data.table)
library(fpp3)
library(kableExtra)
```

```{r}
# Paths
projpath <- "C:/Users/dancu/Documents/Fall2022_ADEC743002"
rawdata <- file.path(projpath, "RawData")
output <- file.path(projpath, "Output")
```

#### **Read in files**
* First read in out-of-pocket spending as a percentage of total health expenditure
* I "melted" the year columns into row values, and did some basic clean-up, mainly renaming columns. 
* Melting the columns positioned the out-of-pocket % as a column of data with yearly row labels.
* For comparison, I included Switzerland, the US, the UK and all 27 countries in the EU. I realize many other combination of countries could be considered as well. 
```{r}
# Out-of-pocket spending 
pocketper<-read.csv(file.path(rawdata, "pocketper.csv"))
head(pocketper,3)
tail(pocketper) #shows empty rows

pocketper<-data.table(pocketper)
pocketper<-pocketper[1:30] #remove empty rows at the bottom

# Melt year columns into rows
pocketper.m <- data.table::melt(data = pocketper,
                                id.vars = c("Country.Name","Series.Code"),
                                measure.vars = c("X2015..YR2015.", "X2016..YR2016.", "X2017..YR2017.", "X2018..YR2018.","X2019..YR2019."))	

# Clean up table, rename column
pocketper.m2<-pocketper.m[,c(1,3,4)]
names(pocketper.m2)[names(pocketper.m2) == "value"] <- "OutPocketPct"
```

* Read in, melt, and clean up life expectancy at birth
```{r}
# Life Expectancy
le<-read.csv(file.path(rawdata, "le.csv"))
head(le,3)

le<-data.table(le)
le<-le[1:30]

# Melt year columns into rows
le.m <- data.table::melt(data = le,
                         id.vars = c("Country.Name","Series.Code"),
                         measure.vars = c("X2015..YR2015.", "X2016..YR2016.", "X2017..YR2017.", "X2018..YR2018.","X2019..YR2019."))	

le.m2<-le.m[,c(1,3,4)]

names(le.m2)[names(le.m2) == "value"] <- "LifeExp"
```

* Merge spending and life expectancy data
```{r}
# Check missing values before merging
sapply(pocketper.m2, function(x) sum(is.na(x)))
sapply(le.m2, function(x) sum(is.na(x)))

health<-merge(pocketper.m2,le.m2)

# rename year values
health[variable == "X2015..YR2015."]$variable<-"2015"
health[variable == "X2016..YR2016."]$variable<-"2016"
health[variable == "X2017..YR2017."]$variable<-"2017"
health[variable == "X2018..YR2018."]$variable<-"2018"
health[variable == "X2019..YR2019."]$variable<-"2019"

names(health)[names(health) == "variable"] <- "Year"
```

* Quick review of new table
```{r}
# Glance at the new table
health[Country.Name=="Switzerland"]

# round decimals
health$LifeExp<-round(health$LifeExp,2)
health$OutPocketPct<-round(health$OutPocketPct,2)
```


#### **Summary Snapshot**
* Out of Pocket % of total health spending and Life Expectancy at Birth
  + EU, Switzerland, UK, US 2015-2019
```{r}
as.array(round(summary(health$OutPocketPct),2)) %>% kbl(caption="Out of Pocket %", col.names = NULL) %>% kable_classic(full_width=F,html_font="Times New Roman")
as.array(round(summary(health$LifeExp),2)) %>% kbl(caption="Life Expectancy", col.names = NULL) %>% kable_classic(full_width=F,html_font="Times New Roman")
```

Switzerland and United States Summary Comparison
```{r}
health[Country.Name=="Switzerland"] %>% kbl(caption="Switzerland Health Spending and LE") %>% kable_classic(full_width=F,html_font="Times New Roman")
health[Country.Name=="United States"] %>% kbl(caption="U.S. Health Spending and LE") %>% kable_classic(full_width=F,html_font="Times New Roman")
```

#### **Scatterplot: Comparing Nations**
I took an average of the 2015-2019 out-of-pocket and life expectancy values to plot them on a scatterplot. 

```{r}
meanout<-aggregate(health$OutPocketPct, by=list(health$Country.Name), FUN=mean)
names(meanout)[names(meanout) == "x"] <- "OutPocketPct"
meanlife<-aggregate(health$LifeExp, by=list(health$Country.Name), FUN=mean)
names(meanlife)[names(meanlife) == "x"] <- "LifeExp"
avgcountry<-merge(meanout,meanlife)
names(avgcountry)[names(avgcountry) == "Group.1"] <- "Country.Name"

ggplot(avgcountry, aes(x = OutPocketPct, y = LifeExp)) +
  labs(title = "EU, Switzerland, UK, US", subtitle="2015-2019 averages",x="Out of Pocket Spending (% of health spending)", y="Life Expectancy (years)")+
  geom_point(size=.75) +
  geom_text(size=2, aes(label=Country.Name)) +
  geom_smooth(method=lm,se=FALSE, fullrange=TRUE, lwd=.75)
```

The line of best fit reflects the slightly negative correlation between out-of-pocket spending and life expectancy. While not an outlier, Switzerland bucks this trend. 
```{r}
cor(health$OutPocketPct,health$LifeExp)
```

##### **Conclusions**
This combination of high out-of-pocket spending and life expectancy provide a good starting point for more rigorous analysis. Burton-Jeangros, et. al. (2019) praise Switzerland's  consumer-driven and decentralized health marketplace. Perhaps their combination of government support, high volume spending, and market forces amount to an optimal mix for producing strong health outcomes. 

* Burton-Jeangros, C., Cullati, S., Oris, M., Remund, A., Sieber, S. (2019, August 31). Longer and healthier lives for all? Successes and failures of
a universal consumer-driven healthcare system, Switzerland, 1990-2014. *International Journal of Public Health*, 64, 1173-1181.
https://doi.org/10.1007/s00038-019-01290-5




