# R Script ==============================================================
library(fpp3)
library(car)
library(kableExtra)
library(zoo)
# Paths
projpath <- "C:/Users/dancu/Documents/Fall2022_ADEC743002"
rawdata <- file.path(projpath, "RawData")
output <- file.path(projpath, "Output")

# Read in city, year, week, total_cases file
labels_train<-read.csv(file.path(rawdata, "dengue_labels_train.csv"))
head(labels_train)
nrow(labels_train)
str(labels_train)
unique(labels_train$city)

# Read in environmental features
features_train<-read.csv(file.path(rawdata, "dengue_features_train.csv"))
head(features_train)
nrow(features_train)
str(features_train)

# Merge data sets
train_merge<-merge(labels_train,features_train,all=TRUE)

# Read in drivendata.org test set
features_test<-read.csv(file.path(rawdata, "dengue_features_test.csv"))
head(features_test)

# Get drivendata.org submission format
sample_sub<-read.csv(file.path(rawdata, "submission_format_dengue.csv"))
head(sample_sub)

# Compare train/test years
summary(features_train$year)
summary(features_test$year)

# Create tsibble time series objects for train and test data
train_merge$week_start_date<-yearweek(train_merge$week_start_date)
full_train_ts<-train_merge %>% as_tsibble(key=city, index=week_start_date)

features_test$week_start_date<-yearweek(features_test$week_start_date)
full_test_ts<-features_test %>% as_tsibble(key=city, index=week_start_date)

# Plot both city time series

sj_full<-full_train_ts %>% filter(city=="sj")
iq_full<-full_train_ts %>% filter(city=="iq")

autoplot(sj_full, .vars=total_cases)+
  labs(title = "Weekly Dengue Cases in San Juan", y="total cases")

autoplot(iq_full, .vars=total_cases)+
  labs(title = "Weekly Dengue Cases in Iquitos", y="total cases")

# Seasonal plots
tsibble::fill_gaps(sj_full) %>%
  gg_season(total_cases, lwd=0.75) +
  labs(y = "total cases",
       title = "Weekly Dengue cases in San Juan", subtitle= "Yearly Seasonal Plot")

tsibble::fill_gaps(iq_full) %>%
  gg_season(total_cases, lwd=0.75) +
  labs(y = "total cases", title = "Weekly Dengue cases in Iquitos", subtitle= "Yearly Seasonal Plot")

# Summarize weekly case data for each city
summary(sj_full$year)
summary(iq_full$year)
print(as.array(summary(sj_full$total_cases))) %>% kbl(caption="San Juan Weekly Cases (1990-2008)", col.names=NULL) %>% kable_classic(full_width=F,html_font="Times New Roman")
print(as.array(summary(iq_full$total_cases))) %>% kbl(caption="Iquitos Weekly Cases (2000-2010)", col.names=NULL) %>% kable_classic(full_width=F,html_font="Times New Roman")

# Make my own train and test set
# Drivendata.org submission must be made with the provided test set.
# Therefore, I need my own test subset to validate my owns models.

test_set<-full_train_ts %>% filter(year(week_start_date)>2004)
train_set<-full_train_ts %>% filter(year(week_start_date)<=2004)

# Collinearity review for environmental variables
colnames(train_set)
sapply(train_set, function(x) sum(is.na(x)))
climate<-train_set[,c(6:25)]
colnames(climate)
climate<-na.omit(climate)
cor(climate)
print(cor(climate)) %>% kbl() %>% kable_classic(full_width=F,html_font="Times New Roman")

# Narrow down variables based on correlation matrix to reduce redundant variables
climate2<-climate[,c(1,3,5,11,14,16,20)]
colnames(climate2)
print(cor(climate2)) %>% kbl() %>% kable_classic(full_width=F,html_font="Times New Roman")

# Keep those columns in train_set
colnames(train_set)
# Keep 1:5,6,8,10,16,19,22,25
train_set2<-train_set[,c(1:5,6,8,10,16,19,21,25)]
colnames(train_set2)
train_set2

# Check Variance Inflation Factor
lmod <- lm(total_cases ~ . - year - city - week_start_date - weekofyear, data = train_set2)
summary(lmod)
vif(lmod)
# Variance Inflation Factor scores all look good

# Model accuracy function
fc <- function(mod,n) {
  acc<-accuracy(mod)
  train.mase<-acc$MASE
  train.mae<-acc$MAE
  f<-forecast(mod,new_data = n)
  facc<-accuracy(f,n)
  test.mase<-facc$MASE
  test.mae<-facc$MAE
  print(c(TrainMASE = train.mase, TestMASE = train.mase, TrainMAE = train.mae, TestMAE = test.mae))
}

# work more with train and test sets
# fill time series gaps, zero-out few rows with missing total_cases
sapply(fill_gaps(train_set2), function(x) sum(is.na(x)))
fill_gaps(train_set2) %>% filter(is.na(total_cases))
train_set3<-fill_gaps(train_set2)
train_set3[is.na(train_set3$total_cases),]$total_cases<-0
sapply(train_set3, function(x) sum(is.na(x)))
test_set2<-fill_gaps(test_set)

# Try models

# ARIMA 
fitautoarima<-train_set3 %>% model(ARIMA(sqrt(total_cases)))
fitautoarima2<-train_set3 %>% model(ARIMA(total_cases))

fc(fitautoarima, test_set2)
fc(fitautoarima2, test_set2)
fitautoarima2

fitautoarima2
# fitautoarima2 without sqrt transformation works better

# SARIMA
fitautoseas<-train_set3 %>% model(ARIMA(total_cases ~ PDQ(1,0,0)))
fc(fitautoseas,test_set2)
fcautoseas<-forecast(fitautoseas, test_set2)

# fcautoseas with SAR(1) is better for sj

# Fourier terms forecast
fit_fourier <- train_set3 %>% model(ARIMA(total_cases ~ fourier(period = 52, K = 10)))
fc(fit_fourier, test_set2)
# Auto ARIMA with 10 fourier terms improves iq, but not sj

fit_fourier2 <- train_set3 %>% model(ARIMA(total_cases ~ fourier(period = 52, K = 5)))
fc(fit_fourier2, test_set2)
# fourier terms K=5 is better

fit_fourier3 <- train_set3 %>% model(ARIMA(total_cases ~ fourier(period = 52, K = 2)))
fc(fit_fourier3, test_set2)
# fitnet<-train_set3 %>% model(NNETAR(total_cases)) # this model did not outperform ARIMAs

# Start to mix in environmental/climate variables
colnames(train_set3)

# Regression with ARIMA
fitclimate<-train_set3 %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                         reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + station_avg_temp_c + station_precip_mm))

fc(fitclimate, test_set2)
# Iq worse, SJ better than fourier K=5

# Regression, ARIMA, Fourier terms
fitclimate2<-train_set3 %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                          reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
                                          station_avg_temp_c + station_precip_mm + fourier(period = 52, K = 5)))
fc(fitclimate2, test_set2)
# Best iq model so far

# Try deterministic with trend
fitclimate3<-train_set3 %>% model(ARIMA(total_cases ~ trend() + ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                          reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
                                          station_avg_temp_c + station_precip_mm + fourier(period = 52, K = 5)))
fc(fitclimate3, test_set2) 

# Try lagging climate variables 4 weeks behind cases
train_lag<-train_set3 %>% mutate(total_cases = c(NA, NA, NA, NA, total_cases[5:1002]))

fitclimlag<-train_lag %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + lag(precipitation_amt_mm,4) +
                                        reanalysis_precip_amt_kg_per_m2 + lag(reanalysis_precip_amt_kg_per_m2, 4)+ reanalysis_specific_humidity_g_per_kg + station_avg_temp_c + station_precip_mm + lag(station_precip_mm,4)))

fc(fitclimlag, test_set2)
# better iq, slightly worse sj

# Try current and lagged variables
fitclimlag2<-train_lag %>% model(ARIMA(total_cases ~ ndvi_ne + lag(ndvi_ne,4) + ndvi_se + lag(ndvi_ne,4) + precipitation_amt_mm + lag(precipitation_amt_mm,4) +
                                         reanalysis_precip_amt_kg_per_m2 + lag(reanalysis_precip_amt_kg_per_m2, 4)+ reanalysis_specific_humidity_g_per_kg + station_avg_temp_c + station_precip_mm + lag(station_precip_mm,4)))

fc(fitclimlag2, test_set2) 
# best sj so far

# Add Fourier Terms
fitclimlag3<-train_lag %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + lag(precipitation_amt_mm,4) +
                                         reanalysis_precip_amt_kg_per_m2 + lag(reanalysis_precip_amt_kg_per_m2, 4)+ reanalysis_specific_humidity_g_per_kg + station_avg_temp_c + station_precip_mm + lag(station_precip_mm,4) + fourier(period = 52, K = 5)))
fc(fitclimlag3, test_set2)
# good, not best

# City-specific models - ultimately not fruitful

sj_train<-train_set3 %>% filter(city=="sj")
iq_train<-train_set3 %>% filter(city=="iq")
sj_test<-test_set2 %>% filter(city=="sj")
iq_test<-test_set2 %>% filter(city=="iq")

lambdasj <- sj_train %>%features(total_cases, features = guerrero) %>% pull(lambda_guerrero)
lambdaiq <- iq_train %>%features(total_cases, features = guerrero) %>% pull(lambda_guerrero)
lambda <- train_set3 %>%features(total_cases, features = guerrero) %>% pull(lambda_guerrero)

iq_train %>%
  features(total_cases, unitroot_ndiffs) 
# 1 difference

iq_train %>% gg_tsdisplay(difference(total_cases), plot_type='partial')
iq_train %>% gg_tsdisplay(difference(total_cases^lambdaiq), plot_type='partial')
# Either AR(1) or MA(1) with one diff

iqarima<-iq_train %>% model(ARIMA(total_cases^lambdaiq ~ 1 + pdq(1,1,0)))
fc(iqarima, iq_test)
# yikes

iqarima2<-iq_train %>% model(ARIMA(total_cases~ 1 + pdq(2,1,0)))
iqarima2
fc(iqarima2, iq_test)
# good, but not best

iqarima3<-iq_train %>% model(ARIMA(total_cases~ 1 + pdq(2,1,0) + ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                     reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
                                     station_avg_temp_c + station_precip_mm + fourier(period = 52, K = 5)))
fc(iqarima3, iq_test)

# good, but not best

iqarima4<-iq_train %>% model(ARIMA(total_cases~ 1 + pdq(0,1,2)))
fc(iqarima4, iq_test)
# no

sj_train %>% gg_tsdisplay(difference(total_cases), plot_type='partial')
sj_train %>% gg_tsdisplay(difference(sqrt(total_cases)), plot_type='partial')

sjarima<-sj_train %>% model(ARIMA(sqrt(total_cases) ~ 1 + pdq(2,1,0)))
fc(sjarima, sj_test)
# no better

sjarima2<-sj_train %>% model(ARIMA(sqrt(total_cases) ~ 1 + pdq(2,1,0)))
fc(sjarima2, sj_test)

# Models with manual parameters aren't helpful

# Add week of year to regression
fitclimwk<-train_set3 %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                        reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
                                        station_avg_temp_c + station_precip_mm + weekofyear))
fc(fitclimwk, test_set2)
# good, iq worse

fitclimwk2<-train_set3 %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                         reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
                                         station_avg_temp_c + station_precip_mm + weekofyear + fourier(period = 52, K = 5)))
fc(fitclimwk2, test_set2)
# no


fitclimlagwk<-train_lag %>% model(ARIMA(total_cases ~ weekofyear + ndvi_ne + ndvi_se + precipitation_amt_mm + lag(precipitation_amt_mm,4) +
                                          reanalysis_precip_amt_kg_per_m2 + lag(reanalysis_precip_amt_kg_per_m2, 4)+ reanalysis_specific_humidity_g_per_kg + station_avg_temp_c + station_precip_mm + lag(station_precip_mm,4)))

fc(fitclimlagwk, test_set2)
# good

# Try adding week^squared
fitclimwk3<-train_set3 %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                         reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
                                         station_avg_temp_c + station_precip_mm + weekofyear + I(weekofyear^2) + fourier(period = 52, K = 5)))
fc(fitclimwk3, test_set2)

fc(fitclimlag2, test_set2)
fc(fitclimate2, test_set2)
fc(fitclimlagwk, test_set2)

# Produce models to submit to drivendata.org
# Model 1 Forecast LM with lagged precipitation, lagged vegetation, humidity and temp with ARIMA errors

full_train_lag<-full_train_ts %>% mutate(total_cases = c(NA, NA, NA, NA, total_cases[5:1456]))

full_train_ts2<-fill_gaps(full_train_ts)

full_train_lag<-full_train_ts2 %>% mutate(total_cases = c(NA, NA, NA, NA, total_cases[5:1461]))


fitclimlag2_full<-full_train_lag %>% model(ARIMA(total_cases ~ ndvi_ne + lag(ndvi_ne,4) + ndvi_se + lag(ndvi_ne,4) + precipitation_amt_mm + lag(precipitation_amt_mm,4) +
                                                   reanalysis_precip_amt_kg_per_m2 + lag(reanalysis_precip_amt_kg_per_m2, 4)+ reanalysis_specific_humidity_g_per_kg + station_avg_temp_c + station_precip_mm + lag(station_precip_mm,4)))

#full_fc1<-forecast(fitclimlag2_full, full_test_ts)

full_fc1<-forecast(fitclimlag2_full, fill_gaps(full_test_ts))

head(sample_sub)

colnames(full_fc1)
full_fc1[,c(1,6,7,5)]

sapply(full_fc1, function(x) sum(is.na(x)))

full_fc1$.mean

summary(full_fc1$ndvi_ne)

full_test_ts2<-fill_gaps(full_test_ts)
full_test_ts2[is.na(full_test_ts2$ndvi_ne),]$ndvi_ne<-mean(na.omit(full_test_ts2$ndvi_ne))

full_test_ts2[is.na(full_test_ts2$ndvi_ne),]

full_fc2<-forecast(fitclimlag2_full, fill_gaps(full_test_ts2))

sapply(full_test_ts2, function(x) sum(is.na(x)))

full_fc2[,c(1,6,7,5)]

sapply(full_fc2, function(x) sum(is.na(x)))

# Remove veg predictors due to missing values - not used

fitclimlag2_full2<-full_train_lag %>% model(ARIMA(total_cases ~ precipitation_amt_mm + lag(precipitation_amt_mm,4) +
                                                    reanalysis_precip_amt_kg_per_m2 + lag(reanalysis_precip_amt_kg_per_m2, 4)+ reanalysis_specific_humidity_g_per_kg + station_avg_temp_c + station_precip_mm + lag(station_precip_mm,4)))

full_fc3<-forecast(fitclimlag2_full2, fill_gaps(full_test_ts2))

full_fc3[,c(1,6,7,5)]

sapply(full_fc3, function(x) sum(is.na(x)))

# Try to impute missing forecasts instead

sapply(full_fc1, function(x) sum(is.na(x)))

full_fc1[is.na(full_fc1$.mean),]

full_fc1$.mean
na.locf(full_fc1$.mean)

ddorg1<-full_fc1[,c(1,6,7,5)]
sapply(ddorg1, function(x) sum(is.na(x)))
ddorg1<-ddorg1[!is.na(ddorg1$year),]
ddorg1<-na.locf(ddorg1)
names(ddorg1)[names(ddorg1) == ".mean"] <- "total_cases"

ddorg1

# write.csv(ddorg1,file.path(output, "dancusicksub1.csv"),row.names=FALSE, quote=FALSE)
# ids not correct in file. Maybe sj needs to be first like the sample. 

ddorg1 %>% filter(city=="iq") #156 rows

ddorg1 %>% filter(city=="sj") #260 rows 

ddorg1[c(157:nrow(ddorg1),1:156),]

ddorg1<-ddorg1[c(157:nrow(ddorg1),1:156),]
# write.csv(ddorg1,file.path(output, "dancusicksub1.csv"),row.names=FALSE, quote=FALSE)

ddorg1$total_cases<-round(ddorg1$total_cases,0)
ddorg1$total_cases<-as.integer(ddorg1$total_cases)
ddorg1

# write.csv(ddorg1,file.path(output, "dancusicksub1.csv"),row.names=FALSE, quote=FALSE)

# Model 2 Forecast LM with climate predictors, Fourier terms and and ARIMA errors

fitclimate2_full<-full_train_ts2 %>% model(ARIMA(total_cases ~ ndvi_ne + ndvi_se + precipitation_amt_mm + 
                                                   reanalysis_precip_amt_kg_per_m2 + reanalysis_specific_humidity_g_per_kg + 
                                                   station_avg_temp_c + station_precip_mm + fourier(period = 52, K = 5)))

full_fc4<-forecast(fitclimate2_full, fill_gaps(full_test_ts))
full_fc4
sapply(full_fc4, function(x) sum(is.na(x)))

ddorg2<-full_fc4[,c(1,6,7,5)]
sapply(ddorg2, function(x) sum(is.na(x)))
ddorg2<-ddorg2[!is.na(ddorg2$year),]
ddorg2<-na.locf(ddorg2)
names(ddorg2)[names(ddorg2) == ".mean"] <- "total_cases"
ddorg2$total_cases<-round(ddorg2$total_cases,0)
ddorg2$total_cases<-as.integer(ddorg2$total_cases)
ddorg2

ddorg2<-ddorg2[c(157:nrow(ddorg2),1:156),]
# write.csv(ddorg2,file.path(output, "dancusicksub2.csv"),row.names=FALSE, quote=FALSE)


# Submission 2 got a better score

# Model 3 Dynamic Harmonic Regression with Fourier Terms and ARIMA errors

fit_fourier2_full <- full_train_ts2 %>% model(ARIMA(total_cases ~ fourier(period = 52, K = 5)))

full_fc5<-forecast(fit_fourier2_full, fill_gaps(full_test_ts))
full_fc5

ddorg3<-full_fc5[,c(1,6,7,5)]
sapply(ddorg3, function(x) sum(is.na(x)))
ddorg3<-ddorg3[!is.na(ddorg3$year),]
names(ddorg3)[names(ddorg3) == ".mean"] <- "total_cases"
ddorg3$total_cases<-round(ddorg3$total_cases,0)
ddorg3$total_cases<-as.integer(ddorg3$total_cases)
ddorg3

ddorg3<-ddorg3[c(157:nrow(ddorg3),1:156),]
# write.csv(ddorg3,file.path(output, "dancusicksub3.csv"),row.names=FALSE, quote=FALSE)

accuracy(fitclimlag2_full)
accuracy(fitclimate2_full)
accuracy(fit_fourier2_full)
