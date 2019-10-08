
#### 0.   INCLUDES / PREPARING DATA _______________________________________ #### 

#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 

pacman::p_load(rstudioapi,dplyr, ggplot2, lubridate, randomForest, caret, 
               rpart,rpart.plot,tidyr, mice,e1071,zoo,reshape2, plotly,
               weathermetrics,gdata,outliers,gridExtra)

# Setwd (set current wd where is the script, then we move back to the 
# general folder)
current_path = getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("..")
rm(current_path)

# Let's read the script with the own functions
source("./script/functions_sara.R")

# reading datasets
data_x <- read.csv("./data/dengue_features_train.csv", stringsAsFactors = F)
data_y<- read.csv("./data/dengue_labels_train.csv", stringsAsFactors = F)
data<- cbind(data_x, total_cases=data_y$total_cases)

validation<- read.csv("./data/dengue_features_test.csv", stringsAsFactors = F)

rm(data_x, data_y)

#### 1.   CLEANING / PREPROCESSING ####
#### 1.1. Transformations _________________________________________________ ####
# Dimensions
dim(data)          # <-  1456 rows    25 columns
dim(validation)     # <-  416  rows    24 columns

# Rename variables 
data <- data %>% rename("precip_amt"= "precipitation_amt_mm",
                        "precip_mm_r"= "reanalysis_sat_precip_amt_mm",
                        "temp_dewpoint_r"="reanalysis_dew_point_temp_k",
                        "temp_air_mean_r" = "reanalysis_air_temp_k",
                        "humid_relative_r" = "reanalysis_relative_humidity_percent",
                        "humid_specific_r" = "reanalysis_specific_humidity_g_per_kg",
                        "precip_kgperm2_r" = "reanalysis_precip_amt_kg_per_m2", 
                        "temp_max_r" = "reanalysis_max_air_temp_k",
                        "temp_min_r" = "reanalysis_min_air_temp_k",
                        "temp_air_avg_r" = "reanalysis_avg_temp_k", 
                        "temp_dir_range_r" = "reanalysis_tdtr_k",
                        "temp_max_st"= "station_max_temp_c",
                        "temp_min_st"= "station_min_temp_c",
                        "temp_avg_st"= "station_avg_temp_c",
                        "precip_st" = "station_precip_mm",
                        "temp_dir_range_st" = "station_diur_temp_rng_c")

# Rename variables 
validation <- validation %>% rename("precip_amt"= "precipitation_amt_mm",
                                    "precip_mm_r"= "reanalysis_sat_precip_amt_mm",
                                    "temp_dewpoint_r"="reanalysis_dew_point_temp_k",
                                    "temp_air_mean_r" = "reanalysis_air_temp_k",
                                    "humid_relative_r" = "reanalysis_relative_humidity_percent",
                                    "humid_specific_r" = "reanalysis_specific_humidity_g_per_kg",
                                    "precip_kgperm2_r" = "reanalysis_precip_amt_kg_per_m2", 
                                    "temp_max_r" = "reanalysis_max_air_temp_k",
                                    "temp_min_r" = "reanalysis_min_air_temp_k",
                                    "temp_air_avg_r" = "reanalysis_avg_temp_k", 
                                    "temp_dir_range_r" = "reanalysis_tdtr_k",
                                    "temp_max_st"= "station_max_temp_c",
                                    "temp_min_st"= "station_min_temp_c",
                                    "temp_avg_st"= "station_avg_temp_c",
                                    "precip_st" = "station_precip_mm",
                                    "temp_dir_range_st" = "station_diur_temp_rng_c")

# Transform some variables to factor/numeric/datetime
data$week_start_date<- ymd(data$week_start_date)
validation$week_start_date<- ymd(validation$week_start_date)

data$city <- as.factor(data$city)
data$year <- year(data$week_start_date)
data$weekofyear<- as.factor(data$weekofyear)
data$month<- month(data$week_start_date)
validation$city <- as.factor(validation$city)
validation$year<- year(validation$week_start_date)
validation$weekofyear <- as.factor(validation$weekofyear)
validation$month<- month(validation$week_start_date)

#### 1.2. Split in two cities _____________________________________________ ####
# Combine datasets for checking distributions data & validation 

validation$total_cases<-0
data_full<-gdata::combine(data, validation)


# data_iq<-data %>% filter(city=="iq")
# data_sj<-data %>% filter(city=="sj")

data_iq<-data_full %>% filter(city=="iq")
data_sj<-data_full %>% filter(city=="sj")

#### 1.3. Missing values __________________________________________________ ####
data<-na.locf(data)
validation<-na.locf(validation)

# where and how many missing values 

missing_values <- data_sj %>%
  filter(year>2000) %>%
  select_if(is.numeric) %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

ggplot(missing_values, aes(x=key, y=num.missing, fill=key)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) 

data_sj<-na.locf(data_sj)
data_iq<-na.locf(data_iq)
rm(missing_values)


#### 1.4. Outliers ________________________________________________________ ####
data_iq %>% 
  select(-city, -year, - weekofyear, -month) %>%
  mutate(ndvi_ne=ndvi_ne*1000, ndvi_nw=ndvi_nw*1000,
         ndvi_se=ndvi_se*1000, ndvi_sw=ndvi_sw*1000) %>%
  melt(c("week_start_date", "source")) %>%
  mutate(value=as.numeric(value)) %>%
  ggplot() + aes(x=variable, y=value) + 
  geom_boxplot() + facet_wrap(~source) +
  coord_flip()    # We can see a huge difference in the level of precip.

data_iq %>%
  select(source,week_start_date, starts_with("precip")) %>%
  melt(c("week_start_date", "source")) %>%
  ggplot() + aes(x=variable, y=value, fill=source) + 
  geom_boxplot() 

data_iq %>% 
  select(source,week_start_date, starts_with("precip")) %>%
  melt(c("week_start_date", "source")) %>%
  group_by(source, variable) %>%
  dplyr::summarise(median_var= max(value, na.rm = TRUE))

# data precip st     md 45.3     max 543
# valid precip st    md 27.2     max 212 

data_sj %>% filter(source=="data") %>%
  select(-city, -year, - weekofyear, -month) %>%
  melt("week_start_date") %>%
  mutate(value=as.numeric(value)) %>%
  ggplot() + aes(x=variable, y=value) + 
  geom_boxplot() + coord_flip()

#### 2.0  EXPLORATORY ANALYSIS #### 

#### 2.1. YEAR ____________________________________________________________ ####
summary(data_iq$year)     # <- 2000 to 2010
summary(data_sj$year)     # <- 1990 to 2010
summary(validation$year)  # <- 2008 to 2013

# Some years have the week mislabeled (first week as 52/53, second week as 1...). 
weird_weeks_iq<-data_iq %>% filter(month(data_iq$week_start_date)==1 & 
                                data_iq$weekofyear==1 & 
                                day(data_iq$week_start_date)!=1)

weird_weeks_sj<-data_sj %>% filter(month(data_sj$week_start_date)==1 & 
                                     data_sj$weekofyear==1 & 
                                     day(data_sj$week_start_date)!=1)

data_iq<- data_iq %>% 
  mutate(weekofyear=as.numeric(weekofyear)) %>%
  mutate(weekofyear=
         ifelse (year %in% weird_weeks_iq$year & 
                 month(week_start_date)==1 &
                 day(week_start_date)==1,1,
             
         ifelse(year %in% weird_weeks_iq$year &
                month(week_start_date)==1 &
                day(week_start_date)!=1, weekofyear +1, weekofyear)     
             ))
             
data_sj<- data_sj %>% 
  mutate(weekofyear=as.numeric(weekofyear)) %>%
  mutate(weekofyear=
           ifelse (year %in% weird_weeks_sj$year & 
                     month(week_start_date)==1 &
                     day(week_start_date)==1,1,
                   
                   ifelse(year %in% weird_weeks_sj$year &
                            month(week_start_date)==1 &
                            day(week_start_date)!=1, weekofyear +1, weekofyear)     
           ))

data_full<- rbind(data_iq, data_sj)

rm(weird_weeks_iq, weird_weeks_sj)

# Cases depending on the week of the year ______________________________________
p_iquitos<-ggplot(data_full[data_full$source=="data" & data_full$city=="iq",], 
       aes(x=weekofyear, y=total_cases, color=factor(year))) + 
       geom_point() + ggtitle("Total cases per week -Iquitos-") +
  guides(colour = guide_legend(ncol = 2))

p_sanjuan<-ggplot(data_full[data_full$source=="data" & data_full$city=="sj",], 
                  aes(x=weekofyear, y=total_cases, color=factor(year))) + 
                  geom_point() + ggtitle("Total cases per week -San Juan-") +
  guides(colour = guide_legend(ncol = 2))


grid.arrange(p_iquitos, p_sanjuan, ncol=1)
rm(p_iquitos, p_sanjuan)

# Remove in data years <= 1999 (validation is from 2008-2013)
# data_sj<- data_sj %>% filter(year %in% c(2000,2001,2002, 2003, 2004, 2005, 2006,
#                                    2007, 2008, 2009, 2010, 2011, 2012, 2013))

#### 2.2. TEMPERATURE IQ & SJ _____________________________________________ ####

# Get the temperature variables
temp_var<-names(data[grep("temp", names(data))])
temp_var_k<-c("temp_air_mean_r","temp_air_avg_r","temp_dewpoint_r","temp_max_r",
            "temp_min_r")

# Change everything to celsius
data_iq[temp_var_k]<- kelvin.to.celsius(data_iq[temp_var_k], round = 2)
data_sj[temp_var_k]<- kelvin.to.celsius(data_sj[temp_var_k], round = 2)

rm(temp_var_k)

# Temperature variables train & validation 
geom.density.function(data=data_iq, variables=c(temp_var[1:5], "source"), 
                      fill="source")

geom.density.function(data=data_sj, variables=c(temp_var[1:5], "source"), 
                      fill="source")

geom.density.function(data=data_iq, variables=c(temp_var[6:10], "source"), 
                      fill="source")

geom.density.function(data=data_sj, variables=c(temp_var[6:10], "source"), 
                      fill="source")

# Plot the progression of the temperature variables
plotly.line.function(data_iq,variables= c(temp_var[1:5], "total_cases", 
                                          "week_start_date"), x="week_start_date")

plotly.line.function(data_iq,variables= c(temp_var[6:10], "total_cases", 
                                          "week_start_date"), x="week_start_date")


datatemp_sj <- data_sj
datatemp_sj[temp_var]<-datatemp_sj[temp_var]*10

plotly.line.function(datatemp_sj,variables= c(temp_var[1:5], "total_cases", 
                                          "week_start_date"), x="week_start_date")

plotly.line.function(datatemp_sj,variables= c(temp_var[6:10], "total_cases", 
                                          "week_start_date"), x="week_start_date")

rm(temp_var)


#### 2.3. HUMIDITY & PRECIP IQ & SJ _______________________________________ ####

# Get the humidity variables
humid_precip_var<-names(data[grep("humid|precip", names(data))])

# humidity variables train & validation 
geom.density.function(data=data_iq, variables=c(humid_precip_var, "source"), 
                      fill="source")

geom.density.function(data=data_sj, variables=c(humid_precip_var, "source"), 
                      fill="source")

# Plot the progression of the temperature variables
plotly.line.function(data_iq,variables= c(humid_precip_var, "total_cases", 
                                          "week_start_date"), x="week_start_date")

plotly.line.function(data_sj,variables= c(humid_precip_var, "total_cases", 
                                          "week_start_date"), x="week_start_date")

rm(temp_humid_precip)

#### 2.4. VEGETATION IQ & SJ ______________________________________________ ####

# Get the vegetation variables
var_veg<-names(data[grep("ndvi", names(data))])

# humidity variables train & validation 
geom.density.function(data=data_iq, variables=c(var_veg, "source"), 
                      fill="source")

geom.density.function(data=data_sj, variables=c(var_veg, "source"), 
                      fill="source")

# Plot the progression of the veg variables
dataveg_iq <- data_iq
dataveg_sj <- data_sj
dataveg_iq[var_veg]<-dataveg_iq[var_veg]*100
dataveg_sj[var_veg]<-dataveg_sj[var_veg]*1000

plotly.line.function(dataveg_iq,variables= c(var_veg, "total_cases", 
                                          "week_start_date"), x="week_start_date")

plotly.line.function(dataveg_sj,variables= c(var_veg, "total_cases", 
                                          "week_start_date"), x="week_start_date")

rm(var_veg,dataveg_iq, dataveg_sj)

#### 3.0  FEATURE ENGINEERING _____________________________________________ ####
#### 4.0  RELATIONSHIP BETWEEEN VARIABLES #### 

#### 4.1. REMOVE COLLINEARITY _____________________________________________ #### 

# Save categorical variables and create a df withouth them
cat_var<-c("week_start_date", "city", "year", "weekofyear", "source")
data_cor_matrix_iq<- data_iq %>% select(-cat_var) 
data_cor_matrix_sj<- data_sj %>% select(-cat_var) 

# Remove collinearity from data and validation
data_iq<-cbind(remove.collinearity(data=data_cor_matrix_iq,         # 27 <- 23
                                   indep_var="total_cases",
                                   threshold=0.80),data_iq[cat_var])

data_sj<-cbind(remove.collinearity(data=data_cor_matrix_sj,         # 27 <- 18
                                   indep_var="total_cases",
                                   threshold=0.80),data_sj[cat_var])


rm(cat_var, data_cor_matrix_iq,data_cor_matrix_sj)


#### 4.2. VAR IMP WITH RANDOM FOREST_______________________________________ ####
# Let's create new variables using the lag
var_lag_iq<-names(data_iq %>% select(-c(year, weekofyear, source, city, week_start_date, month,
                            total_cases)))

var_lag_sj<-names(data_sj %>% select(-c(year, weekofyear, source, city, week_start_date, month,
                            total_cases)))

data_iq_lag1<-sapply(data_iq[var_lag_iq], lag)
colnames(data_iq_lag1) <- paste(colnames(data_iq_lag1), 'lag01', sep='_')

data_iq_lag2<-apply(data_iq[var_lag_iq], 2,function(x) lag(x,n=2))
colnames(data_iq_lag2) <- paste(colnames(data_iq_lag2), 'lag02', sep='_')

data_sj_lag1<-sapply(data_sj[var_lag_sj], lag)
colnames(data_sj_lag1) <- paste(colnames(data_sj_lag1), 'lag01', sep='_')

data_sj_lag2<-apply(data_sj[var_lag_sj], 2,function(x) lag(x,n=2))
colnames(data_sj_lag2) <- paste(colnames(data_sj_lag2), 'lag02', sep='_')

data_iq_withlags<-cbind(data_iq, data_iq_lag1, data_iq_lag2)
data_sj_withlags<-cbind(data_sj, data_sj_lag1, data_sj_lag2)

rm(data_sj_lag1, data_sj_lag2, data_iq_lag1, data_iq_lag2, 
   var_lag_iq, var_lag_sj)

# Let's remove the validation
data_iq_withlags_data<- data_iq_withlags %>% 
  filter(source=="data")%>% 
  filter(complete.cases(.))

data_sj_withlags_data<- data_sj_withlags %>% 
  filter(source=="data")%>% 
  filter(complete.cases(.))

rf_iq<-randomForest(total_cases~. -year-week_start_date-source, 
                    data= data_iq_withlags_data, 
                    importance=T,maximize=T,
                    method="rf", 
                    ntree=500)

rf_sj<-randomForest(total_cases~. -year-week_start_date-source, 
                    data= data_sj_withlags_data,
                    importance=T,maximize=T,
                    method="rf", 
                    ntree=500)

varImp(rf_iq, scale=T)
varImp(rf_sj, scale=T)

#### 4.3. rpart ___________________________________________________________ ####
rpart_iq <- rpart(total_cases~., data=data_iq_withlags[ , -which(names(data_iq) %in% 
            c("year","week_start_date", "weekofyear"))], 
            control = list(maxdepth = 4, minbucket=25))

rpart_sj <- rpart(total_cases~., data=data_sj_withlags[ , -which(names(data_sj) %in% 
            c("year","week_start_date", "weekofyear"))], 
            control = list(maxdepth = 4, minbucket=25))

rpart.plot(rpart_iq,roundint=FALSE)
rpart.plot(rpart_sj,roundint=FALSE)

rm(rpart_iq, rpart_sj)
#### 5.   PREPARING TRAINING AND TEST #### 
# Let's take use createdatapartition and the city
# indices_iq<- createDataPartition(data_iq$total_cases, p=0.7, list=F)
# indices_sj<- createDataPartition(data_sj$total_cases, p=0.7, list=F)

# training_iq<- data_iq[indices_iq,]
# training_sj<- data_sj[indices_sj,]
# testing_iq<- data_iq[-indices_iq,]
# testing_sj<- data_sj[-indices_sj,]

# rm(indices_iq, indices_sj)

training_iq<- data_iq_withlags_data %>% filter(year<=year("2008-01-01"))
testing_iq <- data_iq_withlags_data %>% filter(year>=year("2009-01-01"))

training_sj<- data_sj_withlags_data %>% filter(year<=year("2006-01-01"))
testing_sj <- data_sj_withlags_data %>% filter(year>=year("2007-01-01"))

# Let's check if we have the same distribution in our dependent variable
ggplot(data = training_iq, aes(x=total_cases))+
  geom_density(fill="blue",alpha = 0.2) +
  # Change the fill colour to differentiate it
  geom_density(data=testing_iq, fill="red",alpha = 0.2) +
  labs(title = "Distribution of total cases training/testing")+
  labs(y="Density")+
  labs(x="Total cases")

ggplot(data = training_sj, aes(x=total_cases))+
  geom_density(fill="blue",alpha = 0.2) +
  # Change the fill colour to differentiate it
  geom_density(data=testing_sj, fill="red",alpha = 0.2) +
  labs(title = "Distribution of total cases training/testing")+
  labs(y="Density")+
  labs(x="Total cases")

#### 6.   MODELING: BASELINE - RANDOM FOREST zoo NA _______________________ #### 
# Results: RMSE  25.45    MAE   12.25   Rsquared  0.57

# Variables
var_imp_iq<-varImp(rf_iq, scale=T)
var_imp_sj<-varImp(rf_sj, scale=T)


var_relev_iq<- rownames(var_imp_iq)[order(var_imp_iq$Overall, decreasing=TRUE)][1:12]
var_relev_sj<- rownames(var_imp_sj)[order(var_imp_sj$Overall, decreasing=TRUE)][1:12]


training_iq<- training_iq %>% select(-c(year,week_start_date))
training_sj<- training_sj %>% select(-c(year,week_start_date))

rf_iq<-randomForest(y=training_iq$total_cases, 
                    x= training_iq[var_relev_iq],
                 importance=T,maximize=T,
                 method="rf", 
                 ntree=700)

rf_sj<-randomForest(y=training_sj$total_cases, 
                    x= training_sj[var_relev_sj],
                    importance=T,maximize=T,
                    method="rf", 
                    ntree=700)


predictions_iq<- predict(rf_iq, testing_iq)
predictions_sj<- predict(rf_sj, testing_sj)

postResample(predictions_iq, testing_iq$total_cases) 
postResample(predictions_sj, testing_sj$total_cases) 

hist(predictions_iq-testing_iq$total_cases)
hist(predictions_sj-testing_sj$total_cases)

rm(var_imp_iq, var_imp_sj, predictions_iq, predictions_sj, rf_iq, rf_sj,
   var_relev_iq, var_relev_sj)

#### 6.1. MODELING: RANDOM FOREST WITH EVERYTHING __________________________ ####
rf_iq<-randomForest(total_cases~.,data=training_iq,
                    importance=T,maximize=T,
                    method="rf", 
                    ntree=700)

rf_sj<-randomForest(total_cases~.,data=training_sj,
                    importance=T,maximize=T,
                    method="rf", 
                    ntree=700)

predictions_iq<- predict(rf_iq, testing_iq)
predictions_sj<- predict(rf_sj, testing_sj)

postResample(predictions_iq, testing_iq$total_cases) 
postResample(predictions_sj, testing_sj$total_cases) 

hist(predictions_iq-testing_iq$total_cases)
hist(predictions_sj-testing_sj$total_cases)

# Final model
rf_iq<-randomForest(total_cases~.,data=data_iq_withlags_data,
                    importance=T,maximize=T,
                    method="rf", 
                    ntree=700)

rf_sj<-randomForest(total_cases~.,data=data_sj_withlags_data,
                    importance=T,maximize=T,
                    method="rf", 
                    ntree=700)

#### 7.   VALIDATION ####
validation_iq<- data_iq_withlags %>% filter(city=="iq") %>% 
  filter(source=="validation")

validation_sj<- data_sj_withlags %>% filter(city=="sj") %>% 
  filter(source=="validation")

validation_iq<- na.locf(validation_iq)
validation_sj<- na.locf(validation_sj)

predictions_iq<- predict(rf_iq, validation_iq)
predictions_iq<- round(predictions_iq)
predictions_iq

validation_iq<- validation_iq %>% mutate(total_cases=predictions_iq) %>% 
  arrange(week_start_date) %>%
  select(city, year, weekofyear, total_cases)

predictions_sj<- predict(rf_sj, validation_sj)
predictions_sj<- round(predictions_sj)
predictions_sj
validation_sj<- validation_sj %>% mutate(total_cases=predictions_sj) %>% 
  arrange(week_start_date) %>%
  select(city, year, weekofyear, total_cases)

final_dataset<- rbind(validation_sj,validation_iq) 

write.csv(final_dataset,"./submissions/solution7.csv",row.names=FALSE)



