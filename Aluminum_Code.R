library(tidyverse)
library(dplyr)
library(feather)
library(caret)
library(Metrics)
library(ggplot2)
library(dataRetrieval)
library(nhdplusTools)
library(tidycensus)
library(feather)
######################################################################

Alum_test <- read_feather("C:/Users/robm3/Downloads/R_Code/Create_Proj2_Data/AluminumFeatherCorrected")

Alum_short <- Alum_test %>% 
  select(MonitoringLocationIdentifier, state, dec_lat_va, dec_lon_va, sample_year, sample_season, sample_month, ActivityStartDate, 
         ResultMeasureValue, ResultMeasure.MeasureUnitCode)

Alum_short_omit <- Alum_short[!is.na(Alum_short$ResultMeasureValue),]
##as.numeric(As_short_omit$ResultMeasureValue)

Alum_col1 <- transform(Alum_short_omit, ResultMeasureValue = as.numeric(ResultMeasureValue),
                     dec_lat_va = as.numeric(dec_lat_va),
                     dec_lon_va = as.numeric(dec_lon_va))
unique(Alum_col1$ResultMeasure.MeasureUnitCode)
ThousandTime <- Alum_col1 %>% 
  filter(ResultMeasure.MeasureUnitCode == "mg/kg" |
           ResultMeasure.MeasureUnitCode == "mg/l" |
           ResultMeasure.MeasureUnitCode == "mg/L" |
           ResultMeasure.MeasureUnitCode == "ppm" |
           ResultMeasure.MeasureUnitCode == "ug/g")

ThousandDivide <- Alum_col1 %>% 
  filter(ResultMeasure.MeasureUnitCode == "ng/L")

MillionTime <- Alum_col1 %>% 
  filter(ResultMeasure.MeasureUnitCode == "mg/g")

NoTransform <- Alum_col1 %>% 
  filter(ResultMeasure.MeasureUnitCode == "ppb" |
           ResultMeasure.MeasureUnitCode == "ug/kg" |
           ResultMeasure.MeasureUnitCode == "ug/l" |
           ResultMeasure.MeasureUnitCode == "ug/L")
###############################################################

ThousandDivide$ResultMeasureValue <- ThousandDivide$ResultMeasureValue / 1000
##MillionTime$ResultMeasureValue <- MillionTime$ResultMeasureValue * 1000000
ThousandTime$ResultMeasureValue <- ThousandTime$ResultMeasureValue * 1000

HarmonizedData <- rbind(ThousandDivide, ThousandTime, NoTransform)
Alum_HarmData_NA_omit <- HarmonizedData[!is.na(HarmonizedData$ResultMeasureValue),]

Alum_FilterNeg <- Alum_HarmData_NA_omit %>% 
  filter(ResultMeasureValue > 0 & ResultMeasureValue < 5000) %>% 
  filter(ResultMeasure.MeasureUnitCode == "ppb" |
           ResultMeasure.MeasureUnitCode == "ug/kg" |
           ResultMeasure.MeasureUnitCode == "ug/l" |
           ResultMeasure.MeasureUnitCode == "ug/L") %>% 
  filter(sample_year > 1999)


############################################################

Alum_group <- Alum_FilterNeg %>% 
  group_by(MonitoringLocationIdentifier) %>% 
  summarise(Conc_mean = mean(ResultMeasureValue),
            Conc_max = max(ResultMeasureValue),
            Conc_median = median(ResultMeasureValue),
            Conc_min = max(ResultMeasureValue), 
            Conc_sd = min(ResultMeasureValue), 
            Conc_count = n(),
            Latitude = mean(dec_lat_va),
            Longitude = mean(dec_lon_va))

Alum_group_cut1 <- Alum_group %>% 
  filter(Conc_count > 1)

write.csv(Alum_group_cut1, "C:/Users/robm3/Downloads/R_Code/Good_Group_Site/Aluminum_grouped.csv" )

