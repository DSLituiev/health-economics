# install.packages(c('dplyr', 'tidyr'))
# install_github('walkerke/tidycensus')
# plotting geo data: https://randomjohn.github.io/r-maps-with-census-data/

# install.packages("devtools")
library(devtools)
# install_local('~/repos/pROC')
# install.packages(c('dplyr', 'tidyr', 'ggplot2', 'viridis', 'RColorBrewer', 'shadowtext'))

# install.packages('ggpmisc')
# install.packages('maps')

# install_github('walkerke/tidycensus')

library(pROC)
library(dplyr)
library(tidyr)
# library(gplots)
library(ggplot2)
library(ggpmisc)
# library(reshape2)
library(stringr)
#install.packages("wesanderson")
# library(wesanderson)  # color palettes
# install.packages("RColorBrewer")
library("RColorBrewer") # color palettes
# install.packages("viridis")
library(viridis)

# install.packages('shadowtext')
library(shadowtext)
library(tidycensus)
library(maptools)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(getSrcDirectory()[1])
# 
# data <- read.csv('meta_full.csv')
# dim(data)
# # sum(!is.na(data$sum_totcost_1yr5))
# # sum(!is.na(data$all_totcost_yr1to5))
# 
# # data <- data[!(is.na(data$all_totcost_yr1to5)),]
# dim(data)
# 
# data$zip_code <- factor(data$zip_code)
# colnames(data)
# head(data)
# 
# 
# key = as.character(read.csv('census.key', header = F)[1,1])
# census_api_key(key, overwrite = T, install = T)
# # mo.lm.zip <- lm(all_totcost_yr1to5 ~ zip_code, data)
# 
# geodata <- get_acs(geography = "zcta", 
#               variables = c(medincome = "B19013_001"), 
#               key=key)
# geodata <- geodata %>% rename( !!c('median_income' = "estimate", 'median_income_moe' ="moe"))
# 
#####################
# library(rgdal)    # for readOGR and others
# # tract <- rgdal::readOGR(dsn=".", layer = "cb_2014_36_tract_500k")
# library(maps)
# ca_map <- map_data("county",region="california")
# 
#####################

# head(geodata)
# table(geodata$variable)
# unique(data$zip_code)
# 
# 
# 
# dataj <- inner_join(data, geodata[,c('GEOID','median_income',  'median_income_moe')],
#                     by=c('zip_code'='GEOID'))
# 
# dim(dataj)
# head(dataj)
# 
# hist(dataj$median_income)
dataj <-read.csv('demo_billed_costs_quarts_income.csv')
dim(dataj)

sort(table(dataj$zip_code), decreasing=T)[1:10]


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(getSrcDirectory()[1])

data <- read.csv('demo_billed_costs_quarts.csv')
dim(data)
# sum(!is.na(data$sum_totcost_1yr5))
# sum(!is.na(data$sum_totcost_5yr1))

# data <- data[!(is.na(data$sum_totcost_5yr1)),]
dim(data)

data$zip_code <- factor(data$zip_code)
colnames(data)
head(data)


key = as.character(read.csv('census.key', header = F)[1,1])
census_api_key(key, overwrite = T, install = T)
# mo.lm.zip <- lm(sum_totcost_5yr1 ~ zip_code, data)

geodata <- get_acs(geography = "zcta", 
              variables = c(medincome = "B19013_001"), 
              key=key)
geodata <- geodata %>% rename( !!c('median_income' = "estimate", 'median_income_moe' ="moe"))

head(geodata)
table(geodata$variable)
unique(data$zip_code)



dataj <- inner_join(data, geodata[,c('GEOID','median_income',  'median_income_moe')],
                    by=c('zip_code'='GEOID'))

dim(dataj)
head(dataj)

hist(dataj$median_income)
write.csv(dataj, 'demo_billed_costs_quarts_income.csv')
