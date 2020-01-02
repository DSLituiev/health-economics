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
# # sum(!is.na(data$all_totcost_y1to5))
# 
# # data <- data[!(is.na(data$all_totcost_y1to5)),]
# dim(data)
# 
# data$zip_code <- factor(data$zip_code)
# colnames(data)
# head(data)
# 
# 
# key = as.character(read.csv('census.key', header = F)[1,1])
# census_api_key(key, overwrite = T, install = T)
# # mo.lm.zip <- lm(all_totcost_y1to5 ~ zip_code, data)
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




cols2sum <- c('all_totcost_yr1', 'all_totcost_yr2',
              'all_totcost_yr3', 'all_totcost_yr4', 'all_totcost_yr5')

dataj[,'all_totcost_y1to5'] <- rowSums(dataj[,cols2sum])
colnames(dataj)

dataj[(dataj$sex == 'Unknown'),'sex'] <- NA

plot(dataj$median_income, log10(dataj$all_totcost_y1to5) )




plot.formula <- y~x
scatter_loglog <- ggplot(dataj, aes(y=log10(all_totcost_y1to5), x=log10(median_income))) +
  geom_point(alpha=0.25) + geom_smooth(method='lm', formula= plot.formula) +
  stat_poly_eq(formula = plot.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)

scatter_loglog + labs(x='income, $', y='5 yr expenditure, $') + ggsave('zip_scatter.png', width=4, height = 4)

mask_major_zip <- dataj$zip_code %in% names(sort(table(dataj$zip_code), decreasing=T)[1:12])


octiles <- quantile(dataj$median_income, c(0.125, 1-0.125), na.rm=T)
mask_income <- (dataj$median_income > octiles[1]) & (dataj$median_income < octiles[2])

mean(mask_major_zip)

mask = !is.na(dataj$all_totcost_y1to5) &
  (dataj$all_totcost_y1to5>0) &
  !is.na(dataj$median_income) & !is.na(dataj$sex) &
   !mask_major_zip & mask_income

res_random <- lm(log10(all_totcost_y1to5+1) ~ 1, data=dataj[mask,])
res_income <- lm(log10(all_totcost_y1to5+1) ~ log10(median_income), data=dataj[mask,])


(anova(res_random, res_income))

res_income_age <- lm(log10(all_totcost_y1to5+1) ~ log10(median_income) + age, data=dataj[mask,])
res_income_age_sex <- lm(log10(all_totcost_y1to5+1) ~ log10(median_income) + age + sex, data=dataj[mask,])
res_income_age_sex_race <- lm(log10(all_totcost_y1to5+1) ~ log10(median_income) + age + sex + race, data=dataj[mask,])


print(summary(res_income))
print(summary(res_income_age_sex))

anova(res_income, res_income_age, res_income_age_sex, res_income_age_sex_race)

res.aov <- aov(log10(all_totcost_y1to5+1) ~ log10(median_income) + age + sex + race, data=dataj[mask,])

summary(res.aov)
#################33
# install.packages('pspearman')
library(pspearman)

plot.formula <- y~x
breaks <- 2:7
mask_zip <- !is.na(dataj$median_income) & !is.na(dataj$all_totcost_y1to5)

mask_zip_robust = !is.na(dataj$all_totcost_y1to5) &
  (dataj$all_totcost_y1to5>0) &
  !is.na(dataj$median_income) & !is.na(dataj$sex) &
  !mask_major_zip & mask_income

cor.test(log10(1+dataj[mask_zip_robust,]$all_totcost_y1to5),
    log10(1+dataj[mask_zip_robust,]$median_income))

cor.test(log10(1+dataj[mask_zip_robust,]$all_totcost_y1to5),
         log10(1+dataj[mask_zip_robust,]$median_income), method='spearman', exact=F)

# spearman.test(log10(1+dataj[mask,]$all_totcost_y1to5),
#               log10(1+dataj[mask,]$median_income))


# add jitter for privacy
scatter_loglog <- ggplot(dataj[mask_zip_robust,],
                         aes(y=(1+all_totcost_y1to5), x=(1+median_income))) +
  geom_jitter(width = 0.01, height = 0.0, alpha=0.25, colour = "blue") +
  scale_y_continuous(trans = 'log10',
                     breaks = 10^breaks,
                     labels = formatter,
  ) +
  geom_smooth(method='lm', formula= plot.formula, colour = "black") +
  stat_poly_eq(formula = plot.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               label.y = 0.97,
               label.x = 0.9,
               parse = TRUE) +
  labs(y=txt_expenditure, x='median income, $') + 
  scale_x_continuous(trans = 'log10',
                     minor_breaks=NULL,
                     breaks =  c(3e4, 4e4, 5e4, 6e4, 7e4, 8e4, 9e4, 1e5, 2e5, 3e5),
                     limits = c(3e4, 3e5),
                     labels = c(TeX('$3\\cdot 10^4$'), '','','','',  '','',TeX('10^5$'), TeX('$2\\cdot 10^5$'), TeX('$3\\cdot 10^5$'))
  ) + # scales::trans_format('log10', function(x) round(x)))+
  annotation_logticks(sides = 'lr') + 
  theme_bw() + theme(text = element_text(size=16)) 


scatter_loglog + ggsave('zip_nomajor_noextreme_scatter.png', width=4, height = 4)


