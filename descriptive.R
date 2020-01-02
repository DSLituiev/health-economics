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
library(latex2exp)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(getSrcDirectory()[1])
rm(list = ls(all.names = TRUE))
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
##########################
stat_box_data_thousand_dollar <- function(x, y=NA) {
  med = median(x)
  if (is.na(y)) y = med
  return( 
    data.frame(
      y = y,
      label = paste0('n = ', 
                     format(length(x), big.mark = ",", decimal.mark = ".", scientific = FALSE), 
                     '\n', '\n',
                     'md = $', 
                     format(round(10^(med-3), 0), big.mark = ",", decimal.mark = ".", scientific = FALSE),
                     'K')
    )
  )
}



stat_box_data_dollar <- function(x, y=NA) {
  med = median(x)
  if (is.na(y)) y = med
  return( 
    data.frame(
      y = y,
      label = paste0('n = ', 
                     format(length(x), big.mark = ",", decimal.mark = ".", scientific = FALSE), 
                     '\n', '\n',
                     '$', 
                     format(round(10^(med), 0), big.mark = ",", decimal.mark = ".", scientific = FALSE)
                     )
    )
  )
}
##########################


# dataj <-read.csv('demo_billed_costs_quarts_income.csv')
dataj <-read.csv('data/inspect5.csv')
dim(dataj)
head(dataj)

level_key <- c(`1` = "male", `2`="female")
dataj$sex <- recode(dataj$sex, !!!level_key)


dataj$zip_code <- as.factor(dataj$zip_code)
dataj$cxr_date <- as.Date(dataj$cxr_date, tryFormats = c("%Y-%m-%d"))
# dataj$cxr_date <- as.Date(dataj$cxr_date, tryFormats = c("%Y-%m-%d"))

sort(table(dataj$zip_code), decreasing=T)[1:10]


cols2sum <- c('all_totcost_yr1', 'all_totcost_yr2',
              'all_totcost_yr3', 'all_totcost_yr4', 'all_totcost_yr5')

# dataj[,'all_totcost_y1to5'] <- rowSums(dataj[,cols2sum])
colnames(dataj)

dataj[(dataj$sex == 'Unknown'),'sex'] <- NA

plot(dataj$median_income, log10(dataj$all_totcost_y1to5) )

plot.formula <- y~x
scatter_loglog <- ggplot(dataj, aes(y=log10(all_totcost_y1to5), x=log10(median_income))) +
  geom_point(alpha=0.25) + geom_smooth(method='lm', formula= plot.formula) +
  stat_poly_eq(formula = plot.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)




mask = !is.na(dataj$all_totcost_y1to5) &
  (dataj$all_totcost_y1to5>0) &
  !is.na(dataj$sex)
sum(mask)

data_masked <- dataj[mask,][-c(6217, 9556, 20639),]
res_random <- lm(log10(all_totcost_y1to5+1) ~ 1, data=data_masked)

res_race <- lm(log10(all_totcost_y1to5+1) ~ race, data=data_masked)
res_sex <- lm(log10(all_totcost_y1to5+1) ~ sex, data=data_masked)
res_age_factor <- lm(log10(all_totcost_y1to5+1) ~ factor(age), data=data_masked)

res_sex_agef <- lm(log10(all_totcost_y1to5+1) ~ sex + factor(age), data=data_masked)
res_sex_agef_race <- lm(log10(all_totcost_y1to5+1) ~ sex + factor(age) + race, data=data_masked)


kt.res <- kruskal.test(log10(all_totcost_y1to5+1) ~ zip_code, data=data_masked)
kt.res

kt.res <- kruskal.test(log10(all_totcost_y1to5+1) ~ sex, data=data_masked)
kt.res


kt.res <- kruskal.test(log10(all_totcost_y1to5+1) ~ factor(age), data=data_masked)
kt.res

aov_ <- aov(log10(all_totcost_y1to5+1) ~
              sex + factor(age) + race + median_income +
              sex *  factor(age) + sex*race +  factor(age)*race +
              median_income*sex + median_income * factor(age) + median_income*race,
            data=data_masked)

summary(aov_)


aov_ <- aov(log10(all_totcost_y1to5+1) ~
              sex + factor(age) + race + sex *  factor(age) + sex*race +  factor(age)*race,
            data=data_masked)

summary(aov_)


plot(aov_, 1)

# 2. Normality
plot(aov_, 2)

anova(res_sex)
summary(res_sex)
anova(res_random, res_sex, res_sex_agef, res_sex_agef_race)

t.test(log10(all_totcost_y1to5+1) ~ sex, data=dataj[mask,])

t.test((all_totcost_y1to5+1) ~ sex, data=dataj[mask,])

t.test(log10(all_totcost_y1to5+1) ~ factor(age), data=dataj[mask,])

################################
# QQ-plot
ggplot(dataj, aes(sample=log10(all_totcost_y1to5+1))) + 
    geom_qq(color='blue') + stat_qq_line() +
    theme_bw() +
    ggsave('qqplot_log10.pdf', width=4, height = 4)


set.seed(123)

x <- dataj$all_totcost_y1to5


ll_norm <- function(param){
  if ((param[2]>0)&(param[2]>0)) return(-sum(dnorm(x,param[1],param[2],log=T)))
  else return(Inf)
}
AIC_norm = 2*optim(c(1e3,1),ll_norm)$value + 2*2
# Loglikelihood and AIC for lognormal model

ll_lognorm = function(param){
  if(param[2]>0) return(-sum(dlnorm(x,param[1],param[2],log=T)))
  else return(Inf)
}

AIC_lognorm = 2*optim(c(0,1),ll_lognorm)$value + 2*2

# Loglikelihood and AIC for Pareto model

dpareto=function(x, shape=1, location=1) shape * location^shape / x^(shape + 1)

ll_pareto = function(param){
  if(param[1]>0 & min(x)>=param[2]) return(-sum(log(dpareto(x,param[1],param[2]))))
  else return(Inf)
}


AIC_pareto = 2*optim(c(1,0.01),ll_pareto)$value + 2*2

# Comparison using AIC, which in this case favours the lognormal model.

c(AIC_norm, AIC_lognorm, AIC_pareto)

##############################
# ALL
txt_expenditure <- '5 year expenditure'
formatter <- scales::trans_format("log10", scales::math_format('$'*10^.x))

math_format <- function (expr = '$'*10^.x, accuracy = NULL, scale = 1, prefix = "$", suffix = "", format = force) 
{
  quoted <- substitute(expr)
  subs <- function(x) {
    res <-  do.call("substitute", list(quoted, list(.x = as.name(x))))
    # res <-  do.call(function(x) paste0(prefix, x, suffix)), res)
    res
  }
  function(x) {
    x <- format(x)
    x <- lapply(x, subs)
    
    
    # x <- scales::dollar(x, accuracy = accuracy, scale = scale, 
    #             prefix = prefix, suffix = suffix)
    x
  }
}
math_format()(c(1,2,3,4))

###


# formatter_plain <- scales::dollar_format(1, scale=1000, suffix = "")
# breaks = 2:7
# plain_hist_ <- ggplot(dataj, aes(x=all_totcost_y1to5+1),) + geom_histogram(alpha=0.5, fill='blue') +
#   scale_x_continuous(
#     # breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = formatter_plain
#   ) + 
#   theme(text = element_text(size=16)) + 
#   annotation_logticks(sides = 'b') +
#   labs(x='5 year expenditure', x='count') + theme_bw()
# 
# plain_hist_ + ggsave('histogram.pdf', width=4, height = 4)
###

breaks = 2:7
hist_ <- ggplot(dataj, aes(x=all_totcost_y1to5+1),) + geom_histogram(alpha=0.5, fill='blue') +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = formatter
  ) + 
  theme(text = element_text(size=16)) + 
  annotation_logticks(sides = 'b') +
  labs(x='5 year expenditure', x='count') + theme_bw()


hist_ + ggsave('log_histogram.pdf', width=4, height = 4)

###############################
# SEX

bp_sex <- ggplot(dataj[!is.na(dataj$sex),], aes(y=all_totcost_y1to5,
                                                group=sex,
                                                x=sex)) +
  geom_boxplot(alpha=0.25, fill='blue') +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = formatter
  ) +
  annotation_logticks(sides = 'lr') +
  labs(y=txt_expenditure, x='') + theme_bw() + 
  stat_summary(
    fun.data = function(x) stat_box_data_dollar(x), 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    size=3.6,
  ) 

bp_sex +
  theme(text = element_text(size=17.5)) +
  geom_hline(yintercept=median(dataj$all_totcost_y1to5), #linetype="dashed", 
             color = "red", size=.5, alpha=0.4) +
  ggsave('sex_boxplot.pdf', width=3, height = 4)


# formatter <- scales::trans_format("log10", scales::math_format(10^.x))

# ggplot(diamonds, aes(y=price, x=color)) +
#   geom_boxplot(alpha=0.25, fill='blue') +
#   scale_y_log10(
#     breaks = scales::trans_breaks("log10", function(x) 10^x),
#     labels = formatter
#   ) 
########################################################
# RACE

names(table(dataj$race))

rename_race <- list(`American Indian or Alaska Native`="native",
     `Asian`= 'asian',
     `Black or African American`='african',
     `Native Hawaiian or Other Pacific Islander`='pacific',
      `Other` = 'other',
     `Unknown/Declined` = NA,
      `White or Caucasian` = 'caucasian')

rename_race <- unlist(rename_race)

race_renamer <- function(x) rename_race[x]

dataj$race_short <- factor(race_renamer(dataj$race),
                              levels=c("caucasian" , 'asian', "african", "pacific", "native" ,"other")
                              )

(table(dataj$race_short))


race_expenses <- dataj %>% group_by(race_short) %>% 
  summarise_at(vars((all_totcost_y1to5)), median) %>% 
  arrange(desc(all_totcost_y1to5))

dataj$race_short <- factor(race_renamer(dataj$race),
                           levels=race_expenses$race_short
)


bp_race <- ggplot(dataj, 
                 aes(y=all_totcost_y1to5, x=factor(race_short)),) +
  geom_boxplot(alpha=0.25, fill='blue', notch=T) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = formatter
  ) + annotation_logticks(sides = 'lr') +
  labs(y=txt_expenditure, x='') +
  theme_bw() +
  theme(text = element_text(size=16)) + 
  stat_summary(
    fun.data = function(x) stat_box_data_dollar(x), 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    size=3.6
  )

bp_race  +
  geom_hline(yintercept=median(dataj$all_totcost_y1to5), #linetype="dashed", 
             color = "red", size=.5, alpha=0.4) +
  ggsave('race_boxplot.pdf', width=8, height = 4)


########################################################
# AGE
dataj$age_factor = factor(dataj$calc_age)
age_counts <- dataj %>% group_by(age_factor) %>% tally()

dim(age_counts)
max(age_counts$n)
max_age <- as.numeric(as.character(age_counts$age_factor[which.min(with(age_counts, n > 3))]))


plot.formula <- (y)~x
age_breaks <- seq(20,105,10)

# add jitter for privacy
scatter_age <- ggplot(dataj,
  #dataj[dataj$age<max_age,], 
                      aes(y=(all_totcost_y1to5), x=calc_age)) +
  geom_jitter(width = 0.5, height = 0.0, alpha=0.25) + 
  geom_smooth(method='lm', formula= plot.formula) +
  stat_poly_eq(formula = plot.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x=0.8) +
  labs(y='5 year expenses, $')

scatter_age +
  scale_x_continuous(breaks=age_breaks) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = formatter
  ) +
  theme_bw() + annotation_logticks(sides = 'lr') + 
  ggsave('age_scatter.png', width=8, height = 4)


##############
# Age boxplot



bp_age <- ggplot(dataj[dataj$age<max_age,], 
                 aes(y=all_totcost_y1to5, x=factor(age)),) +
  geom_boxplot(alpha=0.25, fill='blue') +
  scale_x_discrete(breaks=seq(15,105,5)) + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = formatter
  ) + labs(y='5 year expenses, $', x='age') + 
  annotation_logticks(sides = 'lr') 

bp_age + 
  geom_hline(yintercept=median(dataj$all_totcost_y1to5), #linetype="dashed", 
             color = "red", size=.5, alpha=0.5) +
  theme_bw() + 
  theme(text = element_text(size=18)) +
  ggsave('age_boxplot.pdf', width=8, height = 4)

##############
# AGE * SEX
dataj$age_5 <- cut(dataj$age, seq(15,105,5))

bp_age_sex <- ggplot(dataj[dataj$age<max_age,], 
                 aes(y=all_totcost_y1to5, 
                     x=age_5, fill=sex, group=sex,  colour=sex)) +
  geom_boxplot(alpha=0.5, aes(fill=sex), position=-1) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = formatter
  ) + labs(y='5 year expenses, $', x='age') #+ 
  # annotation_logticks(sides = 'lr') 

bp_age_sex + scale_x_discrete(breaks=levels(dataj$age_5), labels=seq(20,105,5)) + 
  scale_fill_manual(values=c("#F8766D", "#619CFF")) +
  #facet_wrap(~sex, ncol=1)  +
  theme_bw() + annotation_logticks(sides = 'lr') + 
  geom_hline(yintercept=median(dataj$all_totcost_y1to5),
             color = "red", size=.5, alpha=0.5) +
ggsave('age_sex_boxplot.pdf', width=8, height = 4)
#####################
### ZIP

plot.formula <- y~x
breaks <- 2:7
mask_zip <- !is.na(dataj$median_income) & !is.na(dataj$all_totcost_y1to5)

scatter_loglog <- ggplot(dataj[mask_zip,], aes(y=(1+all_totcost_y1to5), x=(1+median_income))) +
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


scatter_loglog + ggsave('zip_scatter.png', width=4, height = 4)


library(hexbin)
hex_loglog <- ggplot(dataj[mask_zip,], aes(y=(1+all_totcost_y1to5), x=(1+median_income))) +
  scale_y_continuous(trans = 'log10',
                     breaks = 10^breaks,
                     labels = formatter,
  ) +
  geom_hex() +
  geom_smooth(method='lm', formula= plot.formula) +
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

hex_loglog + ggsave('zip_hex.png', width=4, height = 4)


#########################
# Race x Age

stat_box_data <- function(x, y=NA) {
  med = median(x)
  if (is.na(y)) y = med
  return( 
    data.frame(
      y = y,
      label = paste0('n=', 
                     format(length(x), big.mark = ",", decimal.mark = ".", scientific = FALSE), 
                     '\n', '\n',
                     'md = ', 
                     format(round(med, 0), big.mark = ",", decimal.mark = ".", scientific = FALSE)
                     )
    )
  )
}

ggplot(dataj, aes(x=race_short, y=age)) +
  geom_boxplot(outlier.colour = NA, notch = T,alpha=0.25, fill='blue') +
  scale_y_continuous(breaks=as.numeric(age_breaks)) + 
  coord_cartesian(ylim=c(15,108)) +
  labs(y='age', x='')+
  theme_bw() + theme(text = element_text(size=16)) +
  stat_summary(
    fun.data = function(x) stat_box_data(x), 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.5,
    size=3.6
  ) +
    geom_hline(yintercept=median(dataj$age),
               color = "red", size=.5, alpha=0.5) +
    ggsave('age_race_boxplot.pdf', width=8, height = 4)

  
