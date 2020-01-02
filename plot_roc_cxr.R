library(pROC)
library(dplyr)
library(tidyr)
library(gplots)
library(ggplot2)
# library(reshape2)
library(stringr)
#install.packages("wesanderson")
# library(wesanderson)  # color palettes
# install.packages("RColorBrewer")
library("RColorBrewer") # color palettes
# install.packages("viridis")
library(viridis)
# install.packages("devtools")
library(devtools)
# install.packages('shadowtext')
library(shadowtext)
# install_local('~/repos/pROC')

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(getSrcDirectory()[1])


as.data.frame.rows <- function(row.list, col.names=NA){
  if (is.data.frame(row.list)) {return(row.list)}
  
  result <- data.frame(Reduce(rbind, row.list),
                       row.names = names(row.list)
  )
  if (length(col.names) == ncol(result)){ colnames(result) <- col.names }
  result
}
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


## READ predictions and ground truth  ------------------

fn <- 'predictions/all_classification.csv'
data_pred = read.csv(fn)#[,-1]


data_pred$truth <- as.logical(data_pred$truth)
head(data_pred)


data_pred$model_id <-  ordered(data_pred$model_id, 
                               c('LR', 'Adaboost','GBM','X', 'TX1', 'TX2'))

cols <- sort(unique(data_pred$model_id))

rocs = list()
for (id in cols){
  gr <- data_pred[data_pred$model_id==id,]
  rocs[[id]] <- roc(gr$truth, gr$soft1)
}


## Plot ROC curves ---------------------------------------------------------

color.vec = brewer.pal(n = 8, name = "Dark2")

ggroc(rocs, size=1.2) +
  scale_color_manual(values=color.vec) + 
  theme(text = element_text(size=18)) +
  ggsave('roc.png', device='png', dpi=300, width = 6.25, height =5)





## Collect AUCs and their conifdence intervals into a dataframe -----------
roc.auc.conf.intervals <- list()
for (col in cols){
  cis <- ci.auc(rocs[[col]])
  roc.auc.conf.intervals[[col]] <- (cis)
}

write.csv(roc.auc.conf.intervals, 'auc_confidence_intervals.csv')

roc.auc.conf.intervals <- as.data.frame.rows(roc.auc.conf.intervals,
                                             c('LB', 'AUC', 'UB'))

roc.auc.conf.intervals[,"method"] <- ordered(row.names(roc.auc.conf.intervals),
                                             row.names(roc.auc.conf.intervals)
) 

## Plot AUCs and confidence intervals as a horizontal bar graph ----------
ggplot(roc.auc.conf.intervals,
       aes(x=method, y=AUC, label=round(AUC, 2), fill=method)) + 
  scale_fill_manual(values=color.vec) + 
  geom_bar(stat='identity', colour='black', position = position_dodge()) +
  geom_errorbar(aes(ymin=LB, ymax=UB), width=.2, position = position_dodge()) +
  geom_text(size = 10, position = position_stack(vjust = 0.5)) +
  theme(axis.text.x = element_text(face="bold", # color="#993333", 
                                   size=20, angle=0),
        axis.text.y = element_text(face="bold", # color="#993333", 
                                   size=20, angle=0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  # coord_cartesian(xlim = c(.5, 1.0)) +
  coord_flip(ylim = c(.0, 1.0)) +
  theme(text = element_text(size=18)) +
  ggsave('h-bars-auc-50.png', device='png', dpi=300, width = 7, height =5)

## Plot AUCs and confidence intervals as a bar graph ------------------------
ggplot(roc.auc.conf.intervals,
       aes(x=method, y=AUC, label=round(AUC, 2))) + 
  geom_bar(stat='identity', colour='black', fill='808080',
           position = position_dodge()) +
  geom_errorbar(aes(ymin=LB, ymax=UB), width=.2, position = position_dodge()) +
  coord_cartesian(ylim = c(.5, 1.0)) +
  geom_text(size = 6, angle=0, nudge_y=-.05) +
  theme(axis.text.x = element_text(face="bold", # color="#993333", 
                                   size=14, angle=0),
        axis.text.y = element_text(face="bold", # color="#993333", 
                                   size=14, angle=0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12))


## Calculate pairwise difference significance p-values ------------

roc.comparison.pval = data.frame(row.names=as.character(cols))
roc.comparison.pval[,]=NA
for (ii in 2:length(cols)){
  for (jj in 1:(ii-1)){
    print(paste(ii, ':', jj))
    analysis_ <-  roc.test(rocs[[ii]], rocs[[jj]])
    roc.comparison.pval[as.character(cols[[ii]]), as.character(cols[[jj]])] <- analysis_$p.value
  }
}

roc.comparison.neglog10pval <- log10(roc.comparison.pval)
roc.comparison.neglog10pval$method1 <- row.names(roc.comparison.neglog10pval)

roc.comparison.pval.m <- roc.comparison.neglog10pval %>%
  gather(method2, value, -method1)


roc.comparison.pval.m <- roc.comparison.pval.m[!is.na(roc.comparison.pval.m$value),]

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  # l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  # l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  # l <- gsub("e", "%*%10^", l)
  # return this as an expression
  l <- paste("10^", l)
  parse(text=l)
}

## Plot pairwise difference significance p-values ------------

# (p <- ggplot(roc.comparison.pval.m, aes(ordered(method2,cols), ordered(method1,cols))) 
#   + geom_tile(aes(fill = value), colour = "white") 
#   + geom_shadowtext(aes(label = ifelse(is.na(value),"", sprintf("%.2g", 10^value))),
#              colour='black', size=3, fontface = "bold",
#              bg.colour='white') 
#   # + scale_fill_gradient(high = "white", low = "steelblue")
#   + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#   + xlab("") + ylab("") + labs(fill = "-log10 p-value")
#   + scale_fill_viridis(option='magma', trans = "log10") 
#   +   theme(axis.text.x = element_text(face="bold", # color="#993333", 
#                                        size=14, angle=90),
#             axis.text.y = element_text(face="bold", # color="#993333", 
#                                        size=14, angle=0))
#   )

max_log10 = 25
(p <- ggplot(roc.comparison.pval.m, aes(ordered(method2, cols), ordered(method1,cols))) 
  + geom_tile(aes(fill = 10^value), colour = "white") 
  + geom_shadowtext(aes(label = ifelse(is.na(value),"", sprintf("%.2g", 10^value))),
                    colour='black', size=4, fontface = "bold",
                    bg.colour='white') 
  # + scale_fill_gradient(high = "white", low = "steelblue")
  + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  + xlab("") + ylab("") + labs(fill = "p-value")
  + scale_fill_viridis(option='magma', trans = "log10",
                       limits=c(10^-max_log10, 1.0),
                       breaks=c(0.01, 10^seq(-max_log10,-10,10))) 
  +   theme(axis.text.x = element_text(face="bold", # color="#993333", 
                                       size=18, angle=90),
            axis.text.y = element_text(face="bold", # color="#993333", 
                                       size=18, angle=0)) +
    theme_bw()
)


p +   ggsave('pairwise_auc.pdf', device='pdf', dpi=300, width = 6.5, height =5)

