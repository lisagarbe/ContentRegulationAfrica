#----------------------------------------------------------------
# Script: 03_analyses
# Authors: Lisa Garbe, Lisa-Marie Selvik, Pauline Lemaire
# Article: How African countries respond to fake news and hate speech
#----------------------------------------------------------------

# OVERVIEW OF SCRIPT

# 0. Required packages
# 1. Merge datasets
# 2. Linear mixed models
# 3. Plots

## empty memory (!)
rm(list=ls())

#set working directory
setwd("~")

# Read data
# stm corpus
corpus_stm <- readRDS("Data/corpus_stm")
# stm with 35 topics
load("Topic Models/stm_SearchPrevFit_35.RData")
# TOSCO data: please contact lisa.garbe@wzb.eu for replication data
tos <- read.csv("Data/tosco_state_shares.csv")

# functions for model tests: please contact lisa.garbe@wzb.eu for replication code
source("R Code/diagnostic_fcns.r")
source("R Code/glmm_stability.r")

#----------------------------------------------------------------
# 0. Required packages
#----------------------------------------------------------------

#### required packages #####
library(quanteda)
library(stm)
library(tidyr)
library(tidytext)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggsci)
library(ggplot2)
library(lme4)
library(countrycode)
library(vdemdata)
library(texreg)
library(car)
library(broom.mixed)
library(dotwhisker)
library(ggpubr)
library(wesanderson)
library(ggeffects)
library(egg)


#----------------------------------------------------------------
# 1. Merge datasets
#----------------------------------------------------------------

# preparation of the corpus
plotRemoved(corpus_stm$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(corpus_stm$documents, corpus_stm$vocab,
                     corpus_stm$data, lower.thresh = 15)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

names(out$meta)

# make dataframe
dt2 <- make.dt(SearchPrevFit_35, meta=out$meta)
dt2$year <- as.numeric(dt2$year)

### merge with tosco data
dt2 <- merge(dt2,tos, by = c("year","iso"), all.x = T)

#----------------------------------------------------------------
# 2. Linear mixed models
#----------------------------------------------------------------

str(dt2)
xdata <- dt2 %>% select(Topic5,Topic31,share_st,v2x_freexp_altinf,v2xlg_legcon,v2x_jucon,v2x_polyarchy,year,country)
xdata <- na.omit(xdata)

#save data for plots
#write.csv(xdata,"data_for_plots.csv",row.names = F)

##### Checks DVs ##### 

hist(xdata$Topic5) # skewed 
hist(xdata$Topic31) # skewed

# Make logs 
xdata$Topic5_log <- log(xdata$Topic5)
xdata$Topic31_log <- log(xdata$Topic31)

hist(xdata$Topic5_log) # ok
hist(xdata$Topic31_log) # ok

##### Checks IVs ##### 
hist(xdata$v2x_freexp_altinf) # slightly skewed 
hist(xdata$v2x_jucon) # ok
hist(xdata$v2xlg_legcon) # ok
hist(xdata$share_st) # skewed

# standardize IVs
xdata$z.Freexp <- as.vector(scale(xdata$v2x_freexp_altinf))
xdata$z.Jucon <- as.vector(scale(xdata$v2x_jucon))
xdata$z.Legcon <- as.vector(scale(xdata$v2xlg_legcon))
xdata$z.share_st <- as.vector(scale(xdata$share_st))

##### MODELS ##### 

# TOPIC 6 
full_t5 <- lmer(Topic5_log ~ z.Freexp + z.Legcon + z.Jucon +z.share_st + (1|country) + as.numeric(year), data= xdata)
diagnostics.plot(full_t5) # ok
ranef.diagn.plot(full_t5) # ok 
xx=lm(Topic5_log ~ z.Freexp + z.Legcon + z.Jucon + z.share_st, data=xdata)
vif(xx) # little collinearity 

summary(full_t5)
screenreg(full_t5)

# TOPIC 31
full_t31 <- lmer(Topic31_log ~ z.Freexp + z.Legcon + z.Jucon + z.share_st+ (1|country)+ as.numeric(year), data= xdata)
diagnostics.plot(full_t31) # ok
ranef.diagn.plot(full_t31) # ok 
xx=lm(Topic31_log ~ z.Freexp + z.Legcon + share_st+z.Jucon, data=xdata)
vif(xx) # little collinearity 
summary(xx)

summary(full_t31)
screenreg(full_t31)

#htmlreg(list(full_t5,full_t31), stars = c(0.001,  0.01, 0.05, 0.1),  file = "Plots and Tables/regression_all.doc", type = "table", single.row = T)

#----------------------------------------------------------------
# 3. Plots
#----------------------------------------------------------------

### re-run models with the original scales
full_t5 <- lmer(Topic5 ~ v2x_freexp_altinf + v2xlg_legcon + v2x_jucon +share_st + (1|country) + as.numeric(year), data= xdata)
full_t31 <- lmer(Topic31 ~ v2x_freexp_altinf + v2xlg_legcon + v2x_jucon +share_st + (1|country) + as.numeric(year), data= xdata)


##### Plot 1: Marginal effects plot

# prepare data for plots (with original model)
dt3 <- xdata[,c(4:6,1,2)]
dt3 <- melt(dt3,id=c("Topic5","Topic31"))
dt3$variable <- ifelse(dt3$variable == "v2x_freexp_altinf","freedom of expression",as.character(dt3$variable))
dt3$variable <- ifelse(dt3$variable == "v2xlg_legcon","legislative constraints",as.character(dt3$variable))
dt3$variable <- ifelse(dt3$variable == "v2x_jucon","judicial constraints",as.character(dt3$variable))
names(dt3)[names(dt3) == "variable"] <- "group"

###predict marginal effects
mydf <- ggpredict(full_t5, terms = "v2x_freexp_altinf")
mydf$group <- "freedom of expression"
mydf2 <- ggpredict(full_t5, terms = "v2xlg_legcon")
mydf2$group <- "legislative constraints"
mydf3 <- ggpredict(full_t5, terms = "v2x_jucon")
mydf3$group <- "judicial constraints"

mydf3 <- rbind(mydf,mydf2,mydf3)

mydf4 <- ggpredict(full_t31, terms = "v2x_freexp_altinf")
mydf4$group <- "freedom of expression"
mydf5 <- ggpredict(full_t31, terms = "v2xlg_legcon")
mydf5$group <- "legislative constraints"
mydf6 <- ggpredict(full_t31, terms = "v2x_jucon")
mydf6$group <- "judicial constraints"

mydf7 <- rbind(mydf4,mydf5,mydf6)

p1 <- ggplot(data=mydf3, aes(x=x, y=predicted, color=group)) + 
  geom_point(data=dt3, aes(value, Topic5, color= group), alpha=0.6, color="lightsteelblue",size=0.8)+
  geom_line(aes(group=group),size=0.7,color = "gray20")+
  theme_classic()+
  ylab("Expected Topic proportion")+
  xlab("")+
  ggtitle("Legal")+
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0,1)) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high),alpha = 0.3,color="gray60")+
  theme(axis.text = element_text(size = 6),axis.title.y = element_text(size = 7))+
  facet_wrap(~group)
p1 <- ggpar(p1, legend = "none",legend.title = "")
p1  


p2 <- ggplot(data=mydf7, aes(x=x, y=predicted, color=group)) + 
  geom_point(data=dt3, aes(value, Topic31, color= group), alpha=0.6, color="lightsteelblue", size=0.8)+
  geom_line(aes(group=group),size=0.7,color = "gray20")+
  theme_classic()+
  ylab("Expected Topic proportion")+
  xlab("")+
  ggtitle("Technological")+
  coord_cartesian(ylim = c(0, 0.5), xlim = c(0,1)) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high),alpha = 0.3,color="gray60")+
  theme(axis.text = element_text(size = 6),axis.title.y = element_text(size = 7))+
  facet_wrap(~group)
p2 <- ggpar(p2, legend = "none",legend.title = "")
p2

### save all plots together
tiff("Plots and Tables/marginal_effects.tiff",units = "in", width = 5.5,height =5, res = 800)
ggpubr::ggarrange(p1,p2,nrow = 2,widths=c(1,1))
dev.off()


##### Plot 2: Time trend plot
dt4 <- dt2 %>% select(date,Topic5,Topic31)
dt4 <- melt(dt4,id=c("date"))

dt4$variable <- ifelse(dt4$variable == "Topic5","legislation",as.character(dt4$variable))
dt4$variable <- ifelse(dt4$variable == "Topic31","blocking",as.character(dt4$variable))
dt4$date <- as.Date(dt4$date,origin = "1970-01-01")

tiff("Plots and Tables/time_topics.tiff",units = "in", width = 7.5,height =5.5, res = 800)
p4 <- ggplot(data=dt4, 
             aes(x = date, y = value,
                 color= variable,
                 group=variable)) + 
  scale_color_aaas()+
  geom_smooth()+
  theme_bw()+
  xlab("")+
  ylab("expected topic proportion")+
  coord_cartesian(ylim = c(0,0.15))+
  labs(fill = "")+
  theme(text = element_text(size=14),legend.position = "bottom")

p4 + theme(legend.title = element_blank()) 
dev.off()


# END ------------------------------------------------------
