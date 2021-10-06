#----------------------------------------------------------------
# Script: 04_Topic_validation
# Authors: Lisa Garbe, Lisa-Marie Selvik, Pauline Lemaire
# Article: How African countries respond to fake news and hate speech (Appendix)
#----------------------------------------------------------------

# OVERVIEW OF SCRIPT

# 0. Required packages
# 1. Process and aggregate data
# 2. Visualization
# 3. Statistical tests
# 4. Time period and language
# 5. Convert to STM corpus

## empty memory (!)
rm(list=ls())

#set working directory
setwd("~")

# Read data
corpus_stm <- readRDS("Data/corpus_stm")
# loading model
load("Topic Models/stm_SearchPrevFit_35.RData")
# Freedom of the Net data (manually coded); Note that Freedom on the Net (FoN) reports were merged and subsequently coded for each year. For more details refer to the article's appendix
library(readxl)
fon <- read_excel("Data/FoN_manual_coding.xlsx")

#----------------------------------------------------------------
# 0. Required packages
#----------------------------------------------------------------

library(stm)
library(tidyr)
library(tidytext)
library(dplyr)
library(ggsci)
library(ggplot2)
library(lme4)
library(countrycode)
library(ggpubr)
library(reshape2)

#----------------------------------------------------------------
# 1. Process and aggregate data
#----------------------------------------------------------------

# pre-processing STM
plotRemoved(corpus_stm$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(corpus_stm$documents, corpus_stm$vocab,
                     corpus_stm$data, lower.thresh = 15)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

names(out$meta)

# make dataframe out of STM-35
dt2 <- make.dt(SearchPrevFit_35, meta=out$meta)
dt2$year <- as.numeric(dt2$year)
dt3 <- dt2 %>% select(Topic5,Topic31,v2x_freexp_altinf,v2xlg_legcon,v2x_jucon,v2x_polyarchy,year,country)
dt3 <- na.omit(dt3)

# compute country year mean on topics of interest
dt4 <- dt3 %>%
  dplyr::group_by(country, year) %>% 
  dplyr::summarise(mtopic5 = mean(Topic5), mtopic31 = mean(Topic31))

# summarize variables for manually coded FoN data
fon <- fon %>%
  dplyr::rename(country = COUNTRY, 
         year = YEAR) %>%
  dplyr::select(country, year, TechMan, LegalMan)

# merging both, no NAs 
dt5 <- merge(dt4, fon)

#saveRDS(dt5, "Validation/manualdataforvalidation.rds")

#----------------------------------------------------------------
# 2. Visualization
#----------------------------------------------------------------

#visualizing topic 31 and Freedom on the Net "technological regulation" (TechMan)
dt5$TechMan <- as.factor(dt5$TechMan)
pt <- ggplot(data = dt5, mapping = aes(x = TechMan, y = mtopic31)) + geom_boxplot() + xlab("Freedom on the Net blocking")+
  ylab("Country mean topic proportion") + coord_flip()
pt

#visualizing topic 5 and Freedom on the Net "legal regulation" (LegalMan)
dt5$LegalMan <- as.factor(dt5$LegalMan)
pl <- ggplot(data = dt5, mapping = aes(x = LegalMan, y = mtopic5)) + geom_boxplot() + xlab("Freedom on the Net legislation passed") +
  ylab("Country mean topic proportion") + coord_flip() 
pl


#----------------------------------------------------------------
# 3. Statistical tests
#----------------------------------------------------------------
hist(dt5$mtopic5) # skewed 
hist(dt5$mtopic31) # skewed
dt5$mtopic5_log <- log(dt5$mtopic5)
dt5$mtopic31_log <- log(dt5$mtopic31)

t_log_5man <- t.test(mtopic5_log ~ LegalMan, data = dt5, var.equal = TRUE)
t_log_5man # no statistically significant difference between groups

t_log_31man <-  t.test(mtopic31_log ~ TechMan, data = dt5, var.equal = TRUE)
t_log_31man # statistically significant difference between groups


# END -----------------------------------------------------------