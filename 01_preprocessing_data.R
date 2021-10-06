#----------------------------------------------------------------
# Script: 01_preprocessing_data
# Authors: Lisa Garbe, Lisa-Marie Selvik, Pauline Lemaire
# Article: How African countries respond to fake news and hate speech
#----------------------------------------------------------------

# OVERVIEW OF SCRIPT

# 0. Required packages
# 1. Extract relevant paragraphs
# 2. Select articles covering Africa
# 3. Merge with VDEM data
# 4. Time period and language
# 5. Convert to STM corpus

#set working directory
setwd("~")

# Read data
df_raw <- readRDS("Data/dataframe_Factiva_raw_import") # NOTE: the data used as input here comes from FACTIVA. Details about the sampling can be found in the appendix of the article.

#----------------------------------------------------------------
# 0. Required packages
#----------------------------------------------------------------

library(here)
library(tidyr)
library(dplyr)
library(purrr)
library(tm)
library(tm.plugin.factiva)
library(tidytext)
library(quanteda)
library(dplyr)
library(countrycode)
library(plyr)
library(vdemdata)
library(stringr)
library(readtext)
library(tidytext)
library(stm)

#----------------------------------------------------------------
# 1. Extract relevant paragraphs
#----------------------------------------------------------------
## selecting variables 
df <-
  df_raw %>%
  dplyr::select(
    id, datetimestamp, author, language, origin, publisher, wordcount, coverage,
    heading, text
  ) %>%
  dplyr::rename(date = datetimestamp)

## Split into paragraphs based on "\n" in the text and keep those
## matching with `filterkeys` (i.e. the 'online terms')
filterkeys <- c(
  "online", "Online", "digital", "Digital", "internet", "Internet", "web", "Web",
  "social media", "Social media", "facebook", "Facebook", "twitter", "Twitter",
  "google", "Google", "youtube", "YouTube", "whatsapp", "WhatsApp", "instagram", "Instagram")

df_para <-
  df %>%
  mutate(text = strsplit(text, "\n")) %>%
  unnest(text) %>%
  filter(grepl(paste0(filterkeys, collapse = "|"), text))

# Result: 46.034 obs. of 10 variables
# Note: without including alternative spellings with capital letters: only 33.932 obs (paras)

# inspecting
glimpse(df_para)

# checking some random texts
df_para$text[10]
df_para$text[20]
df_para$text[30]
df_para$text[40]
df_para$text[50]
df_para$text[40000]
df_para$text[35000]

### filter our paras containing "http://www.m2.com" - should remove 83 paras from the df_para
# inspect one example
df$text[id ="MTPW000020190320ef3k003s5"]
# filtering
df_para <- filter(df_para, !grepl("http://www.m2.com", text)) # reduces from 46.034 to 45.951 = 83 obs dropped

## collapsing paragraphs into docs per ID
df_collapsed <- ddply(df_para, .(id), summarize, text=paste(text, collapse=""))   # result: 12.294 obs. of 2 variables

# checking for id: NIGTRI0020201102egb100015 and NIGTRI0020201102egb100004
df_para$id[1:8]

df_para$text[1:4]
df_collapsed$text[8791]

df_para$text[5:7]
df_collapsed$text[8790]

## merging with dataframe
df_final <- df %>%
  select(id, date, author, language, origin, publisher, wordcount, coverage, heading)   # result: 22.457 obs. of 9 variables

df_final <- df_final %>% 
  left_join(df_collapsed, by = "id") %>%
  tidyr::drop_na(any_of("text"))   # removing all the obs (i.e. news stories) that did not contain the filterkeys

glimpse(df_final)   # result: 12,294 obs. of 10 variables 

#date: (POSIXct -> Date)
str(df_final$date)
df_final$date <- as.Date(df_final$date)

#year:
df_final$year <- format(df_final$date, format="%Y")


#----------------------------------------------------------------
# 2. Select articles covering Africa
#----------------------------------------------------------------

# selecting variables:
myvars <- c("id", "year", "coverage")
df_coverage <- df_final[myvars]
glimpse(df_coverage)

df_coverage$country <- unlist(lapply(df_coverage$coverage,paste,collapse=" "))

# create a variable for African countries: 
countries <- data.frame(countryname_dict)
countries$continent <- countrycode(sourcevar = countries[["country.name.en"]],
                                   origin = "country.name.en",
                                   destination = "continent")
africa <- countries[ which(countries$continent=='Africa'), ]

pat <- paste0("\\b", paste(africa$country.name.en , collapse="\\b|\\b"), "\\b")
df_coverage$country_list <- str_extract_all(df_coverage$country, regex(pat, ignore_case = TRUE)) #NB: Time-consuming operation

# selecting the first country name in list
df_coverage$country <- sapply(df_coverage$country_list, function(x) x[1])
df_coverage$country
str(df_coverage$country) # no longer list

## unlist country column
indx <- sapply(df_coverage$country_list, length)
res <- as.data.frame(do.call(rbind,lapply(df_coverage$country_list, `length<-`,
                                          max(indx))))
df_coverage <- cbind(df_coverage, res)
#View(df_coverage)

df_coverage$iso <- countrycode(df_coverage$country, "country.name", "iso3c")
df_cov <- df_coverage[,c(1,2,4, 25)] # choosing  "id"      "year"    "country"         "iso"  
df_cov <- subset(df_cov, !is.na(df_cov$iso))


# saving full coverage data ####
#saveRDS(df_coverage, file = "Data/dataframe_coverage")


#----------------------------------------------------------------
# 3. Merge with VDEM data
#----------------------------------------------------------------

## removing observations with missing country info
## get v-dem data for regime type coding
vdem <- vdemdata::vdem

## subset VDem data
vdem <- subset(vdem, vdem$year > 2014)
vdem$iso <- countrycode(vdem$COWcode, "cown", "iso3c", warn = TRUE, nomatch = NA, custom_dict = NULL, custom_match = NULL, origin_regex = FALSE)
vdem <- vdem %>% select(year, iso, v2x_freexp_altinf,v2xlg_legcon,v2x_jucon,v2x_polyarchy, v2x_regime)

# merge df_cov with vdem data
df_cov2 <- merge(df_cov,vdem, by = c("year","iso"))

variable.names(df_cov2)
glimpse(df_cov2)
# rows: 11,476 

# making regime dich variable: 
df_cov2$regime_dich <- ifelse(df_cov2$v2x_regime == 0 | df_cov2$v2x_regime == 1,"authoritarian","democratic")


# merging with final dataframe: 
glimpse(df_final)   # rows: 12,294
glimpse(df_cov2)   # rows: 11,476

df_final <- merge(df_final, df_cov2, by=c("id","year"))  # keeping only observations with info on coverage

#View(df_final)
df_final$country
glimpse(df_final)

# Result: 
# Rows: 11,476
# Columns: 19


#### saving final dataframe ####
# only with first country name variable
#saveRDS(df_final, file = "Data/dataframe_final")

#----------------------------------------------------------------
# 4. Time period and language
#----------------------------------------------------------------

# reading
#df_final <- readRDS("Data/dataframe_final")

# collapsing heading and text: 
df_final <- mutate(df_final, text = paste(heading, text, sep = "\n\n"))

# exclude obs prior to 2020
df_final <- subset(df_final, df_final$date < "2020-01-01")

# making date numeric (for stm analysis):
str(df_final$date)
df_final$date <- as.numeric(df_final$date)

# making corpus: 
df_final$text_id <- 1:nrow(df_final)
my_corp <- corpus(df_final, docid_field = "text_id", text_field = "text")

my_corp #corpus with 8085 obsverations

# subset corpus, to English texts
eng <- c("en")
my_corp_en <- corpus_subset(my_corp, language %in% eng)
summary(my_corp_en)   #Corpus consisting of 7787 documents 

## saving ####
#saveRDS(my_corp, file = "Data/corpus_full")
#saveRDS(my_corp_en, file = "Data/corpus_eng")


#----------------------------------------------------------------
# 5. Convert to STM corpus
#----------------------------------------------------------------

#my_corp_en <- readRDS("Data/corpus_eng")

# Constructing a document feature matrix 
# pre-processing: lowercases by default
corpdfm <- dfm(my_corp_en,  
                remove = stopwords(), # removing stopwords
                remove_punct = TRUE,
                remove_numbers = TRUE,
                stem = TRUE)
dim(corpdfm)

#removing search and online terms: 
our_terms <- c("hate_speech*", "fake_news*", "misinformation*", "disinformation*", 
               "online*", "digital*", "internet*", "web*", 
               "social_media*", "Facebook*", "Twitter*", "Google*", 
               "YouTube*", "WhatsApp*", "Instagram*")

stops_dfm <- dfm_select(corpdfm, pattern = our_terms, selection = "remove", case_insensitive = TRUE)

dim(stops_dfm)

# for modeling, we want to trim, i.e. removing the low frequency and idiosyncratic words:
small_stops_dfm <- dfm_trim(stops_dfm, min_termfreq = 5, min_docfreq = 5)
dim(small_stops_dfm)

# min_count removes any word that occurs less than 5 times 
# min_docfreq removes any words that occurs any number of times but in fewer than 5 different documents

#coercing into stm corpus:
corpus_stm <- asSTMCorpus(stops_dfm)
corpus_stm


#### saving ####
saveRDS(corpus_stm, file = "Data/corpus_stm")


## END ---------------------------------------------------------------

