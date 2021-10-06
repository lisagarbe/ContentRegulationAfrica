#----------------------------------------------------------------
# Script: 02_structural_topic_models
# Authors: Lisa Garbe, Lisa-Marie Selvik, Pauline Lemaire
# Article: How African countries respond to fake news and hate speech
#----------------------------------------------------------------

# OVERVIEW OF SCRIPT

# 0. Required packages
# 1. Preparations
# 2. Structural topic models
# 3. Model evaluations

#set working directory
setwd("~")

# Read data
corpus_stm <- readRDS("Data/corpus_stm")

#----------------------------------------------------------------
# 0. Required packages
#----------------------------------------------------------------

library(quanteda)
library(stm)

#----------------------------------------------------------------
# 1. Preparations
#----------------------------------------------------------------

plotRemoved(corpus_stm$documents, lower.thresh = seq(1, 200, by = 100))

out <- prepDocuments(corpus_stm$documents, corpus_stm$vocab,
                     corpus_stm$data, lower.thresh = 15)

docs <- out$documents
vocab <- out$vocab
meta <-out$meta

names(out$meta)

#----------------------------------------------------------------
# 2. Structural topic models
#----------------------------------------------------------------

#full running code
# saved at: 
setwd("Topic Models")

### stm for 20 topics with regime as covariate  ####
SearchPrevFit_20 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 20,  seed = 24000,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_20, file = "stm_SearchPrevFit_20.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_20_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_20, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_20_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_20, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 21 topics with regime as covariate  ####
SearchPrevFit_21 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 21,  seed = 24100,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_21, file = "stm_SearchPrevFit_21.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_21_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_21, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_21_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_21, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 22 topics with regime as covariate  ####
SearchPrevFit_22 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 22,  seed = 24200,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_22, file = "stm_SearchPrevFit_22.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_22_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_22, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_22_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_22, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 23 topics with regime as covariate  ####
SearchPrevFit_23 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 23,  seed = 24300,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_23, file = "stm_SearchPrevFit_23.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_23_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_23, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_23_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_23, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 24 topics with regime as covariate  ####
SearchPrevFit_24 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 24,  seed = 24400,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_24, file = "stm_SearchPrevFit_24.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_24_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_24, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_24_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_24, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 25 topics with regime as covariate  ####
SearchPrevFit_25 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 25,  seed = 24500,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_25, file = "stm_SearchPrevFit_25.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_25_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_25, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_25_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_25, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 26 topics with regime as covariate  ####
SearchPrevFit_26 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 26,  seed = 24600,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_26, file = "stm_SearchPrevFit_26.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_26_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_26, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_26_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_26, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 27 topics with regime as covariate  ####
SearchPrevFit_27 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 27,  seed = 24700,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_27, file = "stm_SearchPrevFit_27.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_27_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_27, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_27_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_27, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 28 topics with regime as covariate  ####
SearchPrevFit_28 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 28,  seed = 24800,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_28, file = "stm_SearchPrevFit_28.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_28_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_28, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_28_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_28, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 29 topics with regime as covariate  ####
SearchPrevFit_29 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 29,  seed = 24900,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_29, file = "stm_SearchPrevFit_29.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_29_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_29, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_29_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_29, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 30 topics with regime as covariate ####
SearchPrevFit_30 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 30,  seed = 25000,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_30, file = "stm_SearchPrevFit_30.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_30_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_30, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_30_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_30, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 31 topics with regime as covariate ####
SearchPrevFit_31 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 31,  seed = 25100,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_31, file = "stm_SearchPrevFit_31.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_31_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_31, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_31_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_31, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 32 topics with regime as covariate ####
SearchPrevFit_32 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 32,  seed = 25200,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_32, file = "stm_SearchPrevFit_32.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_32_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_32, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_32_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_32, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 33 topics with regime as covariate ####
SearchPrevFit_33 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 33,  seed = 25300,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_33, file = "stm_SearchPrevFit_33.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_33_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_33, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_33_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_33, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 34 topics with regime as covariate ####
SearchPrevFit_34 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 34,  seed = 25400,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_34, file = "stm_SearchPrevFit_34.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_34_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_34, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_34_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_34, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 35 topics with regime as covariate ####
SearchPrevFit_35 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 35,  seed = 25500,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_35, file = "stm_SearchPrevFit_35.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_35_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_35, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_35_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_35, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 36 topics with regime as covariate ####
SearchPrevFit_36 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 36,  seed = 25600,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_36, file = "stm_SearchPrevFit_36.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_36_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_36, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_36_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_36, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 37 topics with regime as covariate ####
SearchPrevFit_37 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 37,  seed = 25700,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_37, file = "stm_SearchPrevFit_37.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_37_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_37, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_37_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_37, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 38 topics with regime as covariate ####
SearchPrevFit_38 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 38,  seed = 25800,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_38, file = "stm_SearchPrevFit_38.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_38_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_38, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_38_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_38, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 39 topics with regime as covariate ####
SearchPrevFit_39 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 39,  seed = 25900,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_39, file = "stm_SearchPrevFit_39.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_39_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_39, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_39_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_39, type = "labels", labeltype = "frex", n=40)
dev.off()


### stm for 40 topics with regime as covariate ####
SearchPrevFit_40 <- stm(documents = out$documents, vocab = out$vocab,
                        K = 40,  seed = 26000,
                        prevalence =~ v2x_polyarchy + s(date),
                        max.em.its = 75, data = out$meta,
                        init.type = "Spectral")

# SAVE model data
save(SearchPrevFit_40, file = "stm_SearchPrevFit_40.RData")

### plot probability and frequency of topics
jpeg("Rplot_SearchPrevFit_40_prob.jpg", width = 750, height = 3000)
plot(SearchPrevFit_40, type = "labels", labeltype = "prob", n=40)
dev.off()

jpeg("Rplot_SearchPrevFit_40_frex.jpg", width = 750, height = 3000)
plot(SearchPrevFit_40, type = "labels", labeltype = "frex", n=40)
dev.off()



#### SAVING everything ####

#setwd("C:/Users/lse043/OneDrive - University of Bergen/Documents/0 PhD PROJECT/PAPER x Content regulation/Analysis/textanalysiscontentregulation/Data/testing_topics")

#save(SearchPrevFit_20, SearchPrevFit_21, SearchPrevFit_22, SearchPrevFit_23, SearchPrevFit_24,
#     SearchPrevFit_25, SearchPrevFit_26, SearchPrevFit_27, SearchPrevFit_28, SearchPrevFit_29,
#    SearchPrevFit_30, SearchPrevFit_31, SearchPrevFit_32, SearchPrevFit_33, SearchPrevFit_34,
#   SearchPrevFit_35, SearchPrevFit_36, SearchPrevFit_37, SearchPrevFit_38, SearchPrevFit_39,
#  SearchPrevFit_40, 
# file = "stm_20-40.RData")


#----------------------------------------------------------------
# 3. Model evaluations
#----------------------------------------------------------------

# Testing the quality of topics in the chosen STM:
## SearchK evaluation ####
storage <- searchK(out$documents, out$vocab, K = c(10, 20), 
                   prevalence =~ v2x_polyarchy + s(date), data = meta)

plot(storage)

# plotting several: 
K<-c(10,15,20,25,30,35,40,45,50)
documents <- out$documents
vocab <- out$vocab
set.seed(02138)
K<-c(10,15,20,25,30,35,40,45,50)
kresult <- searchK(documents, vocab, K, prevalence =~ v2x_polyarchy + s(date), data = meta)

#saving at: 
setwd("Topic Models")
jpeg("Appendix_B_Model_diagnostics_full.jpg", width = 900, height = 550)
plot(kresult)
dev.off()


## Running Topic Quality check for 35-STM ####
#saving at: 
setwd("Topic Models")
tiff("Appendix_B_Topic_quality_35-STM.tiff", width = 4000, height = 4000, res = 400)
topicQuality(SearchPrevFit_35, out$documents, main = "35 topics", 
             xlab = "Semantic Coherence", ylab = "Exclusivity", labels = 1:ncol(SearchPrevFit_35$theta))
dev.off()

# END ------------------------------------------------------------
