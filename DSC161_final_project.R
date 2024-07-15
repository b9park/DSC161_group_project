rm(list = ls())

library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(stm)
library(seededlda)

# Set working directory
setwd("/Users/thegreatest/Desktop/DSC161/data")

# Load data
metadata <- read_csv("UNGDspeeches.csv")
metadata
# Create a corpus
corpus_sotu <- corpus(metadata, text_field = "text")

# Some common pre-processing
toks <- tokens(corpus_sotu, remove_punct = TRUE, remove_numbers = TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks, stopwords("en"), selection = "remove")
dfm <- dfm(toks)

# Trim the dfm of rare words
dfm_trimmed <- dfm_trim(dfm, min_docfreq = 0.05, docfreq_type = "prop")

# Process the data for STM
temp <- textProcessor(documents = metadata$text, metadata = metadata)
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)

#levels(out$text)[1] "Conservative"

# Fit the STM model
model.stm <- stm(out$documents, out$vocab, K = 10, prevalence = ~s(year), 
                 data = out$meta, max.em.its = 10)
model.stm <- stm(out$documents, out$vocab, K = 10, prevalence = ~s(year), 
                 data = out$meta) 

#Find most probable words in each topic
labelTopics(model.stm)

#And most common topics
plot(model.stm, n=5)
plot(model.stm, n=5, labeltype="frex")

findThoughts(model.stm, texts=out$meta$text, topics=2, n=3)

# Extract document-topic proportions
doc_topics <- as.data.frame(model.stm$theta)

# Add year information to the dataframe
doc_topics$year <- out$meta$year

# Filter data for the year 2018
doc_topics_2018 <- subset(doc_topics, year == 2018)

# Visualize topic prevalence in 2018
barplot(colMeans(doc_topics_2018[, -ncol(doc_topics_2018)]), names.arg = 1:10, 
        xlab = "Topic", ylab = "Proportion", main = "Topic Prevalence in 2018")

# Extract relevant documents for the year 2018
documents_2018 <- subset(out$meta, year == 2018)$text

# Keyword search for climate-related terms
climate_keywords <- c("climate", "environment", "sustainability", "Paris Agreement", "global warming")

# Extract excerpts containing climate-related keywords
climate_excerpts <- lapply(documents_2018, function(doc) grep(paste(climate_keywords, collapse = "|"), doc, value = TRUE))

# Display extracted excerpts
print(climate_excerpts)
