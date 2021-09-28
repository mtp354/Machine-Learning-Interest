# MP Tweet Cleaner
rm(list = ls())
dev.off()
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(textclean)
cat("\014")

# Importing each MPs tweets
MP_tweets <- read.csv("C:\\Users\\mattp\\OneDrive\\Documents\\Job Hunting Stuff\\Tweet Scraping project\\ChloeTweets.csv")

# Remove all non-ascii characters
MP_tweets <- MP_tweets %>% mutate(Text = replace_non_ascii(Text, replacement = "", remove.nonconverted = T))

# Replace hyperlinks with "URL"
MP_tweets <- MP_tweets %>% mutate(Text = replace_url(Text, replacement = 'URL'))

# Replace all reply handles to "NME" to be replaced with previous speaker later
MP_tweets <- MP_tweets %>% mutate(Text = gsub("(?<!\\w)@\\S+", 'NME', Text, perl = T))

write.csv(MP_tweets, file = "ChloeTweets.csv", row.names = T)



