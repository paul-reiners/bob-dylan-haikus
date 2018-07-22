# Load the libraries

#most of the libraries needed
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(stringr)

# Read in the data

bob_dylan_orig <- read.csv("dylan_raw_data.csv", stringsAsFactors = FALSE)

names(bob_dylan_orig)

bob_dylan <- bob_dylan_orig %>% 
    select(lyrics, song, year, album, times_played)

glimpse(bob_dylan[139,])

dim(bob_dylan)

str(bob_dylan[139, ]$lyrics, nchar.max = 300)

# Data Conditioning
# Basic cleaning

# function to remove click statement
removeClickStatement <- function(lyrics) {
    str_replace(lyrics, "Click the 'VIEW ALL' link at the right to see a list of Bob Dylan's live performances of this song.", "")
}
# remove click statements
bob_dylan$lyrics <- sapply(bob_dylan$lyrics, removeClickStatement)

# function to remove copyright statement
removeCopyright <- function(lyrics) {
    index = regexpr("copyright", lyrics)
    if (index > -1) {
        substr(lyrics, 1, index - 1)
    } else {
        lyrics
    }
}

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
    # "won't" is a special case as it does not expand to "wo not"
    doc <- gsub("won't", "will not", doc)
    doc <- gsub("can't", "can not", doc)
    doc <- gsub("n't", " not", doc)
    doc <- gsub("'ll", " will", doc)
    doc <- gsub("'re", " are", doc)
    doc <- gsub("'ve", " have", doc)
    doc <- gsub("'m", " am", doc)
    doc <- gsub("'d", " would", doc)
    # 's could be 'is' or could be possessive: it has no expansion
    doc <- gsub("'s", "", doc)
    return(doc)
}

# fix (expand) contractions
bob_dylan$lyrics <- sapply(bob_dylan$lyrics, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
bob_dylan$lyrics <- sapply(bob_dylan$lyrics, removeSpecialChars)

# convert everything to lower case
bob_dylan$lyrics <- sapply(bob_dylan$lyrics, tolower)
# remove copyright statements
bob_dylan$lyrics <- sapply(bob_dylan$lyrics, removeCopyright)

str(bob_dylan[139, ]$lyrics, nchar.max = 300)

# Remove Bootleg Series
bob_dylan <- bob_dylan[!(startsWith(bob_dylan$album, "The Bootleg Series")),]
# Remove Biograph
bob_dylan <- bob_dylan[!(startsWith(bob_dylan$album, "Biograph")),]
# Remove Greatest Hits
bob_dylan <- bob_dylan[regexpr("Greatest Hits", bob_dylan$album) == -1,]
# Remove 30th Anniversary Concert Celebration
bob_dylan <- bob_dylan[regexpr("30th Anniversary Concert Celebration", bob_dylan$album) == -1,]
# Remove Essential Bob Dylan
bob_dylan <- bob_dylan[regexpr("Essential Bob Dylan", bob_dylan$album) == -1,]
# Change Basement Tapes year
bob_dylan <- within(bob_dylan, year[album == 'The Basement Tapes'] <- 1967)

#get facts about the full dataset
summary(bob_dylan)

# Add a few fields
#create the decade column
bob_dylan <- bob_dylan %>%
    mutate(decade = 
               ifelse(bob_dylan$year %in% 1962:1969, "1960s", 
                      ifelse(bob_dylan$year %in% 1970:1979, "1970s", 
                             ifelse(bob_dylan$year %in% 1980:1989, "1980s", 
                                    ifelse(bob_dylan$year %in% 1990:1999, "1990s", 
                                           ifelse(bob_dylan$year %in% 2000:2009, "2000s", 
                                                  ifelse(bob_dylan$year %in% 2010:2015, "2010s", 
                                                         "NA")))))))

#save the new dataset to .csv for use later 
write.csv(bob_dylan, file = "bob_dylan_new.csv")
