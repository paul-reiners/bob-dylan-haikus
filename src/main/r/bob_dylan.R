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

# function to remove copyright statement
removeCopyright <- function(lyrics) {
    index = regexpr("copyright", lyrics)
    if (index > -1) {
        substr(lyrics, 1, index - 1)
    } else {
        lyrics
    }
}
# remove copyright statements
bob_dylan$lyrics <- sapply(bob_dylan$lyrics, removeCopyright)

# convert everything to lower case
bob_dylan$lyrics <- sapply(bob_dylan$lyrics, tolower)

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

# Descriptive Statistics

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
}

bob_dylan %>%
    filter(decade != "NA") %>%
    group_by(decade) %>%
    summarise(number_of_songs = n()) %>%
    ggplot() + 
    geom_bar(aes(x = decade, y = number_of_songs), stat = "identity")  +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          panel.grid.minor = element_blank()) +
    ggtitle("Released Songs") +
    labs(x = NULL, y = "Song Count")

#look at the full data set at your disposal
bob_dylan %>%
    group_by(decade) %>%
    summarise(number_of_songs = n()) %>%
    ggplot() +
    geom_bar(aes(x = decade, y = number_of_songs), stat = "identity")  +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = NULL, y = "Song Count") +
    ggtitle("All Songs in Data")

library(knitr) # for dynamic reporting
library(kableExtra) # create a nicely formated HTML table
library(formattable) # for the color_tile function

# Text Mining
# Tidy Text Format
# Data Formats and Tokenization
undesirable_words <- 
    c("rybody", "instrumental", "chorus")

head(sample(stop_words$word, 15), 15)

# Word Frequency
full_word_count <- bob_dylan %>%
    unnest_tokens(word, lyrics) %>%
    group_by(song) %>%
    summarise(num_words = n()) %>%
    arrange(desc(num_words)) 

# Word Clouds
#unnest and remove stop, undesirable and short words
bob_dylan_words_filtered <- bob_dylan %>%
    unnest_tokens(word, lyrics) %>%
    anti_join(stop_words) %>%
    distinct() %>%
    filter(!word %in% undesirable_words) %>%
    filter(nchar(word) > 3)

bob_dylan_words_filtered %>% 
    filter(word == "time") %>%
    select(word, song, year, decade) %>%
    arrange() %>%
    top_n(10,song) %>%
    mutate(song = color_tile("lightblue","lightblue")(song)) %>%
    mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
    kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
    kable_styling(bootstrap_options = 
                      c("striped", "condensed", "bordered"), 
                  full_width = FALSE)

full_word_count[1:10,] %>%
    ungroup(num_words, song) %>%
    mutate(num_words = color_bar("lightblue")(num_words)) %>%
    mutate(song = color_tile("lightpink","lightpink")(song)) %>%
    kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
    kable_styling(bootstrap_options = 
                      c("striped", "condensed", "bordered"), 
                  full_width = FALSE)

full_word_count %>%
    ggplot() +
    geom_histogram(aes(x = num_words), binwidth = 64) +
    ylab("Song Count") + 
    xlab("Word Count per Song") +
    ggtitle("Word Count Distribution") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          panel.grid.minor.y = element_blank())

full_word_count %>%
    filter(num_words > 800) %>%
    left_join(bob_dylan, by = "song") %>%
    select(Song = song, 
           "Word Count" = num_words) %>%
    kable("html", escape = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"))

bob_dylan_words_filtered %>%
    count(word, sort = TRUE) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot() +
    geom_col(aes(word, n), fill = my_colors[4]) +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5),
          panel.grid.major = element_blank()) +
    xlab("") + 
    ylab("Song Count") +
    ggtitle("Most Frequently Used Words in Bob Dylan Lyrics") +
    coord_flip()

bob_dylan_words_filtered <- bob_dylan %>%
    unnest_tokens(word, lyrics) %>%
    anti_join(stop_words) %>%
    distinct() %>%
    filter(!word %in% undesirable_words) %>%
    filter(nchar(word) > 3)
bob_dylan_words_counts <- bob_dylan_words_filtered %>%
    count(word, sort = TRUE) 

wordcloud2(bob_dylan_words_counts[1:300, ], size = .5)

# wordcloud2(bob_dylan_words_counts[1:300, ], figPath = "../img/glaser2.png", 
#            color = "random-dark", size = 1.5)

letterCloud(bob_dylan_words_counts[1:300, ], word = "DYLAN", size = 2)

# Timeless Words
timeless_words <- bob_dylan_words_filtered %>% 
    filter(decade != 'NA') %>%
    group_by(decade) %>%
    count(word, decade, sort = TRUE) %>%
    slice(seq_len(8)) %>%
    ungroup() %>%
    arrange(decade,n) %>%
    mutate(row = row_number()) 

timeless_words %>%
    ggplot(aes(row, n, fill = decade)) +
    geom_col(show.legend = NULL) +
    labs(x = NULL, y = "Song Count") +
    ggtitle("Timeless Words") + 
    theme_lyrics() +  
    facet_wrap(~decade, scales = "free", ncol = 5) +
    scale_x_continuous(  # This handles replacement of row 
        breaks = timeless_words$row, # notice need to reuse data frame
        labels = timeless_words$word) +
    coord_flip()

# Word Length
#unnest and remove undesirable words, but leave in stop and short words
bob_dylan_word_lengths <- bob_dylan %>%
    unnest_tokens(word, lyrics) %>%
    group_by(song,decade) %>%
    distinct() %>%
    filter(!word %in% undesirable_words) %>%
    mutate(word_length = nchar(word)) 

bob_dylan_word_lengths %>%
    count(word_length, sort = TRUE) %>%
    ggplot(aes(word_length), 
           binwidth = 10) + 
    geom_histogram(aes(fill = ..count..),
                   breaks = seq(1,25, by = 2), 
                   show.legend = FALSE) + 
    xlab("Word Length") + 
    ylab("Word Count") +
    ggtitle("Word Length Distribution") +
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank())

wc <- bob_dylan_word_lengths %>%
    ungroup() %>%
    select(word, word_length) %>%
    distinct() %>%
    arrange(desc(word_length))

wordcloud2(wc[1:300, ], 
           size = .15,
           minSize = .0005,
           ellipticity = .3, 
           rotateRatio = 1, 
           fontWeight = "bold")

# Lexical Diversity
lex_diversity_per_year <- bob_dylan %>%
    filter(decade != "NA") %>%
    unnest_tokens(word, lyrics) %>%
    group_by(song,year) %>%
    summarise(lex_diversity = n_distinct(word)) %>%
    arrange(desc(lex_diversity)) 

diversity_plot <- lex_diversity_per_year %>%
    ggplot(aes(year, lex_diversity)) +
    geom_point(color = my_colors[3],
               alpha = .4, 
               size = 4, 
               position = "jitter") + 
    stat_smooth(color = "black", se = FALSE, method = "lm") +
    geom_smooth(aes(x = year, y = lex_diversity), se = FALSE,
                color = "blue", lwd = 2) +
    ggtitle("Lexical Diversity") +
    xlab("") + 
    ylab("") +
    scale_color_manual(values = my_colors) +
    theme_classic() + 
    theme_lyrics()

diversity_plot

# Lexical Density
lex_density_per_year <- bob_dylan %>%
    filter(decade != "NA") %>%
    unnest_tokens(word, lyrics) %>%
    group_by(song,year) %>%
    summarise(lex_density = n_distinct(word)/n()) %>%
    arrange(desc(lex_density))

density_plot <- lex_density_per_year %>%
    ggplot(aes(year, lex_density)) + 
    geom_point(color = my_colors[4],
               alpha = .4, 
               size = 4, 
               position = "jitter") + 
    stat_smooth(color = "black", 
                se = FALSE, 
                method = "lm") +
    geom_smooth(aes(x = year, y = lex_density), 
                se = FALSE,
                color = "blue", 
                lwd = 2) +
    ggtitle("Lexical Density") + 
    xlab("") + 
    ylab("") +
    scale_color_manual(values = my_colors) +
    theme_classic() + 
    theme_lyrics()

density_plot

# TF-IDF

tfidf_words_decade <- bob_dylan %>%
    unnest_tokens(word, lyrics) %>%
    distinct() %>%
    filter(!word %in% undesirable_words & decade != 'NA') %>%
    filter(nchar(word) > 3) %>%
    count(decade, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(word, decade, n) %>%
    arrange(desc(tf_idf))

top_tfidf_words_decade <- tfidf_words_decade %>% 
    group_by(decade) %>% 
    slice(seq_len(8)) %>%
    ungroup() %>%
    arrange(decade, tf_idf) %>%
    mutate(row = row_number())

top_tfidf_words_decade %>%
    ggplot(aes(x = row, tf_idf, fill = decade)) +
    geom_col(show.legend = NULL) +
    labs(x = NULL, y = "TF-IDF") + 
    ggtitle("Important Words using TF-IDF by Decade") +
    theme_lyrics() +  
    facet_wrap(~decade, 
               ncol = 3, nrow = 2, 
               scales = "free") +
    scale_x_continuous(  # this handles replacement of row 
        breaks = top_tfidf_words_decade$row, # notice need to reuse data frame
        labels = top_tfidf_words_decade$word) +
    coord_flip()

wc <- tfidf_words_decade %>%
    arrange(desc(tf_idf)) %>%
    select(word, tf_idf)

wordcloud2(wc[1:300, ], 
           color = "random-dark", 
           minRotation = -pi / 6, 
           maxRotation = -pi / 3, 
           minSize = .002, 
           ellipticity = .3, 
           rotateRatio = 1, 
           size = .2, 
           fontWeight = "bold", 
           gridSize = 1.5 )
