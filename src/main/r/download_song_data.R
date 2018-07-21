library(rvest)
library(dplyr)

song_title_page <- read_html("http://www.bobdylan.com/songs/")
song <- song_title_page %>%
  html_nodes(".song") %>%
  html_text()
song <- song[-1]

url <- song_title_page %>%
  html_nodes(".song a") %>% html_attr('href')

albums <- song_title_page %>%
  html_nodes(".release a") %>%
  html_text()

times_played <- song_title_page %>%
  html_nodes(".times") %>%
  html_text() %>% as.numeric()
times_played <- times_played[-1]

songs_df <- data.frame(song, url, albums, times_played, stringsAsFactors = FALSE)
colnames(songs_df) = c("song", "url", "album", "times_played")

albums_page <- read_html("http://www.bobdylan.com/albums/")
album <- albums_page %>%
  html_nodes(".caption") %>%
  html_text() %>%
  trimws()

get_title <- function(caption) {
  trimws(substr(caption, 1, nchar(caption) - 4))
}

get_year <- function(caption) {
  n <- nchar(caption)
  as.numeric(trimws(substr(caption, n - 4, n)))
}

album_titles = sapply(album, get_title)
album_years = sapply(album, get_year)

albums_df = data.frame(album_titles, album_years, stringsAsFactors = FALSE)
colnames(albums_df) = c("album", "year")

get_lyrics <- function(row) {
  url <- row[2]
  tryCatch(
    url %>%
      as.character() %>% 
      read_html() %>% 
      html_node(".lyrics") %>% html_text(), 
    error = function(e) {NA})
}

get_lyrics_again <- function(row) {
  lyrics <- row[5]
  if (is.na(lyrics)) {
    url <- row[2]
    tryCatch(
      url %>%
        as.character() %>% 
        read_html() %>% 
        html_node(".lyrics") %>% html_text(), 
      error = function(e) {NA})
  } else {
    lyrics
  }
}

songs_df$lyrics <- apply(songs_df, 1, get_lyrics)
songs_df$lyrics <- apply(songs_df, 1, get_lyrics_again)

songs_df$url <- as.character(songs_df$url)

dylan_df = merge(x = songs_df, y = albums_df, by = "album", all.x = TRUE)

write.csv(dylan_df, file = "dylan_raw_data.csv")
