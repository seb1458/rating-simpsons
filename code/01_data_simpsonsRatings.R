# ---------------------------------- #
# ---- IMDB: Simpsons - Ratings ---- #
# ---------------------------------- #

## Setup
# Packages
pacman::p_load(rvest, tidyverse)

# Set your working directory
setwd("Set/path/here")

# ---------------------------------------------------------------------------- #
#### Test IMDB ####

# Define URL
url <- paste0("https://www.imdb.com/title/tt0096697/episodes?season=", 1)

# Read HTML from defined URL
imdb <- read_html(url)

imdb %>%
    html_elements(".ipl-rating-star__rating") %>%
    html_table()

# Extract information at desired nodes
imdb_epRate <- html_nodes(imdb, ".ipl-rating-star__rating")
imdb_title <- html_nodes(imdb, "strong")

# ---------------------------------------------------------------------------- #
#### Scrap IMDB ####

## Build for loop
# There are 34 seasons of "The Simpsons", so we can iterate over all seasons to
# extract the information we want.

# Create empty tibble
tbl <- tibble()

# For loop
for (i in 1:34) {
    
    # Define URL
    url <- paste0("https://www.imdb.com/title/tt0096697/episodes?season=", i)
    
    # Read HTML from defined URL
    imdb <- read_html(url)
    
    # Extract information at desired nodes
    imdb_epRate <- html_nodes(imdb, ".ipl-rating-star__rating")
    imdb_title <- html_nodes(imdb, "strong")
    
    # Episode ratings: convert to text and keep entries with a decimal point
    rating_raw <- html_text(imdb_epRate)
    rating_ep <- rating_raw[grepl("\\.", rating_raw)]
    
    # Episode titles: convert to text and keep entries with a decimal point
    title_raw <- html_text(imdb_title)
    title_ep <- title_raw[1:length(rating_ep)]
    
    # Build tbl
    temp_tbl <- tibble(season = as.factor(i),
                       ep_no = 1:length(rating_ep),
                       ep_title = title_ep,
                       ep_rating = as.numeric(rating_ep))
    
    # Bind into final tibble
    tbl <- bind_rows(tbl, temp_tbl)
}

## Store data
write_csv(tbl, file = "./data/simpsonsRatings.csv")
