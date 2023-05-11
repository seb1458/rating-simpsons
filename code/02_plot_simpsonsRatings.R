# ---------------------------------- #
# ---- IMDB: Simpsons - Ratings ---- #
# ---------------------------------- #

## Setup
# Packages
pacman::p_load(tidyverse, forcats, RColorBrewer)

# Set your working directory
setwd("Set/path/here")

# ---------------------------------------------------------------------------- #
#### Visualization: Season Rating Overall ####

## Read file
tbl <- read.csv("./data/simpsonsRatings.csv")

## Prepare data
rating_all <- tbl %>%
    
    # Calculate overall season rating
    group_by(season) %>%
    summarise(rating = mean(ep_rating))

## Plot
(ratingAll_p <- ggplot(rating_all, aes(x = season, y = rating)) +
    geom_segment(aes(x = season, xend = season,
                     y = 0, yend = rating,
                     color = rating)) +
    geom_point(aes(color = rating), size = 3) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 34)) +
    
    # Color
    scale_color_gradient2(low = "red3", mid = "yellow", high = "chartreuse4",
                          midpoint = mean(rating_all$rating)) +
    
    # Theme, axes, ...
    theme_minimal())

# ---------------------------------------------------------------------------- #
#### Visualization: Tile Plot for Ratings of Episodes ####

## Setup factor order
category_order <- c("Great", "Good", "Okay", "Weak", "Bad")

## Prepare data
tbl <- tbl %>%
    
    # Add category based on rating
    mutate(ep_ratingCat = case_when(
        ep_rating < 4.5 ~ "Bad",
        ep_rating >= 4.5 & ep_rating < 6.0 ~ "Weak",
        ep_rating >= 6.0 & ep_rating < 7.5 ~ "Okay",
        ep_rating >= 7.5 & ep_rating < 9.0 ~ "Good",
        ep_rating >= 9.0 ~ "Great",
        
    )) %>%
    
    # Order rating categories according to our definition
    mutate(ep_ratingCat = fct_relevel(ep_ratingCat, category_order))
    

## Plotting
# Setup color palette
my_pal <- rev(brewer.pal(5, "RdYlGn"))

# Setup Simpsons font (needs to be downloaded manually)
extrafont::loadfonts()

# Setup theme
theme_set(theme_minimal())

my_theme <- theme(
    # Add a title with a Simpson style 
    plot.title = element_text(family = "Simpsonfont", size = 25),
    
    # Add a background in Simpsons yellow to remain on theme
    plot.background = element_rect(fill = "#ffd90f"),
    
    # Bold and black font for all axis and legend elements. I did not use the Simpsons font as it becomes hard to read at lower font sizes.
    axis.text = element_text(face = "bold", color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold", vjust = 2),
    legend.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    
    # Color the grid lines to make the plot less crowded
    panel.grid.major = element_line(color = "gray35"),
    panel.grid.minor = element_blank())

# Actual plot of episode ratings
ggplot(tbl, aes(x = season, y = ep_no, fill = ep_ratingCat)) +
    geom_tile(color = "white") +
    coord_fixed() +
    scale_y_reverse(breaks = c(1, 5, 10, 15, 20, 25)) +
    scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 34),
                       position = "top") +
    
    # Labels
    geom_text(aes(label = ep_rating), color = "black", size = 2) +
    
    # Color
    scale_fill_manual(name = "Rating Category", values = my_pal) +
    
    # Title, labels, ...
    labs(title = "Rating of The Simpsons", 
         caption = "Data: Internet Movie Database") +
    xlab("Season") +
    ylab("Episode") +
    
    # Add our defined theme
    my_theme

## Export plot
# ggsave(filename = "simpsonsRating.png", height = 16.79, units = "cm")
