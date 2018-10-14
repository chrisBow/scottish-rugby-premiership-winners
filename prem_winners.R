
# scrape Scottish top tier rugby winners from Wikipedia
library(rvest)

scot_prem_page <- read_html("https://en.wikipedia.org/wiki/Scottish_Premiership_(rugby)")

winners <- scot_prem_page %>%
  html_node(".column-width") %>%
  html_text()


# convert single string of text into list of winners
library(stringr)

winners_2 <-
  str_split(winners, "\n")

winners_3 <- unlist(winners_2)

winners_4 <- winners_3[-1]


# create vector of years
year <- 1974:2018


# build dataframe of winners
winners_df <- data.frame(year, winners_4)


# rename winners_4 column as winner
library(dplyr)
winners_df <- rename(winners_df, winner = winners_4)


# create vector of clubs who have won
winning_clubs <- as.character(unique(winners_df$winner))


# create new column for each winning club, initialise with 0
#for(club in winning_clubs) {    # first go with for loop
#  winners_df[ ,club] <- 0
#}
winners_df[ ,winning_clubs] <- 0  # a neater solution


# create new version of dataframe to guard against complete balls up
winners_df2 <- winners_df


# create long (tidy) version of dataframe
library(tidyr)
winners_df2 <- winners_df2 %>%
  gather(club, win, -year, -winner)


# alter win column to reflect if won that year
winners_df2 <- winners_df2 %>%
  mutate(win = ifelse(club == winner, 1, 0))


# create column for cumulutive wins
winners_df2 <-
  winners_df2 %>%
  arrange(year) %>%
  group_by(club) %>%
  mutate(wins = cumsum(win))


# change club variable to factor
winners_df2$club <- as.factor(winners_df2$club)


# test plot of final year
library(ggplot2)
library(forcats)
winners_df2 %>%
  filter(year == 2018) %>%
  ggplot(aes(x = fct_reorder(club, wins), y = wins)) + 
  geom_col() +
  coord_flip() +
  labs(title = "Scottish Rugby Championships by Club 1974 - 2018",
       x = "Club",
       y ="Number of wins") +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +
  theme_minimal()


# create plot for each year
for (yr in year) {
  filename <- paste0("champion", yr,".jpeg")
  win_plot <-
    winners_df2 %>%
    filter(year == yr) %>%
    ggplot(aes(x = fct_reorder(club, wins), y = wins)) + 
    geom_col() +
    coord_flip() +
    labs(title = "Scottish Rugby Championships by Club 1974 - 2018",
         caption = paste0("Year: ", yr),
         x = "Club",
         y ="Number of wins") +
    ylim(0, 14) + # constrain y limits to prevent scale change over time
    #scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12)) +
    theme_minimal()
  
  name = paste0("winners_", yr,".jpeg")
  
  ggsave(filename = name,
         plot = win_plot,
         device = "jpeg",
         width = 6, height = 4, 
         units = "in")
}

 
# create animation   
library(magick)
frames = c()
images = list.files(pattern = "jpeg")
  
for (i in length(images):1) {
    x = image_read(images[i])
    x = image_scale(x, "600")
    c(x, frames) -> frames
}

animation = image_animate(frames, fps = 4)

image_write(animation, "championship_winners.gif")





