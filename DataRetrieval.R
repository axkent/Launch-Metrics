library(tidyverse)
library(baseballr)
library(gt)
library(plotly)
library(shiny)

# Get a score breakdown for the World Series (WS): Yankees v Dodgers
WS<- baseballr::mlb_schedule_postseason(season = 2024, game_type = "W")

WS <- WS |> select(date, series_game_number, series_description, teams_home_team_name, teams_away_team_name,
                   teams_home_score, teams_away_score)

# Reshape the data to long format
WS_long <- WS |> 
  pivot_longer(
    cols = c(teams_home_score, teams_away_score),
    names_to = "team_type",
    values_to = "score"
  ) |> 
  mutate(
    team_name = ifelse(team_type == "teams_home_score", teams_home_team_name, teams_away_team_name)
  )

# Create the bar chart
WSplot <- ggplot(WS_long, aes(x = factor(series_game_number), y = score, fill = team_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("Los Angeles Dodgers" = "#1e90ff", "New York Yankees" = "#0C2340") # Custom colors
  ) +
  scale_y_continuous(breaks = seq(0, max(WS_long$score), by = 1),
                     expand = c(0, 0)) +
  labs(
    title = "Runs Scored by Team\n2024 World Series",
    x = "Game Number",
    y = "Runs Scored",
    fill = "Team"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position="bottom")
WSplot
#ggsave("WS.png", WSplot, bg="white", width = 1000, height = 869)

####Retrieve 2024 National League Division Series (NLDS): LAD v SDP Data

### Get a score breakdown for the National League Division Series (NLDS): Padres v Dodgers
NLDS<- baseballr::mlb_schedule_postseason(season = 2024, game_type = "D") |> 
  filter(teams_home_team_name %in% c("Los Angeles Dodgers", "San Diego Padres"))

NLDS <- NLDS |> select(date, series_game_number, series_description, teams_home_team_name, teams_away_team_name,
                       teams_home_score, teams_away_score)

# Reshape the data to long format
NLDS_long <- NLDS |> 
  pivot_longer(
    cols = c(teams_home_score, teams_away_score),
    names_to = "team_type",
    values_to = "score"
  ) |> 
  mutate(
    team_name = ifelse(team_type == "teams_home_score", teams_home_team_name, teams_away_team_name)
  )

# Create the bar chart
NLDS_plot <- ggplot(NLDS_long, aes(x = factor(series_game_number), y = score, fill = team_name)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("Los Angeles Dodgers" = "#1e90ff", "San Diego Padres" = "#FFC425") # Custom colors
  ) +
  scale_y_continuous(breaks = seq(0, max(NLDS_long$score), by = 1),
                     expand = c(0, 0)) +
  labs(
    title = "Runs Scored by Team\n2024 National League Division Series",
    x = "Game Number",
    y = "Runs Scored",
    fill = "Team"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), legend.position="bottom")
NLDS_plot
#ggsave("NLDS.png", NLDS_plot, bg="white", width = 1000, height = 869)
###Prepare launch angle visualizations

# Savant retrieval code adapted from: robert-frey
# Source: https://github.com/robert-frey/YouTube/blob/master/Combine%20Video%20with%20a%20Savant%20Dataset!/savant_videos.R
# Accessed on: November 30, 2024

# Function to create the Baseball Savant URL for a given date
generate_url <- function(date) {
  base_url <- "https://baseballsavant.mlb.com/statcast_search/csv?"
  params <- "all=true&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C&hfC&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&player_type=batter&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed&sort_order=desc&min_abs=0&type=details"
  paste0(base_url, params, "&game_date_gt=", date, "&game_date_lt=", date)
}

# Generate a sequence of dates
dates <- seq(as.Date("2024-10-05"), as.Date("2024-10-11"), by = "day")

# Load data for each date and combine into one dataframe
savant_data <- lapply(dates, function(date) {
  url <- generate_url(as.character(date))
  read_csv(url)
}) |> 
  bind_rows()

# get game_pks for pbp data
# Retrieve game_pks data for the date range
pbp <- map_dfr(dates, function(date) {
  baseballr::mlb_game_pks(date) |> 
    filter(teams.away.team.name == "San Diego Padres" | teams.home.team.name == "San Diego Padres")
})

# get pbp data
pbp_data <- 1:nrow(pbp) |>  purrr::map_df(function(x) mlb_pbp(pbp$game_pk[x]))

# filter columns down to get play ids
pbp_data <- pbp_data |>  
  select(game_pk,at_bat_number = atBatIndex, pitch_number = pitchNumber,
         play_id = playId) |> 
  dplyr::mutate(at_bat_number = as.double(at_bat_number),
                at_bat_number = at_bat_number + 1,
                pitch_number = as.double(pitch_number))

# join the savant data and the pbp data to get play ids
both <- left_join(savant_data,pbp_data,by=c('game_pk','at_bat_number',
                                            'pitch_number')) |> 
  mutate(video_url = ifelse(!is.na(play_id),
                            paste0('https://baseballsavant.mlb.com/sporty-videos?playId=',play_id),play_id)) |> 
  filter(video_url != "NA")


# Assuming your NLDS dataset is called `nlds_data`
nlds_data <- both  


chadwick_player_names <- chadwick_player_lu()
pitcher_ids <- distinct(nlds_data, pitcher)

# Join NLDS data with Chadwick data
nlds_data <- nlds_data |> 
  left_join(chadwick_player_names, by = c("pitcher" = "key_mlbam")) |> 
  mutate(pitcher_name = paste(name_first, name_last))  # Combine first and last names into 'pitcher_name'

#include only necessary columns
nlds_data <- nlds_data |>
  select(pitcher_name, player_name, game_date, events, launch_angle,
         launch_speed, video_url)

write.csv(nlds_data, "nlds_data.csv")
nlds_data <- read.csv("data/nlds_data.csv")