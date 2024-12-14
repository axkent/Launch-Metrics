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
nlds_data <- read.csv("nlds_data.csv")
##### The following shiny app only needs the nlds_data df to run. 

ui <- fluidPage(
  titlePanel("Pitcher and Batter Outcomes"),
  
  sidebarLayout(
    sidebarPanel(
      # Pitcher and batter selection
      selectInput("selected_pitcher", "Select Pitcher:", choices = NULL),
      selectInput("selected_batter", "Select Batter:", choices = NULL),
      
      # Date range input
      dateRangeInput("date_range", "Select Date Range:",
                     start = NULL, end = NULL),
      
      # Event selection with Select All button
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(style = "flex-grow: 1;", 
            selectInput("selected_events", "Select Events:",
                        choices = NULL,
                        multiple = TRUE,
                        selected = NULL)
        ),
        div(style = "margin-left: 10px; margin-top: 25px;",
            actionButton("toggle_all", "Select/Deselect All", class = "btn-sm btn-default")
        )
      ),
      # Launch angle range input
      sliderInput("angle_range", "Filter Launch Angle Range:",
                  min = -75, max = 75,
                  value = c(-75, 75)),
      
      # Launch speed range input
      sliderInput("speed_range", "Filter Launch Speed Range:",
                  min = 0, max = 120,
                  value = c(0, 120)),
      
      radioButtons("hover_mode", "Point Interaction Mode:",
                   choices = c("Show Launch Stats" = "stats",
                               "Show Video Links" = "links"),
                   selected = "stats"),
      actionButton("update", "Update Plot", class = "btn-primary")
    ),
    
    mainPanel(
      # Plot panel
      div(class = "well",
          h4("Launch Angle vs. Launch Speed"),
          plotlyOutput("outcomePlot"),
          div(style = "text-align: right; margin-top: 10px;",
              downloadButton("downloadPlot", "Save Plot as HTML")
          )
      ),
      # Tables in two columns
      fluidRow(
        column(4,
               div(class = "well",
                   h4("Launch Angle Distribution"),
                   gt_output("angleTable")
               )
        ),
        column(4,
               div(class = "well",
                   h4("Launch Speed Distribution"),
                   gt_output("speedTable")
               )
        ),
        column(4,
               div(class = "well",
                   h4("Event Frequencies"),
                   gt_output("eventTable")
               )
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  all_events <- reactiveVal()
  
  # Get the overall ranges for launch angle and speed once at startup
  axis_ranges <- reactive({
    list(
      angle_range = range(nlds_data$launch_angle, na.rm = TRUE),
      speed_range = c(min(nlds_data$launch_speed, na.rm = TRUE), 120)
    )
  })
  
  # Update input choices when the app starts
  observe({
    # Update pitcher choices
    pitcher_choices <- c("All Pitchers", unique(nlds_data$pitcher_name))
    updateSelectInput(session, "selected_pitcher", choices = pitcher_choices)
    
    # Update batter choices
    batter_choices <- c("All Batters", unique(nlds_data$player_name))
    updateSelectInput(session, "selected_batter", choices = batter_choices)
    
    # Update date range
    date_range <- range(nlds_data$game_date, na.rm = TRUE)
    updateDateRangeInput(session, "date_range",
                         start = date_range[1],
                         end = date_range[2])
    
    # Get all events from the data and select all of them by default
    event_choices <- sort(unique(nlds_data$events))
    all_events(event_choices)
    updateSelectInput(session, "selected_events", 
                      choices = event_choices,
                      selected = event_choices)
  })
  
  # Toggle all events when button is clicked
  observeEvent(input$toggle_all, {
    if (length(input$selected_events) == length(all_events())) {
      updateSelectInput(session, "selected_events", selected = character(0))
    } else {
      updateSelectInput(session, "selected_events", selected = all_events())
    }
  })
  
  # Reactive function to filter data based on user input
  filtered_data <- reactive({
    data <- nlds_data
    
    if (!is.null(input$selected_pitcher) && input$selected_pitcher != "All Pitchers") {
      data <- data |>  filter(pitcher_name == input$selected_pitcher)
    }
    
    if (!is.null(input$selected_batter) && input$selected_batter != "All Batters") {
      data <- data |>  filter(player_name == input$selected_batter)
    }
    
    if (!is.null(input$date_range)) {
      data <- data |>  
        filter(game_date >= input$date_range[1],
               game_date <= input$date_range[2])
    }
    
    if (!is.null(input$selected_events) && length(input$selected_events) > 0) {
      data <- data |>  filter(events %in% input$selected_events)
    }
    
    # Add launch angle and speed filtering
    data <- data |> 
      filter(
        launch_angle >= input$angle_range[1],
        launch_angle <= input$angle_range[2],
        launch_speed >= input$speed_range[1],
        launch_speed <= input$speed_range[2]
      )
    
    return(data)
  })
  
  # Create launch angle distribution table
  output$angleTable <- render_gt({
    req(input$update)
    
    plot_data <- filtered_data()
    
    # Calculate percentages for launch angle ranges
    angle_dist <- plot_data |> 
      summarise(
        `Below -50°` = mean(launch_angle < -50, na.rm = TRUE) * 100,
        `-50° to 0°` = mean(launch_angle >= -50 & launch_angle < 0, na.rm = TRUE) * 100,
        `0° to 50°` = mean(launch_angle >= 0 & launch_angle < 50, na.rm = TRUE) * 100,
        `Above 50°` = mean(launch_angle >= 50, na.rm = TRUE) * 100
      ) |> 
      pivot_longer(everything(), 
                   names_to = "Range", 
                   values_to = "Percentage")
    
    gt(angle_dist) |> 
      fmt_number(columns = "Percentage", decimals = 1) |> 
      cols_label(Percentage = "% of Observations") |> 
      tab_options(table.font.size = px(14))
  })
  
  # Create launch speed distribution table
  output$speedTable <- render_gt({
    req(input$update)
    
    plot_data <- filtered_data()
    
    # Calculate percentages for launch speed ranges
    speed_dist <- plot_data |> 
      summarise(
        `25-50 mph` = mean(launch_speed >= 25 & launch_speed < 50, na.rm = TRUE) * 100,
        `50-75 mph` = mean(launch_speed >= 50 & launch_speed < 75, na.rm = TRUE) * 100,
        `75-100 mph` = mean(launch_speed >= 75 & launch_speed < 100, na.rm = TRUE) * 100,
        `Above 100 mph` = mean(launch_speed >= 100, na.rm = TRUE) * 100
      ) |> 
      pivot_longer(everything(), 
                   names_to = "Range", 
                   values_to = "Percentage")
    
    gt(speed_dist) |> 
      fmt_number(columns = "Percentage", decimals = 1) |> 
      cols_label(Percentage = "% of Observations") |> 
      tab_options(table.font.size = px(14))
  })
  # Create event frequency table
  output$eventTable <- render_gt({
    req(input$update)
    
    plot_data <- filtered_data()
    
    # Calculate event frequencies
    event_freq <- plot_data |> 
      count(events, sort = TRUE) |> 
      rename(Event = events,
             Frequency = n)
    
    gt(event_freq) |> 
      tab_options(table.font.size = px(14))
  })
  
  
  output$outcomePlot <- renderPlotly({
    req(input$update)
    
    plot_data <- filtered_data()
    ranges <- axis_ranges()
    # Plot code adapted from: bayesball
    # Source: https://gist.github.com/bayesball/a1f8ddb4593e7b31b83022e511f5e560
    # Accessed on: November 30, 2024
    hits <- c("single", "double", "triple", "home_run")
    plot_data <- plot_data |> 
      mutate(
        Hit = ifelse(events %in% hits, "Hit", "Not a hit"),
        # Create different hover text based on mode
        hover_text = case_when(
          input$hover_mode == "stats" ~ sprintf("Launch Angle: %.1f°\nLaunch Speed: %.1f mph\nEvent: %s", 
                                                launch_angle, launch_speed, events),
          input$hover_mode == "links" & !is.na(video_url) ~ "Click to watch video",
          TRUE ~ ""
        )
        
      )
    
    pitcher_title <- if (input$selected_pitcher == "All Pitchers") "All Pitchers" else input$selected_pitcher
    batter_title <- if (input$selected_batter == "All Batters") "All Batters" else input$selected_batter
    
    p <- ggplot(plot_data, aes(x = launch_angle, y = launch_speed, 
                               color = Hit,
                               text = hover_text)) +
      geom_point() +
      geom_point(data = filter(plot_data, events == "home_run"), size = 5) +
      {
        if(input$hover_mode == "links") {
          geom_text(aes(label = ifelse(!is.na(video_url), 
                                       paste0("<a href='", video_url, 
                                              "' style='color: transparent; text-decoration: none;'>Watch</a>"), 
                                       "")),
                    hjust = 1, vjust = 1, check_overlap = TRUE)
        }
      } +
      scale_color_manual(values = c("Hit" = "#1f77b4", "Not a hit" = "#ff7f0e"),
                         breaks = c("Hit", "Not a hit"),
                         drop = FALSE) +
      scale_x_continuous(breaks = seq(-80, 80, by = 10)) +
      coord_cartesian(xlim = ranges$angle_range,
                      ylim = ranges$speed_range) +
      theme_minimal() +
      labs(
        title = sprintf("Launch Variables for Balls in Play:\nPitcher: %s\nBatter: %s", 
                        pitcher_title, batter_title),
        x = "Launch Angle", 
        y = "Launch Speed",
        caption = "*Big dot represents home runs"
      )
    
    # Convert to plotly with different settings based on mode
    if(input$hover_mode == "stats") {
      ggplotly(p, tooltip = "text") |> 
        config(displayModeBar = FALSE)
    } else {
      ggplotly(p) |> 
        config(displayModeBar = FALSE)
    }
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      # Create filename based on current pitcher and batter
      pitcher <- if(input$selected_pitcher == "All Pitchers") "AllPitchers" else input$selected_pitcher
      batter <- if(input$selected_batter == "All Batters") "AllBatters" else input$selected_batter
      paste0("launch_plot_", gsub(" ", "", pitcher), "_vs_", gsub(" ", "", batter), ".html")
    },
    content = function(file) {
      # Save the plot as a standalone HTML file
      p <- isolate({
        plot_data <- filtered_data()
        ranges <- axis_ranges()
        
        hits <- c("single", "double", "triple", "home_run")
        plot_data <- plot_data |> 
          mutate(
            Hit = ifelse(events %in% hits, "Hit", "Not a hit"),
            hover_text = case_when(
              input$hover_mode == "stats" ~ sprintf("Launch Angle: %.1f°\nLaunch Speed: %.1f mph\nEvent: %s", 
                                                    launch_angle, launch_speed, events),
              input$hover_mode == "links" & !is.na(video_url) ~ "Click to watch video",
              TRUE ~ ""
            )
          )
        
        pitcher_title <- if (input$selected_pitcher == "All Pitchers") "All Pitchers" else input$selected_pitcher
        batter_title <- if (input$selected_batter == "All Batters") "All Batters" else input$selected_batter
        
        p <- ggplot(plot_data, aes(x = launch_angle, y = launch_speed, 
                                   color = Hit,
                                   text = hover_text)) +
          geom_point() +
          geom_point(data = filter(plot_data, events == "home_run"), size = 5) +
          {
            if(input$hover_mode == "links") {
              geom_text(aes(label = ifelse(!is.na(video_url), 
                                           paste0("<a href='", video_url, 
                                                  "' style='color: transparent; text-decoration: none;'>Watch</a>"), 
                                           "")),
                        hjust = 1, vjust = 1, check_overlap = TRUE)
            }
          } +
          scale_color_manual(values = c("Hit" = "#1f77b4", "Not a hit" = "#ff7f0e"),
                             breaks = c("Hit", "Not a hit"),
                             drop = FALSE) +
          scale_x_continuous(breaks = seq(-75, 75, by = 10)) +
          coord_cartesian(xlim = ranges$angle_range,
                          ylim = ranges$speed_range) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5,
                                      margin = margin(b = 40)),
            plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
          ) +
          labs(
            title = sprintf("Launch Variables for Balls in Play\nPitcher: %s\nBatter: %s", 
                            pitcher_title, batter_title),
            x = "Launch Angle", 
            y = "Launch Speed",
            caption = "*Big dot represents home runs"
          )
        
        if(input$hover_mode == "stats") {
          ggplotly(p, tooltip = "text") |> 
            config(displayModeBar = FALSE)
        } else {
          ggplotly(p) |> 
            config(displayModeBar = FALSE)
        }
      })
      
      htmlwidgets::saveWidget(p, file, selfcontained = TRUE)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)

ƒ