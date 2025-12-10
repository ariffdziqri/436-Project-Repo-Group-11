## Load libraries

require(shiny)
require(tidyverse)

## Read in data

fp <- getwd()

crash_data <- read.csv(paste0(fp, '/Datasets/Motor_Vehicle_Collisions_-_Crashes_20251017.csv'))

analysis_df <- crash_data |> 
  dplyr::select(all_of(c("CRASH.DATE", 
                         "CONTRIBUTING.FACTOR.VEHICLE.1",
                         "CONTRIBUTING.FACTOR.VEHICLE.2", 
                         "CONTRIBUTING.FACTOR.VEHICLE.3", 
                         "CONTRIBUTING.FACTOR.VEHICLE.4",
                         "CONTRIBUTING.FACTOR.VEHICLE.5",
                         "BOROUGH"))) |> 
  rename(crash_date = CRASH.DATE,
         cfactor_vehicle_1 = CONTRIBUTING.FACTOR.VEHICLE.1,
         cfactor_vehicle_2 = CONTRIBUTING.FACTOR.VEHICLE.2,
         cfactor_vehicle_3 = CONTRIBUTING.FACTOR.VEHICLE.3,
         cfactor_vehicle_4 = CONTRIBUTING.FACTOR.VEHICLE.4,
         cfactor_vehicle_5 = CONTRIBUTING.FACTOR.VEHICLE.5,
         borough = BOROUGH) |> 
  mutate(date_time = with(crash_data, paste(CRASH.DATE, CRASH.TIME)) |> 
           lubridate::mdy_hm(tz = "EST")) |> 
  mutate(year = year(date_time) |> as.numeric(),
         month = month(date_time) |> as.numeric(),
         day = day(date_time),
         hour = format(date_time, format = "%H") |> as.numeric())

## Cleaning

analysis_df$borough <- analysis_df$borough |> replace(analysis_df$borough == "", NA)

## Unknown coding for 1 and 80, treating as NA
analysis_df$cfactor_vehicle_1[analysis_df$cfactor_vehicle_1 %in% c("1", "80", "")] <- NA
analysis_df$cfactor_vehicle_2[analysis_df$cfactor_vehicle_2 %in% c("1", "80", "")] <- NA
analysis_df$cfactor_vehicle_3[analysis_df$cfactor_vehicle_3 %in% c("1", "80", "")] <- NA
analysis_df$cfactor_vehicle_4[analysis_df$cfactor_vehicle_4 %in% c("1", "80", "")] <- NA
analysis_df$cfactor_vehicle_5[analysis_df$cfactor_vehicle_5 %in% c("1", "80", "")] <- NA

## Cleaning
## Mis-spelling of Illness in certain entries
analysis_df$cfactor_vehicle_1[analysis_df$cfactor_vehicle_1 == "Illnes"] <- "Illness"
analysis_df$cfactor_vehicle_2[analysis_df$cfactor_vehicle_2 == "Illnes"] <- "Illness"
analysis_df$cfactor_vehicle_3[analysis_df$cfactor_vehicle_3 == "Illnes"] <- "Illness"
analysis_df$cfactor_vehicle_4[analysis_df$cfactor_vehicle_4 == "Illnes"] <- "Illness"
analysis_df$cfactor_vehicle_5[analysis_df$cfactor_vehicle_5 == "Illnes"] <- "Illness"

shiny_df <- analysis_df |> 
  pivot_longer(
    cols = cfactor_vehicle_1:cfactor_vehicle_5,
    names_to = 'vehicle',
    values_to = 'cfactor') |> 
  group_by(year, cfactor) |> 
  summarise(n = n()) |> 
  ungroup()

## Remove NA contributing factors
target_idx <- which(shiny_df$cfactor |> is.na())
shiny_df <- shiny_df[-target_idx,]

## Remove "Unspecified" contributing factors
target_idx <- which(shiny_df$cfactor == "Unspecified")
shiny_df <- shiny_df[-target_idx,]

# Functions -----------------------------------------------------------

my_bar_graph_function <- function(dat) {
  
  dat |> 
    ggplot(aes(x = cfactor,
               y = density)) +
    geom_col(aes(fill = cfactor)) +
    xlab("Contributing Factor") +
    ylab("Count") +
    labs(title = "Counts of Different Contributing Factors",
         subtitle = "Motor Vehicle Incidents in NYC") +
    theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none")
}

# ui ----------------------------------------------------------------------

years <- shiny_df$year |> unique()

ui <- fluidPage(
  fluidRow(
    titlePanel("Contributing Factors by Year"),
    sliderInput("year", "Year", min = min(shiny_df$year), max = max(shiny_df$year), c(2012, 2025), sep = ""),
    plotOutput("bar"))
)

# server ------------------------------------------------------------------

server <- function(input, output) {
  
  ## browser()
  
  shiny_df_subset <- reactive({
    shiny_df %>%
      mutate(selected = 1 * (
          (year >= input$year[1]) &
          (year <= input$year[2])
      )) |> 
      filter(selected == 1) |> 
      group_by(cfactor) |> 
      summarise(count = sum(n)) |> 
      ungroup() |> 
      mutate(density = count/sum(count))
  })
  
  output$bar <- renderPlot(my_bar_graph_function(shiny_df_subset()))
  
}

shinyApp(ui, server)

# Another thing is that there are many visualizations answering the 
# “where the accidents are” aspect of your main question, but there is 
# not much of the “why the accidents actually occur”. Perhaps you could add 
# another visualization that expands more on the “why” or replace one of the 
# visualizations that focuses on the “where”. One suggestion for such a visualization, 
# given your data, is taking the "contributing factor *” variable values, then deriving 
# counts for the number of collisions that occur because of each variable each year. 
# Essentially, you could derive a new small dataframe where you have a year date column 
# and contributing factor count columns, where each row has the year and the counts of 
# how many accidents are due to each contributing factor. Then you could take that data 
# frame and make a bar plot where the x axis is the contributing factor variable and the 
# y axis is the number of accidents. Then you could make it interactive by letting the 
# user filter the year for the bar plot with a year drop-down or slider. Or you could
# create faceted bar plots by year. This would answer the “why” question of analyzing 
# which contributing factors cause how many or the most/least accidents every year.
