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
    geom_col(aes(fill = "deepskyblue3")) +
    xlab("Contributing Factor") +
    ylab("Density") +
    labs(title = "Counts of Different Contributing Factors",
         subtitle = "Motor Vehicle Incidents in NYC") +
    theme(legend.position = "none") +
    coord_flip()
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
      slice_max(n = 10, order_by = count, with_ties = TRUE) |> ## only retain top 10 contributing factors
      ungroup() |> 
      mutate(density = count/sum(count),
             cfactor = fct_reorder(cfactor, density))
  })
  
  output$bar <- renderPlot(my_bar_graph_function(shiny_df_subset()))
  
}

shinyApp(ui, server)
