# Packages ----
library(shiny)
library(bslib)
library(tidyverse)
library(htmltools)
library(plotly)

# Data ----
load(url("https://raw.githubusercontent.com/ydkristanto/vids-analysis/main/datasets/vid_data.RData"))
# Further readings: https://nrennie.rbind.io/blog/webr-shiny-tidytuesday/

# Links ----
others_link <- tags$a(
  shiny::icon("shapes"),
  "Lainnya",
  href = "https://people.usd.ac.id/~ydkristanto/index.php/media-pengajaran/",
  target = "_blank"
)
github_link <- tags$a(
  shiny::icon("github"),
  "Github",
  href = "https://github.com/ydkristanto/vids-analysis",
  target = "_blank"
)

# User interface ----
ui <- page_navbar(
  title = "Video Analytics",
  id = "vid_analytics",
  ## Sidebar ----
  sidebar = sidebar(
    accordion(
      ### Statistics ----
      accordion_panel(
        title = "Statistics",
        selectInput(
          "stat", "Choose statistics:",
          c("Basic Statistics", "Retention"),
          selected = "Basic Statistics"
        ),
        conditionalPanel(
          condition = "input.stat == 'Basic Statistics'",
          selectInput(
            "period", "Daily/monthly:",
            c("Daily", "Monthly"),
            selected = "Daily"
          ),
          selectInput(
            "chart", "Chart:",
            c("Heatmap", "Line"),
            selected = "Heatmap"
          )
        )
      ),
      ### Filter ----
      accordion_panel(
        title = "Filter",
        conditionalPanel(
          condition = "input.stat == 'Basic Statistics'",
          #### daterange ----
          dateRangeInput(
            "daterange", "Date Range:",
            start = "2020-09-14",
            end = "2020-12-31",
            min = "2020-09-01",
            max = "2021-12-31",
            format = "dd/mm/yy",
            separator = " - "
          )
        ),
        conditionalPanel(
          condition = "input.stat == 'Retention'",
          #### year ----
          checkboxGroupInput(
            "year", "Years:",
            c("2020", "2021"),
            selected = "2020"
          ),
        ),
        #### length ----
        sliderInput(
          "length", "Video lengths (mins):",
          min = 0, max = 20, value = c(3, 10), step = 0.1, ticks = FALSE
        ),
        #### sections ----
        checkboxGroupInput(
          "sections", "Sections:",
          c(
            "Equations & Graphs" = 1,
            "Functions" = 2,
            "Rational Functions" = 3,
            "Exponentials & Logarithmic Functions" = 4,
            "Trigonometric Functions" = 5
          ),
          selected = 1:5
        ),
        #### lesson_position ----
        checkboxGroupInput(
          "lesson_position", "Lesson position in a section:",
          c(
            "1" = 1,
            "2" = 2,
            "3" = 3,
            "4" = 4,
            "5" = 5,
            "6" = 6,
            "7" = 7,
            "8" = 8
          ),
          selected = 1:8,
          inline = TRUE
        ),
        #### vid_position ----
        checkboxGroupInput(
          "vid_position", "Video position in a lesson:",
          1:6,
          selected = 1:6,
          inline = TRUE
        ),
        #### talking_head ----
        checkboxGroupInput(
          "talking_head", "Talking head:",
          c(
            "Yes" = 1,
            "No" = 0
          ),
          selected = c(0, 1),
          inline = TRUE
        )
      ),
      multiple = FALSE,
      open = "Statistics"
    )
  ),
  ## Explorer ----
  nav_panel(
    title = "Explorer",
    layout_columns(
      ### contents_value ----
      value_box(
        title = "Contents",
        value = textOutput("contents_value"),
        showcase = shiny::icon("circle-play")
      ),
      ### views_value ----
      value_box(
        title = "Views",
        value = textOutput("views_value"),
        showcase = shiny::icon("binoculars")
      ),
      ### watch_time_value ----
      value_box(
        title = "Watch time (hours)",
        value = textOutput("watch_time_value"),
        showcase = shiny::icon("stopwatch")
      ),
      card(
        
      ),
      col_widths = c(4, 4, 4, 12),
      row_heights = c(1, 3)
    ),
    icon = shiny::icon("chart-simple")
  ),
  ## Information ----
  nav_panel(
    title = "Information",
    "Here is the information!",
    icon = shiny::icon("circle-info")
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    nav_item(others_link),
    nav_item(github_link),
    icon = shiny::icon("link"),
    align = "right"
  )
)

# Server ----
server <- function(input, output, session) {
  ## basic_stats_dat ----
  basic_stats_dat <- reactive({
    # Temp vars for filter
    min_daterange <- input$daterange[1]
    max_daterange <- input$daterange[2]
    min_length <- input$length[1]
    max_length <- input$length[2]
    sect <- input$sections
    lesson_pos <- input$lesson_position
    vid_pos <- input$vid_position
    head <- input$talking_head
    
    # Apply filter
    dat <- basic_stats_data %>% 
      filter(
        topic_id %in% sect,
        lesson_position %in% lesson_pos,
        vid_position %in% vid_pos,
        talking_head %in% head,
        day >= min_daterange,
        day <= max_daterange,
        duration >= min_length,
        duration <= max_length
      )
    dat
  })
  
  ## retention_dat ----
  retention_dat <- reactive({
    # Temp vars for filter
    year_input <- input$year
    min_length <- input$length[1]
    max_length <- input$length[2]
    sect <- input$sections
    lesson_pos <- input$lesson_position
    vid_pos <- input$vid_position
    head <- input$talking_head
    
    # Apply filter
    dat <- retention_data %>% 
      filter(
        year %in% year_input,
        duration >= min_length,
        duration <= max_length,
        topic_id %in% sect,
        lesson_position %in% lesson_pos,
        vid_position %in% vid_pos,
        talking_head %in% head
      )
    dat
  })
  
  ## contents_value ----
  output$contents_value <- renderText({
    # Temp vars for filter
    min_length <- input$length[1]
    max_length <- input$length[2]
    sect <- input$sections
    lesson_pos <- input$lesson_position
    vid_pos <- input$vid_position
    head <- input$talking_head
    
    data <- video_info %>% 
      filter(
        duration >= min_length,
        duration <= max_length,
        topic_id %in% sect,
        lesson_position %in% lesson_pos,
        vid_position %in% vid_pos,
        talking_head %in% head
      )
    n <- data %>% 
      distinct(vid_id) %>% 
      nrow()
    
    n
  })
  
  ## views_value ----
  output$views_value <- renderText({
    # Temp vars for filter
    min_length <- input$length[1]
    max_length <- input$length[2]
    sect <- input$sections
    lesson_pos <- input$lesson_position
    vid_pos <- input$vid_position
    head <- input$talking_head
    year_input <- input$year
    
    if (input$stat == "Basic Statistics") {
      n <- sum(basic_stats_dat()$views)
    } else {
      data <- basic_stats_data %>% 
        mutate(year_stat = year(day)) %>% 
        filter(
          topic_id %in% sect,
          lesson_position %in% lesson_pos,
          vid_position %in% vid_pos,
          talking_head %in% head,
          year_stat %in% year_input,
          duration >= min_length,
          duration <= max_length
        )
      n <- sum(data$views)
    }
    n
  })
  
  ## watch_time_value ----
  output$watch_time_value <- renderText({
    # Temp vars for filter
    min_length <- input$length[1]
    max_length <- input$length[2]
    sect <- input$sections
    lesson_pos <- input$lesson_position
    vid_pos <- input$vid_position
    head <- input$talking_head
    year_input <- input$year
    
    if (input$stat == "Basic Statistics") {
      n <- sum(basic_stats_dat()$estimatedMinutesWatched)
    } else {
      data <- basic_stats_data %>% 
        mutate(year_stat = year(day)) %>% 
        filter(
          topic_id %in% sect,
          lesson_position %in% lesson_pos,
          vid_position %in% vid_pos,
          talking_head %in% head,
          year_stat %in% year_input,
          duration >= min_length,
          duration <= max_length
        )
      n <- sum(data$estimatedMinutesWatched)
    }
    round(n / 60, 2)
  })
  
}

# App ----
shinyApp(ui, server)
