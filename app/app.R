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
    ## Explorer sidebar ----
    conditionalPanel(
      "input.vid_analytics === 'Explorer'",
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
          ),
          conditionalPanel(
            condition = "input.stat == 'Retention'",
            selectInput(
              "time_type", "Time:",
              c("Real (minutes)", "Ratio"),
              selected = "Percentage"
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
    ### Information sidebar ----
    conditionalPanel(
      "input.vid_analytics === 'Information'",
      h4("Description",
         style = "font-size: inherit; font-weight: bold"
      ),
      p("This Shiny dashboard explore statistics of 92 Algebra and Trigonometry instructional videos from 2020 and 2021."),
      hr(),
      h4("MIT License", style = "font-size: inherit; font-weight: bold"),
      p("Copyright © 2024 Yosep Dwi Kristanto")
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
      ### plot_output ----
      card(
        plotlyOutput("plot_output"),
        full_screen = TRUE
      ),
      col_widths = c(4, 4, 4, 12),
      row_heights = c(1, 3)
    ),
    icon = shiny::icon("chart-simple")
  ),
  ## Information ----
  nav_panel(
    title = "Information",
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        ### About ----
        nav_panel(
          title = "About",
          p("This Shiny dashboard explores statistics from 92 Algebra and Trigonometry instructional videos, a course within the Mathematics Education department at Sanata Dharma University, Yogyakarta, Indonesia. The main focus of the statistics are two: the number of views per video per day/month and viewer retention per video. Additionally, the dashboard also displays accumulations from multiple videos, including the number of views and watch time (hours)."),
          p("Through exploration via this dashboard, users are expected to observe learner behavior while watching instructional videos. Information about this behavior is important for educators who are also content creators of instructional videos.")
        ),
        nav_panel(
          ### Tools ----
          title = "Tools",
          p("This dashboard was developed using the R programming language and the Shiny package. The shinylive package was utilized to export this application so it can be run in a web browser without a separate R server. The dashboard layout is structured using bslib."),
          p("All statistical charts in this dashboard are created using the ggplot2 package. The dplyr package is used to manipulate data obtained from OECD. Both of these packages are part of the tidyverse meta-package.")
        ),
        nav_panel(
          ### Developer ----
          title = "Developer",
          p("The developer and maintainer of this application is ", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto", target = "_blank"), " a lecturer and researcher in ", a("the Mathematics Education department", href = "https://usd.ac.id/s1pmat", target = "_blank"), " at ", a("Sanata Dharma University,", href = "https://www.usd.ac.id/", target = "_blank"), " Yogyakarta, Indonesia.")
        ),
        nav_panel(
          ### Source Code ----
          title = "Source Code",
          p("The source code of this application is available on ", a("GitHub repository.", href = "https://github.com/ydkristanto/vids-analysis", target = "_blank"), " If you would like to report any issues or request additional features for this application, please ", a("create an issue", href = "https://github.com/ydkristanto/vids-analysis/issues", target = "_blank"), " or, even better, submit a pull request in the repository.")
        )
      ),
      ### Data ----
      card(
        card_header("Data"),
        p("The data used in this dashboard spans from September 14, 2020, to December 28, 2021. This period was chosen because, during that time frame, the videos were used in online distance learning, coinciding with the COVID-19 pandemic."),
        p("The videos are organized into lessons. These lessons form a topic. These topics are the main components of the Algebra and Trigonometry course. Therefore, the data used includes variables such as vid_position (video position within a lesson), lesson_position (lesson position within a topic), and topic_id (topic names, for example, 'Equations & Graphs').")
      )
    ),
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
  ## video_info_dat ----
  video_info_dat <- reactive({
    # Temp vars for filter
    min_length <- input$length[1]
    max_length <- input$length[2]
    sect <- input$sections
    lesson_pos <- input$lesson_position
    vid_pos <- input$vid_position
    head <- input$talking_head

    dat <- video_info %>%
      filter(
        duration >= min_length,
        duration <= max_length,
        topic_id %in% sect,
        lesson_position %in% lesson_pos,
        vid_position %in% vid_pos,
        talking_head %in% head
      )
    dat
  })

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
    n <- video_info_dat() %>%
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

  ## plot_output ----
  output$plot_output <- renderPlotly({
    if (input$stat == "Basic Statistics") {
      if (input$period == "Daily") {
        if (input$chart == "Heatmap") {
          p <- basic_stats_dat() %>% 
            mutate(
              vid_seq = 100 * topic_id + 10 * lesson_id + vid_position
            ) %>% 
            mutate(vid_title = fct_reorder(vid_title, vid_seq)) %>% 
            ggplot(aes(x = day, y = vid_title, fill = views)) +
            geom_tile() +
            scale_fill_viridis_c(name = "Views") +
            theme_minimal() +
            theme(
              axis.text.y = element_blank(),
              axis.title.y = element_blank()
            ) +
            labs(
              x = "Day"
            )
          plot <- ggplotly(p)
        } else if (input$chart == "Line") {
          plot <- plot_ly(
            basic_stats_dat(), x = ~day, y = ~views, color = ~vid_title,
            type = "scatter", mode = "lines"
          )%>% 
            layout(xaxis = list(title = "Day"), 
                   yaxis = list(title = "Views"))
        }
      } else if(input$period == "Monthly") {
        data <- basic_stats_dat() %>% 
          mutate(
            vid_seq = 100 * topic_id + 10 * lesson_id + vid_position,
            month = format(day, "%Y-%m")
          ) %>% 
          select(vid_seq, vid_title, day, month, views) %>% 
          arrange(vid_seq, day) %>% 
          group_by(vid_seq, vid_title, month) %>% 
          summarise(views_month = sum(views))
        if (input$chart == "Heatmap") {
          p <- data %>% 
            mutate(vid_title = fct_reorder(vid_title, vid_seq)) %>% 
            ggplot(aes(x = month, y = vid_title, fill = views_month)) +
            geom_tile() +
            scale_fill_viridis_c(name = "Views") +
            theme_minimal() +
            theme(
              axis.text.y = element_blank(),
              axis.title.y = element_blank()
            ) +
            labs(
              x = "Month"
            )
          plot <- ggplotly(p)
        } else if (input$chart == "Line") {
          plot <- plot_ly(
            data, x = ~month, y = ~views_month, color = ~vid_title,
            type = "scatter", mode = "lines"
          ) %>% 
            layout(xaxis = list(title = "Month"), 
                   yaxis = list(title = "Views"))
        }
      }
    } else if (input$stat == "Retention") {
      if (input$time_type == "Real (minutes)") {
        p <- retention_dat() %>% 
          ggplot(aes(x = elapsed_time_mins, y = audience_watch_ratio)) +
          geom_point(alpha = .2) +
          geom_smooth() +
          xlim(0, 20) +
          ylim(0, 1.6) +
          theme_minimal() +
          labs(x = "Elapsed time (minutes)", y = "Audience watch ratio")
        plot <- ggplotly(p)
      } else if (input$time_type == "Ratio") {
        p <- retention_dat() %>% 
          ggplot(aes(x = elapsed_time_ratio, y = audience_watch_ratio)) +
          geom_point(alpha = .2) +
          geom_smooth() +
          theme_minimal() +
          labs(x = "Elapsed time (ratio)", y = "Audience watch ratio")
        plot <- ggplotly(p)
      }
      
    }
    
    plot
  })
}

# App ----
shinyApp(ui, server)
