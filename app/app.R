# Load packages ----
library(shiny)
library(bslib)
library(tidyverse)

# Load data ----
load(url("https://raw.githubusercontent.com/ydkristanto/vids-analysis/main/datasets/vid_analytics.RData"))
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
      accordion_panel(
        title = "Statistics",
        selectInput(
          "stat", "Choose statistics:",
          c("Basic Statistics", "Retention"),
          selected = "Basic Statistics"
        )
      ),
      accordion_panel(
        title = "Filter",
        selectInput(
          "sections", "Sections:",
          c(
            "Equations & Graphs" = "1",
            "Functions" = "2",
            "Rational Functions" = "3",
            "Exponentials & Logarithmic Functions" = "4",
            "Trigonometric Functions" = "5"
          ),
          selected = c("1", "2", "3", "4", "5"),
          multiple = TRUE
        ),
        selectInput(
          "positions", "Positions:",
          1:6,
          selected = 1:6, multiple = TRUE
        ),
        sliderInput(
          "length", "Lengths (mins):",
          min = 0, max = 20, value = c(3, 10), step = 0.1, ticks = FALSE
        )
      ),
      multiple = FALSE,
      open = "Statistics"
    )
  ),
  nav_panel(
    title = "Explore",
    "Hello!",
    icon = shiny::icon("chart-simple")
  ),
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
  output$retention_plot <- renderPlot({
    data <- retention
    data %>%
      ggplot(aes(x = elapsed_time_mins, y = audience_watch_ratio)) +
      geom_point()
  })
}

# App
shinyApp(ui, server)
