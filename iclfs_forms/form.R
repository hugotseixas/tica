# HEADER ----------------------------------------------------------------------
#
# Title:        Form pilot
# Description:
#
# Author:       Hugo Tameirao Seixas
# Contact:      tameirao.hugo@gmail.com
# Date:         2021-09-02
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(geobr)
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(shiny)
library(shinythemes)
#
# OPTIONS ---------------------------------------------------------------------
#
fields_mandatory <- c("token")
#tokens <- read_lines(here::here("iclfs_forms/tokens.txt"))
tokens <- c("p023ru", "81lSDi4")
#
# LOAD BACKGROUND DATA -------------------------------------------------------

## Load geobr states ----
states <- read_state()
states <- cbind(states, st_coordinates(st_centroid(states)))
states_names <- pull(states, abbrev_state)


# SHINY APP UI ----------------------------------------------------------------

ui <-
  fluidPage(

    shinyjs::useShinyjs(),

    ## Theme ----
    theme = shinytheme("flatly"), # Set theme

    ## Title ----
    titlePanel("Perspectives for ICLFS Advances"),

    navlistPanel(
      widths = c(3, 9),

      ## Introduction ----
      tabPanel(
        "Introduction",
        h3("Introduction"),
        p(
          "This is the model of a form that will be used in this project"
        ),
        br(),
        p(
          "The introduction section will be responsible to introduce
          the project objective, the context of the questions that are
          going to be made, and the guide of what the user need to do"
        )
      ),

      ## Question 01: ICFLS area projection ----
      tabPanel(
        "Question 01",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            sliderInput( # Ask projected area for 2030
              "area2030",
              "Future area in 2030",
              value = 5000,
              min = 5000,
              max = 15000
            ),
            sliderInput( # Ask projected area for 2050
              "area2050",
              "Future area in 2050",
              value = 5000,
              min = 5000,
              max = 15000
            )
          ),
          mainPanel( # Outputs panel
            plotOutput("projection", width = "100%")
          )
        )
      ),

      ## Question 02: ICLFS states distribution ----
      tabPanel(
        "Question 02",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            h4("Potential states for ICLFS establishment"),
            fluidRow(
              column(
                3,
                checkboxGroupInput(
                  "state01",
                  NULL,
                  states_names[1:9]
                )
              ),
              column(
                3,
                checkboxGroupInput(
                  "state02",
                  NULL,
                  states_names[10:18]
                )
              ),
              column(
                3,
                checkboxGroupInput(
                  "state03",
                  NULL,
                  states_names[19:27]
                )
              )
            )
          ),
          mainPanel( # Outputs panel
            plotOutput("map", width = "100%")
          )
        )
      ),

      ## Question 03: ----
      tabPanel(
        "Question 03"
      ),

      ## Question 03: ----
      tabPanel(
        "Question 03"
      ),

      ## Question 04: ----
      tabPanel(
        "Question 04"
      ),

      ## Question 05: ----
      tabPanel(
        "Question 05"
      ),

      ## Question 06: ----
      tabPanel(
        "Question 06"
      ),

      ## Question 07: ----
      tabPanel(
        "Question 07"
      ),

      ## Submission ----
      tabPanel(
        "Submission",
        fluidRow(
          column(
            width = 4,
            textInput(
              "token",
              "Token"
            )
          ),
          column(
            width = 2, style = "margin-top: 25px;",
            actionButton("submit", "Submit", class = "btn-primary")
          )
        )
      )
    )
  )

# SHINY APP SERVER ------------------------------------------------------------

server <- function(input, output, session) {

  ## Question 01: Render ICFLS area projection ----
  output$projection <-
    renderPlot(
      {

        # Set data frame
        df <- data.frame(
          year = c(2020, 2030, 2050),
          area = c(5000, input$area2030, input$area2050)
        )

        # Create reactive plot
        ggplot(df, aes(x = year, y = area)) +
          geom_hline(
            yintercept = c(7500, 5000, 15000),
            linetype = 2,
            color = "#6b6b6b"
          ) +
          geom_line(color = "#c2294f", size = 0.9) +
          geom_point(size = 1.5) +
          geom_label(aes(label = area), nudge_y = 500, alpha = 0.7) +
          annotate(
            geom = "text",
            x = c(2017, 2017, 2017),
            y = c(5000, 7500, 15000),
            label = c(
              "Current \n area",
              "NDC \n pledge area",
              "Potential \n area"
            )
          ) +
          coord_cartesian(ylim = c(4500, 15500), xlim = c(2016, 2050)) +
          theme_bw()

      },
      height = 600, width = 900
    )

  ## Question 02: ICLFS states distribution ----
  output$map <-
    renderPlot(
      {

        state_selected <-
          reactive(
            {
              x <- c(input$state01, input$state02, input$state03)
            }
          )

        ggplot() +
          geom_sf(data = states) +
          geom_sf(
            data = states %>%
              filter(abbrev_state %in% state_selected()),
            color = "#c2294f",
            fill = "#c2294f",
            alpha = 0.3
          ) +
          geom_text(
            data = states,
            aes(x = X, y = Y, label = abbrev_state)
          ) +
          theme_bw()

      },
      height = 750, width = 900
    )

  ## Check submission token ----
  observe({

    mandatory_filled <-
      vapply(
        fields_mandatory,
        function(x) { input[[x]] %in% tokens },
        FALSE
      )

    mandatory_filled <- all(mandatory_filled)

    shinyjs::toggleState(
      id = "submit",
      condition = mandatory_filled
    )

    })

}

# DEPLOY APP ------------------------------------------------------------------

shinyApp(ui, server)
