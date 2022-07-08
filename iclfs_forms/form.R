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
    titlePanel("Perspectivas para o avanço do ILPF"),

    navlistPanel(
      widths = c(3, 9),

      ## Introduction ----
      tabPanel(
        "Introdução",
        h3("Introdução"),
        p(
          "Esse é o modelo do questionário que será utilizado nesse projeto."
        ),
        br(),
        p(
          "A seção de introdução vai ser responsável por esclarecer o objetivo
          do projeto, o contexto em que as perguntas serão feitas,
          e o guia de preenchimento do questionário."
        )
      ),

      ## Question 01: ICFLS area projection ----
      tabPanel(
        "Questão 01",
        sidebarLayout(
          fluid = TRUE,
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Segundo suas expectativas, qual a área total ocupada
              por ILPF em 2030 e 2050, em milhões de hectares?"
            ),
            sliderInput( # Ask projected area for 2030
              "area2030",
              "Área total em 2030 (em milhões de hectares)",
              value = 1,
              min = 1,
              max = 80
            ),
            sliderInput( # Ask projected area for 2050
              "area2050",
              "Área total em 2050 (em milhões de hectares)",
              value = 1,
              min = 1,
              max = 80
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
            width = 10,
            titlePanel(
              title = "Segundo suas expectativas, quais estados tem o maior
              potencial de expansão de ILPF?"
            ),
            h4("Estados com potencial para expansão do ILPF no Brasil"),
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
        "Question 03",
        sidebarLayout(
          fluid = TRUE,
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Segundo sua experiência com ILPF, qual é o ciclo médio,
              em anos, da produção de espécies lenhosas (silvicultura) no
              ILPF?"
            ),
            sliderInput( # Ask projected area for 2030
              "area2030",
              "Tempo até o corte das árvores",
              value = 1,
              min = 1,
              max = 30
            )
          ),
          mainPanel( # Outputs panel

          )
        )
      ),

      ## Question 04: ----
      tabPanel(
        "Question 04",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Segundo sua experiência, quais espécies são utilizadas
              com maior frequência no ILPF no Brasil?"
            ),
            h4("Principais espécies utilizadas no ILPF"),
            fluidRow(
              column(2, h4("Lavoura"), textInput("specie01", NULL)),
              column(2, h4("Pastagem"), textInput("specie01", NULL)),
              column(2, h4("Animal"), textInput("specie01", NULL)),
              column(2, h4("Floresta"), textInput("specie01", NULL)),
              column(2, h4("Outras"), textInput("specie01", NULL))
            ),
            fluidRow(
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL))
            ),
            fluidRow(
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL))
            ),
            fluidRow(
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL))
            ),
            fluidRow(
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL)),
              column(2, textInput("specie01", NULL))
            )
          ),
          mainPanel( # Outputs panel

          )
        )
      ),

      ## Question 05: ----
      tabPanel(
        "Question 05",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Segundo sua experiência, há algum tipo de preparo
              convencional do solo (revolvimento do solo) após a implementação
              do ILPF? Se sim, quanto tempo leva, anos em média, desde a implementação
              do ILPF até o revolvimento do solo?"
            ),
            checkboxGroupInput(
              "soilmanagement",
              NULL,
              choices = c("Sim", "Não")
            ),
            sliderInput( # Ask projected area for 2030
              "soilmanagementperiod",
              "Tempo até o revolvimento do solo após implementação do ILPF",
              value = 1,
              min = 1,
              max = 50
            )
          ),
          mainPanel( # Outputs panel

          )
        )
      ),

      ## Question 06: ----
      tabPanel(
        "Question 06",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Perfil do especialista: anos de experiencia, local de
              experiencia"
            )
          ),
          mainPanel( # Outputs panel

          )
        )
      ),

      ## Question 07: ----
      tabPanel(
        "Question 07",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Quanto tempo dura a transição para o ILPF?
              Como ela é feita?"
            )
          ),
          mainPanel( # Outputs panel

          )
        )
      ),

      ## Question 08: ----
      tabPanel(
        "Question 08",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Quais são os insumos utilizados? Quanto?"
            )
          ),
          mainPanel( # Outputs panel

          )
        )
      ),

      ## Question 09: ----
      tabPanel(
        "Question 09",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Qual a área média de um ilpf?"
            )
          ),
          mainPanel( # Outputs panel

          )
        )
      ),

      ## Question 10: ----
      tabPanel(
        "Question 10",
        sidebarLayout(
          sidebarPanel( # Inputs panel
            width = 10,
            titlePanel(
              title = "Quais serviços ecossistêmicos
              estão relacionados ao ILPF?"
            )
          ),
          mainPanel( # Outputs panel

          )
        )
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
          area = c(1, input$area2030, input$area2050)
        )

        # Create reactive plot
        ggplot(df, aes(x = year, y = area)) +
          geom_hline(
            yintercept = c(1, 10, 80),
            linetype = 2,
            color = "#6b6b6b"
          ) +
          geom_line(color = "#c2294f", size = 0.9) +
          geom_point(size = 1.5) +
          geom_label(aes(label = area), nudge_y = 500, alpha = 0.7) +
          annotate(
            geom = "text",
            x = c(2017, 2017, 2017),
            y = c(1, 10, 80),
            label = c(
              "Current \n area",
              "NDC \n pledge area",
              "Potential \n area"
            )
          ) +
          coord_cartesian(ylim = c(0, 81), xlim = c(2016, 2050)) +
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
