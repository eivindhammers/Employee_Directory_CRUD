
library(shiny)
library(shinyjs)
library(shinythemes)

ui <- tagList(
  useShinyjs(),
  navbarPage(
    title = "HammersLes",
    theme = shinytheme("darkly"),
    fluid = TRUE,
    header = tagList(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "submit-form.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "profile-form.css"),
      tags$script(src = "index.js"),
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
    ),
    footer = tagList(
      div(
        class = "footer-container",
        uiOutput("footer_date"),
        div(class = "footer-title", HTML("Eivind Moe Hammersmark &copy; - Hammersmarkske lesekonkurranse"))
      )
    ),
    # Suppress all error messages. Final app only.
    tabPanel(
      "Registrer deltaker",
      div(class = "button-container", align = "right",
          circleButton("add_button", icon = icon("plus"), status = "success",
                       size = "lg")),
      fluidRow(
        div(class = "search-container",
            div(class = "search-input",
                textInput("search_field", label = NULL, value = "", width = NULL, placeholder = "Search")),
            uiOutput("table_filters")
        )
      ),
      uiOutput("download"),
      dataTableOutput("participants_table"),
    ),
    tabPanel(title = "test")
  )
)