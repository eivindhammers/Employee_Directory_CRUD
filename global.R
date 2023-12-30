library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinyalert)
library(DT)
library(shinydashboard)
library(shinythemes)
library(readxl)
library(openxlsx)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(DBI)
library(pool)
library(uuid)




########################Create sql database#############################

pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

###################################Button functions####################

imageDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("images"), label)
}

excelDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("file-excel"), label)
}

tableDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("download"), label)
}
