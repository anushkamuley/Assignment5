#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Baseline Characteristics", tabName = "baseline", icon = icon("users")),
      menuItem("Outcomes", tabName = "outcomes", icon = icon("heartbeat")),
      menuItem("Relationships", tabName = "relationships", icon = icon("project-diagram")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "about",
        fluidRow(
          valueBoxOutput("n_patients"),
          valueBoxOutput("pct_treat"),
          valueBoxOutput("mean_age")
        ),
        fluidRow(
          box(
            title = "About the DIG Trial",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            p("This Shiny application provides an interactive exploration of the Digitalis Investigation Group (DIG) Trial dataset.",
              "Use the menu to explore baseline characteristics, outcomes, and relationships between variables.")
          )
        ),
        fluidRow(
          box(
            title = "Age Distribution",
            width = 6,
            solidHeader = TRUE,
            plotOutput("age_hist")
          ),
          box(
            title = "Ejection Fraction Distribution",
            width = 6,
            solidHeader = TRUE,
            plotOutput("ef_hist")
          )
        )
      ))))
server <- function(input, output, session) {
  
}
shinyApp(ui, server)

