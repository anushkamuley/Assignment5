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
          valueBoxOutput("mean_age"),
          valueBoxOutput("death_rate"),
          valueBoxOutput("survival_rate")
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
      ),
      tabItem(
        tabName = "baseline",
        fluidRow(
          box(
            title = "Filters",
            width = 3,
            solidHeader = TRUE,
            status = "warning",
            selectInput("baseline_trt", "Treatment Group:",
                        choices = c("Both", "Placebo", "Digoxin")),
            selectInput("baseline_sex", "Sex:",
                        choices = c("Both", "Male", "Female")),
            selectInput("baseline_cont", "Continuous Variable:",
                        choices = NULL),
            selectInput("baseline_cat", "Categorical Variable:",
                        choices = NULL)
          ),
          box(
            title = "Continuous Variable Summary",
            width = 9,
            solidHeader = TRUE,
            plotOutput("baseline_cont_plot"),
            br(),
            dataTableOutput("baseline_cont_table")
          )
        ),
        fluidRow(
          box(
            title = "Categorical Variable Summary",
            width = 12,
            solidHeader = TRUE,
            plotOutput("baseline_cat_plot"),
            br(),
            dataTableOutput("baseline_cat_table")
          )
        )
      ),
      tabItem(
        tabName = "outcomes",
        fluidRow(
          box(
            title = "Outcome Settings",
            width = 3,
            solidHeader = TRUE,
            status = "warning",
            selectInput("outcome_var", "Outcome:",
                        choices = NULL),
            selectInput("outcome_group", "Group by:",
                        choices = c("Treatment" = "TRTMT",
                                    "Sex" = "SEX",
                                    "NYHA Class" = "FUNCTCLS"))
          ),
          box(
            title = "Outcome Rates by Group",
            width = 9,
            solidHeader = TRUE,
            plotOutput("outcome_plot"),
            br(),
            dataTableOutput("outcome_table")
          )
        )
      ),
      tabItem(
        tabName = "relationships",
        fluidRow(
          box(
            title = "Variable Selection",
            width = 3,
            solidHeader = TRUE,
            status = "warning",
            selectInput("xvar", "X Variable:", choices = NULL),
            selectInput("yvar", "Y Variable:", choices = NULL),
            selectInput("rel_colour", "Colour By:",
                        choices = c("None", "Treatment" = "TRTMT", "Sex" = "SEX"),
                        selected = "TRTMT"),
            checkboxInput("add_smooth", "Add Smooth Trend Line", TRUE)
          ),
          box(
            title = "Scatterplot",
            width = 9,
            solidHeader = TRUE,
            plotOutput("rel_plot")
          )
        )
      ),
      tabItem(
        tabName = "data",
        box(
          title = "DIG Dataset",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DTOutput("data_table")
        )
      )
    )
  )
)
server <- function(input, output, session) 
{
  
}
shinyApp(ui, server)
