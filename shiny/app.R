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

dig <- read.csv("DIG.csv") %>%
  mutate(
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    RACE = factor(RACE, levels = c(1, 2), labels = c("White", "Nonwhite")),
    TRTMT = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin"))
  )

cont_vars <- c(
  "Age" = "AGE",
  "BMI" = "BMI",
  "Ejection fraction (%)" = "EJF_PER",
  "Heart rate (bpm)" = "HEARTRTE",
  "Systolic BP (mmHg)" = "SYSBP",
  "Diastolic BP (mmHg)" = "DIABP"
)

cat_vars <- c(
  "Sex" = "SEX",
  "Race" = "RACE",
  "NYHA class" = "FUNCTCLS",
  "Diabetes" = "DIABETES",
  "Hypertension" = "HYPERTEN"
)

outcome_vars <- c(
  "Death (any cause)" = "DEATH",
  "Any hospitalization" = "HOSP",
  "CVD hospitalization" = "CVD",
  "HF hospitalization" = "WHF"
)

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

  server <- function(input, output, session) {
    
    filtered <- reactive({
      req(input$age_range, input$sex_filter, input$trt_filter)
      dig %>%
        filter(
          AGE >= input$age_range[1],
          AGE <= input$age_range[2],
          SEX %in% input$sex_filter,
          TRTMT %in% input$trt_filter
        )
    })
    
    output$n_patients <- renderValueBox({
      valueBox(
        value = nrow(dig),
        subtitle = "Patients",
        icon = icon("users"),
        color = "purple"
      )
    })
    
    output$pct_treat <- renderValueBox({
      pct <- mean(dig$TRTMT == "Digoxin") * 100
      valueBox(
        value = sprintf("%.1f%%", pct),
        subtitle = "On digoxin",
        icon = icon("capsules"),
        color = "green"
      )
    })
    
    output$mean_age <- renderValueBox({
      valueBox(
        value = sprintf("%.1f", mean(dig$AGE, na.rm = TRUE)),
        subtitle = "Mean age (years)",
        icon = icon("user-clock"),
        color = "yellow"
      )
    })
    output$death_rate <- renderValueBox({
      dr <- mean(dig$DEATH == 1, na.rm = TRUE) * 100
      valueBox(
        value = sprintf("%.1f%%", dr),
        subtitle = "Death Rate",
        icon = icon("skull-crossbones"),
        color = "red"
      )
    })
    
    output$survival_rate <- renderValueBox({
      sr <- (1 - mean(dig$DEATH == 1, na.rm = TRUE)) * 100
      valueBox(
        value = sprintf("%.1f%%", sr),
        subtitle = "Survival Rate",
        icon = icon("heart"),
        color = "green"
      )
    })
    output$age_hist <- renderPlot({
      ggplot(dig, aes(x = AGE)) +
        geom_histogram(bins = 30) +
        labs(x = "Age (years)", y = "Count")
    })
    
    output$ef_hist <- renderPlot({
      ggplot(dig, aes(x = EJF_PER)) +
        geom_histogram(bins = 30) +
        labs(x = "Ejection fraction (%)", y = "Count")
    })
    
    baseline_data <- reactive({
      d <- dig
      if (input$baseline_trt != "Both") {
        d <- d %>% filter(TRTMT == input$baseline_trt)
      }
      if (input$baseline_sex != "Both") {
        d <- d %>% filter(SEX == input$baseline_sex)
      }
      d
    })
    
    output$baseline_cont_plot <- renderPlot({
      var <- input$baseline_cont
      ggplot(baseline_data(), aes(x = TRTMT, y = .data[[var]])) +
        geom_boxplot() +
        labs(x = "Treatment group", y = names(cont_vars)[cont_vars == var])
    })
    
    output$baseline_cont_table <- renderDataTable({
      var <- input$baseline_cont
      baseline_data() %>%
        group_by(TRTMT) %>%
        summarise(
          n = n(),
          mean = mean(.data[[var]], na.rm = TRUE),
          sd = sd(.data[[var]], na.rm = TRUE)
        )
    })
    
    output$baseline_cat_plot <- renderPlot({
      var <- input$baseline_cat
      ggplot(baseline_data(), aes(x = .data[[var]], fill = TRTMT)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(x = names(cat_vars)[cat_vars == var], y = "Proportion")
    })
    
    output$baseline_cat_table <- renderDataTable({
      var <- input$baseline_cat
      baseline_data() %>%
        count(TRTMT, !!sym(var)) %>%
        group_by(TRTMT) %>%
        mutate(pct = n / sum(n))
    })
    
    outcome_data <- reactive({
      dig %>%
        filter(!is.na(.data[[input$outcome_var]]))
    })
    
    output$outcome_plot <- renderPlot({
      ggplot(outcome_data(),
             aes(x = .data[[input$outcome_group]],
                 fill = .data[[input$outcome_var]])) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(x = input$outcome_group,
             y = "Proportion",
             fill = names(outcome_vars)[outcome_vars == input$outcome_var])
    })
    
    output$outcome_table <- renderDataTable({
      outcome_data() %>%
        count(.data[[input$outcome_group]], .data[[input$outcome_var]]) %>%
        group_by(.data[[input$outcome_group]]) %>%
        mutate(pct = n / sum(n))
    })
    
    output$rel_plot <- renderPlot({
      xvar <- input$xvar
      yvar <- input$yvar
      colour_var <- input$rel_colour
      
      p <- ggplot(dig, aes(x = .data[[xvar]], y = .data[[yvar]]))
      
      if (colour_var != "None") {
        p <- p + aes(colour = .data[[colour_var]])
      }
      
      p <- p + geom_point(alpha = 0.5)
      
      if (input$add_smooth) {
        p <- p + geom_smooth(method = "loess", se = FALSE)
      }
      
      p + labs(
        x = names(cont_vars)[cont_vars == xvar],
        y = names(cont_vars)[cont_vars == yvar]
      )
    })
    
    output$data_table <- renderDT({
      datatable(dig, options = list(pageLength = 25))
    })
  }
  
  
  
  shinyApp(ui = ui, server = server)

shinyApp(ui, server)
