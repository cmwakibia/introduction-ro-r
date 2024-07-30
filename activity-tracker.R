library(shiny)
library(dplyr)
library(DT)
library(gt)
library(gtExtras)
library(plotly)

# Sample data (for demonstration purposes)
sample_data <- data.frame(
  Id = 1:10,
  Name = c("John Doe", "Jane Smith", "Alice Brown", "Bob White", "Charlie Black", "Daisy Blue", "Ella Green", "Fred Red", "Grace Yellow", "Harry Purple"),
  Email = c("john@example.com", "jane@example.com", "alice@example.com", "bob@example.com", "charlie@example.com", "daisy@example.com", "ella@example.com", "fred@example.com", "grace@example.com", "harry@example.com"),
  Gender = sample(c("Male", "Female"), 10, replace = TRUE),
  StrategicOutcome = sample(c("Outcome 1", "Outcome 2", "Outcome 3"), 10, replace = TRUE),
  Program = sample(c("Program 1", "Program 2", "Program 3"), 10, replace = TRUE),
  Country = sample(c("Kenya", "Uganda", "Tanzania"), 10, replace = TRUE),
  ActivityName = paste("Activity", 1:10),
  Month = sample(month.name, 10, replace = TRUE),
  StartDate = sample(seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-31"), by = "day"), 10),
  EndDate = sample(seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-31"), by = "day"), 10),
  TotalAttendees = sample(50:100, 10),
  AttendeesBreakdown = replicate(10, paste(sample(c("Youth", "Children", "Teachers", "Policy Makers"), 2, replace = TRUE), collapse = ", ")),
  Purpose = replicate(10, paste(sample(c("Training", "Workshop", "Seminar", "Meeting"), 1), collapse = "")),
  KeyIssues = replicate(10, paste("Key issue", 1:10)),
  FollowUp = replicate(10, paste("Follow-up", 1:10)),
  Learning = replicate(10, paste("Learning", 1:10)),
  Challenges = replicate(10, paste("Challenge", 1:10))
)

# UI
ui <- fluidPage(
  titlePanel("Activity/Event Data Collection Dashboard"),
  
  # Navigation tabs
  navbarPage("Dashboard",
             tabPanel("Data Entry",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("id", "ID"),
                          textInput("name", "Name"),
                          textInput("email", "Email"),
                          selectInput("gender", "Gender", choices = c("Male", "Female", "Other")),
                          selectInput("strategic_outcome", "Which strategic outcome area does the activity or event align with?", 
                                      choices = c("Outcome 1", "Outcome 2", "Outcome 3")),
                          selectInput("program", "Which program does the activity align with?", 
                                      choices = c("Program 1", "Program 2", "Program 3")),
                          textInput("country", "Country (specify county if in Kenya)"),
                          textInput("activity_name", "Name of the activity or the event"),
                          selectInput("month", "Month", choices = month.name),
                          dateInput("start_date", "Start date of the activity or event"),
                          dateInput("end_date", "End date of the event or activity"),
                          numericInput("total_attendees", "Total number of attendees", value = 0, min = 0),
                          textAreaInput("attendees_breakdown", "Please break down attendees into categories and include numbers", rows = 3),
                          textAreaInput("purpose", "What was the purpose of the event or activity?", rows = 3),
                          textAreaInput("key_issues", "What were the key issues, highlights, questions, or insights discussed from the activity or event?", rows = 3),
                          textAreaInput("follow_up", "What are the follow-up activities that will come up after this activity or event?", rows = 3),
                          textAreaInput("learning", "What have you learned from this activity or event? Please also give recommendation from the learning.", rows = 3),
                          textAreaInput("challenges", "Challenges", rows = 3),
                          actionButton("submit", "Submit")
                        ),
                        
                        mainPanel(
                          DTOutput("data_table")
                        )
                      )
             ),
             
             tabPanel("Data View",
                      fluidRow(
                        column(12, 
                               h3("Collected Data"),
                               dataTableOutput("view_table"))
                      )
             ),
             
             tabPanel("Dashboard",
                      fluidRow(
                        column(6, plotlyOutput("activities_by_month")),
                        column(6, plotlyOutput("major_attendees"))
                      ),
                      fluidRow(
                        column(12, gt_output("key_challenges"))
                      )
             )
  )
)

# Server
server <- function(input, output, session) {
  data <- reactiveVal(sample_data)
  
  observeEvent(input$submit, {
    new_entry <- data.frame(
      Id = input$id,
      Name = input$name,
      Email = input$email,
      Gender = input$gender,
      StrategicOutcome = input$strategic_outcome,
      Program = input$program,
      Country = input$country,
      ActivityName = input$activity_name,
      Month = input$month,
      StartDate = as.Date(input$start_date),
      EndDate = as.Date(input$end_date),
      TotalAttendees = input$total_attendees,
      AttendeesBreakdown = input$attendees_breakdown,
      Purpose = input$purpose,
      KeyIssues = input$key_issues,
      FollowUp = input$follow_up,
      Learning = input$learning,
      Challenges = input$challenges,
      stringsAsFactors = FALSE
    )
    
    # Update the reactive data frame
    data(rbind(data(), new_entry))
  })
  
  output$data_table <- renderDT({
    datatable(data(), options = list(pageLength = 5))
  })
  
  output$view_table <- renderDataTable({
    data() %>%
      datatable(options = list(pageLength = 5))
  })
  
  output$activities_by_month <- renderPlotly({
    df <- data() %>%
      count(Month) %>%
      arrange(desc(n))
    
    plot_ly(df, x = ~Month, y = ~n, type = 'bar', name = 'Activities by Month') %>%
      layout(title = 'Activities by Month')
  })
  
  output$major_attendees <- renderPlotly({
    df <- data() %>%
      count(AttendeesBreakdown) %>%
      arrange(desc(n))
    
    plot_ly(df, x = ~AttendeesBreakdown, y = ~n, type = 'bar', name = 'Major Attendees') %>%
      layout(title = 'Major Attendees')
  })
  
  output$key_challenges <- render_gt({
    data() %>%
      select(Challenges) %>%
      gt() %>%
      fmt_markdown(columns = everything()) %>%
      tab_header(
        title = "Key Challenges"
      ) %>%
      gtExtras::gt_theme_dark()
  })
}

# Run the app
shinyApp(ui, server)
