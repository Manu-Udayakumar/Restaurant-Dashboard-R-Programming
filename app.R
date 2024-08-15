library(shiny)
library(shinydashboard)
library(dplyr)

data <- read.csv("./fastfood_calories.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Restaurant Dashboard"),
  dashboardSidebar(
    selectInput("restaurant", "Select Restaurant", choices = unique(data$restaurant)),
    selectInput("item", "Select Item", choices = NULL)
  ),
  dashboardBody(
    fluidRow(
      column(width = 12,
             box(
               title = "Average Calorie by Restaurant",
               status = "primary",
               solidHeader = TRUE,
               plotOutput("calorie_plot", height = 300)
             )
      ),
    column(width = 8,
           box(
             title = "Statistics",
             status = "info",
             solidHeader = TRUE,
             p("You can see McDonald's has the highest average calories "),
             p("and Chick-fil-A has the lowest ")
           )
    ) ),
  
    fluidRow(
      box(
        title = "Nutritional Information",
        status = "info",
        solidHeader = TRUE,
        uiOutput("nutritional_info")
      )
    ),
    fluidRow(
      column(width = 12,
             box(
               title = "Heading Fat Content",
               status = "warning",
               solidHeader = TRUE,
               plotOutput("fat_content_plot", height = 300)
             )
      )
    ),
    fluidRow(
      column(width = 12,
             box(
               title = "Total Fat, Total Carb, Sugar, and Protein",
               status = "warning",
               solidHeader = TRUE,
               plotOutput("calorie_fat_plot", height = 300)
             )
      )
    ),
    fluidRow(
      column(width = 12,
             box(
               title = "Vitamins & Minerals",
               status = "warning",
               solidHeader = TRUE,
               plotOutput("vitamin_mineral_plot", height = 300)
             )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Filter data based on selected restaurant and item
  filtered_data <- reactive({
    req(input$restaurant, input$item)
    if (!is.null(input$item)) {
      data[data$restaurant == input$restaurant & data$item == input$item, ]
    } else {
      data[data$restaurant == input$restaurant, ]
    }
  })
  
  # Update item choices based on selected restaurant
  observe({
    if (!is.null(input$restaurant)) {
      items <- unique(data[data$restaurant == input$restaurant, ]$item)
      updateSelectInput(session, "item", choices = if (length(items) > 0) items else NULL)
    } else {
      updateSelectInput(session, "item", choices = NULL)
    }
  })
  
  # Calculate overall average calorie
  overall_calorie <- reactive({
    data %>%
      summarize(avg_calories = mean(calories))
  })
  
  # Calculate average calorie for each restaurant
  calorie_data <- reactive({
    data %>%
      group_by(restaurant) %>%
      summarize(avg_calories = mean(calories))
  })
  
  # Render overall calorie bar plot
  output$calorie_plot <- renderPlot({
    par(mar = c(6, 10, 4, 2))
    barplot(c(overall_calorie()$avg_calories, calorie_data()$avg_calories), 
            names.arg = c("Overall", calorie_data()$restaurant), las = 3,
            xlab = "", ylab = "Average Calorie", main = "Average Calorie by Restaurant",
            col = c("#3366CC", rep("#339933", length(unique(data$restaurant)))))
  })
  
  # Render nutritional information for selected item
  output$nutritional_info <- renderUI({
    if (!is.null(input$item)) {
      nutritional_info <- filtered_data() %>%
        select(calories, cal_fat, total_fat, sat_fat, trans_fat, cholesterol, sodium, total_carb, fiber, sugar, protein)
      tags$div(
        style = "height: 150px; overflow-y: scroll;",
        tags$table(
          class = "table table-striped",
          tags$thead(
            tags$tr(
              lapply(names(nutritional_info), function(col) {
                tags$th(col)
              })
            )
          ),
          tags$tbody(
            tags$tr(
              lapply(nutritional_info, function(val) {
                tags$td(val)
              })
            )
          )
        )
      )
    }
  })
  
  # Render fat content plot for selected item
  output$fat_content_plot <- renderPlot({
    if (!is.null(input$item)) {
      fat_content_names <- c("Saturated Fat", "Trans Fat")
      fat_content_values <- filtered_data() %>%
        select(sat_fat, trans_fat)
      
      barplot(as.matrix(t(fat_content_values)), beside = TRUE,
              legend.text = TRUE, args.legend = list(x = "topright"), 
              col = c("skyblue", "lightgreen"), 
              main = "Fat Content",
              ylim = c(0, max(fat_content_values, na.rm = TRUE) * 1.2),
              names.arg = c(input$item), las = 1,
              xlab = "", ylab = "")
    }
  })
  
  # Render total fat, total carb, sugar, and protein plot for selected item
  output$calorie_fat_plot <- renderPlot({
    if (!is.null(input$item)) {
      calorie_fat_names <- c("Total Fat", "Total Carbohydrates", "Sugar", "Protein")
      calorie_fat_values <- filtered_data() %>%
        select(total_fat, total_carb, sugar, protein)
      
      barplot(as.matrix(t(calorie_fat_values)), beside = TRUE,
              legend.text = TRUE, args.legend = list(x = "topright"), 
              col = c("skyblue", "lightgreen", "orange", "pink"), 
              main = "Total Fat, Total Carb, Sugar, and Protein",
              ylim = c(0, max(calorie_fat_values, na.rm = TRUE) * 1.2),
              names.arg = c(input$item), las = 1,
              xlab = "", ylab = "")
    }
  })
  
  
  

  # Render vitamin & mineral plot for selected item
  output$vitamin_mineral_plot <- renderPlot({
    if (!is.null(input$item)) {
      vitamin_mineral_names <- c("Vitamin A", "Vitamin C", "Calcium")
      vitamin_mineral_values <- filtered_data() %>%
        select(vit_a, vit_c, calcium)
      
      max_value <- max(vitamin_mineral_values, na.rm = TRUE)
      ylim <- c(0, ifelse(is.finite(max_value), max_value * 1.2, 1))
      
      barplot(as.matrix(t(vitamin_mineral_values)), beside = TRUE,
              legend.text = TRUE, args.legend = list(x = "topright"), 
              col = c("skyblue", "lightgreen", "orange"), 
              main = "Vitamins & Minerals",
              ylim = ylim,
              names.arg = c(input$item), las = 1,
              xlab = "", ylab = "")
    }
  })
}

shinyApp(ui = ui, server = server)

