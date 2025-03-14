library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(forcats)

### LOAD DATA
data <- read.csv("final_data.csv")


### DEFINE TOPIC LABELS
topic_mapping <- c(
  "Topic 1" = "Habits &\n Goals",
  "Topic 2" = "Education &\n Career",
  "Topic 3" = "Social\n life",
  "Topic 4" = "Health &\n Fitness",
  "Topic 5" = "Social\n media &\n Entertainment",
  "Topic 6" = "Confidence &\n Dating",
  "Topic 7" = "Family &\n Finances",
  "Topic 8" = "Learning",
  "Topic 9" = "Mental\n health"
)

### DEFINE TOPIC COLORS
color_topics = c(
  "Habits &\n Goals" = "#E69F00",
  "Education &\n Career" = "#0072B2",
  "Social\n life" = "#56B4E9",
  "Health &\n Fitness" = "#009E73",
  "Social\n media &\n Entertainment" = "#F0E442",
  "Confidence &\n Dating" = "#D55E00",
  "Family &\n Finances" = "#CC79A7",
  "Learning" = "#999999",
  "Mental\n health" = "#660099")


### DEFINE PLOT FUNCTION

base_plot <- function(input_data, column, plot_title) {
  #Always reorder based on the total mean of the foundation
  ggplot(input_data, aes(x = reorder(topic_name, total_mean), 
                         y = input_data[[column]], fill = topic_name),
         clip = "off") +
    geom_bar(stat = "identity", show.legend = FALSE) +
    scale_fill_manual(values = color_topics) + 
    labs(
      title = plot_title,
      x = "Topics",
      y = "Score"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "noto_sans"), 
      plot.title = element_text(size = 20, hjust=0.5),  
      plot.subtitle = element_text(size = 10, hjust=0.5),
      axis.title.x = element_text(size = 10),             
      axis.title.y = element_text(size = 10),
      axis.text.x = element_text(size = 9),              
      axis.text.y = element_text(size = 9),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 60)
    ) +
    scale_y_continuous(limits = c(0, 1.6)) +
    coord_flip() }


### USER INTERFACE
ui <- page_fluid(
  
  selectInput('foundation', 
              'Choose moral foundation',
              list("Care", "Fairness", "Loyalty", "Authority", "Sanctity")
              ,
              selected = "Sanctity",
              width = "100%"),
  
  fluidRow(
    column(width = 12,
           
           plotOutput("total")
    )), 
  
  # Second row: Two half-width plots
  
  fluidRow(
    column(width = 6,
           plotOutput("virtue")
    ),
    column(width = 6,
           plotOutput("vice")
    )
  )
)

### SERVER
server <- function(input, output) {
  
  # PREPROCESS DATA FOR PLOTTING
  filtered_data <- reactive({
    
    # Get name of foundation's columns in original data
    selected_foundation <- input$foundation
    total_col <- paste0(selected_foundation, "_total")
    virtue_col <- paste0(selected_foundation, "_Virtue")
    vice_col <- paste0(selected_foundation, "_Vice")
    
    # Manipulate original data 
    data %>%
      #Use all_of to treat dynamic column names as columns and not strings
      select(dominant_topic, all_of(c(total_col, virtue_col, vice_col))) %>%
      group_by(dominant_topic) %>%
      #Get mean values of columns for all documents 
      summarise(across(all_of(c(total_col, virtue_col, vice_col)), mean, .names = "{.col}_mean")) %>%
      #Put shorter names
      rename_with(~c("total_mean", "virtue_mean", "vice_mean"), 
                  all_of(paste0(c(total_col, virtue_col, vice_col), "_mean"))) %>%
      # Put topics labels
      mutate(topic_name = recode(dominant_topic, !!!topic_mapping)) %>%
      # Reorder so topics with more morality appear at the top in the plot
      mutate(topic_name = fct_reorder(topic_name, total_mean, .desc = TRUE))
  })
  
  
  # PLOTS
  output$total <- renderPlot({
    base_plot(
      input_data = filtered_data(),
      column = "total_mean", 
      plot_title = paste("Total Moral Language\n", input$foundation))
  })
  
  output$virtue <- renderPlot({
    base_plot(
      input_data = filtered_data(),
      column = "virtue_mean", 
      plot_title = paste("Virtue Moral Language\n", input$foundation))
  })   
  
  output$vice <- renderPlot({
    base_plot(
      input_data = filtered_data(),
      column = "vice_mean", 
      plot_title = paste("Vice Moral Language\n", input$foundation))
  })   
}

# Run the application 
shinyApp(ui = ui, server = server)
