
### Necessary Packages ###
library(tidyverse)
library(sportyR)
library(dplyr)
library(gt)
library(ggplot2)
library(rsconnect)
library(shiny)


merged_df_final <- read.csv("merged_df_final.csv")

### Shiny UI ###
ui <- fluidPage(
  titlePanel("Weighted Faceoff Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("player", "Select Player: (use delete to type a name)", choices = unique(merged_df_final$event_player_1_name), selected = unique(merged_df_final$event_player_1_name)[1]),
    ),
    
    mainPanel(
      tableOutput("playerStats"),
      
      br(),
      
      fluidRow(
        column(6, plotOutput("percentilePlot")),  
        br(), br(), br(), br(),  
        
        column(6, tableOutput("percentilesTable"))  
      )
    )
  )
)




### Shiny Server ###
server <- function(input, output) {
  
  selected_player_stats <- reactive({
    merged_df_final %>% 
      mutate(
        faceoff_percentage_diff = weighted_win_percentage - win_percentage
      ) %>%
      filter(event_player_1_name == input$player) %>%
      select(
        Player = event_player_1_name,
        `Normal Win %` = win_percentage,
        `Total Faceoffs` = total_faceoffs,
        `Weighted Win %` = weighted_win_percentage,
        `Faceoff Diff` = faceoff_percentage_diff,
        `Importance Factor` = importance_factor
      )
  })
  
  stat_names <- c("Weighted Win %" = "weighted_win_percentage",
                  "Normal Win %" = "win_percentage",
                  "Total Faceoffs" = "total_faceoffs",
                  "Importance Factor" = "importance_factor")
  
# Calculating/Finding Percentiles for each stat
  output$percentilesTable <- renderTable({
    percentiles <- sapply(stat_names, function(stat) {
      percentile_column <- merged_df_final %>%
        mutate(percentile = percent_rank(!!sym(stat))) %>%
        filter(event_player_1_name == input$player) %>%
        select(percentile) %>%
        pull()
      return(round(percentile_column * 100, 2))
    })
    
    data.frame(Statistic = names(stat_names), Percentile = percentiles)
  })


  output$percentilePlot <- renderPlot({
    percentiles <- sapply(stat_names, function(stat) {
      percentile_column <- merged_df_final %>%
        mutate(percentile = percent_rank(!!sym(stat))) %>%
        filter(event_player_1_name == input$player) %>%
        select(percentile) %>%
        pull()
      return(round(percentile_column * 100, 2))
    })
    
    percentile_data <- data.frame(
      Statistic = names(stat_names),
      Percentile = percentiles
    )

    percentile_data$Statistic <- factor(percentile_data$Statistic, levels = names(stat_names))
    
# Percentlies bar graph
    ggplot(percentile_data, aes(x = Statistic, y = Percentile, fill = Statistic)) +
      geom_bar(stat = "identity", color = "black") + 
      ylim(0, 100) +
      labs(
        title = "Player Percentile in Each Category",
        x = "Statistic", y = "Percentile (%)", subtitle = "Data from the 2023-24 NHL Season",
        caption = "created by Kyle Galsky"
      ) +
      theme_minimal() +  # Minimalistic theme
      theme(
        axis.text = element_text(size = 12),      
        axis.title = element_text(size = 14),   
        plot.title = element_text(size = 16),  
        panel.grid.major = element_line(color = "gray", size = 0.5),  
        panel.grid.minor = element_line(color = "gray", size = 0.25), 
        legend.position = "none" 
      )
  })
  
# Player stats table
  output$playerStats <- renderTable({
    selected_player_stats() 
  })
}


### Run the Shiny app ###
shinyApp(ui = ui, server = server)

