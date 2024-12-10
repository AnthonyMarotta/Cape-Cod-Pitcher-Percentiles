library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DT)
library(scales)
setwd("~/Downloads/CCBLCSV")
# Sample Data - Replace this with actual data loading
FullCCBL <- read.csv("SeasonCCBL.csv")

# Rename the teams in the PitcherTeam column
FullCCBL <- FullCCBL %>%
  mutate(PitcherTeam = case_when(
    PitcherTeam == "BRE_WHI" ~ "Brewster Whitecaps",
    PitcherTeam == "BOU_BRA" ~ "Bourne Braves",
    PitcherTeam == "CHA_ANG" ~ "Chatham Anglers",
    PitcherTeam == "YAR_RED" ~ "Yarmouth-Dennis Red Sox",
    PitcherTeam == "FAL_COM" ~ "Falmouth Commodores",
    PitcherTeam == "WAR_GAT" ~ "Wareham Gatemen",
    PitcherTeam == "HAR_MAR" ~ "Harwich Mariners",
    PitcherTeam == "HYA_HAR" ~ "Hyannis Harbor Hawks",
    PitcherTeam == "ORL_FIR" ~ "Orleans Firebirds",
    PitcherTeam ==  "COT_KET" ~ "Cotuit Kettleers",
    TRUE ~ PitcherTeam
  ))

# Data processing
FullCCBL <- FullCCBL %>%
  mutate(
    HardHitCheck = case_when(between(ExitSpeed, 95, 120) ~ TRUE, TRUE ~ FALSE),
    InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
    Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
    Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0)
  ) %>%
  group_by(Pitcher, PitcherTeam, AutoPitchType) %>%
  summarise(
    PitchCount = n(),
    AvgVelocity = round(mean(RelSpeed, na.rm = TRUE), 1),
    AvgSpinRate = round(mean(SpinRate, na.rm = TRUE), 1),
    Whiffs = sum(PitchCall == "StrikeSwinging"),
    Swing = sum(PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay")),
    Whiff_Percentage = round(Whiffs / Swing * 100, 1),
    HardHits = sum(HardHitCheck, na.rm = TRUE),
    TotalBBE = sum(PlayResult %in% c("Out", "Single", "Double", "Triple", "HomeRun", "Error", "FieldersChoice", "Sacrifice")),
    HardHit_Percentage = round(sum(HardHits, na.rm = TRUE) / sum(TotalBBE, na.rm = TRUE) * 100, 1),
    Chase_Percentage = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
    AvgExitSpeedInPlay = round(mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  filter(PitchCount > 50) %>% 
  mutate(
    RelSpeed_percentile = round(percent_rank(AvgVelocity), 2),
    SpinRate_percentile = round(percent_rank(AvgSpinRate), 2),
    Whiff_Percentile = round(percent_rank(Whiff_Percentage / 100), 2),
    HardHit_Percentile = round(1 - percent_rank(HardHit_Percentage / 100), 2), # Reverse percentile
    Chase_Percentile = round(percent_rank(Chase_Percentage / 100), 2),
    ExitSpeed_percentile = round(1 - percent_rank(AvgExitSpeedInPlay), 2) # New percentile for AvgExitSpeedInPlay
  )

# UI
ui <- fluidPage(
  titlePanel("Pitcher Performance Percentiles"),
  sidebarLayout(
    sidebarPanel(
      selectInput("PitcherTeam", "Select Pitcher Team:", choices = unique(FullCCBL$PitcherTeam)),
      selectInput("AutoPitchType", "Select Auto Pitch Type:", choices = c("All", unique(FullCCBL$AutoPitchType))),
      selectInput("Pitcher", "Select Pitcher:", choices = NULL)
    ),
    mainPanel(
      plotOutput("Percentiles"),
      dataTableOutput("Percentiles_Data")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Update Pitcher choices based on selected PitcherTeam and AutoPitchType
  observe({
    filteredData <- FullCCBL %>%
      filter(PitcherTeam == input$PitcherTeam,
             if (input$AutoPitchType != "All") AutoPitchType == input$AutoPitchType else TRUE)
    
    updateSelectInput(session, "Pitcher", choices = unique(filteredData$Pitcher))
  })
  
  # Percentiles plot
  output$Percentiles <- renderPlot({
    # Filtered data for selected pitcher and AutoPitchType
    selectedPitcher <- FullCCBL %>%
      filter(Pitcher == input$Pitcher,
             if (input$AutoPitchType != "All") AutoPitchType == input$AutoPitchType else TRUE)
    
    if (input$AutoPitchType == "All") {
      # Calculate averages for all pitch types
      selectedPitcher <- selectedPitcher %>%
        summarise(
          AvgVelocity = mean(AvgVelocity, na.rm = TRUE),
          RelSpeed_percentile = mean(RelSpeed_percentile, na.rm = TRUE),
          AvgSpinRate = mean(AvgSpinRate, na.rm = TRUE),
          SpinRate_percentile = mean(SpinRate_percentile, na.rm = TRUE),
          Whiff_Percentage = mean(Whiff_Percentage, na.rm = TRUE),
          Whiff_Percentile = mean(Whiff_Percentile, na.rm = TRUE),
          HardHit_Percentage = mean(HardHit_Percentage, na.rm = TRUE),
          HardHit_Percentile = mean(HardHit_Percentile, na.rm = TRUE),
          Chase_Percentage = mean(Chase_Percentage, na.rm = TRUE),
          Chase_Percentile = mean(Chase_Percentile, na.rm = TRUE),
          AvgExitSpeedInPlay = mean(AvgExitSpeedInPlay, na.rm = TRUE),
          ExitSpeed_percentile = mean(ExitSpeed_percentile, na.rm = TRUE)
        ) %>%
        mutate(AutoPitchType = "All")
    }
    
    # Determine color based on percentile for each metric
    selectedPitcher <- selectedPitcher %>%
      mutate(
        RelSpeed_color = ifelse(RelSpeed_percentile <= 0.25, "blue",
                                ifelse(RelSpeed_percentile <= 0.45, "lightblue",
                                       ifelse(RelSpeed_percentile <= 0.55, "grey",
                                              ifelse(RelSpeed_percentile <= 0.75, "lightcoral", "red")))),
        SpinRate_color = ifelse(SpinRate_percentile <= 0.25, "blue",
                                ifelse(SpinRate_percentile <= 0.45, "lightblue",
                                       ifelse(SpinRate_percentile <= 0.55, "grey",
                                              ifelse(SpinRate_percentile <= 0.75, "lightcoral", "red")))),
        Whiff_Percentile_color = ifelse(Whiff_Percentile <= 0.25, "blue",
                                        ifelse(Whiff_Percentile <= 0.45, "lightblue",
                                               ifelse(Whiff_Percentile <= 0.55, "grey",
                                                      ifelse(Whiff_Percentile <= 0.75, "lightcoral", "red")))),
        HardHit_Percentile_color = ifelse(HardHit_Percentile <= 0.25, "blue",
                                          ifelse(HardHit_Percentile <= 0.45, "lightblue",
                                                 ifelse(HardHit_Percentile <= 0.55, "grey",
                                                        ifelse(HardHit_Percentile <= 0.75, "lightcoral", "red")))),
        Chase_Percentile_color = ifelse(Chase_Percentile <= 0.25, "blue",
                                        ifelse(Chase_Percentile <= 0.45, "lightblue",
                                               ifelse(Chase_Percentile <= 0.55, "grey",
                                                      ifelse(Chase_Percentile <= 0.75, "lightcoral", "red")))),
        ExitSpeed_color = ifelse(ExitSpeed_percentile <= 0.25, "blue",
                                 ifelse(ExitSpeed_percentile <= 0.45, "lightblue",
                                        ifelse(ExitSpeed_percentile <= 0.55, "grey",
                                               ifelse(ExitSpeed_percentile <= 0.75, "lightcoral", "red"))))
      )
    
    # Plotting RelSpeed percentiles
    plotRelSpeed <- ggplot(selectedPitcher, aes(x = RelSpeed_percentile, y = AvgVelocity)) +
      geom_point(size = 9, aes(color = RelSpeed_color)) +
      ggtitle("Avg Velocity Percentile") + xlim(0, 1) + ylim(min(selectedPitcher$AvgVelocity), max(selectedPitcher$AvgVelocity)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedPitcher$AvgVelocity), yend = max(selectedPitcher$AvgVelocity)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(RelSpeed_percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting SpinRate percentiles
    plotSpinRate <- ggplot(selectedPitcher, aes(x = SpinRate_percentile, y = AvgSpinRate)) +
      geom_point(size = 9, aes(color = SpinRate_color)) +
      ggtitle("Avg Spin Rate Percentile") + xlim(0, 1) + ylim(min(selectedPitcher$AvgSpinRate), max(selectedPitcher$AvgSpinRate)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedPitcher$AvgSpinRate), yend = max(selectedPitcher$AvgSpinRate)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(SpinRate_percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting Whiff percentiles
    plotWhiff <- ggplot(selectedPitcher, aes(x = Whiff_Percentile, y = Whiff_Percentage)) +
      geom_point(size = 9, aes(color = Whiff_Percentile_color)) +
      ggtitle("Whiff Percentile") + xlim(0, 1) + ylim(min(selectedPitcher$Whiff_Percentage), max(selectedPitcher$Whiff_Percentage)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedPitcher$Whiff_Percentage), yend = max(selectedPitcher$Whiff_Percentage)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(Whiff_Percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting HardHit percentiles
    plotHardHit <- ggplot(selectedPitcher, aes(x = HardHit_Percentile, y = HardHit_Percentage)) +
      geom_point(size = 9, aes(color = HardHit_Percentile_color)) +
      ggtitle("Hard Hit Percentile") + xlim(0, 1) + ylim(min(selectedPitcher$HardHit_Percentage), max(selectedPitcher$HardHit_Percentage)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedPitcher$HardHit_Percentage), yend = max(selectedPitcher$HardHit_Percentage)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(HardHit_Percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting Chase percentiles
    plotChase <- ggplot(selectedPitcher, aes(x = Chase_Percentile, y = Chase_Percentage)) +
      geom_point(size = 9, aes(color = Chase_Percentile_color)) +
      ggtitle("Chase Percentile") + xlim(0, 1) + ylim(min(selectedPitcher$Chase_Percentage), max(selectedPitcher$Chase_Percentage)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedPitcher$Chase_Percentage), yend = max(selectedPitcher$Chase_Percentage)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(Chase_Percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Plotting Exit Speed percentiles
    plotExitSpeed <- ggplot(selectedPitcher, aes(x = ExitSpeed_percentile, y = AvgExitSpeedInPlay)) +
      geom_point(size = 9, aes(color = ExitSpeed_color)) +
      ggtitle("Avg Exit Velocity Percentile") + xlim(0, 1) + ylim(min(selectedPitcher$AvgExitSpeedInPlay), max(selectedPitcher$AvgExitSpeedInPlay)) +
      scale_color_identity() +
      theme(
        plot.title = element_text(color = "black", size = 15, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "italic", colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"
      ) +
      geom_segment(aes(x = 0, xend = 1, y = min(selectedPitcher$AvgExitSpeedInPlay), yend = max(selectedPitcher$AvgExitSpeedInPlay)), color = "#9b9b9b", size = 1) +
      geom_text(aes(label = scales::percent(ExitSpeed_percentile)), hjust = .5, vjust = .4, color = "black", size = 5)
    
    # Arrange the plots in a grid
    grid.arrange(plotRelSpeed, plotSpinRate, plotWhiff, plotHardHit, plotChase, plotExitSpeed, ncol = 1, nrow = 6)
  })
  
  # Percentiles data table
  output$Percentiles_Data <- renderDataTable({
    selectedPitcher <- FullCCBL %>%
      filter(Pitcher == input$Pitcher,
             if (input$AutoPitchType != "All") AutoPitchType == input$AutoPitchType else TRUE)
    
    if (input$AutoPitchType == "All") {
      # Calculate averages for all pitch types
      selectedPitcher <- selectedPitcher %>%
        summarise(
          AvgVelocity = mean(AvgVelocity, na.rm = TRUE),
          RelSpeed_percentile = mean(RelSpeed_percentile, na.rm = TRUE),
          AvgSpinRate = mean(AvgSpinRate, na.rm = TRUE),
          SpinRate_percentile = mean(SpinRate_percentile, na.rm = TRUE),
          Whiff_Percentage = mean(Whiff_Percentage, na.rm = TRUE),
          Whiff_Percentile = mean(Whiff_Percentile, na.rm = TRUE),
          HardHit_Percentage = mean(HardHit_Percentage, na.rm = TRUE),
          HardHit_Percentile = mean(HardHit_Percentile, na.rm = TRUE),
          Chase_Percentage = mean(Chase_Percentage, na.rm = TRUE),
          Chase_Percentile = mean(Chase_Percentile, na.rm = TRUE),
          AvgExitSpeedInPlay = mean(AvgExitSpeedInPlay, na.rm = TRUE),
          ExitSpeed_percentile = mean(ExitSpeed_percentile, na.rm = TRUE)
        )
    }
    
    selectedPitcher %>%
      select(
        AvgVelocity, RelSpeed_percentile, AvgSpinRate, SpinRate_percentile,
        Whiff_Percentage, Whiff_Percentile, HardHit_Percentage, HardHit_Percentile,
        Chase_Percentage, Chase_Percentile, AvgExitSpeedInPlay, ExitSpeed_percentile
      ) %>%
      mutate_if(is.numeric, round, digits = 2) %>%
      datatable(options = list(pageLength = 10, autoWidth = TRUE))
  })
}

shinyApp(ui, server)




