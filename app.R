library(shiny)
library(tidyverse)
library(RColorBrewer)
library(shinythemes)

# Pull in function to run the attacks
source(file = "attack_function.R", local = TRUE)

monsters <- readRDS(file = "data/open5e_monster_data.rds")[, c("name", "armor_class")]


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  # Theme
  theme = shinytheme("slate"),
  
  # Title
  titlePanel("DnD 5e Barbarian Attack Simulator"),
  
  # Small header
  h5("For simulating damage against a hypothetical opponent"),
  
  # Creating sidebar
  sidebarLayout(
    sidebarPanel(
      
      # Strength
      numericInput("STR",
                   "Strength modifier",
                   min = -20,
                   max = 100,
                   value = c(3)),
      # Proficiency
      numericInput("PROF",
                   "Proficiency modifier",
                   min = 1,
                   max = 100,
                   value = c(2)),
      # Number of dice on damage roll
      numericInput("num_dice",
                   "Number of dice per damage roll",
                   min = 1,
                   max = 100,
                   value = c(1)),
      # Number of dice on crit roll
      numericInput("crit_dice",
                   "Total number of dice rolled on a critical hit damage roll",
                   min = 1,
                   max = 100,
                   value = c(2)),
      # Threshold to get a critical hit
      numericInput("crit_thresh",
                   "Minimum attack roll on which you get a critical hit",
                   min = 1,
                   max = 20,
                   value = c(20)),
      # Type of dice to roll for damage
      selectInput(inputId = "typedice",
                  label = "Damage roll die",
                  choices = list(
                    "d4" = 4,
                    "d6" = 6,
                    "d8" = 8,
                    "d10" = 10,
                    "d12" = 12),
                  selected = 12,
                  multiple = FALSE),
      # Armor class
      numericInput("AC",
                   "AC of monster",
                   min = 1,
                   max = 100,
                   value = c(12)),
      # Allow user to pick a monster to generate AC automatically
      selectInput(inputId = "mon_name",
                  label = "Optional: Select a monster to autofill AC",
                  choices = unique(monsters$name),
                  selected = "Kobold",
                  multiple = FALSE),
      # Any attack/damage bonus added by a weapon
      numericInput("WEAP",
                   "Weapon attack/damage bonus, if any",
                   min = 0,
                   max = 3,
                   value = c(0)),
      # How many attacks the user can take on a turn
      numericInput("atk",
                   "Number of attacks",
                   min = 1,
                   max = 10,
                   value = c(1)),
      # How many simulations to run
      numericInput("runs",
                   "Number of times to simulate",
                   min = 1,
                   max = 1000000,
                   value = c(1000)),
      # Define an action button
      actionButton("go",
                   "Simulate")
      
    ),
    mainPanel(
      tabsetPanel(
        
        # Create a tab with a plot as output from this process, called "results"
        tabPanel("Plot", plotOutput("results")),
        # Create a tab with a table as output from this process, called "table"
        tabPanel("Hit stats", tableOutput("table"))
        
      ),
      
      # Statistics about the simulation output
      textOutput("sim_info"),
      br(),
      # Include median damage statement
      textOutput("median_text"),
      # Include mean damage statement
      textOutput("mean_text")
      
    )
    
  ),
  
  hr(),
  
  # Include Open5e/OGL cite
  print(includeHTML("data/cite.html"))
  
)


# Server ------------------------------------------------------------------

server <- function(input, output, session){
  
  # React to the 'go' button being pushed by running the simulation
  simulated_attacks <- eventReactive(input$go, {
    
    # Run simulation including a progress bar
    withProgress(message = "Running simulation:", value = 0,
                 expr = {
                   
                   # Now run simulation using purrr. Below method is similar to purrr::rerun, but
                   # likely more stable long-term. It is also slower than a for() loop.
                   simulate_encounters <- map_dbl(.x = 1:input$runs,
                                                  .f =  ~ {
                                                    incProgress(amount = 1 / input$runs,
                                                                detail = paste("Doing run", .x))
                                                    
                                                    sim_damage_roll(num_attacks = input$atk,
                                                                    STR = input$STR,
                                                                    PROF = input$PROF,
                                                                    sides = input$typedice,
                                                                    num_dice = input$num_dice,
                                                                    AC = input$AC,
                                                                    crit = input$crit_dice,
                                                                    WEAP = input$WEAP,
                                                                    crit_thresh = input$crit_thresh)
                                                  })
                   
                 })
    
  })
  
  # React to 'go' button being pushed by creating text
  sim_info <- eventReactive(input$go, {
    
    print(paste("The results of simulating an attack aganst a monster of ",
                input$AC, " AC ", run.num(), " times are below:", sep = ""))
    
  })
  
  median_text <- eventReactive(input$go, {
    
    print(paste("The median damage is", median(simulated_attacks())))
    
  })
  
  mean_text <- eventReactive(input$go, {
    
    print(paste("The mean damage is", round(mean(simulated_attacks()), digits = 1)))
    
  })
  
  # React to 'go' button being pushed by updating number of runs for text in app
  run.num <- eventReactive(input$go, {
    input$runs
  })
  
  # Update the AC when the user selects a monster
  observe({
    
    new_ac <- monsters %>%
      filter(name == input$mon_name) %>%
      pull(armor_class)
    
    updateNumericInput(session = session, inputId = "AC", value = new_ac)
    
  })
  
  # Plot the simulation output
  output$results <- renderPlot({
    
    # Set up data to plot lines for median & mean attack
    plotlines <- data.frame(
      # Lower X & Y bounds
      X = c(median(simulated_attacks()),  mean(simulated_attacks())),
      Y = c(0, 0),
      # Upper X & Y bounds
      Xend = c(median(simulated_attacks()), mean(simulated_attacks())),
      Yend = c(Inf, Inf),
      # Differentiate lines
      Group = c("Median attack", "Mean attack"))
    
    # Compose plot
    ggplot(data = data.frame(simulated_attacks()), aes(simulated_attacks())) +
      geom_histogram(bins = max(simulated_attacks()), color = "black") +
      geom_segment(data = plotlines,
                   aes(x = X, xend = Xend, y = Y, yend = Yend, color = Group)) +
      scale_color_manual(name = "", values = c("deepskyblue3", "green4"),
                         # Color by line meaning
                         labels = c("Median attack", "Mean attack")) +
      theme_bw() +
      xlab("Damage amount") +
      ylab("Number of occurrences")
    
  })
  
  # Render the summary table of the simulation results
  output$table <- renderTable({
    tbl <- table(simulated_attacks())
    tbl <- data.frame(tbl)
    tbl <- mutate(tbl, percentage = 100 * (tbl$Freq / sum(tbl$Freq)))
    names(tbl) <- c("Damage",
                    paste("Freq per", paste(run.num()), sep = " "),
                    "% of attacks")
    
    tbl
    
  })
  
  # Render the informational text
  output$sim_info <- renderText({
    
    sim_info()
  })
  
  # Render the median attack text
  output$median_text <- renderText({
    
    median_text()
  })
  
  # Render the mean attack text
  output$mean_text <- renderText({
    
    mean_text()
  })
}


shinyApp(ui = ui, server = server)