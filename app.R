library(gtools)
library(tidyverse)
library(shiny)
library(gt)
library(nbaplotR)

lottery_odds <- read.csv("https://raw.githubusercontent.com/Henryjean/data/refs/heads/main/draftodds.csv") %>% 
  mutate(team = str_replace_all(team, "SAS via ATL", "ATL"),
         team = str_replace_all(team, "HOU via PHX", "PHX"),
         team = str_replace_all(team, "SAS", "SA"),
         team = str_replace_all(team, "NOP", "NO"))

all_combinations = combinations(14,4)

standings = c("UTA", "WAS", "CHA", "NO", "PHI", "BKN", "TOR", "SA", "PHX", "POR", "DAL", "CHI", "SAC", "ATL")

df = as.data.frame(all_combinations)

teams = rep("UTA", 140)
teams = c(teams, rep("WAS", 140), rep("CHA", 140), rep("NO", 125), rep("PHI", 105),
          rep("BKN", 90), rep("TOR", 75), rep("SA", 60), rep("PHX", 38), rep("POR", 37),
          rep("DAL", 18), rep("CHI", 17), rep("SAC", 8), rep("ATL", 7), NA)
id = c(1:1000)
df = df %>% 
  mutate(team = sample(teams, size = n(), replace = FALSE)) %>%
  relocate(team) %>% 
  filter(!is.na(team)) %>% 
  cbind(id)

simulate_lottery <- function(df, standings,
                             force_team1 = "No Selection", force_team2 = "No Selection",
                             force_team3 = "No Selection", force_team4 = "No Selection",
                             force_team5 = "No Selection", force_team6 = "No Selection",
                             force_team7 = "No Selection", force_team8 = "No Selection",
                             force_team9 = "No Selection", force_team10 = "No Selection",
                             force_team11 = "No Selection", force_team12= "No Selection",
                             force_team13 = "No Selection", force_team14 = "No Selection") {
  
  lottery_results <- rep(NA, 14)
  forced_teams <- c(force_team1, force_team2, force_team3, force_team4,
                    force_team5, force_team6, force_team7, force_team8,
                    force_team9, force_team10, force_team11, force_team12,
                    force_team13, force_team14)
  
  forced_teams_noselection <- forced_teams[forced_teams != "No Selection"]
  if (any(duplicated(forced_teams_noselection))) {
    stop("You selected the same team more than once.")
  }
  
  # Figure out teams that might have jumped
  teams_that_must_be_top4 <- c()
  for (pick_pos in 5:14) {
    team_i <- forced_teams[pick_pos]
    if (!is.null(team_i) && team_i != "No Selection") {
      standing_rank <- match(team_i, standings)
      if (!is.na(standing_rank) && pick_pos > standing_rank) {
        worse_teams <- standings[(standing_rank + 1):length(standings)]
        teams_that_must_be_top4 <- union(teams_that_must_be_top4, worse_teams)
      }
    }
  }
  
  # Assign forced top 4 if desired
  forced_top4 <- forced_teams[1:4]
  for (i in 1:4) {
    team_i <- forced_top4[i]
    if (!is.null(team_i) && team_i != "No Selection") {
      if (!team_i %in% df$team) {
        stop(paste0("Team ", team_i, " has no combinations — conditioning is impossible."))
      }
      lottery_results[i] <- team_i
      df <- df[df$team != team_i, ]
    }
  }
  
  # Make sure teams who are forced 5-14 stay there, figure out which teams jumped
  locked_teams <- forced_teams[5:14]
  locked_teams <- locked_teams[locked_teams != "No Selection"]
  teams_that_must_be_top4 <- setdiff(teams_that_must_be_top4, locked_teams)
  
  locked_top_4 <- c()
  for (pick in 1:4) {
    team_i <- forced_top4[pick]
    if (!is.null(team_i) && team_i != "No Selection") {
      locked_top_4 <- union(locked_top_4, team_i)
    }
  }
  
  teams_that_must_be_top4 <- setdiff(teams_that_must_be_top4, locked_top_4)
  
  # Simulate top 4
  attempts <- 0
  max_attempts <- 10000
  while (TRUE) {
    temp_df <- df[!df$team %in% locked_teams, ]
    temp_results <- lottery_results[1:4]
    fill_slots <- which(is.na(temp_results))
    temp_must_be_top4 <- teams_that_must_be_top4
    
    for (slot in fill_slots) {
      eligible <- temp_df[temp_df$team %in% temp_must_be_top4 | !(temp_df$team %in% forced_teams), ]
      if (nrow(eligible) == 0) stop("No eligible teams to fill top 4 slots under constraints.")
      
      selected <- eligible[sample(nrow(eligible), 1), ]
      temp_results[slot] <- selected$team
      temp_df <- temp_df[temp_df$team != selected$team, ]
      temp_must_be_top4 <- setdiff(temp_must_be_top4, selected$team)
    }
    
    if (length(temp_must_be_top4) == 0) {
      lottery_results[1:4] <- temp_results
      df <- df[!df$team %in% temp_results, ]
      break
    }
    
    attempts <- attempts + 1
    if (attempts > max_attempts) stop("Could not satisfy top-4 constraints after many attempts.")
  }
  
  # Assign picks 5–14 (worst-best record)
  pick <- 5
  for (i in 5:14) {
    team_i <- forced_teams[i]
    if (!is.null(team_i) && team_i != "No Selection") {
      lottery_results[pick] <- team_i
    } else {
      already_selected <- c(lottery_results, locked_teams)
      remaining <- standings[!(standings %in% already_selected)]
      lottery_results[pick] <- remaining[1]
    }
    pick <- pick + 1
  }
  
  return(lottery_results)
}




ui <- fluidPage(

 
    titlePanel("2025 NBA Draft Lottery Simulator"),

    sidebarLayout(
        sidebarPanel(width = 3,
                     actionButton("simulate_btn", "Simulate Lottery", style = "color: white; background-color: #0072B2; border-color: #005b96;"),
                     h4("Optionally Set Forced Picks:"),
            selectInput("1", 
                        "1: ", 
                        choices = c("No Selection", standings),
                        selected = "No Selection"),
            selectInput("2", 
                        "2: ", 
                        choices = c("No Selection", standings),
                        selected = "No Selection"),
            selectInput("3", 
                        "3: ", 
                        choices = c("No Selection", standings),
                        selected = "No Selection"),
            selectInput("4", 
                        "4: ", 
                        choices = c("No Selection", standings),
                        selected = "No Selection"),
            selectInput("5", 
                        "5: ", 
                        choices = c("No Selection", "UTA", "WAS", "CHA", "NO", "PHI"),
                        selected = "No Selection"),
            selectInput("6", 
                        "6: ", 
                        choices = c("No Selection", "CHA", "NO", "WAS", "PHI", "BKN"),
                        selected = "No Selection"),
            selectInput("7", 
                        "7: ", 
                        choices = c("No Selection", "BKN", "PHI", "TOR", "NO", "CHA"),
                        selected = "No Selection"),
            selectInput("8", 
                        "8: ", 
                        choices = c("No Selection", "SA", "TOR", "BKN", "PHI", "NO"),
                        selected = "No Selection"),
            selectInput("9", 
                        "9: ", 
                        choices = c("No Selection","PHX", "SA", "TOR", "BKN", "PHI"),
                        selected = "No Selection"),
            selectInput("10", 
                        "10: ", 
                        choices = c("No Selection", "POR", "PHX", "SA", "TOR", "BKN"),
                        selected = "No Selection"),
            selectInput("11", 
                        "11: ", 
                        choices = c("No Selection", "DAL", "POR", "PHX", "SA", "TOR"),
                        selected = "No Selection"),
            selectInput("12", 
                        "12: ", 
                        choices = c("No Selection", "CHI", "DAL", "POR", "PHX", "SA"),
                        selected = "No Selection"),
            selectInput("13", 
                        "13: ", 
                        choices = c("No Selection", "SAC", "CHI", "DAL", "POR", "PHX"),
                        selected = "No Selection"),
            selectInput("14", 
                        "14: ", 
                        choices = c("No Selection", "ATL", "SAC", "CHI", "DAL", "POR"),
                        selected = "No Selection")
        ),

       
        mainPanel(
          h3("Simulated Odds", style = "text-align: center;"),
           tableOutput("tbl"),
          h6("Note: The lottery is simulated 10,000 times. Since these are simulated odds, they aren't the exact probabilities.", style = "text-align: center;"),
          h3("Original Odds", style = "text-align: center;"),
          tableOutput("tbl2"),
          h4("Picks owed to other teams"),
          HTML("<p style='color: #002D62; font-weight: bold;'>- OKC owns PHI's pick, top-6 protected</p>"),
          HTML("<p style='color: #CE1141; font-weight: bold;'>- HOU owns PHX's pick</p>"),
          HTML("<p style='color: #FDB927; font-weight: bold;'>- ATL owns SAC's pick, top-12 protected</p>"),
          HTML("<p style='color: #000000; font-weight: bold;'>- SA owns ATL's pick</p>")
        )
    ),
    absolutePanel(
      top = 10, right = 10,
      width = "auto", height = "auto",
      style = "background-color: transparent; font-size: 12px; color: gray;",
      "Created by Shane Faberman"
    )
)


server <- function(input, output) {
  
  
  output$lottery_table <- renderTable({
    data.frame(Pick = 1:14, Team = results())
  })
  
  reactive_sim <- eventReactive(input$simulate_btn, {
    withProgress(message = "Simulating... (Depending on your selections, this could take a few minutes.)", value = 0.2, {
      results_list <- replicate(
        n = 10000,
        expr = simulate_lottery(df, standings,
                                force_team1 = input$`1`, force_team2 = input$`2`,
                                force_team3 = input$`3`, force_team4 = input$`4`, force_team5 = input$`5`,
                                force_team6 = input$`6`, force_team7 = input$`7`, force_team8 = input$`8`,
                                force_team9 = input$`9`, force_team10 = input$`10`, force_team11 = input$`11`,
                                force_team12 = input$`12`, force_team13 = input$`13`, force_team14 = input$`14`
        ),
        simplify = FALSE
      )
      
      results_df <- results_list %>%
        map_dfr(~ set_names(as.list(.x), 1:14))
      
      results_long <- results_df %>%
        pivot_longer(cols = everything(), names_to = "Pick", values_to = "Team") %>%
        group_by(Pick, Team) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(freq = count / 10000,
               Pick = as.integer(Pick)) %>%
        select(-count) %>%
        arrange(Pick, Team) %>%
        pivot_wider(id_cols = Team, names_from = Pick, values_from = freq)
      
      results_long <- results_long %>%
        mutate(Team = fct_relevel(Team, standings)) %>%
        arrange(Team)
      
      results_long
    })
  })
  

    output$tbl <- render_gt({
        
      reactive_sim() %>% 
        gt() %>% 
        fmt_percent(  
          columns = -Team,
          decimals = 2
        ) %>% 
        gt_nba_logos(columns = c("Team")) %>% 
        opt_stylize(style = 5, color = "blue") %>% 
        sub_missing(
          columns = everything(),
          rows = everything(),
          missing_text = ""
        ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "black"),
            cell_text(color = "#C4CED4", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`1`:`14`),
            rows = Team == "ATL"
          )
        ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "#FDB927"),
            cell_text(color = "#C8102E", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`13`:`14`),
            rows = Team == "SAC"
          )
        ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "#CE1141"),
            cell_text(color = "#000000", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`1`:`14`),
            rows = Team == "PHX"
          )
        ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "#002D62"),
            cell_text(color = "#EF3B24", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`7`:`14`),
            rows = Team == "PHI"
          )
        )
    })
    
    output$tbl2 <- render_gt({
      
      lottery_odds %>% 
        select(team, pick, odds) %>% 
        mutate(odds = round(odds,2),
               odds = ifelse(team == "TOR" & pick == 1, 7.50, odds),
               odds = ifelse(odds == "0.00%", "-", odds)) %>% 
        mutate(odds = sprintf("%.2f%%", odds),
               odds = ifelse(odds == "0.00%", "", odds)) %>% 
        pivot_wider(names_from = pick, values_from = odds) %>%
        rename(Team = team) %>% 
        gt() %>% 
        gt_nba_logos(columns = c("Team")) %>% 
        opt_stylize(style = 5, color = "blue") %>% 
        tab_style(
          style = list(
            cell_fill(color = "black"),
            cell_text(color = "#C4CED4", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`1`:`14`),
            rows = Team == "ATL"
          )
        ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "#FDB927"),
            cell_text(color = "#C8102E", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`13`:`14`),
            rows = Team == "SAC"
          )
        ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "#CE1141"),
            cell_text(color = "#000000", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`1`:`14`),
            rows = Team == "PHX"
          )
        ) %>% 
        tab_style(
          style = list(
            cell_fill(color = "#002D62"),
            cell_text(color = "#EF3B24", weight = "bold")
          ),
          locations = cells_body(
            columns = c(`7`:`14`),
            rows = Team == "PHI"
          )
        )
    })
}


shinyApp(ui = ui, server = server)
