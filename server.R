# Load libraries
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(DT)
library(dplyr)
library(RPostgreSQL)
library(DBI)
library(RSQLite)
library(reshape2)
library(yaml)
library(config)
library(RJDBC)

# Read connection data from yaml
yml <- read_yaml("aws.yaml")

# Connect to AWS
# Connect to database
aws <- dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = yml$host,
    user = yml$user,
    password = yml$password,
    port = yml$port
)

# Function to generate predictions
generatePredictions <- function(names, event, num_sim, updateProgress = NULL) {
    # Loop to create predictions
    names <- names
    runner_name <- c()
    runner_time <- c()
    simulation <- c()
    
    # Get top performances
    perfs_cy <- lines %>%
        filter(EVENT == event & (NAME %in% names)) %>% #& (YEAR %in% c("2021", "2020"))) %>%
        group_by(NAME) %>%
        #arrange(TIME) %>%
        #slice_head(n = 2) %>%
        mutate(
            TIME_ADJ = case_when(
                YEAR == '2021' ~ TIME,
                YEAR == '2020' ~ TIME * 1.01,
                T ~ TIME * 1.02
            )
        ) %>%
        arrange(TIME_ADJ) %>%
        slice_head(n = 1)
    
    # Get all performances
    perfs_all <- lines %>%
        filter(EVENT == event & (NAME %in% names)) %>% #& (YEAR %in% c("2021", "2020", "2019"))) %>%
        group_by(NAME) %>%
        arrange(TIME) %>%
        slice_head(n = 5) %>%
        mutate(
            TIME_ADJ = TIME
        )
    
    # Get sd of group
    grp_sd <- sd(perfs_all$TIME_ADJ, na.rm = T)
    
    
    for(j in 1:num_sim)
    {
        # Print out for running simulation
        print(paste0("Running simulation: ", j))
        
        # Progress update for UI
        text <- paste0("Running simulation: ", j)
        updateProgress(detail = text)
        
        for(i in 1:length(names))
        {
            # Subset data to runner
            results <- perfs_cy %>% filter(NAME == names[i])
            results_all <- perfs_all %>% filter(NAME == names[i])
            # Personal best
            pb <- min(results$TIME_ADJ, na.rm = T)
            # Create an SD to use
            temp_sd <- case_when(
                is.na(sd(results_all$TIME_ADJ)) ~ grp_sd,
                T ~ sd(results_all$TIME_ADJ, na.rm = T)
            )
            # Create a distribution
            dist <- rnorm(1000, mean(results$TIME_ADJ, na.rm = T), temp_sd)
            # Filter out times > 5% better than the personal best
            dist <- dist[(dist > (pb * 0.95))]
            # Generate random sample
            pred_time <- sample(dist, 1)
            # Add time and name to vectors
            runner_name <- append(runner_name, names[i])
            runner_time <- append(runner_time, pred_time)
            simulation <- append(simulation, j)
        }
    }
    
    # Dataframe of results
    sim <- as.data.frame(cbind(runner_name, runner_time, simulation)) %>%
        mutate(
            runner_time = as.numeric(runner_time)
        )
    
    sim_results <- sim %>%
        group_by(simulation) %>%
        arrange(runner_time) %>%
        mutate(
            pred_place = 1:n()
        )
    
    sim_grouped <- sim_results %>%
        group_by(runner_name) %>%
        summarise(
            pred_wins = n_distinct(simulation[pred_place==1]),
            avg_pred_place = mean(pred_place)
        ) %>%
        mutate(
            pred_win_prob = round((pred_wins / num_sim) * 100, 2)
        ) %>%
        arrange((avg_pred_place)) %>%
        mutate(
            avg_place = 1:n()
        )
    
    # Return data
    return(sim_grouped)
}
 
# # Query the current data for individuals
df <- dbGetQuery(aws, 
                 'SELECT 
                    "ATHLETE" AS "Athlete",
                    "TEAM" AS "Team",
                    "YEAR" AS "Class",
                    "GENDER" AS "Gender",
                    "POINTS" AS "Total Points",
                    "EVENTS" AS "Events",
                    "PTS_PER_EVENT" AS "Points Per Event",
                    "RANK_NUM" AS "Power Ranking",
                    "EVENT_YEAR" AS "Event Year",
                    "DVISION" AS "Division",
                    "LOAD_DATE" AS "Load Date"
                 FROM current_power_rankings 
                 WHERE "LOAD_DATE" IN (SELECT MAX(DATE("LOAD_DATE")) FROM current_power_rankings)
                 AND "EVENT_YEAR"::int8 = 2021 ')

# Query historic data
df_hist <- dbGetQuery(aws, 'SELECT
                                "ATHLETE" AS "Athlete",
                                "TEAM" AS "Team",
                                "YEAR" AS "Class",
                                "GENDER" AS "Gender",
                                "POINTS" AS "Total Points",
                                "EVENTS" AS "Events",
                                "PTS_PER_EVENT" AS "Points Per Event",
                                "RANK_NUM" AS "Power Ranking",
                                "EVENT_YEAR" AS "Event Year",
                                "DVISION" AS "Division",
                                "LOAD_DATE" AS "Load Date"
                            FROM current_power_rankings
                            WHERE "EVENT_YEAR"::int8 <> 2021 ')

# Query team data
teams <- dbGetQuery(aws,
                    'SELECT
                        "TEAM" AS "Team",
                        "GENDER" AS "Gender",
                        "athletes" AS "Athletes",
                        "events" AS "Events",
                        "avg_rank" AS "Average Power Ranking",
                        "avg_rank_score" AS "Average Ranking Score",
                        "total_points" AS "Total Points",
                        "avg_points_per_event" AS "Average Points Per Event",
                        "division" AS "Division",
                        "load_date" AS "Load Date"
                    FROM team_rankings 
                    WHERE "load_date" IN (SELECT MAX(DATE("load_date")) FROM team_rankings) 
                    AND "athletes" > 1 ')

# Athlete line items for simulations
lines <- dbGetQuery(aws, 'SELECT DISTINCT * FROM performance_lines')

# # Disconnect
dbDisconnect(aws)

# Bind individual data
all_df <- rbind(df, df_hist)

# Reorder columns
all_df <- all_df %>%
    select(`Power Ranking`, Athlete, Team, Class, Gender, Division, `Event Year`, `Total Points`, Events, `Points Per Event`, `Load Date`) %>%
    mutate(
        `Points Per Event` = round(`Points Per Event`, 2),
        `Total Points` = round(`Total Points`, 2)
    ) %>%
    arrange(
        `Power Ranking`
    )

# Split by gender
men <- all_df %>% 
    filter(Gender == 'M')
women <- all_df %>% 
    filter(Gender == 'F')

# Reorder teams columns & split by gender

menTeams <- teams %>% filter(Gender == 'M')
womenTeams <- teams %>% filter(Gender == 'F')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ### INPUTS
    
    # Set year
    setYear <- reactive({
        return(input$year)
    })
    
    # Set division (individuals)
    setIndDiv <- reactive({
        return(input$indDivision)
    })
    
    # Set division (team)
    setTeamDiv <- reactive({
        return(input$teamDivision)
    })
    
    ### DATA HANDLING FUNCTIONS
    
    # Function to select which division to query
    # Individuals
    mensData <- eventReactive(input$loadData, {
        df <- men %>% 
            filter(Division == setIndDiv() & `Event Year` == setYear())
        
        # Drop unneeded cols
        df <- df %>% select(-c(`Event Year`, Gender, Division, `Load Date`))
        
        return(df)
    }, ignoreNULL = TRUE)
    
    womensData <- eventReactive(input$loadData, {
        df <- women %>% 
            filter(Division == setIndDiv() & `Event Year` == setYear())
        
        # Drop unneeded cols
        df <- df %>% select(-c(`Event Year`, Gender, Division, `Load Date`))
        
        return(df)
    }, ignoreNULL = TRUE)
    
    # Teams
    menTeamData <- eventReactive(input$loadTeamData, {
        df <- menTeams %>%
            filter(Division == setTeamDiv())

        # Drop unneeded cols
        df <- df %>% select(-c(Gender, Division))

        return(df)
    }, ignoreNULL = TRUE)

    womenTeamData <- eventReactive(input$loadTeamData, {
        df <- womenTeams %>%
            filter(Division == setTeamDiv())

        # Drop unneeded cols
        df <- df %>% select(-c(Gender, Division))

        return(df)
    }, ignoreNULL = TRUE)
    
    #### CONTENT TO BE DISPLAYED
    ### Info Boxes for Rankings Pages
    output$dataUpdated <- renderInfoBox({
        # Get data
        updateDate <- max(df$`Load Date`)
        valueBox(
            updateDate, "Data Updated On"
        )
    })
    
    output$mensAthletes <- renderInfoBox({
        # Get data and count athletes
        df <- mensData()
        maleRunners <- n_distinct(df$Athlete)
        valueBox(
           maleRunners, "Ranked Men's Runners"
        )
    })
    
    output$womensAthletes <- renderInfoBox({
        # Get data and count athletes
        df <- womensData()
        femaleRunners <- n_distinct(df$Athlete)
        valueBox(
            femaleRunners, "Ranked Women's Runners"
        )
    })
    
    ### Data Tables for Mens and Womens Individual Rankings
    
    output$menRank = DT::renderDataTable(mensData(), rownames = FALSE, filter = 'top')
    
    output$womenRank = DT::renderDataTable(womensData(), rownames = FALSE, filter = 'top')
    
    ### Data Tables for Mens and Womens Team Rankings
    
    output$menTeamRank = DT::renderDataTable(menTeamData(), rownames = FALSE, filter = 'top')
    
    output$womenTeamRank = DT::renderDataTable(womenTeamData(), rownames = FALSE, filter = 'top')
    
    ######################################################
    # CODE TO RUN EVENT SIMULATIONS
    # Render UI list
    updateSelectizeInput(session, 'runnerNames', choices = lines$NAME, server = TRUE)
    # Read in values for predictions (runners, event)
    # Event
    setEvent <- reactive({
        return(input$simEvent)
    })
    
    # Runners
    setRunners <- reactive({
        return(input$runnerNames)
    })
    
    # Number of sims
    setSims <- reactive({
        return(input$numSims)
    })
    
    # Run simulation
    runSimulation <- eventReactive(input$runSim, {
        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = "Running", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        updateProgress <- function(value = NULL, detail = NULL) {
            if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / input$numSims
            }
            progress$set(value = value, detail = detail)
        }
        
        # Run the simulation
        preds <- generatePredictions(names = setRunners(), event = setEvent(), num_sim = setSims(), updateProgress)
        # Subset
        preds <- preds %>%
            select(runner_name, pred_wins, pred_win_prob, avg_place)
        # Rename columns
        names(preds) <- c("Runner", "Predicted Wins", "Win Probability (%)", "Average Predicted Place")
        # Return the data
        return(preds)
    }, ignoreNULL = TRUE)
    
    # Output for data table
    output$raceSimResults <- DT::renderDataTable(runSimulation(), rownames = FALSE, filter = 'top')
    
})

