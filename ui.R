# Load libraries
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(stats)
library(knitr)
library(dplyr)
library(png)
library(DT)

# Define UI for application
ui <- navbarPage("DPI",
  # Individual Rankings
  tabPanel("Distance Power Index",
    # Sidebar
    sidebarLayout(
      sidebarPanel(
        # Select Division for data
        selectInput("indDivision", 
                    "Select Division",
                    choices = c("D1", "D2", "D3")),
        # Select Division for data
        selectInput("year", 
                    "Select Year",
                    choices = c("2021", "2020", "2019", "2018", "2017")),
        # Button to change data
        actionButton("loadData", label = "Load Rankings")
      ),
      # Main page layout
      mainPanel(
        wellPanel(
          h1("Distance Power Index"),
          p("Please read the 'About' page to answer questions. Select a year and division and press 'Load Rankings' to generate rankings")
        ),
        # Info box row
        # wellPanel(
        #   valueBoxOutput("dataUpdated"),
        #   valueBoxOutput("mensAthletes"),
        #   valueBoxOutput("womensAthletes")
        # ),
        # Mens rankings
        wellPanel(
          box(
            title = "Mens Power Rankings", solidHeader = TRUE,
            collapsible = TRUE, background = "light-blue", width = '100%',
            DT::dataTableOutput('menRank'), style = "overflow-x: scroll;"
          )
        ),
        # Womens rankings
        wellPanel(
          box(
            title = "Womens Power Rankings", solidHeader = TRUE,
            collapsible = TRUE, background = "light-blue", width = '100%',
            DT::dataTableOutput('womenRank'), style = "overflow-x: scroll;"
          )
        )
      )
    ),
  ),
  # Team Rankings
  tabPanel("Team Power Rankings",
           # Sidebar
           sidebarLayout(
             sidebarPanel(
               # Select Division for data
               selectInput("teamDivision", 
                           "Select Division",
                           choices = c("D1", "D2", "D3")),
               # Button to change data
               actionButton("loadTeamData", label = "Load Team Rankings")
             ),
             # Main page layout
             mainPanel(
               wellPanel(
                 h1("Team Power Rankings"),
                 p("Please read the 'About' page to answer questions. Select a year and division and press 'Load Rankings' to generate rankings")
               ),
               # Mens rankings
               wellPanel(
                 box(
                   title = "Mens Team Rankings", solidHeader = TRUE,
                   collapsible = TRUE, background = "light-blue", width = '100%',
                   DT::dataTableOutput('menTeamRank'), style = "overflow-x: scroll;"
                 )
               ),
               # Womens rankings
               wellPanel(
                 box(
                   title = "Womens Team Rankings", solidHeader = TRUE,
                   collapsible = TRUE, background = "light-blue", width = '100%',
                   DT::dataTableOutput('womenTeamRank'), style = "overflow-x: scroll;"
                 )
               )
             )
           )
        ),
  # Race simulations
  tabPanel("Race Simulator",
    sidebarLayout(
      # Sidebar
      sidebarPanel(
        # Enter runner names
        selectizeInput('runnerNames', choices = NULL, label = "Select Runners:", multiple = TRUE),
        # Enter event
        selectInput('simEvent', 
                    choices = c("800m", "1500m", "Mile", "3000m", "5000m", "10000m", "3000S"),
                    label = "Select Event:",
                    multiple = FALSE),
        # Sim slider
        numericInput("numSims", "Number of Simulations: (Max 5000)",
                    min = 1, max = 5000, value = 100
        ),
        # Button to run simulation
        actionButton("runSim", "Run Simulation")
      ),
      # Main Panel
      mainPanel(
        # About section
        wellPanel(
          h1("Race Simulator (BETA)"),
          p("This is the race simulator. Given this is still in the beta stage, keep a couple of things in mind.
            First, this is based on the data we have and the modeling process is still very much in the works. Second, while you can pick some historic athletes, the data for them may be more sparse 
            and lead to 'strange' results. You can mix genders in a race, if you so choose. If you pick athletes for a particular event that they have no data on, it may skew them to look worse than they 
            might be in reality.")
        ),
        # Panel for results
        wellPanel(
          box(
            title = "Race Simulation Results", solidHeader = TRUE,
            collapsible = TRUE, background = "light-blue", width = '100%',
            DT::dataTableOutput('raceSimResults'), style = "overflow-x: scroll;"
          )
        )
      )
    )
  ),
  # About tab
  tabPanel("About",
    mainPanel(
      h1("Welcome to the Distance Power Index (DPI)"),
      h2("How does it work?"),
      p(
        "In short, we calculate scores for athletes based on a number of factors. Performances are evaluated based on how an athlete sits in the NCAA, how strong their time is relative to others in that event, and their strength across multiple events.
                            An athlete accumulates points on all of these metrics. After weighting a number of developed metrics, we end up with a `RANK` value which is their final score, and determines their overall power ranking."
      ),
      h2("Can you explain the different columns? There are a lot of numbers..."),
      p("Sure. It can be a little confusing so here is an explanation of our developed metrics:"),
      p("Total Points: The total number of points the ranking system gave an athlete based on their standing in the NCAA and their specific time."),
      p("Events: Number of events an athlete is in the NCAA Top 100 for at the moment."),
      p("Points Per Event: The average number of points (out of 230) an athlete has for all their events."),
      p("Power Ranking: Where the athlete sits in their respective Division and Gender after ordering by our Final Rank Score."),
      h2("Why aren't all the athletes in the NCAA in the lists?"),
      p("In short - data limitations. Read more below."),
      h2("Some of your rankings seem...bad."),
      p("Well, that's not a question but it's a fair point. We are in the early stages of developing these (automated) rankings which means 1. There's a lot of improvements to be made and 2. We don't make manual 'corrections', what the algorithms develop is what you get.
                          No matter how good we tune them, there will ALWAYS be errors. The goal is that with more time and work, we can really dial these in. We also don't count relays right now. It's coming but not available just yet.
                          Of course, if you are upset about a ranking, just remember, blame the computer, not us."),
      h2("Team Rankings:"),
      p("Team rankings are not rankings in the same way we do individuals. Based on the scores that DPI gives different athletes, we aggregated it by team to see how different schools stack up. For now, we are not doing a definitive ranking of the schools but simply providing
        mor data for people to take a look at."),
      h2("Race Simulator:"),
      p("This is where things get fun. We've wanted a model for track races for awhile now and it's officially live (in beta) on the site. You can simulate races across different events, different eras of running, and different genders, if you want. There's plenty of work being done on 
        this front, but if you have feedback on how we can improve it, drop us a note at @distancepowerindex on Instagram."),
      h2("What comes next?"),
      p("We've mentioned things about our next steps already but there are a bunch of future adaptations that we'd love to build. Creating better algorithms, getting more (all) athletes into our system, adding more features like comparing runners, schools, etc. Maybe even doing 
                          cross country and/or outdoor track. There are plenty of opportunities to make this much better and this is essentially a 'beta' version. We wanted to get it public so you can check it out, maybe use it, and provide feedback on the good, bad, and otherwise. If you have 
                          comments, suggestions, or find bugs, please reach out and let us know. You can find us on Instagram at @distancepowerindex.")
    )
  )
)


