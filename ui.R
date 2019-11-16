#Name: Pradnya Alchetti
#Student Id - 29595916
#Visualization Project


#ui.R
library(shiny)
library(leaflet)
library(plotly)
library(chorddiag)
library(shinythemes)

#Main Page Panel
fluidPage(theme=shinytheme("cosmo"),
navbarPage("Taxi Statistics", id="nav",
           # Tab 1 for airport insights
           tabPanel("NYC AirPort Insights",
                div(class="outer",
                    
                  tags$head(
                  # Include our custom CSS
                      includeCSS("styles.css")
                  ),
                
                
                  # If not using custom CSS, set height of leafletOutput to a number instead of percent
                  #render leaflet with new york as focus view
                  leafletOutput("siteMap", width="100%", height="100%"),
                  
                  # Shiny versions prior to 0.11 should use class = "modal" instead.
                  #render leaft panel on the leaflet
                  absolutePanel(id = "controls_1", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = 20, right = 600, bottom = "auto",
                                width = 200, height = "auto",
                                h3("Trips Explorer"),
                                #Side panel with controls to select airport
                                checkboxGroupInput("airport", "Airports",
                                                   c("JFK" = "JFK",
                                                     "LGA" = "LGA",
                                                     "EWR" = "EWR")),
                                #dropdown for month 
                                selectInput("month", "Month of the year 2016",
                                            c("October" = "10",
                                              "November" = "11",
                                              "December" = "12"))
                  ),
                  # render right panel on the leaflet
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                width = 500, height = "auto",
                                # render heatmap for tip
                                plotlyOutput("tip"),
                                # render donut chart for payment
                                plotlyOutput("payment",height = 200)
                    
                )
              )  
           ),
           
           # Tab 2 for Fare Estimator
           tabPanel("NYC Fare Estimator",
              fluidRow(
                # dropdown for borough to be placed vertically
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                  selectInput("borough", "NY Borough",
                              c("Bronx" = "Bronx",
                                "Brooklyn" = "Brooklyn",
                                "Manhattan" = "Manhattan",
                                "Queens" = "Queens"))),
                # dropdown for pickupZone
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                  selectInput("pickupZone", "NY Pickup Zone",
                              c("Dummy"))),
                #dropdown for dropoff zone
                div(style="display: inline-block;vertical-align:top; width: 150px;",
                  selectInput("dropZone", "NY Dropoff Zone",
                              c("Dummy"))),
                #dropdown for hour
                div(style="display: inline-block;vertical-align:top; width: 100px;",
                    selectInput("time", "Hour",
                                c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)))
              ),
              #render chord diagram
              div(class="outer2",
                 chorddiagOutput("zoneCount", width = 600, height = 400)
              ),
              #render leaflet with map of New York focussed
              div(class="outer1",
                  leafletOutput("map", height = 400, width = 700)
              ),
              #render line chart
              div(class="outer3",
                  plotOutput("line", height = 200, width = 500)
              )
      )
) 
)