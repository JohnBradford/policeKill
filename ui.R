library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Washington Post, Killed by Police in 2015"),

  sidebarLayout(
    
    sidebarPanel(
      selectInput("mode", label="Organization of Data", choices=c("Percent of Group", "Total Frequency"), selected="Percent of Group"),
      selectInput("v1", label="Group 1", 
                  c("Race", "Armed", "threat_level" , "mental_illness" , "manner_of_death" , "gender", "age"),
                  selected="Race"),
      
      selectInput("v2", label="Group 2", 
                  c("NONE", "Race", "Armed", "threat_level" , "mental_illness" , "manner_of_death" , "gender", "age"),
                  selected="NONE"),
      h6("by John H. Bradford."),
      a("Data from Washing Post Github Repository", href="https://github.com/washingtonpost/data-police-shootings")
      ),

    mainPanel(
      tabsetPanel(
        tabPanel("Bar Graphs",
                 
      plotOutput("plot1")
        ),
    tabPanel("Map",
             plotOutput("map")   )  
      
      )
      
    )
  )
))