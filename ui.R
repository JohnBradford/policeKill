library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("People Killed by Police in the United States, 2015-2016"),

  sidebarLayout(
    
    sidebarPanel(
      selectInput("dataSource", label="Data Source", choices=c("Washington Post", "The Guardian"), selected="Washington Post"),
      selectInput("mode", label="Organization of Data", choices=c("Percent of Group", "Total Frequency"), selected="Percent of Group"),
      selectInput("v1", label="Group 1", 
                  c("Race", "Armed", "threat_level" , "mental_illness" , "manner_of_death" , "gender", "age"),
                  selected="Race"),
      selectInput("v2", label="Group 2", 
                  c("NONE", "Race", "Armed", "threat_level" , "mental_illness" , "manner_of_death" , "gender"),
                  selected="NONE"),
      selectInput("v3", label="Group 3",
                  c("NONE", "Race", "Armed", "threat_level" , "mental_illness" , "manner_of_death" , "gender"),
                  selected="NONE"),
      downloadButton('downloadGraph', 'Download Graph'),
      downloadButton('downloadData', 'Download Data'),
      h6("by John H. Bradford."),
      a("My Github Repo", href="https://github.com/JohnBradford/policeKill.git")
      ),

    mainPanel(
      tabsetPanel(
        tabPanel("Bar Graphs",
                 
      plotOutput("plot1"),
      verbatimTextOutput("click_info")
        )
    # tabPanel("Map",
    #          plotOutput("map")   )  
      
      )
      
    )
  )
))