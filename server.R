library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(ggmap)

#Washington Post
w <- read.csv("https://cdn.rawgit.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
w$date <- as.Date(w$date)
# filter by unarmed vs other, create new categorical variable and recombine
w_unarmed <- w %>%
  filter(armed == "unarmed") %>%
  mutate(unarmed = "Unarmed")
w_other <- w %>%
  filter(armed != "unarmed" | is.na(armed)) %>%
  mutate(unarmed = "Armed/Undetermined")
w <- bind_rows(w_unarmed,w_other)

# collapse the race categories into black, white, hispanic, and other
w_main_races <- w %>%
  filter(race == "B" | race == "W" | race == "H") %>%
  mutate(Race = race)
w_other_races <- anti_join(w, w_main_races, by="id") %>%
  mutate(Race = "Other")
w <- bind_rows(w_main_races,w_other_races) %>% as.data.frame()

w$Race[which(w$Race=="W")] <- "White"
w$Race[which(w$Race=="B")] <- "Black"
w$Race[which(w$Race=="H")] <- "Hispanic"

##Reducing, renaming
names(w)[names(w)=="signs_of_mental_illness"] <- "mental_illness"
names(w)[names(w)=="unarmed" ] <- "Armed"
source("stateListFunction.R")
w$state <- stateFromLower(w$state)
w$Race <- factor(w$Race, 
                 levels=c("White", "Black" , "Hispanic" , "Other"))
w$Armed <- as.character(w$Armed)
w$Armed[which(w$Armed == "Armed/Undetermined")] <- "Armed"
w$state <- as.character(w$state)


##Guardian
G <- read.csv("https://cdn.rawgit.com/flother/thecounted/master/data/the-counted-2015.csv")
G2 <- read.csv("https://cdn.rawgit.com/flother/thecounted/master/data/the-counted-2016.csv")
G <- bind_rows(G, G2)
names(G)[names(G)=="raceethnicity"] <- "race"
#names(G)[names(G)=="armed" ] <- "Armed"
names(G)[names(G)=="classification" ] <- "manner_of_death"
G$m <- NA
for(i in 1:nrow(G)) {
  G$m[i] <- switch(as.character(G$month[i]), 
                   "January" = 1, "February" =2, "March"  = 3, "April" =4, 
                   "May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9, 
                   "October" = 10, "November" = 11, "December" = 12)}
G$date <- as.Date(paste(G$year, G$m, G$day, sep="-"))

# collapse the race categories into black, white, hispanic, and other
g_main_races <- G %>%
  filter(race == "Black" | race == "White" | race == "Hispanic/Latino") %>%
  mutate(Race = race)
g_other_races <- anti_join(G, g_main_races, by="uid") %>%
  mutate(Race = "Other")
G <- bind_rows(g_main_races,g_other_races)
G$Race[which(G$Race =="Hispanic/Latino")] <- "Hispanic"
G$age <- as.numeric(G$age)
G$Race <- factor(G$Race, 
                 levels=c("White", "Black" , "Hispanic" , "Other"))

# filter by unarmed vs armed/unknown/disputed, create new categorical variable and recombine
g_unarmed <- G %>%
  filter(armed=="No") %>%
  mutate(Armed="Unarmed")
g_other <- G %>%
  filter(armed != "No") %>%
  mutate(Armed="Other")
G <- bind_rows(g_unarmed,g_other) %>% as.data.frame()

myPalette <- colorRampPalette(rev(brewer.pal(5, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0,160))
t <-  theme(text=element_text(size=14),
            plot.title = element_text(size=18), axis.title.x= element_text(size=12)) 


shinyServer(function(input, output) {


  
  datasetInput <-  reactive({
  
  if(input$dataSource=="Washington Post"){df <- w}    
  if(input$dataSource=="The Guardian"){df <- G}

    
    col1 <- match(input$v1, names(df))
    col2 <- match(input$v2, names(df))
    col3 <- match(input$v3, names(df))
    df$VAR1 <- df[,col1]
    df$VAR2 <- df[,col1]
    df$VAR3 <- df[,col1]
    

    if(input$v2 != "NONE") {
      df$VAR2 <- df[,col2]
      if(input$v3 != "NONE") {
        df$VAR3 <- df[,col3]
      } 
    } 
    
      if(input$mode == "Percent of Group") {
        if(input$v2 != "NONE") {
          if(input$v3 != "NONE") {
            df <- df %>% group_by(VAR1, VAR2, VAR3) %>% 
              summarise (n = n()) %>% 
              mutate(percent = 100* n / sum(n))
          } else {
            df <- df %>% group_by(VAR1, VAR2) %>% 
              summarise (n = n()) %>% 
              mutate(percent = 100* n / sum(n))
          }
        } }
  
    df <- data.frame(df)
    df
   

    })
    
  plotInput <- function(){
    
    df <- datasetInput()
    
    lg <- t + theme_gdocs() 
    
    if(input$v1 == "age"){
      if (input$v2 != "NONE") {
        thePlot <- ggplot(data=df) +
          geom_boxplot(aes(y=VAR1, x=VAR2, fill=VAR2), alpha=0.6) +
          ylab(input$v1) + xlab(input$v2) +
          scale_color_colorblind() + lg + theme(legend.title=element_blank())
          
        
        if(input$v3 != "NONE") {
          thePlot <- thePlot + facet_wrap(~VAR3) 
          thePlot
        } else {
          thePlot
        }
        
      } else {
        thePlot <- ggplot(data=df) +
          geom_histogram(bins=20, aes(x=VAR1), alpha=0.6) +
          + xlab(input$v1) +
          scale_color_colorblind() + lg + theme(legend.title=element_blank())
        thePlot
      }
    } else {
      
      if(input$mode == "Percent of Group") {
        
        gp <- geom_bar(aes(y=percent), stat="identity", position="dodge", alpha=0.5) 
        gt1 <- geom_text(data=df, aes(x=VAR1, y=percent, label=round(percent),2), 
                                                                     position = position_dodge(width=1)) 
        
        if (input$v2 != "NONE") {
          if(input$v3 != "NONE") {
            thePlot <-  ggplot(data=df, aes(x=VAR1, fill=VAR3)) + 
              ylab("Percent") + xlab(input$v1) +
              gp + gt1 + facet_wrap(~VAR2) + lg  + theme(legend.title=element_blank())
            thePlot
          } else {
            thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR2)) + 
              ylab("Percent") + xlab(input$v1) + 
              gp + gt1 + lg + theme(legend.title=element_blank())
            thePlot
          }
        } else {
          thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR1)) +
            geom_bar(aes(y = 100*(..count..)/sum(..count..)), stat="count", position="dodge", alpha=0.5) +
            ylab("Percent") + xlab(input$v1) +
            geom_text(stat='count', aes(y = 100*(..count..)/sum(..count..),
                                        label= round(100*(..count..)/sum(..count..), 2)),
                      position = position_dodge(width=1)) + 
            lg + theme(legend.title=element_blank())
          thePlot
          
        }
      } else {
        
        gf <-  geom_bar(aes(),position="dodge", alpha=0.5) 
        gt2 <-geom_text(stat='count', aes(label=..count..),
                                     position = position_dodge(width=1))           
     
        
        if (input$v2 != "NONE") {
          thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR2)) +
            ylab("Frequency") + xlab(input$v1) +
            gf + gt2 + lg + theme(legend.title=element_blank())
          
          if(input$v3 != "NONE") {
            thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR3)) +
              ylab("Frequency") + xlab(input$v1) +
            gf + gt2 + lg + facet_wrap(~VAR2) + theme(legend.title=element_blank())
            thePlot
          } else {
            thePlot
          }
        } else {
          thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR1)) +
            ylab("Frequency") + xlab(input$v1) +
            gf + gt2 + lg + theme(legend.title=element_blank())
          thePlot
        }
      }
    }
    
    
  }

  
  output$plot1 <- renderPlot({
    
  plotInput()

  })
 
  output$click_info <- renderPrint({
    
    if(input$dataSource=="Washington Post"){df <- w}    
    if(input$dataSource=="The Guardian"){df <- G}
    
     paste("Date Range: ", min(df$date), " to ", max(df$date))
    
  })


output$downloadGraph <- downloadHandler(
    filename = function() {
      paste('PoliceKill_',  Sys.Date(), '.png', sep='')
    },
  content = function(file) {
    png(file)
    plot(plotInput())
    dev.off()
  },
  contentType = "image/png"
  ) 
  
output$downloadData <- downloadHandler(
  filename = function() {
    paste('PoliceKill_', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    write.csv(w, file)
  }
) 
  
})
