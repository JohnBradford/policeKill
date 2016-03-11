library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(ggmap)


w <- read.csv("wShort.csv")
w$Race <- factor(w$Race, 
                    levels=c("White", "Black" , "Hispanic" , "Other"))

 w$Armed <- as.character(w$Armed)
 w$Armed[which(w$Armed == "Armed/Undetermined")] <- "Armed"

 w$state <- as.character(w$state)
 wapo_AK <- subset(w, state != "alaska" & state != "hawaii")
 wapo_AK <- wapo_AK[,-14]
 
wState <- wapo_AK %>% group_by(state) %>% 
  summarize(Killed=n(), armed=sum(Armed == "Armed", na.rm=T),
            unarmed=sum(Armed=="Unarmed", na.rm=T))
wState$region <- wState$state



myPalette <- colorRampPalette(rev(brewer.pal(5, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0,160))
t <-  theme(text=element_text(size=14),
            plot.title = element_text(size=18), axis.title.x= element_text(size=12)) 


shinyServer(function(input, output) {

  datasetInput <-  reactive({
    
    col1 <- match(input$v1, names(w))
    col2 <- match(input$v2, names(w))
    
    
    VAR1 <-  w[,col1] 
    VAR2 <- w[,col1]
   
     df <- cbind.data.frame(VAR1, VAR2, stringsAsFactors = F)
    
    if(input$v2 != "NONE") {
      VAR2 <-  w[,col2]
      df <- cbind.data.frame(VAR1, VAR2, stringsAsFactors = F)

    }
    
  
      if(input$mode == "Percent of Group") {
        if(input$v2 == "NONE") {
          
          
        }  else {

          df <- df %>% group_by(VAR1, VAR2) %>% 
            summarise (n = n()) %>% 
            mutate(percent = 100* n / sum(n))
        
      } }
  
    df  
   

    })
    


  
  output$plot1 <- renderPlot({
    
    df <- datasetInput()
    
    if(input$v1 == "age"){
      if (input$v2 != "NONE") {
      ggplot(data=df) +
        geom_boxplot(aes(y=VAR1, x=VAR2, fill=VAR2), alpha=0.6) +
        theme_gdocs() + ylab(input$v1) + xlab(input$v2) +
          scale_color_colorblind() +
          theme(legend.title=element_blank()) 
      } else {
        ggplot(data=df) +
          geom_histogram(bins=20, aes(x=VAR1), alpha=0.6) +
          theme_gdocs() + xlab(input$v1) +
          scale_color_colorblind() +
          theme(legend.title=element_blank())
      
      }
    } else {

    if(input$mode == "Percent of Group") {
       if (input$v2 != "NONE") {
           thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR2)) +
             geom_bar(aes(y=percent), stat="identity", position="dodge", alpha=0.5) +
             theme_gdocs() + ylab("Percent") + xlab(input$v1) +
             geom_text(data=df, aes(x=VAR1, y=percent, label=round(percent),2), 
                       position = position_dodge(width=1)) +
             theme(legend.title=element_blank())
           thePlot
           
       } else {
         thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR1)) +
           geom_bar(aes(y = 100*(..count..)/sum(..count..)), stat="count", position="dodge", alpha=0.5) +
           theme_gdocs() + ylab("Percent")  + xlab(input$v1) +
           geom_text(stat='count', aes(y = 100*(..count..)/sum(..count..),
                                       label= round(100*(..count..)/sum(..count..), 2)),
                      position = position_dodge(width=1)) +
           theme(legend.title=element_blank())
         thePlot
           
       }
      } else {
        if (input$v2 != "NONE") {
            thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR2)) +
              geom_bar(aes(),position="dodge", alpha=0.5) +
              theme_gdocs() + ylab("Frequency") + xlab(input$v1) +
              geom_text(stat='count', aes(label=..count..),
                        position = position_dodge(width=1)) +
              theme(legend.title=element_blank())
            thePlot
        } else {
          thePlot <- ggplot(data=df, aes(x=VAR1, fill=VAR1)) +
            geom_bar(position="dodge", alpha=0.5) +
            theme_gdocs() + ylab("Frequency") + xlab(input$v1) +
            geom_text(stat='count',aes(label=..count..), 
                      position = position_dodge(width=1)) +
            theme(legend.title=element_blank())
          thePlot
      }
      }
    }

  })
 
output$map <- renderPlot({
  data(state.map)
  ggplot() +
    geom_map(data=wState, map=state.map, aes(map_id=region, fill=Killed), 
             alpha=0.9) + 
    geom_point(data=wapo_AK, aes(x=lon, y=lat, color=Armed),size=1) +
    scale_color_colorblind() +  
    theme_gdocs() +
     sc + t 
})
  

  
})
