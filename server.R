library(highcharter)
library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(pacman)
library(XML)
library(dplyr)
library(stringr)
library(rvest)
library(audio)
library(sentimentr)
library(stringi)
amazon_scraper <- function(id,pages){
  library(tidyverse)
  library(rvest)
  i<-1
  review_final <- data.frame()
  while(i <= pages){
    url <- paste0("https://www.amazon.com/product-reviews/",id,"/ref=cm_cr_arp_d_paging_btm_next_2?pageNumber=",i)
    reviews <- url %>% 
      read_html() %>% 
      html_nodes("div.a-row.a-spacing-small.review-data") %>% 
      html_text()
    
    star <- url %>% 
      read_html() %>% 
      html_nodes(".a-icon-alt") %>% 
      html_text()
    average_rating <- star[1]
    star <- as.list(star)
    star <- star[4:13]
    star <- as.character(star)
    
    review_temp <- data.frame(ratings=star,Reviews=reviews)
    review_final <- rbind(review_final,review_temp)
    i <- i+1
  }
  return(review_final)
}

shinyServer(function(input, output,session) {

        withProgress({
        setProgress(message = "Loading Please Wait !! ")
        ## Loading Libraries
        pacman::p_load(XML, dplyr, stringr, rvest, audio)
         
        ## Remove all white space
        trim <- function (x) gsub("^\\s+|\\s+$", "", x)

        ## Enter the Product Code and Change the URL as necessary
        id = input$id
        pages= input$pages

        ############### Extraction of Reviews ###################
        
        ## Change Page length as required
        #pages <- 2

        ## Install and Load the libraries ("stringi" and "sentimentr") before this
        ## Again Change the URL as required
        
        reviews_all <- amazon_scraper(id,pages)
        reviews_all <- cSplit(reviews_all,"ratings"," ")
        reviews_all <- reviews_all[,c("Reviews","ratings_1")]
        names(reviews_all) <- c("Reviews","Ratings")
        reviews_all$Ratings <- as.numeric(reviews_all$Ratings)
          })
      
  
  output$table1 <- renderDataTable({
        
        input$Submit
        reviews_all 
        
      })
  output$hcontainer <- renderHighchart({      
          
          input$Submit
          tt= reviews_all
          tt1 = sqldf("select Ratings,count(Ratings) as count,count(Ratings) as helpful from tt group by Ratings")
          
          canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
          legendClickFunction <- JS("function(event) {Shiny.onInputChange('legendClicked', this.name);}")
          
          highchart() %>% 
            hc_chart(type = "column") %>%
            
            hc_title(text = "<b>Distribution of Ratings</b>",
                                   margin = 20, align = "center",
                                   style = list(color = "blue", useHTML = TRUE)) %>% 
            hc_xAxis(title=list(text = "Ratings"),categories = unique(tt1$Ratings)) %>% 
            hc_add_series(data = tt1$count,name = "Ratings")
              
        })      
        
      output$tablecust1 <- renderValueBox({
          
          input$Submit
          
         a =  isolate(round(mean(reviews_all$Ratings,0)))
          valueBox(
            value = format(a),
            subtitle = "Average Rating",
            icon = if (a >=3.5) icon("thumbs-up") else icon("thumbs-down"),
            color = if (a >= 3.5) "aqua" else "red"
          )
          
        })
        
        output$tablecust2 <- renderValueBox({
          input$Submit
          sent_agg <- isolate(with(reviews_all, sentiment_by(as.character(reviews_all$Reviews))))
          
          a = ifelse(mean(sent_agg$ave_sentiment)<0,"Negative",ifelse(mean(sent_agg$ave_sentiment)>0 & mean(sent_agg$ave_sentiment)<0.3,"Neutral","Positive"))
          
          valueBox(
            value = format(a),
            subtitle = "Average Sentiment",
            icon = if (a >= 0.1) icon("thumbs-up") else icon("thumbs-down"),
            color = if (a >= 0.1) "aqua" else "red")
          
        })
        
        getPage<-function() {
          
          input$Submit
          sent_agg <- isolate(with(reviews_all, sentiment_by(as.character(reviews_all$Reviews))))
          best_reviews <- isolate(slice(r(), top_n(sent_agg, 3, ave_sentiment)$element_id))
          
          with(best_reviews, sentiment_by(comments)) %>% highlight()
          return(includeHTML("polarity.html"))
        }
        
        getPage1<-function() {
          input$Submit
          sent_agg <- isolate(with(reviews_all, sentiment_by(as.character(reviews_all$Reviews))))
          worst_reviews <- isolate(slice(r(), top_n(sent_agg, 3, -ave_sentiment)$element_id))
          with(worst_reviews, sentiment_by(comments)) %>% highlight() 
          return(includeHTML("polarity.html"))
        }
        
        
        
        output$inc<-renderUI({getPage()})
        output$inc1<-renderUI({getPage1()})
      
    terms <- reactive({
    # Change when the "update" button is pressed...
    input$Submit
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(reviews_all$Reviews)
      })
    })
  })
 
  wordcloud_rep  <- repeatable(wordcloud)
  
  
  output$cloud1  = renderPlot({
    v <-  terms()
    wordcloud_rep(words = v$word, freq = v$freq, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
    
    })
 
  })
  
  

