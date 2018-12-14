
amazon_scraper <- function(id,pages){
  library(tidyverse)
  library(rvest)
  i<-1
  review_final <- data.frame()
  while(i <= 4){
    url <- "https://www.amazon.com/product-reviews/B076FS857X/ref=cm_cr_arp_d_paging_btm_next_2?pageNumber=2"
    remDr$navigate(url)
    
    #webElem_state <- remDr$findElement(using = "css", "div.a-row.a-spacing-small.review-data")
    
    page_source <- remDr$getPageSource()
    reviews <-url 
      read_html(page_source[[1]]) %>%     # read table 
      html_nodes("div.a-row.a-spacing-small.review-data")%>% 
      html_text()
      
      star <- read_html(page_source[[1]]) %>% 
        html_nodes(".a-icon-alt") %>% 
        html_text()
      
      average_rating <- star[1]
      star <- as.list(star)
      star <- star[4:13]
      star <- as.character(star)
      
      review_temp <- data.frame(stars=star,comments=reviews)
      review_final <- rbind(review_final,review_temp)
      i <- i+1
     }
    return(review_final)
}
      

reviews <- amazon_scraper('B0758XXCSC',40)

