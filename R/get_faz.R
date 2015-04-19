

# At first, you find here the final get_faz function. The process that let to this final function is explained below.

#get_faz(topic, from, to, area, email):
#-area can be speicified as 'area="TI"' so the query will only be for headlines, or you specify 'area="q"' and the query will be done in full text
#-there are search aids for specifing the topic research: 
#  - type 'topic="Merkel UND Obama"' for finding reports that include   
#Merkel AND Obama
#- type 'topic="Merkel ODER Obama"' for finding reports that include 
#Merkel OR Obama
#- type 'topic="Merkel NICHT Obama"' for finding reports that include 
#Merkel BUT NOT Obama 
#- type 'topic="Merk*"' for retrieving all articles that include a 
#word starting with Merk, such as Merkur, Merkel etc.
#- type 'topic="*Ei"' for retrieving all articles that include a 
#word ending with Ei, such as Osterei, Feierei etc.
#there is the see.freq(df, title) function which can be executed after the faz query has been done, it shows you how often the topic queried was mentioned during the deinfed months

get_faz <- function(topic, from, to, area, email) {
  
  all_links <- find_faz_links(topic, from, to, area)
  faz_df <- get_fazbasics(all_links, email)
  
  return(faz_df)
}

# This is an example how to handle when searching in full text:
#ostern1<-get_faz(topic="Ostern", from="01.04.2015", to="10.04.2015", area="q", email="i.bonenkamp@live.de")
# This is an example how to handle when searching only in headlines:
#ostern2<-get_faz(topic="Ostern", from="01.01.2014", to="10.04.2015", area="TI", email="i.bonenkamp@live.de")

# Before explaining the single steps that lead to the final function you find here a function for giving an example for further use of the information retrieved with the get_faz function
# It depicts how frequent your topic has been mentioned (per month)
# Unfortunately the frequency cannot be depicted as per day because unless you do not create a Faz online account and pay for the articles there is no way to access the real days of the articles

see.freq<- function(faz_df, title) {
  
  df_months <- as.data.frame(table(faz_df$months))
  colnames(df_months) <- c("Months", "Frequency")
  
   gg <- ggplot(df_months, aes(Months,Frequency))
   y  <- scale_y_continuous(breaks = seq(1,10,1))
   t  <- ggtitle(title)
  
  plot <- (gg + geom_point() + t + y)
  
  return(plot)
}



# Now the explanation for the final code above.

# 1. Here we accessed the FAZ online query form:

faz_modify <- function(topic, from, to, area) {
  
  faz_topic_url <- modify_url("http://fazarchiv.faz.net/", 
                              query=list(q = topic,
                                         search_in = area,
                                         timePeriod= NULL,
                                         DT_from = from,
                                         DT_to = to, 
                                         KO="FAZ.NET", 
                                         crxdefs=NULL, 
                                         NN=NULL, 
                                         CO=NULL, 
                                         CN=NULL, 
                                         BC=NULL, 
                                         submitSearch="Suchen", 
                                         sext="1", 
                                         maxHits=NULL, 
                                         sorting=NULL, 
                                         toggleFilter=NULL, 
                                         dosearch="new#hitlist"))
  
  return(faz_topic_url) 
}



# 2. Then we find out how many result pages there are and get their full url-code for acessing:

find_faz_links <- function(topic, from, to, area) {
  
  baseurl <- faz_modify(topic, from, to, area)
  url_parsed <- htmlParse(baseurl)
  nrs <- c(as.character(xpathSApply(url_parsed,
                                    "//*[@class='step']", 
                                    xmlValue)))
  
  if(length(nrs)>0) {
    
    l <- as.numeric(max(nrs))
    n <- l-1
    
    list <- paste("http://fazarchiv.faz.net/", 
                  "?KO=FAZ.NET",
                  "&DT_from=", from,
                  "&q=", topic,
                  "&search_in=", area,
                  "&DT_to=", to,
                  "&submitSearch=Suchen&dosearch=new%23hitlist&sext=1&offset=",
                  1:n, "0#hitlist",
                  sep="")
    
    all_links <- c(baseurl, list)
    
    return(all_links)  
  }
  return(baseurl)
}


# 3. Now that we have all links from all result pages we can retrieve the basic information needed for further processing/sorting etc. :



get_fazbasics <- function(all_links, email) {
  
  handle <- handle_friendly(email)
  list_faz <- getURL(all_links, curl=handle)
  list_parsed <- htmlParse(list_faz)
  
  title <- xpathSApply(list_parsed,
                       "//div[@class='globalFormRow clearfix']//a",
                       xmlValue)
  
  supertitle <- xpathSApply(list_parsed, 
                           "//div[@class='module13 clearfix']/h2[1]",
                            xmlValue)
  
  headlines_faz <- c(paste(supertitle, title, sep=": "))
  
  links_faz<-c(xpathSApply(list_parsed,
                          "//div[@class='globalFormRow clearfix']//a", 
                            xmlGetAttr, "href"))
  
  months_faz <- c(xpathSApply(list_parsed,
                              "//div[@class='innerModule1 clearfix']//li[1]", 
                              xmlValue))
  
  faz_df <- data.frame(cbind(headlines_faz, months_faz, links_faz))
  
  colnames(faz_df) <- c("headlines", "months", "links")
  rm(headlines_faz, months_faz, links_faz)
  
  return(faz_df)
}



# Now see the three functions to applied in one at the top.


