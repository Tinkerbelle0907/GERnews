
#This is the final function for retrieving information from Zeit Online
#This is an example how to use it:
#get_zeit(topic="Flüchtling", from="01.01.2013", to="01.04.2015", email= "Your@email.com")

get_zeit <- function(topic, from, to, email){
  
  
  baseurl <-    zeit_modify(topic, from, to)
  all_links <-  find_zeit_links(baseurl)
  basic_info <- get_zeitbasics(all_links, email)
  
  return(basic_info)
}


#The final function is the shortened version of a longer procedure. It contains four other functions, from which three are explained here:

#(find the explantion for the handle_friendly() function in the handle_friendly document)

# 1. We have to access the query form of the Zeit Online Archive. This is done with help from the httr package:


zeit_modify <- function(topic, from, to) {
  
  zeit_topic_url <- modify_url("http://www.zeit.de/suche/index",                         
                          query=list(
                                     q = topic,
                                     from = from,
                                     to = to))
  
  return(zeit_topic_url)
}



# 2. Now that we have the first result page for our query, we need to find out how many result pages there are and pick their url-code:

find_zeit_links <- function(baseurl){
  
  url_parsed <- htmlParse(baseurl)
  
  nr1 <-        as.character(xpathSApply(url_parsed, 
                                 "//*[@class='numbers']", 
                                  xmlValue))
  
  nr2 <-        str_extract(nr1, "/[[:digit:]]*[[:digit:]]")
  nr3 <-        str_extract_all(nr2, 
                                 "[[:digit:]]*[[:digit:]]")
  
  n <-          as.numeric(nr3)
  
  all_links <-  paste(baseurl, "&p=", 1:n, sep="")
  
  return(all_links)
}



# 3. As we now have all links for all result pages we can start scraping the basic information of our query. So with help from the XML package we get the headlines, the dates and the links leading to the complete articles:


get_zeitbasics <- function(all_links, email) {
  
  handle <-getCurlHandle(useragent = str_c(R.version$platform,
                                           R.version$version.string,
                                           sep=", "),
                         httpheader = c(from = email),
                         followlocation = TRUE,
                         cookiefile = "")
  
  list <- getURL(all_links, curl=handle)
  list_parsed <- htmlParse(list)
  
  headlines_zeit <- c(xpathSApply(list_parsed,
                                  "//a[@rel='bookmark']",
                                  xmlGetAttr,"title"))
  
  links_zeit <- c(xpathSApply(list_parsed,
                              "//a[@rel='bookmark']",
                              xmlGetAttr,"href"))
  
  day <- as.character(xpathSApply(list_parsed,
                                  "//*[@id='main']//div/
                                  descendant::p/text()[1]",
                                  xmlValue ))
  
  date_zeit <- c(str_extract_all(day, 
                                 "[[:digit:]]{2}[[:punct:]][[:digit:]]{2}[[:punct:]]20[[:digit:]]{2}"))
  
  zeit_df <- data.frame(cbind(headlines_zeit, date_zeit, links_zeit))
  
  colnames(zeit_df) <- c("headlines", "dates", "links")
  rm(headlines_zeit, date_zeit, links_zeit)
  
  return(zeit_df)

}

# Now go back to the top. You will find all three functions included in one. 
