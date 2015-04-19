

# You find here the final get_spiegel function. The process that let to this final function is explained below.

#get_spiegel(topic, from, to, area, email):
#-area can be left empty (area="") for searching in full text or be defined as area="kopftext" for only searching in headlines
#-there are search aids for specifing the topic research: 
#  - type 'topic="Merkel Obama"' for finding reports that include Merkel AND Obama
#- type 'topic="Merkel, Obama"' for finding reports that include 
#Merkel OR Obama
#- type 'topic="Merkel -Obama"' for finding reports that include 
#Merkel BUT NOT Obama 
#- type 'topic="Merk*"' for retrieving all articles that include a 
#word starting with Merk, such as Merkur, Merkel etc.


get_spiegel <- function(topic, from, to, area, email) {
  
  
  handle <- handle_friendly(email)
  all_links <- find_spiegel_links(topic, from, to, area)
  list <- getURL(all_links, curl = handle)
  list_parsed <- htmlParse(list)
  basics <- get_spiegelbasics(list_parsed)
  
  return(basics)
}

# This is an example how to use the function: 
#get_spiegel(topic="Ostern", from="01.01.15", to="08.04.15", area="", email="i.bonenkamp@live.de")


# Now the explanation for the final code above.

# 1. Here we accessed the Spiegel online query form:


spiegel_modify <- function(topic, from, to, area) {
  
  spiegel_topic_url <- modify_url("http://www.spiegel.de/suche/index.html",
                                  query=list(suchbegriff  =topic,
                                             quellenGroup ="SPOX",
                                             quellenGroup ="SP",
                                             suchbereich  =area,
                                             fromDate     =from,
                                             toDate       =to))
  
  return(spiegel_topic_url)
}

# 2. Then we find out how many result pages there are. This function was created by Friedrike Preu who was so kind to let me copy it as she did it so perfectly. Thank you very much!


find_lastp<-function(url) {
  
  url <- getURL(url)
  url_p <- htmlParse(url)
  links <- xpathSApply(url_p, 
                       "//div[@class='search-paginator'][1]/ul/li/a",
                       xmlGetAttr,"href")
  
  full_links <- paste("www.spiegel.de",links,sep="")
  
  nrs <- as.numeric(str_replace_all(links,
                                    ".+pageNumber=(\\d{1,})"  
                                    ,"\\1"))
  
  len<-length(nrs)
  
  
  if(nrs[len]<nrs[len-1]){
    find_lastp(full_links[len-1])    
  }
  else{
    if(nrs[len]==nrs[len-1]){
      np<-nrs[len-1]
      return(np)
    }
    if(nrs[len]>nrs[len-1]){
      np<-nrs[len]
      return(np)
    }
  }
  
}


# 3. Now we get their full url-code for acessing:

find_spiegel_links <- function(topic, from, to, area) {
  
  
  baseurl <- spiegel_modify(topic, from, to, area)
  np <- find_lastp(baseurl)
  all_links <- paste(baseurl, "&pageNumber=", 1:np, sep="")
  
  return(all_links)
  
}

# 3. Now that we have all links from all result pages we can retrieve the basic information needed for further processing/sorting etc. :

get_spiegelbasics <- function(list_parsed){
  
  headlines_spiegel <- c(paste(xpathSApply(list_parsed,
                                           "//div[@class='search-teaser']/descendant::span[@class='headline-intro']",
                                           xmlValue), 
                               xpathSApply(list_parsed,
                                           "//div[@class='search-teaser']/descendant::span[@class='headline']",
                                           xmlValue), sep=" " ))
  
  source.date <- xpathSApply(list_parsed,
                             "//div[@class='search-teaser']/descendant::span[@class='source-date']",xmlValue)
  
  date_spiegel<-c(str_extract(source.date, 
                              "[[:digit:]][[:digit:]].[[:digit:]].{2}[[:digit:]][[:digit:]][[:digit:]][[:digit:]]"))
  
  links_spiegel<-c(xpathSApply(list_parsed,
                               "//p[@class='article-intro']/descendant::a",xmlGetAttr,"href"))
  
  spiegel_df <- data.frame(cbind(headlines_spiegel,
                         date_spiegel, links_spiegel))
  
  colnames(spiegel_df) <- c("headlines", "dates", "links")
  rm(headlines_spiegel, date_spiegel, links_spiegel)
  
  return(spiegel_df)
}


# Now see the three functions to applied in one at the top.




