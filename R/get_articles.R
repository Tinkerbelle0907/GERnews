#This is the download-function for the articles that have been retrieved by the get_spiegel, get_zeit and get_faz functions. With help from the function below, it creates a folder and pastes the queried articles in the folder.
#ATTENTION! This function will not help you with faz-articles as you cannot access the articles in the online faz-archive without paying a fee. Thus you can only have the headlines from those articles.

get_articles <- function(links, folder) {
  
  l_ply(links, download.art(),
        folder = folder,
        handle = handle)
  
  art_files<-list.files(folder)
  
  return(art_files)
  
}

# example: get_articles(zeit_df$links, folder="Ostern")


#This is the function behind the final function above. It is copied from Munzert, Simon, Christian Rubba, Peter Meißner, und Dominic Nyhuis (2014), but renamed. 


download.art <- function(pageurl, folder, handle) {
  
  dir.create(folder, showWarnings = FALSE)
  
  page_name <- str_c(str_extract(pageurl, "/P.+"), ".html")
  
  if (page_name == "NA.html") { page_name <- "/base.html" }
  if (!file.exists(str_c(folder, "/", page_name))) {
    content <- try(getURL(pageurl, curl = handle))
    write(content, str_c(folder, "/", page_name))
    Sys.sleep(1)
  } 
}

# However, once you retrieved the links for the articles you can also download in any other way. But never forget being friendly!


 
