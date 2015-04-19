
#  I do not want anybody to use this package for commercial use. By applying this function, I hope to prevent everyone from doing so. By including it in the final functions you are forced to give your email address when scraping the information from the newspapers. This is also being friendly to the newspaper server and may help prevent any legal copyright issues. 


handle_friendly <- function(email) {
  
  handle <-getCurlHandle(useragent = str_c(R.version$platform,
                                           R.version$version.string,
                                           sep=", "),
                         httpheader = c(from = email),
                         followlocation = TRUE,
                         cookiefile = "")
  return(handle)
}



#example: handle_friendly("tata@tata.com")
