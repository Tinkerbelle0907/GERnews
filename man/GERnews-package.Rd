\name{GERnews-package}
\alias{GERnews-package}
\alias{GERnews}
\docType{package}
\title{GERnews simplifies online media research
}
\description{
GERnews simplifies online media research. It allows to access the search engines of the German online newspapers Faz, Spiegel and Zeit in RStudio and thus enables the user to scrape headlines, dates and links to articles from the online archives. 
}
\details{
\tabular{ll}{
Package: \tab GERnews\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-23\cr
License: \tab No commercial use\cr
}
~~
get_spiegel(topic, from, to, area, email)
get_faz(topic, from, to, area, email)
see.freq(faz_df, title) 
get_zeit(topic, from, to, email)
~~
}
\author{
Isabelle Bonenkamp
Maintainer:  <i.bonenkamp@live.de>
~~ Isabelle Bonenkamp ~~
}
\references{
~~ Friedrike Preu (2014): Alle nur parteiisch? Berichterstattung im Sommer 2014 am Beispiel von SPON und Zeit Online. Universität Konstanz. Blogartikel.

Munzert, Simon, Christian Rubba, Peter Meißner, und Dominic Nyhuis (2014): Automated Data Collection with R. A Practical Guide to Web Scraping and Text Mining for the Social Sciences. Hoboken, NJ: John Wiley & Sons. Manuskript.

Hadley Wickham (2015): R Packages. O'Reilly Media. URL http://r-pkgs.had.co.nz/~~
}
~~ Optionally other standard keywords, one per line, from file ~~
~~ KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ https://github.com/Tinkerbelle0907. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~  flucht1_df <- get_spiegel(topic="Flucht", from="01.01.15", to="08.04.15", area="kopftext", email="Your@email.com")

flucht1_df <- get_spiegel(topic="Flucht", from="01.01.15", to="08.04.15", area="", email="Your@email.com")

flüchtling_df <- get_zeit(topic="Flüchtling", from="01.01.2015", to="01.02.2015", email= "Your@email.com")

ostern1_df <- get_faz(topic="Ostern", from="01.01.2013", to="10.04.2015", area="TI", email="Your@email.com")

ostern2_df <- get_faz(topic="Ostern", from="01.01.2013", to="10.04.2015", area="q", email="Your@email.com")
~~
}
