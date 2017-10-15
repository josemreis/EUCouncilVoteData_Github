# Pulling General Data on Votes at the European Council (2006-2017)
José M. Reis
13/10/2017

## Disclaimer

The data is still in a preliminary stage. Before using it please contact me (jose.reis@ile-graduateschool.de).

## Pulling the data

In this session we will extract the dates and title of all public votes at the European Council website. One could've used the [European Union API] (http://api.epdb.eu/), however it covers less years.

We start by loading some relevant packages. "Rvest" will be used to parse and extract information from the page's html code. "RCurl" will be used to manage the HTTP requests. We will mostly resort to RCurl::getURL() for that. Tidyverse packages, namely "dplyr", "stringr"", and "lubridate" will mostly be used for shapping up the extracted content.

```{r}
# packages
library(lubridate)
require(tidyverse)
require(stringr)
require(RCurl)
require(rvest)
require(XML)

options(stringsAsFactors = FALSE)
```


### Start

```{r}
############################################################################################################

# author: J.M.Reis

# Purpose: get and clean the title of the voted act and the voting date from the yearly pages of published voting acts (e.g. )

# date:
(dateOfAccess <- Sys.Date())
#[1] "2017-10-13
#
###########################################################################################################
```


### Extraction strategy

The data on title of public vote and its date can be found in the yearly pages. There is one master page for each year, and there we can find a find a variable number of pages (depending on the number of results). We will have to scrape the number of pages, and then loop around them exploiting the static URL of the website (which discrimiantes not only year but also page).

We start by setting the static URLs.

```{r}
## The static URL is always the same
staticURL1 <- "http://www.consilium.europa.eu/register/en/content/out/?PUB_DOC=%3E0&DOC_SUBJECT=VOTE&ORDERBY=DOC_DATE+DESC&DOC_LANCD=EN&DOC_YEAR="# After year, we put the relevant year yyyy

staticURL2 <- "&i=VT&ROWSPP=25&typ=SET&NRROWS=500&RESULTSET="# after= we put the page we are currently scraping it always goes from 1:lastpage
```

Then, we can already define the first components of the first dynamic part of the URL, a year-vector.

```{r}
 #the dynURL for years will be a numerical vector 
dynURL_year <- c(2006:2017)

```

For the second dynUrl, the page number, we will have to create a function that goes to the main page of the year, extracts the value of the node in the postion of the last page and uses that number to set the parameters for the page loop. The following function should do.

```{r}
# for the dynURL_page we need first to create a function which tells us the numerical value of the last page.
findLastPage <- function(yyyy=as.numeric()) {
  # selector for the last page
  cssLastPage <- ".last-page a"
  
  staticURL1 <- "http://www.consilium.europa.eu/register/en/content/out/?PUB_DOC=%3E0&DOC_SUBJECT=VOTE&ORDERBY=DOC_DATE+DESC&DOC_LANCD=EN&DOC_YEAR="# After year, we put the relevant year yyyy
  staticURL2 <- "&i=VT&ROWSPP=25&typ=SET&NRROWS=500&RESULTSET=1"#initial page
  
  lastPage <- getURL(paste0(staticURL1,yyyy, staticURL2))%>%
    read_html()%>%
    html_nodes(cssLastPage)%>%
    html_text()%>%
    as.numeric()
  
  return(lastPage)
  
}
```

### The extraction function

This function takes a year, as numeric, as input goes to the web page with the public vote data from the council for taht year, and loops across all the existant pages extracting title and date, cleans them up, and returns a dataframe.

```{r}
conciliumVoteData <- function(yyyy=numeric()) {

### setting some things up for the extraction
  ## The static URL
  staticURL1 <- "http://www.consilium.europa.eu/register/en/content/out/?PUB_DOC=%3E0&DOC_SUBJECT=VOTE&ORDERBY=DOC_DATE+DESC&DOC_LANCD=EN&DOC_YEAR="# After year, we put the relevant year yyyy
  staticURL2 <- "&i=VT&ROWSPP=25&typ=SET&NRROWS=500&RESULTSET="# after= we put the page we are currently scraping it always goes from 1:lastpage
  dynURL_year <- yyyy
  
  # function for finding the last page for the dynURL_page
  lastPage_fun <- function(year) {
    # selector for the last page
    cssLastPage <- ".last-page a"
    staticURL1 <- "http://www.consilium.europa.eu/register/en/content/out/?PUB_DOC=%3E0&DOC_SUBJECT=VOTE&ORDERBY=DOC_DATE+DESC&DOC_LANCD=EN&DOC_YEAR="# After year, we put the relevant year yyyy
    staticURL2 <- "&i=VT&ROWSPP=25&typ=SET&NRROWS=500&RESULTSET=1"#initial page
    lastPage <- getURL(paste0(staticURL1,year, staticURL2))%>%
      read_html()%>%
      html_nodes(cssLastPage)%>%
      html_text()%>%
      as.numeric()
  }
  
  lastPage <- lastPage_fun(year = yyyy)
  dynURL_page <- c(1:lastPage)
  
  ## set the css selectors
  cssTitle <- "td.no-border a"
  cssDate <- "td:nth-child(3) div"
  
  ## containers
  containerDate <- list()
  containerTitle <- list()
  
  ### ** Extraction **
  
  for (i in 1:length(dynURL_page)) {
    # get date
  containerDate[[i]] <- getURL(paste0(staticURL1,yyyy,staticURL2, dynURL_page[i]))%>%
    read_html()%>%
    html_nodes(cssDate)%>%
    html_text()%>%
    str_trim()
  #emoty strings
  containerDate[which(nchar(containerDate)<1)] <- NULL
  
  # get title
  containerTitle[[i]]<- getURL(paste0(staticURL1,dynURL_year,staticURL2, dynURL_page[i]))%>%
    read_html()%>%
    html_nodes(cssTitle)%>%
    html_text()%>%
    str_trim()
  
  #empty strings
  containerTitle[which(nchar(containerTitle)<1)] <- NULL
  
  }
  
  ## put together a dataframe with the results
  # create an empty tible with two variables. Date and title. This will be the returned dataframe after the extraction
  
  if( length(unlist(containerDate)) == length(unlist(containerTitle))) {
  
  voteData_df <- tibble(date=unlist(containerDate), title=unlist(containerTitle))
  
  #remove the empty strings
  voteData_df[voteData_df$date=="" & voteData_df$title=="",] <- NA
  voteData_df <- voteData_df[complete.cases(voteData_df[,]),]
  
  ## dateOfAccess
  voteData_df$dateOfAccess <- c(rep(Sys.Date(), nrow(voteData_df[,1])))
  
  return(voteData_df)
  
  } else {
    ## error in extraction in the date container
    x <- unlist(containerDate)
    ## remove the bad encoding "javascript:try{document.getElementById('IMG_AREA_\r\n\t\t\t\t\t\t72\r\n\t\t\t\t\t\t').style.cursor='pointer';}catch(exception){alert(exception);};"
    x <- x[!nchar(x) > 20]
    
    ## create the data frame now...
    voteData_df2 <- tibble(date=x, title=unlist(containerTitle))
    
    #remove the empty strings
    voteData_df2[voteData_df2$date=="" & voteData_df2$title=="",] <- NA
    voteData_df2 <- voteData_df2[complete.cases(voteData_df2[,]),]
    
    ## dateOfAccess
    voteData_df2$dateOfAccess <- c(rep(Sys.Date(), nrow(voteData_df2[,1])))
    containerDate <<- containerDate
    containerTitle <<- containerTitle
    return(voteData_df2)
  }
}  
```

### Generate voting data data frames for each year

Now we just have to call the function on each of the available years.

```{r}
### councils votes from 2006
publicVotes_2006 <- conciliumVoteData(yyyy = 2006)

save(publicVotes_2006, 
     file="data/publicVotes_2006.RData")

### councils votes from 2007
publicVotes_2007 <- conciliumVoteData(yyyy = 2007)

save(publicVotes_2007, 
     file="data/publicVotes_2007.RData")

### councils votes from 2008
publicVotes_2008 <- conciliumVoteData(yyyy = 2008)

save(publicVotes_2008, 
     file="data/publicVotes_2008.RData")

### councils votes from 2009
publicVotes_2009 <-conciliumVoteData(yyyy = 2009)
save(publicVotes_2009, 
     file="data/publicVotes_2009.RData")

### council's votes from 2010

publicVotes_2010 <- conciliumVoteData(yyyy = 2010)

save(publicVotes_2010, 
     file="data/publicVotes_2010.RData")

### council's votes from 2011

publicVotes_2011 <- conciliumVoteData(yyyy = 2011)

save(publicVotes_2011, 
     file="data/publicVotes_2011.RData")

### council's votes from 2012

publicVotes_2012 <- conciliumVoteData(yyyy = 2012)

save(publicVotes_2012, 
     file="data/publicVotes_2012.RData")

### council's votes from 2013

publicVotes_2013 <- conciliumVoteData(yyyy = 2013)

save(publicVotes_2013, 
     file="data/publicVotes_2013.RData")

### council's votes from 2014

publicVotes_2014 <- conciliumVoteData(yyyy = 2014)

save(publicVotes_2014, 
     file="data/publicVotes_2014.RData")

### council's votes from 2015

publicVotes_2015 <- conciliumVoteData(yyyy = 2015)

save(publicVotes_2015, 
     file="data/publicVotes_2015.RData")

### council's votes from 2016

publicVotes_2016 <- conciliumVoteData(yyyy = 2016)

save(publicVotes_2016, 
     file="data/publicVotes_2016.RData")

### council's votes from 2017

publicVotes_2017 <- conciliumVoteData(yyyy = 2017)

save(publicVotes_2017, 
     file="data/publicVotes_2017.RData")
```

### Merge the data frames and export them

Finally, we merge all the yearly dataframes into one and export them.

```{r}
#### All in one ----------------------------------------------------------------------------------------
merged_publicVote<- Reduce(function(x, y) merge(x, y, all=TRUE), list(publicVotes_2006, publicVotes_2007, publicVotes_2008, publicVotes_2009, publicVotes_2010, publicVotes_2011, publicVotes_2012, publicVotes_2013, publicVotes_2014, publicVotes_2015, publicVotes_2016, publicVotes_2017))

## convert date into date class
merged_publicVote$date <- dmy(merged_publicVote$date)

## arrange by date
merged_publicVote <- merged_publicVote%>%
  arrange(date)

#### Export the data frame------------------------------------------------------------------------------

councilPubVote_data <- merged_publicVote 

save(councilPubVote_data,
     file="data/councilPubVote_data.RData")
write.csv(councilPubVote_data,
          file="data/councilPubVote_data.csv")

################################# END ############################################
```
