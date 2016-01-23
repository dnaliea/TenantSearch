require(RCurl)
require(rjson)
require(jsonlite)
require(df2json)
require(plyr)
require(data.table)
require(stringi)
require(qdap)

#installing from Github
#install.packages("devtools")
#devtools::install_github("username/packagename")
####

########FUNCTIONS###############
#assemble URL from Immoscout Pagenumber
getUrl <- function(page) {
    root <- "https://www.immobilienscout24.de/suchagent/api/v2/search/profilenew?"
    u <- paste0(root,"page=", page,'&realEstateType=APARTMENT_RENT&sorting=byDateDesc' )
    return(URLencode(u))
}

data <- getURL(getUrl(0),ssl.verifypeer=0L, followlocation=1L)
data <- stri_replace_all_regex(data, "\\n"," ")


data <- fromJSON(data, flatten=TRUE)
content <- data$content
totalPages<- data$totalPages
number <- data$number



for (i in 1:totalPages) {
    data <- getURL(getUrl(i),ssl.verifypeer=0L, followlocation=1L)
    data <- stri_replace_all_regex(data, "\\n"," ")
    data <- fromJSON(data, flatten=TRUE)
    number <- data$number
    if (data$numberOfElements == 0) content = content
    else content = rbind.fill(content,data$content)
}

#get rid of everything except the content and functions
keep = c("content", "data")
rm(list=ls()[!ls() %in% c(keep, (ls()[ls() %in%  lsf.str()]))])
