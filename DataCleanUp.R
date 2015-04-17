
#DATA CLEANUP

#fix datetime
#for that: convert ms to seconds
ct<- content$modificationDate
content$modificationDate<-as.POSIXct(ct/1000, origin="1970-01-01")

#make data frame, and produce master copy
content <- as.data.frame(content)
content2 <- as.data.frame(content)

#flatten nested lists for locations
for (i in 1:length(content$locations)){
    district_ = (list(unique(content$locations[i][[1]]$title)))
    content$district[i] =I(district_)
    city = (list(unique(content$locations[i][[1]]$cityName)))
    content$cityName[i] =I(city)}

drops = "locations"
content3 = content[,!(names(content) %in% drops)]

#string processing: PETS
require(stringr)
#for (i in 1:length(content$pets)){
pets_ = tolower(content$pets)
#remove everything that is not alphanumeric, a space or a '!?.
pets_ = str_replace_all(pets_, pattern = "[^[:alnum:][:space:]['!?.]", " ") 
#remove all punctuation (replace by whitespace), #replace multiple whitespaces with single
pets_ = tolower(str_replace_all(content$pets, pattern = "[[:punct:]]", " ")) 
pets_ = str_replace_all(pets_, pattern = "\\s+", " ") 
#split by whitespace#}
pets_ = str_split(pets_, " ") 

#pull out cats and Dogs, and no pets
Katze = c("katze","katzen", "kater", "wohnungskatze",  "wohnungskatzen" , "freigängerkatze", "freigängerkatzen", 
          "hauskatze" , "hauskatzen", "stubentiger" , "stubenkater" , "stubenkatze", "hauskater", "hauskatze") 
Hund = c("hund", "hunde",  "hunden", "hündin")
NoPets = c("kein","keine","nein","0", NA, "keine!", "keines", "keins")
pets__= as.vector(pets_)

for (i in c(1:length(pets_))) {
    if (is.element(pets_[i], NoPets)) pets__[i] = "None" else {
        if (is.element(pets_[i], Katze)) pets__[i] = "cat" else {
            if (is.element(pets_[i], Hund)) pets__[i] = "dog" else pets__[i] = "Other"}}} 
#table(unlist(pets__))
content$pets = unlist(pets__)

# THIS SECTION WOULD SEPERATE THE CONTENT INTO THREE DISTINCT TABLES: FEATURES; INFO AND TEST########

# #first convert content into data.table, for easier removal
# content <- as.data.table(content)
# 
# #make contentFeat, from there remove unwanted vars
# contentFeat <- content 
# varsFeat=c("district" ,                               
#             "cityName",
#             "moveInDate" ,
#             "pets" ,
#             "income",
#             "profession" ,
#             "levelOfEmployment",
#             "filteredDistrictsAsString",
#             "roomsMax",
#             "roomsMin" ,
#             "livingAreaMax",
#             "livingArea",
#             "price"  ,
#             "schufaCode" ,
#             "id",
#             "modificationDate",
#             "homeSize")
# NotvarsFeat<-setdiff(colnames(content),varsFeat)
# contentFeat <- data.table(content)
# contentFeat <- contentFeat[,(NotvarsFeat):=NULL]
# 
# #make ContentINFO
# #assemble basic info (not to be clustered on in dedicated table)
# varsInfo=c("id" ,
#           "modificationDate" ,
#            "photoUrl" ,
#            "image",
#            "profileTitle",
#            "realEstateType", 
#            "moveInDate",  "fullImageUrl" , 
#            "moveInDateType" 
#            )
# NotvarsInfo<-setdiff(colnames(content),varsInfo)
# contentInfo <- data.table(content)
# contentInfo <- contentInfo[,(NotvarsInfo):=NULL]
# 
# #make contentText for text processing of NLP Features
# varsText=c("searchRequestAdditionalInfo", "profileAdditionalInfo"  , "id", "profileTitle")
# NotvarsText<-setdiff(colnames(content),varsText)
# contentText <- data.table(content)
# contentText <- contentText[,(NotvarsText):=NULL]


#extract number of words and sentences
text_=tolower(content$searchRequestAdditionalInfo)
text_ = str_replace_all(text_, pattern = "[\\n]", " ")
text_ = str_replace_all(text_, pattern = "[&amp]", " ")
text_ = str_replace_all(text_, pattern = "[quot]", " ")
text_ = str_replace_all(text_, pattern = "[^[:alnum:][:space:]['!?:]]", " ") #remove everything that is not alphanumeric, a space or a '!?.
text = str_split(text_, "[.!?]") #split into sentences#
content$sentences_per_searchRequestAdditionalInfo = sapply(text, length)
text_ = str_replace_all(text_, pattern = "[[:punct:]]", " ") #remove all punctuation
text_ = str_replace_all(text_, pattern = "\\s+", " ") #replace multiple whitespaces with single
text_ = str_split(text_, " ") #split by whitespace#}
content$words_per_searchRequestAdditionalInfo = sapply(text_, length)

# text_=tolower(content$profileAdditionalInfo)
# text_ = str_replace_all(text_, pattern = "\\n", " ")
# text_ = str_replace_all(text_, pattern = "&amp", "")
# text_ = str_replace_all(text_, pattern = "quot", "")
# text_ = str_replace_all(text_, pattern = "[^[:alnum:][:space:]['!?:\\-\\,]]", " ") #remove everything that is not alphanumeric, a space or a '!?.
# text_ = str_replace_all(text_, pattern = "[0-9][0-9]\\.+","")#remove two letters followed by dot (DATES)
# text_ = str_replace_all(text_, pattern = "\\b\\d{4}\\b","") # remove 4 digits
# text_ = str_replace_all(text_, pattern = "[0-9][\\-][zr]\\|[0-9]+\\s[zr]","") #remove room numbers fis this!
# text_ = str_replace_all(text_, pattern = "[0-9][\\,][5]","") #remove room numbers with commas
# text_ = str_replace_all(text_, pattern = "[0-9]+\\s[q]\\|[0-9]+[q]","")#remove qm
# text_ = str_replace_all(text_, pattern = "[0-9]+\\s[m]\\|[0-9]+[m]","")#remove m2
# text_  = str_replace_all(text_ , pattern = "^[0][0-9]+", "") #remove anything that starts with a 0 -->dates!
# text_  = str_replace_all(text_ , pattern = "^\\d{4}$", "") #remove years
# numbers = str_extract_all(text_, pattern = "[0-9]+\\b")#

text_=tolower(content$profileAdditionalInfo)
text = str_split(text_, "[.!?]") #split into sentences#
content$sentences_per_profileAdditionalInfo = sapply(text, length)
text_ = str_replace_all(text_, pattern = "[[:punct:]]", " ") #remove all punctuation
text_ = str_replace_all(text_, pattern = "\\s+", " ") #replace multiple whitespaces with single
text_ = str_split(text_, " ") #split by whitespace#}
content$words_per_profileAdditionalInfo = sapply(text_, length)

content$word_per_Text = sum(content$word_per_Text,contentText$words_per_searchRequestAdditionalInfo )
content$sentences_per_Text = sum(content$sentences_per_Text,contentText$sentences_per_searchRequestAdditionalInfo )

#get rid of everything except the content and functions
keep = c("content", "content2","data")
rm(list=ls()[!ls() %in% c(keep, (ls()[ls() %in%  lsf.str()]))])

#numbers<-plyr::ldply(numbers, rbind)
#text2<-as.data.frame(text_)
#text2 <-cbind(text2,numbers)
#names(text2)



