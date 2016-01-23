#Generate example dataframe with character column

Dummyfier<-function(data, cols, idcol){
    dummyContent <- as.data.frame(data[idcol])
    
    for (col in c(1:length(cols))){
        example <- as.data.frame(data[cols[col]])
        names(example) <- "strcol"
    
        #For every unique value in the string column, create a new 1/0 column
        #This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
        for(level in unique(example$strcol)){
            example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
        }
        #remove dummy_NA column
        drops = "dummy_NA"
        example = example[,!(names(example) %in% drops)]
        dummyContent <- cbind(dummyContent, example)
    }
}
    

data=content
#cols=c("homeSize", "levelOfEmployment")
#idcol="id"
#content2 <- content
#dummyTest<-Dummyfier(content, c("homeSize", "levelOfEmployment"),"id")

example <- as.data.frame(cbind(content["id"], content["homeSize"]))
names(example) <- "strcol"

example <- as.data.frame(content["homeSize"])
                         names(example) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
    example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}

drops = "dummy_NA"
content3 = example[,!(names(example) %in% drops)]
dummyContent_homesize <- cbind(content["id"], content3)


##

example2 <- as.data.frame(content["levelOfEmployment"])
names(example) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
    example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}

drops = "dummy_NA"
content3 = example[,!(names(example) %in% drops)]
dummyContent_Employment <- cbind(content["id"], content3)
#########



rm(content3)
