names(content)
content2[cityName=="Berlin"]

######"levelOfEmployment"#############
example <- as.data.frame(content["levelOfEmployment"])
names(example) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
    example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}

drops = "dummy_NA"
content3 = example[,!(names(example) %in% drops)]
dummyContent_Employment <- cbind(content["id"], content3)
######"levelOfEmployment"#############END####





###### Home Size #############END####

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
dummyContent_homeSize <- cbind(content["id"], content3)

#########


###### income #############END####

example <- as.data.frame(content["income"])
names(example) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
    example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}

names(content)
drops = "dummy_NA"
content3 = example[,!(names(example) %in% drops)]
dummyContent_income <- cbind(content["id"], content3)

#########

###### pets #############END####

example <- as.data.frame(content["pets"])
names(example) <- "strcol"
#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
    example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}

names(content)
drops = "dummy_NA"
content3 = example[,!(names(example) %in% drops)]
dummyContent_pets <- cbind(content["id"], content3)

#########
#schufaCode TRUE/FALSE
content$schufaCode = !is.na(content$schufaCode)

