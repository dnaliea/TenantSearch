#Generate example dataframe with character column
example <- as.data.frame(c("A", "A", "B", "F", "C", "G", "C", "D", "E", "F"))
names(example) <- "strcol"

#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
for(level in unique(example$strcol)){
    example[paste("dummy", level, sep = "_")] <- ifelse(example$strcol == level, 1, 0)
}

