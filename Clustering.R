content.df <- as.data.table(content)


Feat = content[c(#"id",                         
               #"schufaCode",                                                                   
               "livingArea",
               "homeSize",
               "income"
               )]

#Feat <- cbind(Feat, dummyContent_homeSize[3:ncol(dummyContent_homesize)])
#Feat <- cbind(Feat, dummyContent_income[3:ncol(dummyContent_income)])
#Feat <- cbind(Feat, dummyContent_pets[3:ncol(dummyContent_pets)])
#Feat <- cbind(Feat, dummyContent_Employment[3:ncol(dummyContent_Employment)])

table(Feat$income)
Feat$homeSize[Feat$homeSize=="onePerson"] <-1 
Feat$homeSize[Feat$homeSize=="twoPerson"] <-2 
Feat$homeSize[Feat$homeSize=="family"] <-3 
Feat$homeSize[Feat$homeSize=="bigGroup"] <-4 
Feat$homeSize[is.na(Feat$homeSize)] <-0 

Feat$income[Feat$income=="below500"] <-1 
Feat$income[Feat$income=="over500_upto1000"] <-2
Feat$income[Feat$income=="over1000_upto1500"] <-3 
Feat$income[Feat$income=="over1500_upto2000"] <-4 
Feat$income[Feat$income=="over2000_upto3000"] <-5 
Feat$income[Feat$income=="over3000_upto4000"] <-6 
Feat$income[Feat$income=="over4000_upto5000"] <-7 
Feat$income[Feat$income=="over5000"] <-8 
Feat$income <- as.numeric(Feat$income)
Feat$income[is.na(Feat$income)] <-0 
#Feat$income<- as.integer(Feat$income)
Feat$homeSize<- as.numeric(Feat$homeSize)
class(Feat$homeSize)

Feat[is.na(Feat)] <-0 

#homeSize<- normalize(Feat$homeSize)


normalize <- function(x) { 
    x <- sweep(x, 2, apply(x, 2, min)) 
    sweep(x, 2, apply(x, 2, max), "/") 
} 

textFeat.norm <-normalize(textFeat)

#keep = c("content", "content_master","data", "textFeat", "Feat")
#rm(list=ls()[!ls() %in% c(keep, (ls()[ls() %in%  lsf.str()]))])


Feat.cl<- cbind(textFeat.norm, Feat)
Feat.cl$livingArea <- Feat.cl$livingArea/max(Feat.cl$livingArea)
#Feat.cl$homeSize <- Feat.cl$lhomeSize/max(Feat.cl$homeSize)

df <- Feat.cl

#install.packages("NbClust")
library(NbClust)
set.seed(1234)


df$livingArea[is.na(df$livingArea)] <- mean(df$livingArea)
df[is.na(df)] <- 0
df.norm <-normalize(df)
nc <- NbClust(textFeat, min.nc=2, max.nc=7, method="kmeans")


barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters")

fit.km <- kmeans(df, 4, nstart=25)  

fit.km$size
#fit.km$size
[1] 198 523 531 392 523  23 329

centers <-fit.km$centers
groups <- aggregate(df, by=list(cluster=fit.km$cluster), mean)
