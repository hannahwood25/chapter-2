### Clean data formating code #####

# setwd("C:/Users/Hannah/Desktop/Chap 1 stats")
setwd("~/Documents/Chap 1 stats")
getwd()

## read in data in csv format, encoding to get rid of weird symbols in coordinates
###csv<- read.csv("UserAnswers.csv", encoding ="UTF-8")

csv2 <- read.csv("wosdec.csv", encoding = "UTF-8")


#### Separate all the columns that have multiple entries into additional rows.
# make a function to do this 

####### Packages needed #######
# tidyr package for separating rows
library(tidyr)

### for removing (trimming) spaces from around things
install.packages('stringr')
library('stringr')

colnames(csv2)
                                                       
# [14] "coordinates"                                                                 
# [15] "Study.length..years."                                                        
# [16] "Starting.year.of.data.collection"                                            
# [18] "Species.family"                                                              
# [19] "Species.Latin.Name.s."                                                       
# [20] "Conservation.status.es."                                                     
# [21] "Sample.size..individuals."                                                   
# [29] "Temporal.resolution.of.tracking.data"                                        
# [32] "Repository..website..source.of.remotely.sensed.data"                         
# [33] "Satellite.Source.Space.Agency"                                               
# [34] "Satellite.platform.for.remote.sensing.data"                                  
# [35] "Satellite.sensor.used"                                                       
# [36] "Satellite.Product.Environmental.Variables"                                   
# [37] "Derived.metrics"                                                             
# [38] "Non.SRS.environmental.variables"                                             
# [39] "Temporal.Resolution.of.SRS..days."                                           
# [40] "Spatial.resolution.of.SRS..mxm."                                             


######### "Year.of.publication"  ########

str_trim(csv2$Year.of.publication)

pubyear <- table(csv2$'Year.of.publication')
barplot(pubyear, main="Publication Frequency over time",
        las=2,
        xlab="Year",
        ylab= "Number of publications")


### ggplot graph #####

year <- table(csv2$Year.of.publication)
yeardf = data.frame(year)

colnames(yeardf) = c("year", "frequency")


ggplot(yeardf, aes(x = year, y = frequency, fill = frequency)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(high = "blue", low = "pink")


######### Study.Location.Coordinates..Decimal.Degrees. ########

str_trim(csv2$coordinates)

## new data frame called newcords created with coordinates separated into rows
## ;;; removing semicolons between sets of coordinates
ncoords <- separate_rows(csv2, Study.Location.Coordinates..Decimal.Degrees., sep = ";;;")

### Create new csv with column separated
write.csv(ncoords, file = "coordinates.csv")



############# [15] "Study.length..years. ################

str_trim(csv2$Study.length..years.)

## new data frame called  created with column separated into rows
## ;;; removing semicolon between values

studylength <- separate_rows(csv2, Study.length..years., sep = ";;;")

## Create new csv with column separated
write.csv(studylength, file = "studylength.csv")



######### [16] "Starting.year.of.data.collection" ######

str_trim(csv2$Starting.year.of.data.collection)

## new data frame called  created with column separated into rows
## ;;; removing semicolon between values

startyear <- separate_rows(csv2, Starting.year.of.data.collection, sep = ";;;")

## Create new csv with column separated
write.csv(startyear, file = "startyear.csv")


########## [19] "Species.Latin.Name.s."  ######

str_trim(csv2$Species.Latin.Name.s.)

## new data frame called  created with column separated into rows
## ;;; removing semicolon between values

latinnames2 <- separate_rows(csv2, Species.Latin.Name.s., sep = ";;;")

head(latinnames2)

## Create new csv with column separated
write.csv(latinnames2, file = "latinnames2.csv")


########## Can't do families until species are separated. Add to latinnames file######

######### [20] "Conservation.status.es."  

### Add conservation statuses to the new latinnames file by hand

######[21] "Sample.size..individuals."######  

str_trim(csv2$Sample.size..individuals.)

## new data frame called  created with column separated into rows
## ;;; removing semicolon between values

samplesizei <- separate_rows(csv2, Sample.size..individuals., sep = ";;;")

## Create new csv with column separated
write.csv(samplesizei, file = "samplesizei.csv")



#### [29] "Temporal.resolution.of.tracking.data" #####

## new data frame called  created with column separated into rows
## ;;; removing semicolon between values

trackingres <- separate_rows(csv2, Temporal.resolution.of.tracking.data, sep = ";;;")

## Create new csv with column separated
write.csv(trackingres, file = "trackingres.csv")


####### [30] "Type.of.remote.sensing"   ####

rstypes <- separate_rows(csv2, Type.of.remote.sensing, sep = ";;;")

rstypesnoblanks = subset(rstypes, (Type.of.remote.sensing!=""))


rstype <- table(rstypesnoblanks$Type.of.remote.sensing)
rstypedf = data.frame(rstype)

colnames(rstypedf) = c("rstype", "frequency")


ggplot(rstypedf, aes(x = rstype, y = frequency, fill = frequency)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(high = "blue", low = "pink")



# [32] "Repository..website..source.of.remotely.sensed.data"  

repository <- separate_rows(csv2, Repository..website..source.of.remotely.sensed.data, sep = ";;;")

write.csv(repository, file = "repository.csv")


# [33] "Satellite.Source.Space.Agency"      
spaceagency <- separate_rows(csv2, Satellite.Source.Space.Agency, sep = ";;;")

write.csv(spaceagency, file = "spaceagency.csv")


# [34] "Satellite.platform.for.remote.sensing.data"  
satellite <- separate_rows(csv2, Satellite.platform.for.remote.sensing.data, sep = ";;;")

write.csv(satellite, file = "satellite.csv")


# [35] "Satellite.sensor.used"              
sensor <- separate_rows(csv2, Satellite.sensor.used, sep = ";;;")

write.csv(sensor, file = "sensor.csv")


# [36] "Satellite.Product.Environmental.Variables"  
envproduct <- separate_rows(csv2, Satellite.Product.Environmental.Variables, sep = ";;;")

write.csv(envproduct, file = "envproduct.csv")


# [37] "Derived.metrics"       

derived <- separate_rows(csv2, Derived.metrics, sep = ";;;")

write.csv(derived, file = "derived.csv")


# [38] "Non.SRS.environmental.variables"  #######3
nonsrs <- separate_rows(csv2, Non.SRS.environmental.variables, sep = ";;;")

write.csv(nonsrs, file = "nonsrs.csv")


# [39] "Temporal.Resolution.of.SRS..days."#######
tempres <- separate_rows(csv2, Temporal.Resolution.of.SRS..days., sep = ";;;")

write.csv(tempres, file = "tempres.csv")


# [40] "Spatial.resolution.of.SRS..mxm. #######
spatres <- separate_rows(csv2, Spatial.resolution.of.SRS..mxm., sep = ";;;")

write.csv(spatres, file = "spatres.csv")

