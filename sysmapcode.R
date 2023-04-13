# Sys Map Analysis ####


### Packages ####

library(readr)

install.packages("dplyr")

install.packages("rworldmap")
library('rworldmap')

# tidyr package for separating rows
library(tidyr)

### for removing (trimming) spaces from around things
install.packages('stringr')
library('stringr')
# load packages
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
install.packages("rgeos")
library(rgeos)

#### Dataset ####

sysmap <- read_csv("OneDrive - King's College London/SysMap.csv")
View(sysmap)

### Check column titles

colnames(sysmap)
# [1] "Article ID"                                                                  
# [2] "Article Title"                                                               
# [3] "User Name"                                                                   
# [4] "Resolve?"                                                                    
# [5] "Include"                                                                     
# [6] "SearchEngine"                                                                
# [7] "Exclusion stage"                                                             
# [8] "Reason for exclusion"                                                        
# [9] "Details of reason for exclusion if other"                                    
# [10] "Article Topic: notes"                                                        
# [11] "Yearofpublication"                                                           
# [12] "Country of Primary Author"                                                   
# [13] "Study Location Name"                                                         
# [14] "Study Country"                                                               
# [15] "Study Location Coordinates (coordinates)"                                    
# [16] "DecimalCoordinates"                                                          
# [17] "Study length (years)"                                                        
# [18] "yearsofSeabirddata"                                                          
# [19] "yearsofrsdata"                                                               
# [20] "Starting year of data collection"                                            
# [21] "Starting year of data collection (birds)"                                    
# [22] "Starting year of data collection (RS)"                                       
# [23] "Numberofstudyspecies"                                                        
# [24] "Speciesfamily"                                                               
# [25] "Species Latin Name(s)"                                                       
# [26] "Conservation status(es)"                                                     
# [27] "Samplesize (individuals)"                                                    
# [28] "Samplesize(tagswithdata)"                                                    
# [29] "Samplesize(colonies)"                                                        
# [30] "Samplesize(other)"                                                           
# [31] "ResearchFocus"                                                               
# [32] "Research Focus if other or additional details"                               
# [33] "Source of seabird location information."                                     
# [34] "Source of seabird location information if other"                             
# [35] "Tracking accuracy (m)"                                                       
# [36] "Temporal resolution of tracking data"                                        
# [37] "Type of remote sensing"                                                      
# [38] "Type of remote sensing if other"                                             
# [39] "Repository (website) source of remotely sensed data"                         
# [40] "Satellite Source/Space Agency"                                               
# [41] "Satellite platform for remote sensing data"                                  
# [42] "Satellite sensor used"                                                       
# [43] "Satellite Product/Environmental Variables"                                   
# [44] "Derived metrics"                                                             
# [45] "Non-SRS environmental variables"                                             
# [46] "Temporal Resolution of SRS (days)"                                           
# [47] "Spatial resolution of SRS (mxm)"                                             
# [48] "Satellite imagery processing/analysis software"                              
# [49] "Was satellite data verified? How?"                                           
# [50] "Details of how satellite data was verified if Other"                         
# [51] "Do/How do authors account for or discuss temporal variation in environment?" 
# [52] "Analytical method for linking movement and environment"                      
# [53] "Do the authors make or discuss specific predicted changes in seabird ecology"
# [54] "Link to policy or conservation"                                              
# [55] "Additional notes"                                                            
# [56] "User Note"                                                                   
# [57] "Title"                                                                       
# [58] "Journal"                                                                     
# [59] "Authors" 

## No of papers

nrow(sysmap)
## 700

### Search Engines
table(sysmap['Search Engine'])
# GS Scopus    WOS 
# 284    300    116 

### Exclusion stages
table(sysmap['Exclusion stage'])
# Included: 122
# Title/Abstract: 449
# Main body: 129

### Reason for exclusion
table(sysmap['Reason for exclusion'])

## subset different search engines
scopus <- subset(sysmap, SearchEngine=='Scopus')
wos <- subset(sysmap, SearchEngine=='WOS')
gs <- subset(sysmap, SearchEngine=='GS')

table(wos['Exclusion stage'])
table(wos['Reason for exclusion'])

table(scopus['Exclusion stage'])
table(scopus['Reason for exclusion'])


table(gs['Exclusion stage'])
table(gs['Reason for exclusion'])


### Publication Year

pubyear <- table(sysmap$'Year of publication')
barplot(pubyear, main="Publication Frequency over time",
        las=2,
        xlab="Year",
        ylab= "Number of publications")

#### Years of data collection

### Seabirds
seabirdstart <- table(sysmap$'Starting year of data collection (birds)')
barplot(seabirdstart, main="First year of Seabird data",
        las=2,
        xlab="Year",
        ylab= "Number of publications")

### Remote sensing
rsstart <- table(sysmap$'Starting year of data collection (RS)')
barplot(rsstart, main="First year of Remote Sensing data",
        las=2,
        xlab="Year",
        ylab= "Number of publications")

### Years worth of data Bird and RS

seabirddata <- table(sysmap$yearsofSeabirddata)
barplot(seabirddata, main="Length of seabird data series",
        las=2,
        xlab="Years",
        ylab= "Number of publications")

mean(sysmap$yearsofSeabirddata, na.rm = TRUE)
max(sysmap$yearsofSeabirddata, na.rm = TRUE)
min(sysmap$yearsofSeabirddata, na.rm = TRUE)

rsdata <- table(sysmap$yearsofrsdata)
barplot(rsdata, main="Length of remote sensing data series",
        las=2,
        xlab="Years",
        ylab= "Number of publications")

mean(sysmap$yearsofrsdata, na.rm = TRUE)
max(sysmap$yearsofrsdata, na.rm = TRUE)
min(sysmap$yearsofrsdata, na.rm = TRUE)

## Country of publication
# would be great to get a map with colour shading based on number of times country is mentioned
# las=2 makes it show all the x labels
authorcountry <- table(sysmap$`Country of Primary Author`)


barplot(authorcountry, main="Country of First Author",
        las=2,
        ylab= "Number of publications")

head(authorcountry)

author_df <- as.data.frame(t(authorcountry))
author_df

ggplot(data=author_df, aes(x=reorder(Var2, -Freq), y=Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## matching my countries to the map
sPDF <- joinCountryData2Map( author_df, joinCode = "NAME",nameJoinColumn = "Var2")

## to correct any mismatched country names use:
identifyCountries() 

#create a map-shaped window
mapDevice() #create world map shaped window

#makes map 

mapCountryData(sPDF, nameColumnToPlot = "Freq", catMethod="fixedWidth", mapTitle='Number of Primary Authors by Country') 

##### Country of study

## Country of publication
# would be great to get a map with colour shading based on number of times country is mentioned
# las=2 makes it show all the x labels

studycountrysep <- separate_rows(sysmap, StudyCountry, sep = ";;;")
studycountry <- table(studycountrysep$'StudyCountry')
studycountry_df <- as.data.frame(t(studycountry))

head(studycountry)


ggplot(data=studycountry_df, aes(x=reorder(Var2, -Freq), y=Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## matching my countries to the map
sPDF <- joinCountryData2Map(studycountry_df, joinCode = "NAME",nameJoinColumn = "Var2")

## to correct any mismatched country names use:
identifyCountries() 

#create a map-shaped window
mapDevice() #create world map shaped window

#makes map 

mapCountryData(sPDF, nameColumnToPlot = "Freq", catMethod="fixedWidth", mapTitle='Study Country') 


##### Mapping study location coordinates

###remove spaces in coordinates

str_trim(sysmap$DecimalCoordinates)
## new data frame called newcords created with coordinates separated into rows
## ||| removing semicolons between sets of coordinates

ncoords <- separate_rows(sysmap, DecimalCoordinates, sep = ";;;")

### Separate the coordinates into lat and long

latlong <- separate(ncoords, DecimalCoordinates,  sep = ",", into= c("lat", "long"), remove = FALSE)

latlong$lat = as.numeric(latlong$lat)
latlong$long = as.numeric(latlong$long)

###### Trying to map the coordinates

### rworldmap is more simplistic  #######

# plot data on world map

install.packages("spam")
library(spam)
library(rworldmap)
# get map
worldmap <- getMap(resolution = "coarse")
# plot world map


### grey and blue with countries on it
plot(worldmap, col = "lightgrey", 
     fill = T, border = "darkgray",
     xlim = c(-180, 180), ylim = c(-90, 90),
     bg = "aliceblue",
     asp = 1, wrap=c(-180,180))

#### blue sea and landmasses in silhouette

plot(worldmap, xlim = c(-80, 160), ylim = c(-50, 100), 
     asp = 1, bg = "lightblue", col = "black", fill = T)

### trying to add study location points to the map

points(latlong$long, latlong$lat, col = "red", pch = 1)


####################

######## rnatural earth for fancier maps #######


### locations of studies as a subset with NAs removed

(sites <- st_as_sf(subset(latlong, !is.na(long) & !is.na(lat)), coords = c("long", "lat"), 
                   crs = 4326, agr = "constant"))

# load world map data

world <- ne_countries(scale = "medium", returnclass = "sf")


#### # world map: studies locations coloured by no of species

head(world)
library("ggspatial")

ggplot(data = world) +
        geom_sf(fill = "grey", color ="darkgrey") +
        geom_sf(data = sites, size = 2, aes(color = Numberofstudyspecies)) +
        labs( x = "Longitude", y = "Latitude") +
        ggtitle("Location of studies", subtitle = paste0("(", nrow(sites), " studies)")) +
        scale_color_gradient(low = "blue", high = "red")

## Error: Discrete value supplied to continuous scale may need to change to category not aes colour

(sites <- st_as_sf(subset(latlong, !is.na(long) & !is.na(lat)), coords = c("long", "lat"), 
                   crs = 4326, agr = "constant"))

sites$Numberofstudyspecies=as.numeric(sites$Numberofstudyspecies)

ggplot(data = world) +
        geom_sf(fill = "grey", color ="darkgrey") +
        geom_sf(data = sites, size = 2, aes(color = Numberofstudyspecies)) +
        labs( x = "Longitude", y = "Latitude") +
        ggtitle("Location of studies", subtitle = paste0("(", nrow(sites), " studies)")) +
        scale_color_gradient(low = "blue", high = "red") +
        theme(panel.background = element_rect(fill = "aliceblue"))

### Green map, with transparent shapes

ggplot(data = world) +
        geom_sf(fill = "darkseagreen1", color ="darkseagreen4") + ### land colours
        geom_sf(data = sites, size = 2, shape  =23, aes(color = Numberofstudyspecies)) +
        labs( x = "Longitude", y = "Latitude") +
        ggtitle("Study Locations") +
        theme(plot.title = element_text(hjust = 0.5)) + ### centring title as defalut is to the left
        scale_color_gradient(low = "blue", high = "red") +
        theme(panel.grid.major = element_line(color = gray(0.1), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

#### trying to bin the no of study species

ggplot(data = world) +
        geom_sf(fill = "darkseagreen1", color ="darkseagreen4") + ### land colours
        geom_sf(data = sites, size = 2, shape  =23, aes(color = Numberofstudyspecies)) +
        labs( x = "Longitude", y = "Latitude") +
        ggtitle("Study Locations") +
        theme(plot.title = element_text(hjust = 0.5)) + ### centring title as defalut is to the left
        scale_colour_binned(type = "viridis",
                            breaks = c(1, 5, 15, 25,35,45),
                            guide = guide_coloursteps(even.steps = FALSE,
                                                      show.limits = TRUE)) +
        theme(panel.grid.major = element_line(color = gray(0.1), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

#### manually making the bin colours

ggplot(data = world) +     
        geom_sf(fill = "darkseagreen1", color ="darkseagreen4") + ### land colours
        geom_sf(data = sites, size = 2, shape  =23,  stroke = 0.8, aes(color = Numberofstudyspecies)) +
         labs( x = "Longitude", y = "Latitude") +
         ggtitle("Study Locations") +
         theme(plot.title = element_text(hjust = 0.5)) + ### centring title as defalut is to the left
         scale_colour_stepsn(colours = c("turquoise4", "steelblue2", "goldenrod1", "darkorange2", "firebrick3", "red4"),
                             breaks = c(1, 5, 15, 25,35,45),
                              guide = guide_coloursteps(even.steps = FALSE,
                              show.limits = TRUE)) +
          theme(panel.grid.major = element_line(color = gray(0.1), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))


ggplot(data = world) +
        geom_sf(fill = "antiquewhite1") +
        geom_sf(data = sites, size = 1, shape =23, aes(fill = Numberofstudyspecies)) +
        labs( x = "Longitude", y = "Latitude") +
        ggtitle("Study Location") +
        scale_colour_stepsn(colours = c("darkblue", "green", "yellow", "orange", "red", "purple"),
                            breaks = c(1, 5, 15, 25,35,45),
                            guide = guide_coloursteps(even.steps = FALSE,
                                                      show.limits = TRUE)) +
        theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))



#############   Research focus ############# 

researchfocus <- table(sysmap$researchfocuscombined)


barplot(researchfocus, main="Research Focus",
        las=2,
        ylab= "Number of publications")


###### ggplot

rfdf <- data.frame(researchfocus)

library("ggplot2")

ggplot(rfdf)


ggplot(data=rfdf, aes(x=reorder(Var1, -Freq), y=Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

flip <- ggplot(data=rfdf, aes(x=reorder(Var1, Freq), y=Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

flip + coord_flip()


###### Seabird data ######


### Species ####
library(dplyr)

sepsps <- separate_rows(sysmap, splatinname, sep = ";;;")


##### top ten species #########

toptensp <- sepsps %>%
                count(splatinname) %>%
                drop_na() %>%
                arrange(-n) %>%
                top_n(10)
        
toptensp

ggplot(toptensp, aes(x = reorder(splatinname, -n), y = n)) +
        geom_bar(stat= "identity", fill="steelblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sepsp <- table(separatedspecies$SpeciesLatinName)

#### and then compare that to the overall abundance from PNAS data

abundance <- read_excel("Desktop/abundance.xlsx")


ggplot(abundance, aes(x = species, y = globaln)) +
        geom_bar(stat= "identity", fill="steelblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# lock in factor level order

abundance$species <- factor(abundance$species, levels = abundance$species)

# plot

ggplot(abundance, aes(x = species, y = globaln)) +
        geom_bar(stat= "identity", fill="steelblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(abundance, aes(x = species, y = sysmapn)) +
        geom_bar(stat= "identity", fill="steelblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

########### Get IUCN status for each species.###############

###### Packages ######

install.packages("rredlist")
library("rredlist")
install.packages("rtools40")
library("Rtools")

install.packages("taxize")
library("taxize")

install.packages("jqr")
library("jqr")

iucn_redlist_key ='0c92437cb2eafaca9ccb75a3b75cb630f36504f7352af5ffda8cf018c38ecaf1'

##### test redlist and taxise working

rl_search_('Fratercula arctica', key=iucn_redlist_key) %>% dot()

iucn_summary('Fratercula arctica', key = iucn_redlist_key)


##### sorting out the data (separating the species out)

#### I want to make a new data frame in which all the species have been separated into separate rows

sepsps <- separate_rows(sysmap, splatinname, sep = ";;;")

##### Save this as a csv to consult
write.csv(sepsps, file = "sepsps.csv")

#### now use taxise package to get family

#### creating a function

getstatus = function(sepsps){
        bird <- iucn_summary(sepsps, key = iucn_redlist_key)
        return(bird[[1]]$status)  
}

### test function 

testsp <- c('Fratercula arctica', '', 'Uria aalge', 'Rissa tridactyla')

testbirdstatus<- sapply(testsp, getstatus)


## remove spaces

str_trim(sepsps$splatinname)

#make a separate list of species names so i don't mess up the 'spspecies' dataframe
species <- (sepsps$splatinname)

## use function from above to get the IUCN status
birdstatus<- sapply(species, getstatus)

birdstatus2<- sapply(sepsps$splatinname, getstatus)

iucn <- do.call(rbind, Map(data.frame, A=species, B=birdstatus))

write.csv(iucn, file = "iucn2.csv")

iucn

### trying to do a rough plot
status <- table(iucn$B)
barplot(status, main = 'IUCN status of study species',
       xlab= "Status",
       ylab= "Frequency of this category species included in a study") 


#### create a dataframe

iucndf <- data.frame(table(iucn$B))

### plotting

library("ggplot2")

ggplot(data=iucndf, aes(x = Var1, y = Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#### With threat categories re-ordered

threat <- c("LC", "NT", "VU", "EN", "CR")  ### writing the threat orders

### plotting with threat orders specified
ggplot(iucndf, aes(x = Var1, y = Freq)) +
        scale_x_discrete(limits = threat) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##### Species  #######

#### no of species in a study

nospeciesdf <- data.frame(table(sysmap$Numberofstudyspecies))

ggplot(nospeciesdf, aes(x=reorder(Var1, -Freq), y=Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

studysp <- data.frame(table(spspecies$SpeciesLatinName))

ggplot(nospeciesdf, aes(x=reorder(Var1, -Freq), y=Freq)) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


