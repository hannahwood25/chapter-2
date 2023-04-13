### Stat for DTP poster


setwd("~/Desktop/Chapter 2 SysMap")
getwd()

# read the data from excel file Answers_WOS
# can't use read.table or read.csv as different file format
# need a special package for excel files

library(readxl)
library(readr)
wosfeb <- read_csv("wosfeb.csv")
# Look at data in R
View(Answers_WOS)


#Figure out how to separate all columns which have multiple answers in a row

#Replace all 'NI's with 'Not Included'

## make a graph showing years of publication over time
# Simple Bar Plot

pubyear <- table(Answers_WOS$'Year of publication')
barplot(pubyear, main="Publication Frequency over time",
        las=2,
        xlab="Year",
        ylab= "Number of publications")



## Country of publication
# would be great to get a map with colour shading based on number of times country is mentioned
# las=2 makes it show all the x labels
authorcountry <- table(Answers_WOS$`Country of Primary Author`)
barplot(authorcountry, main="Country of First Author",
        las=2,
        ylab= "Number of publications")

head(authorcountry)

install.packages("rworldmap")
library('rworldmap')

final_df <- as.data.frame(t(authorcountry))
final_df

## matching my countries to the map
sPDF <- joinCountryData2Map( final_df
                             ,joinCode = "NAME"
                             ,nameJoinColumn = "Var2")

## to correct any mismatched country names use:
identifyCountries() 

#create a map-shaped window
mapDevice() #create world map shaped window

#makes map 

# mapCountryData(sPDF, nameColumnToPlot = "Freq", catMethod="fixedWidth")

mapCountryData(sPDF,
               nameColumnToPlot = "Freq",
               catMethod="fixedWidth",
               mapTitle='Number of Primary Authors by Country') 

# haven't figured out how to add a title to the default legend yet


###### Country of study ###################

## Crrent system won't work in the long-term as 8 of the countries are not recognised
## sub-antarctica probably an issue.

# as above
Studycountry <- table(Answers_WOS$'Study Country')
barplot(Studycountry, main="Study Location",
        las=2,
        ylab= "Number of studies")

head(Studycountry)

# change the format of the data into useful columns with Freq count
studycountry_df <- as.data.frame(t(Studycountry))

head(studycountry_df)

sPDFS <- joinCountryData2Map( studycountry_df
                             ,joinCode = "NAME"
                             ,nameJoinColumn = "Var2")


#create a map-shaped window
mapDevice() #create world map shaped window

#makes map 

# mapCountryData(sPDF, nameColumnToPlot = "Freq", catMethod="fixedWidth")

mapCountryData(sPDFS,
               nameColumnToPlot = "Freq",
               catMethod="fixedWidth",
               mapTitle='Number of studies per Country') 

## to correct any mismatched country names use:
identifyCountries() 

# haven't figured out how to add a title to the default legend yet
Answers_WOS$Study Country$

# Study length
# min, mean, max, distribution, ie 90% or studies less than 2 years etc
# Short term vs long term studies


colnames(wosfeb)
# [1] "Article ID"                                                                  
# [2] "Article Title"                                                               
# [3] "User Name"                                                                   
# [4] "Resolve?"                                                                    
# [5] "Include"                                                                     
# [6] "Search Engine"                                                               
# [7] "Exclusion stage"                                                             
# [8] "Reason for exclusion"                                                        
# [9] "Details of reason for exclusion if other"                                    
# [10] "Article Topic: notes"                                                        
# [11] "Year of publication"                                                         
# [12] "Country of Primary Author"                                                   
# [13] "Study Location Name"                                                         
# [14] "Study Country"                                                               
# [15] "Study Location Coordinates (Decimal Degrees)"                                
# [16] "Study length (years)"                                                        
# [17] "Starting year of data collection"                                            
# [18] "Number of study species"                                                     
# [19] "Species family"                                                              
# [20] "Species Latin Name(s)"                                                       
# [21] "Conservation status(es)"                                                     
# [22] "Sample size (individuals)"                                                   
# [23] "Sample size (colonies)"                                                      
# [24] "Sample size (other)"                                                         
# [25] "Research Focus"                                                              
# [26] "Research Focus if other or additional details"                               

# Starting year of data collection

studylength<- strsplit(wosfeb$'Study length (years)', ";;;")
studylength


Studylength <- table(wosfeb$'Study length (years)')
Studylength
barplot(Studylength, main="Study Length",
        las=2,
        ylab= "Number of studies")
# No of study species
# simple bar chart

#Species family
# Fill in these details!
# bar chart with image of families to shoe proportion of representation?
#Family tree with relative species circled? to show spread of representation?

###### species latin name #####
# another way to show species representation?
# are northern species better respresented than southern? etc
# rare versus common? Discuss
# GET LIST OF NAMES

View(Answers_WOS$`Species Latin Name(s)`)
View(Answers_WOS)

#install.packages("tidyr")
#library('tidyr')

# separate_rows(Answers_WOS, `Species Latin Name(s)`, sep = ";;;", convert = FALSE)

View(Answers_WOS$`Species Latin Name(s)`)

latinnames<- strsplit(Answers_WOS$`Species Latin Name(s)`, ";;;")

latinnames

# Conservation status
#NEED TO SEPARATE
# bar chart, most common categories

# package rredlist
# taxize

install.packages("rredlist")
library("rredlist")
install.packages("Rtools")
library("Rtools")

### Note the stupid fucking American spelling
install.packages("taxize")
library("taxize")

rl_citation()
#> [1] "IUCN 2015. IUCN Red List of Threatened Species. Version 2017-2 <www.iucnredlist.org>"



# just gives a list doesn't make a dataframe
rl_search('Fratercula arctica', parse = FALSE, key=iucn_redlist_key)

#or

# gives a clearer list
install.packages("jqr")
library("jqr")
rl_search_('Fratercula arctica', key=iucn_redlist_key) %>% dot()

rl_search_category('Fratercula arctica', key=iucn_redlist_key) %>% dot()

rl_use_iucn

### message("After getting your key set it as IUCN_REDLIST_KEY in .Renviron.\n IUCN_REDLIST_KEY='youractualkeynotthisstring'\n For that, use usethis::edit_r_environ()")
### invisible("https://apiv3.iucnredlist.org/api/v3/token")

iucn_redlist_key ='0c92437cb2eafaca9ccb75a3b75cb630f36504f7352af5ffda8cf018c38ecaf1'

iucn_summary('Fratercula arctica', distr_detail = FALSE, key = iucn_redlist_key)

puffin <- iucn_summary('Fratercula arctica', key = iucn_redlist_key)

puffin$`Fratercula arctica`$status

## tapply the list of species

## creating a function

getstatus = function(species){
    bird <- iucn_summary(species, key = iucn_redlist_key)
    
    return(bird[[1]]$status)  
}

### creates a list of species

sp <- list('Fratercula arctica', 'Puffinus puffinus', 'Uria aalge', 'Rissa tridactyla')
lapply(sp, getstatus)

## vector

sp <- c('Fratercula arctica', '', 'Uria aalge', 'Rissa tridactyla')
birdstatus<- sapply(sp, getstatus)

birdstatus

### sAMPLE SIZES #####

##Research focus

researchfocus <- table(wosfeb$`Research Focus`)
researchfocus

lbls2 <- c("Abundance/Population Estimates", "Other", "Abundance/Population Estimates", "Foraging Distribution", "At-Sea Distribution")


barplot(researchfocus, main="Ecological Focus",
        las=2,
        names.arg = lbls,
        ylab= "Number of publications")


lbls <- c("Breeding Location/Distribution", "Other", "Abundance/Population Estimates", "Foraging Distribution", "At-Sea Distribution")


researchfocus <- table(wosfeb$`Research Focus`)
rfdf <- data.frame(researchfocus)

library("ggplot2")

ggplot(rfdf, aes( y = frequency, fill = frequency)) +
    geom_bar(stat="identity") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_gradient(high = "blue", low = "pink")

pie(researchfocus, 
    labels = lbls,
    main="Ecological Focus")


### Seabird data from
# [27] "Source of seabird location information."                                     
# [28] "Source of seabird location information if other"    





# [29] "Tracking accuracy (m)"  


# [30] "Temporal resolution of tracking data"  

trackres <- table(wosfeb$'Temporal resolution of tracking data')
trackr <- data.frame(trackres)
min(wosfeb$'Temporal resolution of tracking data', na.rm = TRUE)
max(wosfeb$'Temporal resolution of tracking data', na.rm = TRUE)

barplot(pubyear, main="Publication Frequency over time",
        las=2,
        xlab="Year",
        ylab= "Number of publications")

# [31] "Type of remote sensing"                                                      
# [32] "Type of remote sensing if other"                                             
# [33] "Repository (website) source of remotely sensed data"                         
# [34] "Satellite Source/Space Agency"                                               
# [35] "Satellite platform for remote sensing data"                                  
# [36] "Satellite sensor used"                                                       
# [37] "Satellite Product/Environmental Variables"                                   
# [38] "Derived metrics"                                                             
# [39] "Non-SRS environmental variables"   


# [40] "Temporal Resolution of SRS (days)"                                           
# [41] "Spatial resolution of SRS (mxm)"    

# [42] "Satellite imagery processing/analysis software"   

# [43] "Was satellite data verified? How?"                                           
# [44] "Details of how satellite data was verified if Other"                         
# [45] "Do/How do authors account for or discuss temporal variation in environment?" 
# [46] "Analytical method for linking movement and environment"                      
# [47] "Do the authors make or discuss specific predicted changes in seabird ecology"
# [48] "Link to policy or conservation"                                              
# [49] "Additional notes"                                                            
# [50] "User Note"                                                                   
# [51] "Title"                                                                       
# [52] "Journal"                                                                     
# [53] "Authors" # [27] "Source of seabird location information."                                     
# [28] "Source of seabird location information if other"                             
# [29] "Tracking accuracy (m)"                                                       
# [30] "Temporal resolution of tracking data"                                        
# [31] "Type of remote sensing"                                                      
# [32] "Type of remote sensing if other"                                             
# [33] "Repository (website) source of remotely sensed data"                         
# [34] "Satellite Source/Space Agency"                                               
# [35] "Satellite platform for remote sensing data"                                  
# [36] "Satellite sensor used"                                                       
# [37] "Satellite Product/Environmental Variables"                                   
# [38] "Derived metrics"                                                             
# [39] "Non-SRS environmental variables"                                             
# [40] "Temporal Resolution of SRS (days)"                                           
# [41] "Spatial resolution of SRS (mxm)"                                             
# [42] "Satellite imagery processing/analysis software"                              
# [43] "Was satellite data verified? How?"                                           
# [44] "Details of how satellite data was verified if Other"                         
# [45] "Do/How do authors account for or discuss temporal variation in environment?" 
# [46] "Analytical method for linking movement and environment"                      
# [47] "Do the authors make or discuss specific predicted changes in seabird ecology"
# [48] "Link to policy or conservation"                                              
# [49] "Additional notes"                                                            
# [50] "User Note"                                                                   
# [51] "Title"                                                                       
# [52] "Journal"                                                                     
# [53] "Authors" 