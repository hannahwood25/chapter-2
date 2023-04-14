### Stat for DTP poster


setwd("C:/Users/Hannah/Desktop/Chap 1 stats")
getwd()

# read the data from excel file Answers_WOS
# can't use read.table or read.csv as different file format
# need a special package for excel files

library(readxl)
Answers_WOS <- read_excel("Answers_WOS.xlsx")
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

# Starting year of data collection

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

researchfocus <- table(Answers_WOS$`Research Focus`)
head(researchfocus)

#barplot(researchfocus, main="Ecological Focus",
        las=2,
        ylab= "Number of publications")

lbls <- c("Breeding Location/Distribution", "Other", "Abundance/Population Estimates", "Foraging Distribution", "At-Sea Distribution")

pie(researchfocus, 
    labels = lbls,
    main="Ecological Focus")
