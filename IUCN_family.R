######## Data tidying for WOS results

# setwd("C:/Users/Hannah/Desktop/Chap 1 stats")
setwd("~/Documents/Chap 1 stats")
getwd()

## read in data in csv format, encoding to get rid of weird symbols in coordinates
csv<- read.csv("UserAnswers.csv", encoding ="UTF-8")

### Check column titles

head(csv)

## Separate all the columns that have multiple entries into additional rows.

# make a function to do this 

#### coordinates
# species latin name
# species family
# data start year
# data set length
# Repository (website) source of remotely sensed data
# Satellite Source/Space Agency
# Satellite platform for remote sensing data
# Satellite sensor used
# satellite Product/Environmental Variables
# Derived metrics
# Non-SRS environmental variables
# Satellite imagery processing/analysis software).

########   Tidying coordinates  #######

head(csv$coordinates)

print(csv$coordinates)

## Need to separate rows with multiple entries 

# string split
# tidyr package
library(tidyr)

# dataframe seprows(), duplicate, pivot.

# csv$coordinates <- as.character(csv$coordinates)

## csvcoord <- data.frame(lapply(csv, as.character), stringsAsFactors=FALSE)

## new data frame called newcords created with separated rows of coordinates.
# newcoords <- separate_rows(csv, coordinates, sep = ";")

# Same as above but gets rid of spaces
#newcoords <- separate_rows(csv, coordinates, sep = ";\\s+")

newcoords <- separate_rows(coordinates, sep = ";\\s+")

# This is another option although it only saves rows with coordinates in.
# install.packages("dplyr")
# library(dplyr)
# othercoords <- csv %>% 
#  mutate(coordinates = strsplit(as.character(coordinates), ";")) %>% 
#  unnest(coordinates)

### separate_rows(csv$coordinates, sep = ";")

head(csv$coordinates)
print(csv$coordinates)


### trim spaces from around things

install.packages('stringr')
library('stringr')
str_trim(csv$coordinates)

library(dplyr)

df %>% 
  mutate(across(where(is.character), str_trim))


#### exporting the coordinates into a separate file ###

write.csv(csv$coordinates, file = "coordinates.csv")

write.csv(csv, file = "coordexpand.csv")



##################### Mapping the study coordinates  #############

## Trying to remove the weird characters

coords <-read.csv("coordmaster.csv", encoding ="UTF-8")

### Important column is coords$coordinates

## new data frame called newcords created with separated rows of coordinates.

colnames(coords)

newcoords <- separate_rows(coords, coordinates, sep = ";")

# Same as above but gets rid of spaces
#newcoords <- separate_rows(csv, coordinates, sep = ";\\s+")

newcoords <- separate_rows(coords, coordinates, sep = ";\\s+")

install.packages('stringr')
library('stringr')
str_trim(coords$coordinates)

## says it wont work because its a character
# separate_rows(coords$coordinates, sep = ";")

as.factor(coords$coordinates)

newcoords <- separate_rows(coords$coordinates, sep = ";")

newcoords

######
### Separate the coordinates into lat and long

latlong <- separate(newcoords, coordinates,  sep = ",", into= c("lat", "long"), remove = FALSE)

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

points(latlong$long, latlong$lat, col = "red", pch = 19)

head(latlong$lat)

warnings()

### getting an error Warning message:
# In xy.coords(x, y) : NAs introduced by coercion


######## rnatural earth for fancier maps #######

# load packages
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
install.packages("rgeos")
library(rgeos)


### locations of studies as a subset with NAs removed

(sites <- st_as_sf(subset(latlong, !is.na(long) & !is.na(lat)), coords = c("long", "lat"), 
                   crs = 4326, agr = "constant"))

# load world map data

world <- ne_countries(scale = "medium", returnclass = "sf")


# world map: studies locations coloured by year

ggplot(data = world) +
  geom_sf(fill = "gray", color ="black") +
  geom_sf(data = sites, size = 4, aes(color = Year.of.publication)) +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Location of studies", subtitle = paste0("(", nrow(sites), " studies)")) +
  scale_color_gradient(low = "red", high = "yellow") +
  theme(panel.background = element_rect(fill = "aliceblue"))


#### # world map: studies locations coloured by no of species

head(world)

ggplot(data = world) +
  geom_sf(fill = "black", color ="white") +
  geom_sf(data = sites, size = 4, aes(color = Number.of.study.species)) +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Location of studies", subtitle = paste0("(", nrow(sites), " studies)")) +
  scale_color_gradient(low = "red", high = "yellow")

## Error: Discrete value supplied to continuous scale may need tp change to category not aes colour

### Same error as before when trying to add points. Something about the NAs


#################### Mappin country of primary author #########

csv<- read.csv("wosdec.csv", encoding ="UTF-8")

ncountry <- as.data.frame(table(csv$Country.of.Primary.Author))
colnames(ncountry)[1] <- "country" 



install.packages("countrycode")
library(countrycode)

countryname(ncountry$country, origin = 'country.name', destination = "iso.name.n")



install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")

ggplot(data = world) +
  geom_sf(aes(fill = ncountry$Freq)) +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("Country of primary author", subtitle = paste0("(", nrow(sites), " studies)"))

# data <- read.csv("coordinates.csv")
# # 
# colnames(data)
# # 
# author <- as.data.frame(data$Country.of.Primary.Author)
# # 
# head(author)
# # 
# final_df <- as.data.frame(t(author))final_df
# 
# ## matching my countries to the map
# sPDF <- joinCountryData2Map( final_df
#                              ,joinCode = "NAME"
#                              ,nameJoinColumn = "Var2")
# 
# ## to correct any mismatched country names use:
# identifyCountries() 
# 
# #create a map-shaped window
# mapDevice() #create world map shaped window
# 
# #makes map 
# 
# # mapCountryData(sPDF, nameColumnToPlot = "Freq", catMethod="fixedWidth")
# 
# mapCountryData(sPDF,
#                nameColumnToPlot = "Freq",
#                catMethod="fixedWidth",
#                mapTitle='Number of Primary Authors by Country') 
# 
# 

library(rworldmap)
install.packages("Xquartz")
library("Xquartz")

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="country")

mapCountryData(spdf, nameColumnToPlot="value", catMethod="fixedWidth")


########### Get IUCN status for each species.###############

install.packages("rredlist")
library("rredlist")
install.packages("rtools40")
library("Rtools")

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

 #iucn_summary('Fratercula arctica', distr_detail = FALSE, key = iucn_redlist_key)

### do spaces make a difference?
iucn_summary('Fratercula arctica', key = iucn_redlist_key)
# iucn_summary('Fratercula   arctica', key = iucn_redlist_key)
# iucn_summary('Fraterculaarctica', key = iucn_redlist_key)

# puffin$`Fratercula arctica`$status

## tapply the list of species

## creating a function

getstatus = function(species){
  bird <- iucn_summary(species, key = iucn_redlist_key)
  
  return(bird[[1]]$status)  
}

### creates a list of species

# sp <- list('Fratercula arctica', 'Puffinus puffinus', 'Uria aalge', 'Rissa tridactyla')
# lapply(sp, getstatus)

## vector

# sp <- c('Fratercula arctica', '', 'Uria aalge', 'Rissa tridactyla')
# birdstatus<- sapply(sp, getstatus)

# birdstatus

### getting list of IUCN for all species

#######################
########################
### Hand checked and edited master spreadsheet "speciesmaster" ############
######################
#######################

species <- read.csv("speciesmaster.csv")

### for removing (trimming) spaces from around things
install.packages('stringr')
library('stringr')

## species <- separate_rows(species, Species.Latin.Name.s., sep = ";\\s+")

## remove spaces
##str_trim(species$Species.Latin.Name.s.)

#make a list of species names
## sp <- (species$Species.Latin.Name.s.)

## use function from above to get the IUCN status
## birdstatus<- sapply(sp, getstatus)

## head(birdstatus) 

## iucn <- do.call(rbind, Map(data.frame, A=sp, B=birdstatus))

### write.csv(iucn, file = "iucnstatus.csv")

## status <- table(iucn$B)
## barplot(status3, main = 'IUCN status of study species',
##        xlab= "Status",
##        ylab= "?") 


# iucnstatus <- table(species$IUCN, useNA = "ifany")
# barplot(iucnstatus, main = 'IUCN status of study species',
#         xlab= "Status",
#         ylab= "?") 
# 
# species$IUCN= factor(species$IUCN, levels = c("LC", "NT", "VU", "EN", "CR"))
# ggplot(species, aes(x=IUCN)) +geom_bar()
# 
# iucnstatus

# install.packages('ggplot2')
# library("ggplot2")

# status2 <- as.data.frame(status)

# status2

# str(iucnstatus)
# 
# iucnstatus
# 
# ## row_order <- c("LC", "VU", "NT", "EN", "CR")
# row_order <- c(3, 4, 5, 2, 1)
# status3 <- status2[row_order, ]
# status3
# 
# ### column order change
# column_order <- c("3", "5", "4", "2", "1")
# iucnstatus2 <- iucnstatus[, column_order]

### Error in `[.default`(status, , column_order) : 
 ##### incorrect number of dimensions

# status2



#### don't know how to get the labels onto the bars
# 
# barplot(status3$Freq, main = 'IUCN status of study species',
#         xlab= "Status",
#         ylab= "?") 

#### Trying ggplot for IUCN

## iucnstatus.csv

head(species)
colnames(species)

iucnnoblanks = subset(species, (IUCN!=""))

iucn <- table(iucnnoblanks$IUCN)
iucndf <- data.frame(iucn)

colnames(iucndf)

colnames(iucndf) = c("iucn", "frequency")


### Put into order of frequency, but should it be in order of threat categories?

iucndf$iucn = factor(iucndf$iucn, levels = iucndf$iucn[order(iucndf$frequency)])

### Put in order of threat categories?

iucndf$iucn <- factor(iucndf$iucn,levels = c("LC", "NT", "VU", "EN", "CR"))

### plotting

library("ggplot2")

ggplot(iucndf, aes(x = iucn, y = frequency, fill = frequency)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(high = "blue", low = "pink")





# Get family for each species

speciesnoblanks = subset(species, (Species.family!=""))

family <- table(speciesnoblanks$Species.family)
familydf = data.frame(family)

colnames(familydf) = c("family", "frequency")

familydf$family = factor(familydf$family, levels = familydf$family[order(familydf$frequency)])


ggplot(familydf, aes(x = family, y = frequency, fill = frequency)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(trans="reverse")



ggplot(familydf, aes(x = family, y = frequency, fill = frequency)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_gradient(high = "blue", low = "pink")
 

barplot(family, main = 'Taxonomic family of study species',
        las=2,
        cex.names=.8,
        ylab= "?")

# Trying to get rid of the NAs

head(family)

## Getting families into a table

family <- table(species$Species.family)

### removing first column which has NAs in

family <- family[-1]

family

### Making a barplot with small x axis names

pdf("myfamilyplot.pdf")

par(mar = c(6, 4.5, 1, 1))
barplot(family, main = 'Taxonomic family of study species',
        las=2, 
        cex.names=.7,
        ylab= "?")

dev.off()

library("ggplot2")

speciesnoblanks = subset(species, (Species.family!=""))

ggplot(speciesnoblanks, aes(x=Species.family)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




### need make labels fit



# Get coordinates into decimal format.
