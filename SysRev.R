######## Data tidying for WOS results

setwd("C:/Users/Hannah/Desktop/Chap 1 stats")
getwd()

## read in data in csv format, encoding to get rid of weird symbols in coordinates
csv<- read.csv("UserAnswers.csv", encoding ="UTF-8")

### Check column titles

head(csv)

########   Tidying coordinates  #######

head(csv$coordinates)

print(csv$coordinates)

## Need to separate rows with multiple entries 

# string split
# tidyr package
library(tidyr)

# dataframe seprows(), duplicate, pivot.

csv$coordinates <- as.character(csv$coordinates)

## csvcoord <- data.frame(lapply(csv, as.character), stringsAsFactors=FALSE)

## newcoords <- separate_rows(csv, coordinates, sep = ";\\s+")


## new data frame
newcoords <- separate_rows(csv, coordinates, sep = ";")


othercoords <- csv %>% 
  mutate(coordinates = strsplit(as.character(coordinates), ";")) %>% 
  unnest(coordinates)

### separate_rows(csv$coordinates, sep = ";")

head(csv$coordinates)
print(csv$coordinates)

#### exporting the coordinates into a separate file ###


write.csv(csv$coordinates, file = "coordinates.csv")

write.csv(csv, file = "coordexpand.csv")


### trim spaces from around things

install.packages('stringr')
library('stringr')
str_trim(csv$coordinates)

library(dplyr)

df %>% 
  mutate(across(where(is.character), str_trim))
