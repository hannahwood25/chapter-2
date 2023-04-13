#### Species ####

### Packages ####

library(readr)
library(ggplot2)
library(tidyr) # tidyr package for separating rows

install.packages("dplyr")
library(dplyr)
### for removing (trimming) spaces from around things
install.packages('stringr')
library('stringr')

install.packages("ggpubr")
library('ggpubr')
## for reading excel files

## (library(readxl)) the read.excel


##### import csv ###
sysmap <- read_csv("OneDrive - King's College London/Chapter 1/Chapter 1 R/SysMap.csv")


colnames(sysmap)

#### something to consider for the numbers
### readr::parse_number or read.csv("filename.csv", stringsAsFactors = FALSE)


#### no of species in a study  #######

nospeciesdf <- data.frame(table(sysmap$Numberofstudyspecies))

### csv fro write up information

write.csv(nospeciesdf, file = "noofspecies.csv")

##### Plot the no of species in a study

ggplot(nospeciesdf, aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#### Ordering the x axis by numbers not alphabetically

nospeciesdf$Var1 <- as.integer(as.character(nospeciesdf$Var1))

ggplot(nospeciesdf, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### adding axes titles
studysp <- ggplot(nospeciesdf, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggtitle("Number of species in a study") +
  xlab("Number of species") + ylab("Number of studies")

studysp

studysp + theme(
  plot.title = element_text(hjust =0.5))

# Change the color, the size and the face of
# the main title, x and y axis labels
studysp + theme(
  plot.title = element_text(color="red", size=14, face="bold.italic", hjust =0.5),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold"))
##### sorting out the data (separating the species out into separate rows)


sepsps <- separate_rows(sysmap, splatinname, sep = ";;;")

##### Save this as a csv to consult

### write.csv(sepsps, file = "sepspecies.csv")


###### General Species information ########

#### How many species?



      ##### Species representation #########

head(sepsps)

# old file abundance_gl = read.csv("~/Desktop/pnas.2023170118.csv")

abundance_pnas <- read.csv("OneDrive - King's College London/Chapter 1/Chapter 1 R/pnas.2023170118.csv")

### stop it alphabetising
# old file sepsps_abundance <- merge(sepsps, abundance_gl, by.x = "splatinname", by.y = "Scientific.name", all.x = TRUE, sort =FALSE)
# old filesepsps_abundance <- merge(sepsps, abundance_gl, by.x = "splatinname", by.y = "Scientific.name", all.x = TRUE)

sepsps_abundance <- merge(sepsps, abundance_pnas, by.x = "splatinname", by.y = "Scientific.name", all.x = TRUE, sort =FALSE)

##subsetting to check the name have matched visually
check <- sepsps_abundance %>%
  dplyr::select(splatinname, Abundance.estimate) %>%
  distinct()

check

### edit csv of gloabal abundance so latin names match


### scatterplot all abundances

spfreq_abundance <- sepsps_abundance %>% 
  group_by(splatinname) %>% #groups by species
  summarise(sysmapn = n(), globaln = unique(Abundance.estimate)) %>% # gets one value rather than all of them
  drop_na() %>%
  arrange(-sysmapn)

### scatterplot of abundance and representation in studies

ggplot(spfreq_abundance, aes(x =log10(globaln), y= sysmapn)) +
  geom_smooth(method="lm") +
  geom_point()

### Add axes labels

abundancemodel <- ggplot(spfreq_abundance, aes(x =log10(globaln), y= sysmapn)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  plot.title = element_text(hjust =0.5)) + 
  ggtitle("Species frequency in studies versus global abundance") +
  xlab("Global abundance (log10)") + ylab("Frequency of species in studies")

abundancemodel


### model details

m1 = lm(sysmapn~globaln, data = spfreq_abundance)

summary(m1)


##### Try model with poisson instead

glm <- glm(sysmapn ~ log10(globaln), data = spfreq_abundance, family = poisson(link = "log"))

summary(glm)

poissonmodel <- ggplot(spfreq_abundance, aes(x =log10(globaln), y= sysmapn)) +
  geom_smooth(method = 'glm', method.args = list(family = 'poisson')) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  plot.title = element_text(hjust =0.5)) + 
  ggtitle("Species frequency in studies versus global abundance") +
  xlab("Global abundance (log10)") + ylab("Frequency of species in studies")

poissonmodel

### r squared is small here. R squared = 1 for good fit, R squared = 0 means data is nowhere near the model

# Call:
#   lm(formula = sysmapn ~ globaln, data = spfreq_abundance)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.5425 -1.4739 -0.5860  0.5347 11.4237 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.462e+00  1.835e-01  13.417   <2e-16 ***
#   globaln     2.506e-09  1.198e-09   2.092   0.0379 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.287 on 170 degrees of freedom
# Multiple R-squared:  0.0251,	Adjusted R-squared:  0.01937 
# F-statistic: 4.377 on 1 and 170 DF,  p-value: 0.03791



### make a count of the frequency of each species #####
spfreq <- sepsps %>%
  count(splatinname) %>%
  drop_na() %>%
  arrange(-n)


#### make a csv of the frequency of all the species for my write up #####

spfreq

write.csv(spfreq, file = "speciesfrequency.csv")


###### Plot all the species and their frequency #####
  
  ggplot(spfreq, aes(x = reorder(splatinname, -n), y = n)) +
  geom_bar(stat= "identity", fill="steelblue") +
  theme_minimal() +
  theme(text = element_text(size=4),
        plot.title = element_text(hjust =0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Frequency of species in studies") +
  xlab("Species name") + ylab("Number of studies")

### making the text smaller so you read the species names

ggplot(spfreq, aes(x = reorder(splatinname, -n), y = n)) +
  geom_bar(stat= "identity", fill="steelblue") +
  theme(text = element_text(size=4),
        axis.text.x = element_text(angle=90, hjust=1))

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
theme(plot.title = element_text(hjust =0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Most frequently studied species") +
  xlab("Species name") + ylab("Number of studies")


###### Relative abundance in top ten VERSUS global abundance ######

abundance <- read_excel("Desktop/abundance.xlsx")

### scatterplot

ggplot(abundance, aes(x =log10(globaln), y= sysmapn)) +
  geom_point()

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


#### now use taxise package to get iucn status

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

#### create a datframe of the IUCN statuses with the species

iucn <- do.call(rbind, Map(data.frame, A=species, B=birdstatus))

#### make a csv of the Species name and IUCN category dataframe

write.csv(iucn, file = "iucn2.csv")


#### Make a table of the frequency of each category

status <- table(iucn$B)

### Rough base plot
barplot(status, main = 'IUCN status of study species',
        xlab= "Status",
        ylab= "Frequency of this category species included in a study") 


#### create a dataframe with frequencies of each category

iucndf <- data.frame(table(iucn$B))

#### csv of iucn frequency
write.csv(iucndf, file = "iucnfreq.csv")

### plotting

library("ggplot2")

ggplot(data=iucndf, aes(x = Var1, y = Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#### With threat categories re-ordered

threat <- c("LC", "NT", "VU", "EN", "CR")  ### writing the threat orders

paletteiucn = c("darkgreen", "lightgreen","yellow", "orange", "red")



### plotting with threat orders specified
ggplot(iucndf, aes(x = Var1, y = Freq, fill = Var1)) +
  scale_x_discrete(limits = threat) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("CR" = "red",
                                        "EN" = "orange",
                                        "VU" = "yellow",
                                        "NT" = "lightgreen",
                                        "LC" = "darkgreen")) +
theme_minimal() +
  theme(plot.title = element_text(hjust =0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("IUCN category of study species") +
  xlab("IUCN Category") + ylab("Number of studies")

#### reordeing data so that the legend is in the right order not alphabetical

reordiucndf <- iucndf

reordiucndf$Var1 <- factor(reordiucndf$Var1,                 # Relevel group factor
                         levels = c("LC", "NT", "VU", "EN", "CR"))

## repoltting in IUCN order

ggplot(reordiucndf, aes(x = Var1, y = Freq, fill = Var1)) +
  scale_x_discrete(limits = threat) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("CR" = "red",
                               "EN" = "orange",
                               "VU" = "yellow",
                               "NT" = "lightgreen",
                               "LC" = "darkgreen"),
                    name = "IUCN category",
                    labels=c("Least Concern", "Near Threatened", "Vulnerable", "Endangered", "Critically Endangered")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust =0.5),
        axis.text.x = element_blank()) +
  ggtitle("IUCN category of study species") +
  xlab("IUCN Category") + ylab("Number of species recorded in a study")

####### FAMILY #######

#### command to get details for a species from taxize

#### COMMANDS

rl_search_('Fratercula arctica', key=iucn_redlist_key) %>% dot()
### gives a bunch of data
tax_name(sci = "Fratercula arctica", get= "family", db = "ncbi")
#### gives just the family

##### TESTING

### Using the test dataset of 4
tax_name(sci = testsp, get = "family", db = "ncbi")
#### making a dataframe of the test results
testfam <- tax_name(sci = testsp, get = "family", db = "ncbi")


##### ACTUAL DATA

##### getting the family for all my birds and putting into a new dataframe
spfam <- tax_name(sci = sepsps$splatinname, get = "family", db = "ncbi")

### a csv for the families
write.csv(spfam, file = "speciesfamily.csv")

##### I have combined the iucn and family data into a spreadsheet called "SepSpeciesMaster"
SpeciesMaster <- read_csv("Desktop/SepSpeciesMaster.csv")

#### make a database of the families and their frequency

famfreq <- SpeciesMaster %>%
  count(family) %>%
  drop_na() %>%
  arrange(-n)

##### make a csv of family frequency
write.csv(famfreq, file = "famfreq.csv")

#### plot the frequency of families

ggplot(famfreq, aes(x = reorder(family, -n), y = n)) +
  geom_bar(stat= "identity", fill="steelblue") +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=90, hjust=1))

#### angle the family names and add titles

ggplot(famfreq, aes(x = reorder(family, -n), y = n)) +
  geom_bar(stat= "identity", fill="steelblue") +
  theme(text = element_text(size=12),
        plot.title = element_text(hjust =0.5),
        axis.text.x = element_text(angle=45, hjust=1)) +
ggtitle("Families recorded in studies") +
  xlab("Family name") + ylab("Frequency of family being studied")


###### For doing interesting things with the species/families/iucn category
##### "sepspecies" has all collated data

SpeciesMaster <- read_csv("Desktop/SepSpeciesMaster.csv")


###### STACKED BAR CHARTS #######

##### family, iucn status, species ########


### Making a dataframe with the relevant info plus renaming the column headings
famiucn <- data.frame(species = SpeciesMaster$splatinname, family = SpeciesMaster$family, iucn = SpeciesMaster$iucn)

famiucn <- subset(famiucn, !is.na(iucn))

###### Want to get the number of different families

length(unique(famiucn[["family"]]))
### 21

###### Want to get the number of different species in each family

### Get the names of all the families

(unique(famiucn[["family"]]))

### Subset all the families to get the no of species

dio <- famiucn %>% 
  filter(family == "Diomedeidae") %>% 
group_by(species) %>% 
  summarise(n = n())

length(unique(dio[["species"]]))

## 10 species in Diomedeidae

alc <- famiucn %>% 
  filter(family == "Alcidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(alc[["species"]]))

#### Alcids = 15

pro <- famiucn %>% 
  filter(family == "Procellariidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(pro[["species"]]))

### Procellariidae = 51

lar <- famiucn %>% 
  filter(family == "Laridae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(lar[["species"]]))

#### laridae 49


gav <- famiucn %>% 
  filter(family == "Gaviidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(gav[["species"]]))

## gaviidae 4

hyd <- famiucn %>% 
  filter(family == "Hydrobatidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(hyd[["species"]]))

### "Hydrobatidae" 6

sph <- famiucn %>% 
  filter(family == "Spheniscidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(sph[["species"]]))

### Spheniscidae 12

oce <- famiucn %>% 
  filter(family == "Oceanitidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(oce[["species"]]))

### Oceanitidae 2


acc <- famiucn %>% 
  filter(family == "Accipitridae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(acc[["species"]]))

### Accipitridae 1

ana <- famiucn %>% 
  filter(family == "Anatidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(ana[["species"]]))

### Anatidae 2

pha <- famiucn %>% 
  filter(family == "Phalacrocoracidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(pha[["species"]]))

#### Phalacrocoracidae 11

sul <- famiucn %>% 
  filter(family == "Sulidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(sul[["species"]]))

notable = famiucn %>% count(family)

notable
### Sulidae 6

ste <- famiucn %>% 
  filter(family == "Stercorariidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(ste[["species"]]))

### Stercorariidae 7

pha <- famiucn %>% 
  filter(family == "Phaethontidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(pha[["species"]]))

#### Phaethontidae 2

pan <- famiucn %>% 
  filter(family == "Pandionidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(pan[["species"]]))

#### Pandionidae 1

sco <- famiucn %>% 
  filter(family == "Scolopacidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(sco[["species"]]))

### Scolopacidae 3

pod <- famiucn %>% 
  filter(family == "Podicipedidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(pod[["species"]]))

### Podicipedidae 3

cha <- famiucn %>% 
  filter(family == "Charadriidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(cha[["species"]]))

### Charadriidae 2

fre <- famiucn %>% 
  filter(family == "Fregatidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(fre[["species"]]))

### Frigates = 3

pel <- famiucn %>% 
  filter(family == "Pelecanoididae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(cha[["species"]]))

### Pelecanoididae 2

pele <- famiucn %>% 
filter(family == "Pelecanidae") %>% 
  group_by(species) %>% 
  summarise(n = n())

length(unique(pele[["species"]]))

### Pelecanidae 1


##### (1) IUCN categories within Family

### plotting the data in stacked bar charts

## arranging the IUCN categories
famiucn$iucn = factor(famiucn$iucn, levels=rev(c("LC", "NT", "VU", "EN", "CR")))

ggplot(famiucn, aes(x =family, group = iucn, fill = iucn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("CR" = "red",
                             "EN" = "orange",
                             "VU" = "yellow",
                             "NT" = "lightgreen",
                             "LC" = "darkgreen"))


#### (2) Species within Family

# # Install Wes andreson colour pallette
# install.packages("wesanderson")
# # Load
# library(wesanderson)
# 
# ggplot(data = famiucn, mapping = aes(x = family)) +
#   geom_bar(aes(fill = species), show.legend = FALSE)+
#   theme(axis.title.x=element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#         axis.ticks.x=element_blank()) +
#   scale_fill_manual(values = wes_palette(200, name = "Zissou1", type = "continuous"), name = "")

#### Trying to get the bars in frequency order

library(forcats)

ggplot(data = famiucn, mapping = aes(fct_infreq(as.factor(family)))) +
  geom_bar(aes(fill = species), show.legend = FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values = wes_palette(200, name = "Zissou1", type = "continuous"), name = "")

### WANT THE COLOURS TO BE THE SAME ON EACH BAR AND N AT THE TOP

### Finding nice colours
install.packages("RColorBrewer")
library(RColorBrewer)
brewer.pal(n = 8, name = "Blues")

### Trying to get the number of different species in each family to add to axis


### no or records
notable <- famiucn %>%
  group_by(family) %>%
  count()

notable

notable$familytext = paste0(notable$family, " (", notable$n, ")")

famiucnb <- merge(famiucn, notable, by="family")


### no of unique species in each family

spnotable <-famiucn %>%
  group_by(family) %>%
  summarise(n = n_distinct(species))

spnotable

spnotable$familytext = paste0(spnotable$family, " (", spnotable$n, ")")

famiucnc <- merge(famiucn, spnotable, by="family")

####

ggplot(data = famiucnc, mapping = aes(fct_infreq(as.factor(familytext)))) +
  geom_bar(aes(fill = species), show.legend = FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values=rep(c("#2171B5", "#9ECAE1", "#4292C6","#084594"),70))  ### adding nice blues
 
## Trying to remove colour and use black borders
 
ggplot(data = famiucnc, mapping = aes(fct_infreq(as.factor(familytext)))) +
  geom_bar(aes(fill = fct_rev(fct_infreq(species))), size = 0.2, show.legend = FALSE, color = "darkblue") +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values=rep(c("lightblue"),200)) +
theme(text = element_text(size=12),
      plot.title = element_text(hjust =0.5),
      axis.text.x = element_text(angle=47, vjust = 1.0)) +
  ggtitle("Species representation in families") +
  xlab("Family name") + ylab("presentation of species within families")


ggplot(data = famiucnc, mapping = aes(fct_infreq(as.factor(familytext)))) +
  geom_bar(aes(fill = fct_rev(fct_infreq(species))), size = 0.2, show.legend = FALSE, color = "darkblue") +
 theme_pubclean() +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values=rep(c("lightblue"),200)) +
  theme(text = element_text(size=12),
        plot.title = element_text(hjust =0.5),
        axis.text.x = element_text(angle=47, vjust = 1.0)) +
  ggtitle("Species representation in families") +
  xlab("Family name") + ylab("presentation of species within families")
  

## geom_text(data = notable, aes(label=n, y = n), position = position_stack(vjust= 0.5),colour = "black", size = 5)


### Trying to get the number of different species in each family to add to axis

famnos <- data.frame(table(famiucn$family, famiucn$species))


### These are the bars all the same length

ggplot(data = famiucn, mapping = aes(x = family)) +
  geom_bar(position = "fill", aes(fill = species), show.legend = FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.ticks.x=element_blank()) +  
scale_fill_manual(values=rep(c("#2171B5", "#9ECAE1", "#4292C6","#084594"),70))



##### Need to add in the global numbers too ######

### Global proportions of iucn categories within a family 


### import csv iucn_birds_with_tax

iucn_birds_with_tax <- read_csv("OneDrive - King's College London/iucn_birds_with_tax.csv")

##### Extract the seabirds from the 20 families I have in my data

seabirds = subset(iucn_birds_with_tax, familyName %in%
                    c("DIOMEDEIDAE",
                      "ALCIDAE",
                      "PROCELLARIIDAE",
                      "LARIDAE",
                      "ANATIDAE",
                      "SCOLOPACIDAE",
                      "GAVIIDAE",
                      "PHALACROCORACIDAE",
                      "PODICIPEDIDAE",
                      "HYDROBATIDAE",
                      "SULIDAE",
                      "CHARADRIIDAE",
                      "SPHENISCIDAE",
                      "STERCORARIIDAE",
                      "FREGATIDAE",
                      "OCEANITIDAE",
                      "PHAETHONTIDAE",
                      "ACCIPITRIDAE",
                      "PANDIONIDAE",
                      "PELECANIDAE"))


(unique(famiucn[["family"]])) ### names of the families i have
# "Diomedeidae"       "Alcidae"           "Procellariidae"   
# "Laridae"           "Anatidae"          "Scolopacidae"     
# "Gaviidae"          "Phalacrocoracidae" "Podicipedidae"    
# "Hydrobatidae"      "Sulidae"           "Charadriidae"     
# "Spheniscidae"      "Stercorariidae"    "Fregatidae"       
# "Oceanitidae"       "Phaethontidae"     "Pelecanoididae"   
# "Accipitridae"      "Pandionidae"       "Pelecanidae"      


seabirds$iucn = factor(seabirds$iucn, levels=rev(c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")))

library(ggplot2)

### PLot with just family names and EX/EW
ggplot(seabirds, aes(x = familyName, group = iucn, fill = iucn)) +
  geom_bar(position = "fill") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c(
    "DD" = "grey",
    "EX" = "black", 
    "EW" = "darkgrey", 
    "CR" = "red", 
    "EN" = "orange", 
    "VU" = "yellow", 
    "NT" = "lightgreen", 
    "LC" = "darkgreen"))

### Sysmap data

#### No of species represented in each family

spnoiucn <-famiucn %>%
  group_by(family) %>%
  summarise(n = n_distinct(species))

spnoiucn$familytext = paste0(spnoiucn$family, " (", spnoiucn$n, ")")

spnoiucn <- merge(famiucn, spnoiucn, by="family")

#### Neater with axes labels

ggplot(spnoiucn, aes(x =familytext, group = iucn, fill = iucn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  theme(text = element_text(size=12),
        plot.title = element_text(hjust =0.5),
        axis.text.x = element_text(angle= 45, vjust = 1, hjust=1)) +
  ggtitle("IUCN threat categories of seabirds studied with remote sensing") +
  xlab("Family") + ylab("Proportion of family in IUCN category") +
  scale_fill_manual(values = c("CR" = "red",
                               "EN" = "orange",
                               "VU" = "yellow",
                               "NT" = "lightgreen",
                               "LC" = "darkgreen"),
                    name = "IUCN category",
                    labels=c("Critically Endangered",
                             "Endangered",
                             "Vulnerable",
                            "Near Threatened",
                             "Least Concern"))

#### birdlife data add sp nos

seabirds$iucn = factor(seabirds$iucn, levels=rev(c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD")))

spseabirds <-seabirds %>%
  group_by(familyName) %>%
  summarise(n = n_distinct(scientificName.x))

spseabirds$familytext = paste0(spseabirds$familyName, " (", spseabirds$n, ")")

spseabirds <- merge(seabirds, spseabirds, by="familyName")

livebirds <- as.data.frame(seabirds)

livebirds$iucn = factor(livebirds$iucn, levels=rev(c("LC", "NT", "VU", "EN", "CR", "DD")))

### removing Extinct and Extinct in the wild

#method one
### livebirds2 = subset(livebirds, iucn != "EX" & iucn != "EW")


### method two
splivebirds <-livebirds %>%
  filter(iucn != "EW") %>%
  filter(iucn != "EX")  %>% ## filters remove the rows with "EX" similar to subset
  group_by(familyName) %>%
  summarise(n = n_distinct(scientificName.x))

splivebirds$familytext = paste0(splivebirds$familyName, " (", splivebirds$n, ")")

splivebirds <- merge(livebirds, splivebirds, by="familyName")

splivebirds <- splivebirds %>%
  drop_na(iucn)

### Change family text to reflect new numbers without EX and EW

ggplot(splivebirds, aes(x = familytext, group = iucn, fill = iucn)) +
  geom_bar(position = "fill") + 
  theme_minimal() + 
  theme(text = element_text(size=12),
        plot.title = element_text(hjust =0.2),
        axis.text.x = element_text(angle= 60, vjust = 1, hjust=1)) +
  ggtitle("Global IUCN threat categories in families containing seabird species") +
  xlab("Family") + ylab("Proportion of family in IUCN category") +
  scale_fill_manual(values = c(
    "DD" = "grey",
    "CR" = "red", 
    "EN" = "orange", 
    "VU" = "yellow", 
    "NT" = "lightgreen", 
    "LC" = "darkgreen"),
    name = "IUCN category",
    labels=c("Data Deficient",
    "Critically Endangered",
             "Endangered",
             "Vulnerable",
             "Near Threatened",
             "Least Concern"))


#### Neater with axes labels

spnoiucn$familytextcp <- toupper(spnoiucn$familytext)

ggplot(spnoiucn, aes(x =familytextcp, group = iucn, fill = iucn)) +
  geom_bar(position = "fill", show.legend = FALSE) +
  theme_minimal() +
  theme(text = element_text(size=12),
        plot.title = element_text(hjust =0.5),
        axis.text.x = element_text(angle= 60, vjust = 1, hjust=1)) +
  ggtitle("IUCN threat categories of seabirds studied with remote sensing") +
  xlab("Family") + ylab("Proportion of family in IUCN category") +
  scale_fill_manual(values = c("CR" = "red",
                               "EN" = "orange",
                               "VU" = "yellow",
                               "NT" = "lightgreen",
                               "LC" = "darkgreen"),
                    name = "IUCN category",
                    labels=c("Critically Endangered",
                             "Endangered",
                             "Vulnerable",
                             "Near Threatened",
                             "Least Concern"))

### test whether the distribution of species within IUCN categories is
## significantly different between the sysrev and iucn data

# compare the totals 

families = unique(spnoiucn$family)

for(i in 1:length(families))
{
  thisfamily = families[i]
  thisfamilydata = subset(spnoiucn, family == thisfamily)
  thisfamilyiucn = subset(splivebirdsnona, familyName == toupper(thisfamily))
  prop.test(thisfamilydata$iucn, thisfamilyiucn$iucn)
}

thisfamily

splivebirdsnona

prop.test(thisfamilydata$iucn, thisfamilyiucn$iucn)

prop.test(spnoiucn$iucn, splivebirdsnona)

### x and y are different lengths, do I need to make it a proportion instead

### proportions of different threat categories in each dataset

## manually (length of tdiff cats)
