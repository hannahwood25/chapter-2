#### treemap


# 
install.packages("treemap")
library(treemap)

# Create data
group <- c("group-1","group-2","group-3")
value <- c(13,5,22)
treemapdata <- data.frame(group,value)

# treemap
treemap(treemapdata,
        index="group",
        vSize="value",
        type="index"
)


### multiple groups

# Build Dataset
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
treemapdata <- data.frame(group,subgroup,value)

# treemap
treemap(treemapdata,
        index=c("group","subgroup"),
        vSize="value",
        type="index"
) 



### My data ##########


##### LAYER ONE #####
## Trying to get just the variables to begin with

treemap <- read.csv("/Users/hannahwood/Desktop/current data files/treemapdata.csv")


## check column names
head(treemap)

## select just the column of environmental variables
variablename <- treemap["Variable"]


### create a dataframe with a column of counts for each variable type

variable <- as.data.frame(table(variablename))


head(variable)
head(varsens)# make the treemap

treemap(variable,
        index="Variable",
        vSize="Freq",
        type="index"
)

### It works!! :)

### but is too big for the plot tab


### LAYER TWO #######

####### Variable and satellite #####

treemap <- read.csv("/Users/hannahwood/Desktop/current data files/treemapdata.csv")

head(treemap)

## checking that the variables re factors
# is.factor(treemap$sensor)
# as.factor(treemap$sensor)


### create a dataframe with both columns plis dataframe adds a count column
varsens <- as.data.frame(table(treemap$Variable, treemap$sensor))

print(varsens)

## from Emma code for another method to group and count combined.
## summarise(group_by(treemap, Variable, sensor),count =n())


# treemap

treemap(varsens,
        index=c("Var1", "Var2"),
        vSize="Freq",
        type="index"
)

### this works but i need to tidy the data