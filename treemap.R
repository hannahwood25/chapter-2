### Treemap  #####

####### Packages needed #######

# tidyr package for separating rows
library(tidyr)

### for removing (trimming) spaces from around things
install.packages('stringr')
library('stringr')


###data ####

sysmap <- read_csv("OneDrive - King's College London/Chapter 2/Chapter 2 R/SysMap.csv")


### make dataframe for SRS data


srs <- data.frame(sysmap$`Type of remote sensing`,
                  sysmap$`Repository (website) source of remotely sensed data`,
                  sysmap$`Satellite Source/Space Agency`,
                  sysmap$`Satellite platform for remote sensing data`,
                  sysmap$`Satellite sensor used`,
                  sysmap$`Satellite Product/Environmental Variables`,
                  sysmap$`Derived metrics`)


#### sort out the rows with multiple data in

separate_rows()

studylength <- separate_rows(srs, Study.length..years., sep = ";;;")

## Create new csv with column separated
### visual check of data via csv

write.csv(srs, file = "srs.csv")





### remove NAs, UAVs, Aircraft (manned) and other (anything but satellite)

== "satellite"