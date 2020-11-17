## Adam Hughes
## UARK VCRI
##
## 2020-11-04 methods uark meeting
##
## referenced from https://learn.datacamp.com/courses/introduction-to-importing-data-in-r
##
## Load Library ####
# install.packages("tidyverse")
library(tidyverse)
# install.packages("xlsx")
library(xlsx)
# install.packages("XLConnect")
library(XLConnect)

## Read Data ####
data <- read.xlsx(file = "ebird_small.xlsx", 
                  sheetName = "raw_tab_seperated", 
                  header = FALSE)

head(data)

simple_data <- data %>%
  select(X2:X6)

summary(data)


## Connect to xlsx ####

createWorkbook(type = "xlsx")
createSheet(wb, name = "sheet_alpha")

## Not run: 
# Load workbook; create if not existing
wb <- loadWorkbook("XLConnect.xlsx")

# Create a worksheet
createSheet(wb, name = "mtcars")

# Create a name reference
createName(wb, name = "mtcars", formula = "mtcars!$C$5")

# Write built-in data.frame 'mtcars' to the specified named region
writeNamedRegion(wb, mtcars, name = "mtcars")

# Save workbook
saveWorkbook(wb)

# clean up 
file.remove("XLConnect.xlsx")

## End(Not run)