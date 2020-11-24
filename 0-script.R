## Adam Hughes
## UARK VCRI
##
## 2020-11-04 METHODS uark meeting
##
## Real Data
## xlsx data
## fast calculation
## XLConnect
## 
## referenced from https://learn.datacamp.com/courses/introduction-to-importing-data-in-r
##
## Load Library ####
# install.packages("tidyverse")
library(tidyverse)

ebird_data <- read_tsv("ebird.csv", 
                 col_names = FALSE
                 )

colnames(ebird_data)
View(ebird_data)

ebird_data %>%
  filter(X5 == "Mallard")

ebird_data %>%
  filter(X15 == "Arkansas")

unique(ebird_data$X15)

ebird_data_sample <- ebird_data %>%
  mutate(ID = paste0(X31, "_", X32)) %>%
  select(X2, ID, X30, X3, X5, X6, X9, X33, X43, X23)

## filter out "X" replace with "0"
ebird_data_sample %>% filter(X9 == "X")

new <- ebird_data_sample %>% str_replace("X", "0")


ebird_data_sample %>%
  group_by(ID) %>%
  # head(100) %>%
  summarise(freq = sum(X43)) %>%
  arrange(desc(freq)) 





hist_test <- ebird_data_sample %>%
  group_by(ID) %>%
  # head(100) %>%
  summarise(freq = sum(X43)) %>%
  arrange(desc(freq)) %>%
  head(10)

hist_test %>%
  ggplot(aes(ID)) +
      geom_histogram(stat = freq)



# install.packages("xlsx")
library(xlsx)

## Read Data ####
data <- read.xlsx(file = "ebird_small.xlsx", 
                  sheetName = "raw_tab_seperated", 
                  header = FALSE)

head(data)

simple_data <- data %>%
  select(X2:X6)

summary(data)


## xlconnect ####
# require(devtools)

# Installs the master branch of XLConnect (= current development version)
# install_github("miraisolutions/xlconnect")

# Installs XLConnect 0.2-14
# install_github("miraisolutions/xlconnect", ref = "0.2-14")

library(rJava)

# install.packages("XLConnect")
library(XLConnect)
## java 8-11 needed

## Connect to xlsx ##

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