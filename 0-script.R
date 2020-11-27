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

## Windows
ebird_data <- read_csv("ebird.csv", 
                 col_names = FALSE
                 )

## mac
# ebird_data <- read_tsv("ebird.csv", 
#                        col_names = FALSE
# )

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

## make xlsx ####
head(mtcars)
library(xlsx)
write.xlsx(mtcars, "R_mtcars_data.xlsx", sheetName = "raw_data")

## xlconnect ####

## http://www.sthda.com/english/articles/2-r/4-xlconnect-read-write-and-manipulate-microsoft-excel-files-from-within-r/
# require(devtools)

# Installs the master branch of XLConnect (= current development version)
# install_github("miraisolutions/xlconnect")

# Installs XLConnect 0.2-14
# install_github("miraisolutions/xlconnect", ref = "0.2-14")

library(rJava)

# install.packages("XLConnect")
library(XLConnect)
## java 8-11 needed
## works on MLKG windows machine

## Connect to xlsx ##

# createWorkbook(type = "xlsx")
# createSheet(wb, name = "sheet_alpha")

## Not run: R_mtcars_data.xlsx
# Load workbook; create if not existing
#wb <- loadWorkbook("XLConnect.xlsx")
wb <- loadWorkbook("R_mtcars_data.xlsx", create = TRUE)

wb_data <- readWorksheet(wb, sheet = "raw_data")

createSheet(wb, "mtcars_cleaned_data")

colnames(wb_data)[1] <- "MakeModel"

library(tidyverse)

wb_data %>% 
  arrange(desc(mpg))

wb_data <- wb_data %>%
  mutate(mpg_by_wt = mpg * wt)

wb_data <- wb_data %>%
  arrange(desc(mpg_by_wt))

colnames(wb_data)

mpg_plot <- wb_data %>% ggplot(aes(x = mpg, y = mpg_by_wt)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  # facet_wrap(vars(cyl)) +
  facet_wrap(~cyl)


## https://www.r-bloggers.com/2016/03/few-steps-to-connect-r-with-excel-xlconnect-2/
library(ggplot2)
fileGraph <- paste(outDir,'graph.png',sep='/')
png(filename = fileGraph, width = 1800, height = 1600)

ozone.plot <- ggplot(dtAir, aes(x=Day, y=Ozone)) + 
  geom_point() + 
  geom_smooth()+
  facet_wrap(~Month, nrow=1)
print(ozone.plot)
invisible(dev.off())
addImage(exc2,fileGraph, 'OzonePlot',TRUE)
saveWorkbook(exc2)library(ggplot2)
fileGraph <- paste(outDir,'graph.png',sep='/')
png(filename = fileGraph, width = 800, height = 600)
ozone.plot <- ggplot(dtAir, aes(x=Day, y=Ozone)) + 
  geom_point() + 
  geom_smooth()+
  facet_wrap(~Month, nrow=1)
print(ozone.plot)
invisible(dev.off())
addImage(exc2,fileGraph, 'OzonePlot',TRUE)
saveWorkbook(exc2)


writeWorksheet(wb, wb_data, sheet = "mtcars_cleaned_data", startRow = 3, startCol = 4)

saveWorkbook(wb)


## plot

# Create a named region called 'boxplot' referring to the sheet
# called 'boxplot' 
createName(wb, name = "boxplot", formula = "boxplot!$B$2")

# Create R plot to a png device
png(filename = "boxplot.png", width = 800, height = 600)
boxplot(mpg ~ mpg_by_wt, data = wb_data, col = "lightgray")
dev.off()

# Write image to the named region created above
library(lattice)
addImage(wb, filename = "boxplot.png", name="boxplot", originalSize = TRUE)

# Save workbook (this actually writes the file to disk)
saveWorkbook(wb)



# # Create a worksheet
# createSheet(wb, name = "mtcars")
# 
# # Create a name reference
# createName(wb, name = "mtcars", formula = "mtcars!$C$5")
# 
# # Write built-in data.frame 'mtcars' to the specified named region
# writeNamedRegion(wb, mtcars, name = "mtcars")
# 
# # Save workbook
# saveWorkbook(wb)
# 
# # clean up 
# file.remove("XLConnect.xlsx")
# 
# ## End(Not run)