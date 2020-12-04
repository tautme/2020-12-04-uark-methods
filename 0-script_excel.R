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
## DO NOT LOAD ANY OTHER PACKAGE BEFORE XLCONNECT OR IT WILL NOT WORK
## IF SO, RESTART R in Session menu, or Ctrl+Shift+F10

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

## Load Library ####
# install.packages("tidyverse")
library(tidyverse)

wb_data %>% 
  arrange(desc(mpg))

wb_data <- wb_data %>%
  mutate(mpg_by_wt = mpg * wt)

wb_data <- wb_data %>%
  arrange(desc(mpg_by_wt))

colnames(wb_data)

## write to sheet
writeWorksheet(wb, wb_data, sheet = "mtcars_cleaned_data", startRow = 5, startCol = 5)

wb_data %>% ggplot(aes(x = mpg, y = mpg_by_wt)) +
  geom_point()




wb_data %>% ggplot(aes(x = mpg, y = mpg_by_wt)) +
  geom_point() +
  # geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  # facet_wrap(vars(cyl)) +
  facet_wrap(~cyl)



createSheet(wb, name = "mpg_plot")
createName(wb, name = "mpg_plot", formul = "mpg_plot!$B$2")

png(filename = "mpg_plot.png", width = 800, height = 600)
wb_data %>% ggplot(aes(x = mpg, y = mpg_by_wt)) +
  geom_point()
dev.off()
addImage(wb, filename = "mpg_plot.png", name = "mpg_plot", originalSize = TRUE)

saveWorkbook(wb)




# writeWorksheet(wb, mpg_plot, sheet = "mtcars_cleaned_data", startRow = 5, startCol = 20)
# addImage(wb, filename = "mpg_plot.png", name = "mpg_plot", originalSize = TRUE)
# 
# createSheet(wb, name = "boxplot")
# createName(wb, name = "boxplot", formul = "boxplot!$B$2")
# 
# png(filename = "boxplot.png", width = 800, height = 600)
# boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
# dev.off()
# 
# addImage(wb, filename = "boxplot.png", name = "boxplot")
# saveWorkbook(wb)


# Create a worksheet
# createSheet(wb, name = "mtcars")

# Create a name reference
createName(wb, name = "mtcars", formula = "mtcars!$C$5")

# Write built-in data.frame 'mtcars' to the specified named region
# writeNamedRegion(wb, mtcars, name = "mtcars")

# Save workbook
saveWorkbook(wb)




## make xlsx ####
# install.packages("xlsx")
library(xlsx)

## takes Too long to run
# write.xlsx(ebird_data_sample, "ebird_data_sample.xlsx")

head(mtcars)
write.xlsx(mtcars, "R_mtcars_data.xlsx", sheetName = "raw_data")
write.xlsx(mtcars, "R_mtcars_data_original.xlsx", sheetName = "raw_data")




## ebird ####
## Windows
ebird_data <- read_tsv("ebird.csv", 
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

## trim
ebird_data_sample <- ebird_data %>%
  mutate(ID = paste0(X31, "_", X32)) %>%
  select(X2, ID, X30, X3, X5, X6, X9, X33, X43, X23)

## filter out "X" in col X9
ebird_data_sample <- ebird_data_sample %>% filter(X9 != "X")

# new <- ebird_data_sample %>% str_replace("X", "0")


## who is making the most data entries?
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

# hist_test %>%
#   ggplot(aes(ID)) +
#       geom_histogram(stat = freq)

hist(hist_test$freq)




