# Load required libraries
library(rvest)
library(dplyr)


url <- "https://stockanalysis.com/list/pakistan-stock-exchange"


webpage <- read_html(url)


table_node <- html_node(webpage, xpath = "/html/body/div/div[1]/div[2]/main/div[2]/div/div/div[4]/table")

psx_data <- html_table(table_node, fill = TRUE)


colnames(psx_data) <- make.names(colnames(psx_data))


print(head(psx_data))


write.csv(psx_data, "KSE500_List.csv", row.names = FALSE)

getwd()

