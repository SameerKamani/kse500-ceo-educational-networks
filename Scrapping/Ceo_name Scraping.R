install.packages(c("writexl"))

library(readxl)
library(writexl)
library(rvest)
library(httr)
library(dplyr)


df <- read_excel("KSE500_List.xlsx")


for (i in 1:nrow(df)) {
  ceo_name <- df$`Ceo Name`[i]
  
  
  if (is.na(ceo_name) || ceo_name == "") {
    symbol <- df$Symbol[i]
    url <- paste0("https://dps.psx.com.pk/company/", symbol)
    cat("Fetching CEO for", symbol, "...\n")
    
    
    res <- try(GET(url, timeout(10)), silent = TRUE)
    if (inherits(res, "try-error") || status_code(res) != 200) {
      cat("❌ Failed to fetch", symbol, "\n")
      next
    }
    
    
    page <- read_html(content(res, as = "text", encoding = "UTF-8"))
    
    
    tables <- html_elements(page, "table.tbl")
    
    ceo_found <- NA
    
    for (tbl in tables) {
      rows <- html_elements(tbl, "tr")
      for (row_html in rows) {
        cols <- html_elements(row_html, "td")
        if (length(cols) == 2) {
          role <- html_text(cols[2], trim = TRUE)
          if (grepl("CEO", role, ignore.case = TRUE)) {
            ceo_found <- html_text(cols[1], trim = TRUE)
            break
          }
        }
      }
      if (!is.na(ceo_found)) break
    }
    
    
    if (!is.na(ceo_found)) {
      df$`Ceo Name`[i] <- ceo_found
      cat("✅ Found CEO for", symbol, ":", ceo_found, "\n")
    } else {
      cat("⚠️ CEO not found for", symbol, "\n")
    }
    
    
    Sys.sleep(1.5)
  }
}


output_file <- "KSE500_List_updated.xlsx"
write_xlsx(df, output_file)

cat("\n✅ Done! Updated file saved as:", output_file, "\n")
