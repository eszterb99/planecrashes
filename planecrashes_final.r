library(rvest)
library(data.table)
library(tidyverse)


# get one accident's information
get_info_of_accident  <- function(my_final_url) {
  t <- read_html(my_final_url)
  row_df <- t %>% 
    html_table() %>% 
    "[["(1)
  names(row_df) <- c('my_key', 'my_value')
  return(spread(row_df, key = my_key, value = my_value))
  
}


# get information for all accidents for a year
get_one_year <- function(year) {
  url <- paste0("http://www.planecrashinfo.com/",year,"/",year,".htm")
  t <- read_html(url)
  
  table_list <- 
    t%>%
    html_table()
  
  number_of_accident <- nrow(table_list[[1]]) -1 
  my_final_urls <- paste0("http://www.planecrashinfo.com/",year,"/",year, "-",1:number_of_accident,  ".htm")
  list_of_df_accidents <- lapply(my_final_urls, get_info_of_accident)
  df <- rbindlist(list_of_df_accidents, fill = T)
  df <- select(df, -c('-'))
  write_rds(df, path = paste0(year, ".rds"))
  print(paste0(year, " ready"))
}


# function to get information for every year listed on the main website
create_final_output <- function(year_list) {
  lapply(year_list, get_one_year)
}


# website with list of years
url1 <- 'http://www.planecrashinfo.com/database.htm'

t1 <- read_html(url1)

dates <- t1 %>% 
  html_nodes('br+ table td :nth-child(1) :nth-child(1)') %>% 
  html_text()

dates <- trimws(dates)

# write .rds files for each year separately
create_final_output(dates)








