##load necessary packages

library(dplyr)
library(tidyr)
library(readxl)
library(fuzzyjoin)

## read in raw data and transform

tools <- read_excel("C://users//perry//downloads//Tool Database.xlsx") %>%
    ## keep only Acronym and url columns
    select(Acronym, starts_with("Home URL"), starts_with("url")) %>%
    ## create a value to match via regex to "hits" or "downloads" files
    ## that will identify a match for any Page url value that 
    ## starts with a url in any tool bucket url column for a product
    unite("regex_url", `Home URL1`:url12, sep = "|^", na.rm = TRUE) %>%
    ## fix to include starts with indicator on first url
    mutate(regex_url = paste0("^", regex_url))

## read in "hits" data
hits <- read.csv("C://users//perry//downloads//MBR Online Tools Report_Aug-Oct 2021.csv")

## join hits to tools on any regex match of the Page value to a project
## with any url that the Page url starts with
test <- hits %>%
    regex_left_join(tools, by = c("Page" = "regex_url"))

## identify and isolate duplicates
dupes <- test %>%
    group_by(Page) %>%
    mutate(count = n_distinct(Acronym)) %>%
    filter(count > 1) %>%
    select(Page, Acronym, regex_val)

## identify how many Pages had no match
nomatch <- test %>%
    filter(is.na(Acronym))
