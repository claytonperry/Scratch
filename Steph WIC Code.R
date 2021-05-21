set_wd('~/') #set your working directory ie the place you want to write the final file

library(plyr)
library(googledrive)
library(googlesheets4)
library(readxl)

drive_auth()

WIC_files <- drive_find(pattern = 'Lowes')

WIC_list <- list()

for (i in WIC_files$id) {
  temp <- tempfile(fileext = ".xlsx")
  dl <- drive_download(
    as_id(i), path = temp, overwrite = T
  )
  j <- read_xlsx(temp)
  skip_num <- sum(is.na(j$...10)) + 2
  colnames(j) <- j[skip_num-1,]
  WIC_list[[i]] <- j[skip_num:nrow(j),]
  }

WIC_set <- do.call('rbind.fill',WIC_list)

write.csv(WIC_set,'WIC_set.csv')
