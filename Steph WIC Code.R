
#install.packages('plyr')
#install.packages('googledrive')
#install.packages('googlesheets4')
#install.packages('readxl')
#install.packages('tidyverse')

library(plyr)
library(googledrive)
library(googlesheets4)
library(readxl)
library(tidyverse)

drive_auth() #respond to prompt in console before continuing
gs4_auth() #respond to prompt in console before continuing

columns <- read_sheet(ss = '1MMfmHSoEBb1_Wmr0ncTMNhsD_3t5-GVe4dlQz7ukwJI')

WIC_files <- drive_find(q = "name contains 'Lowes*'") 

WIC_list <- list()

for (i in WIC_files$id) {
  temp <- tempfile(fileext = ".xlsx")
  dl <- drive_download(
    as_id(i), path = temp, overwrite = T
  )
  j <- read_xlsx(temp)
  if (length(j$...10) == 3) {
    print('all null')
  } 
  else {
    skip_num <- sum(is.na(j$...10)) + 2
    colnames(j) <- j[skip_num-1,]
    if (skip_num > 3) {
      k <- j[skip_num:nrow(j),] %>%
        select(columns$t2) %>%
        rename_at(vars(columns$t2), ~columns$t1) %>%
        mutate(Local_Date = as.Date(`Local Date Time`, format = '%m/%d/%Y'))
        }
    else {
      k <- j[skip_num:nrow(j),]  %>%
        mutate(Local_Date = as.Date(as.double(`Local Date Time`), origin = '1900-01-01'))
    }
    l <- k %>%
      mutate(Category = as.factor(Category),
             Category_Desc  = `Category Description`,
             Sub_Category = as.factor(`Sub-Category`),
             Sub_Category_Desc = `Sub-Category Description`,
             UPC_PLU  = as.factor(`UPC/PLU`),
             UPC_PLU_Desc = `UPC/PLU Description`,
             Vendor_ID = as.factor(`Vendor ID`),
             Vendor_Name = as.factor(`Vendor Name`),
             Peer_Group = as.factor(`Peer Group`),
             Claim_File_Name = as.factor(`Claim File Name`),
             UOM = as.factor(UOM),
             Purchased_Units = as.double(`Purchased Units`),
             Purchased_Quantity = as.double(`Purchase Quantity`),
             Claim_Amount_Total = as.double(`Claim Amount Total`),
             Paid_Amount_Total = as.double(`Paid Amount Total`),
             Adj_Amount_Total = as.double(`Adj Amount Total`),
             E2_MRC = as.factor(`E2 MRC`),
             D4_MRC = as.factor(`D4 MRC`),
             NTE = as.numeric(NTE),
             Override = as.factor(Override),
             Claim_Amount_Per_Item = as.double(`Claim Amount Per Item`),
             Paid_Amount_Per_Item = as.double(`Paid Amount Per Item`),
             Amt_for_NTE_Calc = as.double(`Amt for NTE Calculation`),
             Package_Size = as.double(`Package Size`)) %>%
      select (Category, Category_Desc, Sub_Category, Sub_Category_Desc, UPC_PLU, UPC_PLU_Desc, Vendor_ID, Vendor_Name, Peer_Group, Claim_File_Name,
              Local_Date, UOM, Purchased_Units, Purchased_Quantity, Claim_Amount_Total, Paid_Amount_Total, Adj_Amount_Total, E2_MRC, D4_MRC, NTE, Override,
              Claim_Amount_Per_Item, Paid_Amount_Per_Item, Paid_Amount_Per_Item, Amt_for_NTE_Calc, Package_Size)
    WIC_list[[WIC_files %>% filter(id ==i) %>% pull(name)]] <- l
    }
  }

WIC_set <- do.call('rbind',WIC_list) %>%
  mutate(row_num = row_number())

#columns that contain nulls:
names(which(colSums(is.na(WIC_set)) > 0))

setwd('~/') #set your working directory ie the place you want to write the final file

write.csv(WIC_set,'WIC_set.csv')



#temp <- tempfile(fileext = '.xlsx')
#dl <- drive_download(
#  as_id('1aIczGEK9BlM6atYgcmjGYQRliJzj2RYv'), path = temp, overwrite = T)
#t1 <- read_xlsx(temp)
#skip_num <- sum(is.na(t1$...10)) + 2
#colnames(t1) <- t1[skip_num-1,]
#t1 <- t1[skip_num:nrow(t1),]
#t1 %>%
#  mutate(Local_Date = as.Date(as.double(`Local Date Time`), origin = '1900-01-01')) %>%
#  select(Local_Date)

# temp <- tempfile(fileext = '.xlsx')
# dl <- drive_download(
#   as_id('1WQoc89ijRSW9rPea_QMQB4TooNE0-4iC'), path = temp, overwrite = T)
# t2 <- read_xlsx(temp) %>%
#   select(-`...12`)
# skip_num <- sum(is.na(t2$...10)) + 2
# colnames(t2) <- t2[skip_num-1,]
# t2 <- t2[skip_num:nrow(t2),]
# t2 %>%
#   select(columns$t2) %>%
#   rename_at(vars(columns$t2), ~columns$t1) %>%
#   mutate(Local_Date = as.Date(`Local Date Time`, format = '%m/%d/%Y')) %>%
#   select(Local_Date)
